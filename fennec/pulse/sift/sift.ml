(* Sift — the public facade: the bson-free {!Sift_core} PLUS the BSON format (the [sift.bson] plugin —
   the BSON-tree codec, the zero-copy byte codecs, the extended-JSON bridge, the Mongo query-vocabulary
   field helpers). The full [Sift.*] API fennec uses; standalone users can depend on {!Sift_core} alone
   (no bson). *)

include Sift_core

(** Extended-JSON AST (re-exported), for composing with other JSON code. *)
module Json = Fennec_mongo_json.Json

module Bson_json = Fennec_mongo_bson_json.Bson_json

(* ---- the BSON-tree codec ------------------------------------------------------------------ *)

(* decode = RAW shape decode (the BSON-tree read), then the gated check phase — refinement violations
   COLLECT across stacked checks, sibling fields, and record-level rules. *)
let decode_value shape b =
  match Bson_engine.read shape b with
  | Error es -> Error es
  | Ok v -> if not (Engine.needs_checks shape) then Ok v else ( match Engine.run_checks shape v with [] -> Ok v | es -> Error es)

let enc c v = Bson_engine.write c.shape v
let dec c b = match decode_value c.shape b with Ok v -> Ok v | Error es -> Error (errors_to_string es)
let decode c b = decode_value c.shape b
let encode_checked c v = match Engine.run_checks c.shape v with [] -> Ok (enc c v) | es -> Error es
let to_bson c v : Bson.t = enc c v

(* ---- extended-JSON I/O (the Bson_json bridge — $oid/$date, lossless for mongosh interchange) ---- *)
let to_json c v = Bson_json.to_json (enc c v)
let of_json c j = decode_value c.shape (Bson_json.of_json j)
let to_json_string c v = Bson_json.to_string (enc c v)

let of_json_string c s =
  match Bson_json.of_string_opt s with Some b -> decode_value c.shape b | None -> Error [ mkerr ~code:"json" "malformed JSON" ]

(* ---- the dynamic BSON escape + the Mongo query-vocabulary field helpers ------------------- *)

(* the dynamic escape typed as a raw {!Bson.t} — a LOSSLESS conv over the neutral {!dyn} (the rich
   Value mirrors Bson 1:1); keeps the deriver's [Sift.bson]-for-Bson.t-field emit working *)
let bson = conv (fun v -> Ok (Value_bson.to_bson v)) Value_bson.of_bson dyn

let make ~enc ~dec = conv dec enc bson
let field_enc f v = Bson_engine.write f.item v

(* encode ONE element of a list field (for $push-style modifiers) *)
let field_elem_enc (f : 'a list field) (v : 'a) : Bson.t =
  match Bson_engine.write f.item [ v ] with Bson.Array [ x ] -> x | _ -> invalid_arg "Sift.field_elem_enc"

(* decode ONE field out of a document (raw decode + the field's checks) — the projection primitive *)
let field_get (f : 'a field) (doc : Bson.t) : ('a, error list) result =
  match doc with
  | Bson.Document kvs -> (
      let raw =
        match List.assoc_opt f.key kvs with
        | Some v -> ( match Bson_engine.read f.item v with Ok x -> Ok x | Error es -> Error (List.map (at f.key) es))
        | None -> ( match f.fallback with Some d -> Ok d | None -> Error [ mkerr ~code:"required" ~path:[ f.key ] "is required" ])
      in
      match raw with Error es -> Error es | Ok v -> ( match Engine.run_checks f.item v with [] -> Ok v | es -> Error (List.map (at f.key) es)))
  | _ -> Error [ mkerr ~code:"type" ~path:[ f.key ] "expected document" ]

(* ---- zero-copy BSON byte codecs ----------------------------------------------------------- *)

let decode_bytes (c : 'a t) (buf : Bigstringaf.t) : ('a, error list) result = Bson_reader.decode_value_bytes c.shape buf
let fold_bytes (c : 'a t) (buf : Bigstringaf.t) (init : 'b) (f : 'b -> ('a, error list) result -> 'b) : 'b = Bson_reader.fold_value_bytes c.shape buf init f
let peek (c : 'a t) (key : string) (buf : Bigstringaf.t) : ('a, error list) result option = Bson_reader.peek_field c.shape key buf
let valid_bytes (c : 'a t) (buf : Bigstringaf.t) : (unit, error list) result = Bson_reader.valid_value_bytes c.shape buf
let scan_valid (buf : Bigstringaf.t) : bool = Bson_reader.scan_valid buf
let size (c : 'a t) (v : 'a) : int = Bson_engine.size c.shape v
let encode_bytes (c : 'a t) (v : 'a) : Bigstringaf.t = Bson_writer.encode_value_bytes c.shape v

(* ---- structural diff / patch (Mongo-style $set/$unset; the reactive-sync primitive) ------- *)

type delta = { set : (string * Bson.t) list; unset : string list }

let no_change = { set = []; unset = [] }

let rec diff_shape : type a. a shape -> a -> a -> delta =
 fun shape old new_ ->
  match shape with
  | TObj o -> diff_members o.members old new_
  | TMap el -> diff_map el old new_
  | TVariant { tag; cases } -> diff_variant shape tag cases old new_
  | TCheck (_, _, _, inner) -> diff_shape inner old new_
  | TNorm (_, inner) -> diff_shape inner old new_
  | TConv (inj, _, inner) -> diff_shape inner (inj old) (inj new_)
  | TCoerce inner -> diff_shape inner old new_
  | TLazy l -> diff_shape (Lazy.force l) old new_
  | _ -> invalid_arg "Sift.diff: needs a document-shaped codec (record / map / variant)"

and diff_members : type r. r bound_field list -> r -> r -> delta =
 fun members old new_ ->
  let rec go set unset = function
    | [] -> { set = List.rev set; unset = List.rev unset }
    | Bound_field f :: rest ->
        let ov = f.get old and nv = f.get new_ in
        if Derived.equal f.shape ov nv then go set unset rest
        else if f.omit nv then go set (f.name :: unset) rest
        else go ((f.name, Bson_engine.write f.shape nv) :: set) unset rest
  in
  go [] [] members

and diff_map : type a. a shape -> (string * a) list -> (string * a) list -> delta =
 fun el old new_ ->
  let set = List.filter_map (fun (k, nv) -> match List.assoc_opt k old with Some ov when Derived.equal el ov nv -> None | _ -> Some (k, Bson_engine.write el nv)) new_ in
  let unset = List.filter_map (fun (k, _) -> if List.mem_assoc k new_ then None else Some k) old in
  { set; unset }

and diff_variant : type r. r shape -> string -> r case list -> r -> r -> delta =
 fun shape _tag cases old new_ ->
  let rec go = function
    | [] -> no_change
    | Case c :: rest -> (
        match c.project old with
        | Some a -> ( match c.project new_ with Some b -> diff_members c.body.members a b | None -> full_replace shape old new_)
        | None -> go rest)
  in
  go cases

and full_replace : type a. a shape -> a -> a -> delta =
 fun shape old new_ ->
  let fields v = match Bson_engine.write shape v with Bson.Document kvs -> kvs | _ -> [] in
  let new_fields = fields new_ in
  let new_names = List.map fst new_fields in
  { set = new_fields; unset = List.filter_map (fun (n, _) -> if List.mem n new_names then None else Some n) (fields old) }

let diff (c : 'a t) (old : 'a) (new_ : 'a) : delta = diff_shape c.shape old new_

let patch (c : 'a t) (old : 'a) (d : delta) : ('a, error list) result =
  let kvs = match enc c old with Bson.Document kvs -> kvs | _ -> [] in
  let touched = d.unset @ List.map fst d.set in
  let kept = List.filter (fun (k, _) -> not (List.mem k touched)) kvs in
  decode c (Bson.Document (kept @ d.set))

(* ---- positional parameter lists (DDP method params) ----------------------------------------- *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

let encode_args (a : 'a args) (v : 'a) : Bson.t list = a.enc_args v
let decode_args (a : 'a args) (params : Bson.t list) : ('a, string) result = a.dec_args params
let a0 = { enc_args = (fun () -> []); dec_args = (function [] -> Ok () | _ -> Error "expected no arguments") }

let a1 c =
  { enc_args = (fun a -> [ enc c a ]);
    dec_args = (function [ x ] -> dec c x | l -> Error (Printf.sprintf "expected 1 argument, got %d" (List.length l))) }

let a2 c1 c2 =
  { enc_args = (fun (a, b) -> [ enc c1 a; enc c2 b ]);
    dec_args =
      (function
      | [ x; y ] -> ( match (dec c1 x, dec c2 y) with Ok a, Ok b -> Ok (a, b) | Error e, _ | _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 2 arguments, got %d" (List.length l))) }

let a3 c1 c2 c3 =
  { enc_args = (fun (a, b, c) -> [ enc c1 a; enc c2 b; enc c3 c ]);
    dec_args =
      (function
      | [ x; y; z ] -> (
          match (dec c1 x, dec c2 y, dec c3 z) with
          | Ok a, Ok b, Ok c -> Ok (a, b, c)
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 3 arguments, got %d" (List.length l))) }
