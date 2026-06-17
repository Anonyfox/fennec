(* Sift — the shape language. One declaration drives BSON + JSON encode/decode, validation,
   pretty-printing, and a neutral [view] reflection. The implementation is split by concern:
   {!Shape} (representation) · {!Engine} (interpreters) · {!Codec} (the [t] value + JSON) ·
   {!Combinators} (the builder surface). This file ties them into one flat [Sift.*] API. *)

include Shape
include Codec
include Combinators

(* ---- zero-copy decode: straight from a BSON document buffer (SIFT-K2) ------------------- *)

(* Decode a top-level BSON document buffer into the codec's value WITHOUT building a {!Bson.t} tree:
   a single linear pass per document level scans fields into a flat span-tape, and the shape pulls
   exactly the fields it wants, reading each value's bytes straight into the OCaml value. Unwanted
   fields are skipped by their length prefix; keys are matched in place; strings copy only when an
   owned result is demanded. Returns the SAME value and SAME collected errors as {!decode} (the raw
   read mirrors the engine; the refinement phase is the shared one). For the storage/wire fast path —
   {!decode} (over an already-parsed {!Bson.t}) stays the tree entry. *)
let decode_bytes (c : 'a t) (buf : Bigstringaf.t) : ('a, error list) result = Bson_reader.decode_value_bytes c.shape buf

(* Decode ONE top-level field straight from a buffer, reading only its bytes (the rest of the document
   is skipped, never materialized). [None] if absent. The projection primitive — route a raw document
   by its _id or a variant discriminant, or pull a single value, without decoding the whole record. *)
let peek (c : 'a t) (key : string) (buf : Bigstringaf.t) : ('a, error list) result option = Bson_reader.peek_field c.shape key buf

(* ---- introspection: the neutral reflection renderers consume + positional params ---- *)

type view =
  | V_string
  | V_int
  | V_float
  | V_bool
  | V_date
  | V_id
  | V_bson
  | V_unit
  | V_list of view
  | V_option of view
  | V_map of view
  | V_check of hint * view
  | V_obj of (string * bool (* required *) * view) list
  | V_variant of string * (string * (string * bool * view) list) list

let rec reflect : type a. a shape -> view = function
  | TString -> V_string
  | TInt -> V_int
  | TFloat _ -> V_float
  | TBool -> V_bool
  | TDate -> V_date
  | TId -> V_id
  | TBson -> V_bson
  | TUnit -> V_unit
  | TList el -> V_list (reflect el)
  | TOption el -> V_option (reflect el)
  | TMap el -> V_map (reflect el)
  | TCheck (_, _, h, inner) -> V_check (h, reflect inner)
  | TNorm (_, inner) -> reflect inner
  | TConv (_, _, inner) -> reflect inner
  | TObj o -> V_obj (List.map (fun (Bound_field p) -> (p.name, p.required, reflect p.shape)) o.members)
  | TVariant { tag; cases } ->
      V_variant
        (tag,
         List.map
           (fun (Case c) -> (c.name, List.map (fun (Bound_field p) -> (p.name, p.required, reflect p.shape)) c.body.members))
           cases)
  | TCoerce inner -> reflect inner (* transparent: a coerced leaf reflects as its inner type *)
  | TLazy _ -> V_bson (* recursion: opaque to schema reflection (a finite view can't unfold a cycle) *)

let view c = reflect c.shape

(* the structural view of ONE field's shape — drives form input-type inference + HTML5 constraint
   attributes (a renderer reads the leaf kind + refinement hints without touching the GADT) *)
let field_view (f : 'a field) : view = reflect f.item
let field_required (f : 'a field) : bool = f.needed

(* ---- positional parameter lists (DDP method params) — unchanged surface ----------------- *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

let a0 = { enc_args = (fun () -> []); dec_args = (function [] -> Ok () | _ -> Error "expected no arguments") }

let a1 c =
  { enc_args = (fun a -> [ c.enc a ]);
    dec_args = (function [ x ] -> c.dec x | l -> Error (Printf.sprintf "expected 1 argument, got %d" (List.length l))) }

let a2 c1 c2 =
  { enc_args = (fun (a, b) -> [ c1.enc a; c2.enc b ]);
    dec_args =
      (function
      | [ x; y ] -> ( match (c1.dec x, c2.dec y) with Ok a, Ok b -> Ok (a, b) | Error e, _ | _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 2 arguments, got %d" (List.length l))) }

let a3 c1 c2 c3 =
  { enc_args = (fun (a, b, c) -> [ c1.enc a; c2.enc b; c3.enc c ]);
    dec_args =
      (function
      | [ x; y; z ] -> (
          match (c1.dec x, c2.dec y, c3.dec z) with
          | Ok a, Ok b, Ok c -> Ok (a, b, c)
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 3 arguments, got %d" (List.length l))) }
