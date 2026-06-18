(* Sift — the shape language. One declaration drives BSON + JSON encode/decode, validation,
   pretty-printing, and a neutral [view] reflection. The implementation is split by concern:
   {!Shape} (representation) · {!Engine} (interpreters) · {!Codec} (the [t] value + JSON) ·
   {!Combinators} (the builder surface). This file ties them into one flat [Sift.*] API. *)

include Shape
include Codec
include Combinators

(* The neutral data model — the format-agnostic interchange ({!to_value}/{!of_value} project a typed
   value to/from it). Lean by design; the common denominator BSON, JSON and later formats share. *)
module Value = Value

(* ---- zero-copy decode: straight from a BSON document buffer (SIFT-K2) ------------------- *)

(* Decode a top-level BSON document buffer into the codec's value WITHOUT building a {!Bson.t} tree:
   a single linear pass per document level scans fields into a flat span-tape, and the shape pulls
   exactly the fields it wants, reading each value's bytes straight into the OCaml value. Unwanted
   fields are skipped by their length prefix; keys are matched in place; strings copy only when an
   owned result is demanded. Returns the SAME value and SAME collected errors as {!decode} (the raw
   read mirrors the engine; the refinement phase is the shared one). For the storage/wire fast path —
   {!decode} (over an already-parsed {!Bson.t}) stays the tree entry. *)
let decode_bytes (c : 'a t) (buf : Bigstringaf.t) : ('a, error list) result = Bson_reader.decode_value_bytes c.shape buf

(* Stream a buffer of back-to-back BSON documents: decode each in turn and fold the per-document result
   — query-result / cursor iteration without materialising every value at once. *)
let fold_bytes (c : 'a t) (buf : Bigstringaf.t) (init : 'b) (f : 'b -> ('a, error list) result -> 'b) : 'b = Bson_reader.fold_value_bytes c.shape buf init f

(* Decode ONE top-level field straight from a buffer, reading only its bytes (the rest of the document
   is skipped, never materialized). [None] if absent. The projection primitive — route a raw document
   by its _id or a variant discriminant, or pull a single value, without decoding the whole record. *)
let peek (c : 'a t) (key : string) (buf : Bigstringaf.t) : ('a, error list) result option = Bson_reader.peek_field c.shape key buf

(* Validate a BSON document buffer against the shape WITHOUT building the value — the alloc-free tier.
   Same Ok/Error verdict as [decode_bytes]; alloc-free (scan speed) when the shape's checks are all
   structural, falling back to a full decode for refinement/cross-field rules that need the value. *)
let valid_bytes (c : 'a t) (buf : Bigstringaf.t) : (unit, error list) result = Bson_reader.valid_value_bytes c.shape buf

(* Is [buf] a structurally well-formed BSON document? Shape-agnostic, single pass, zero allocation —
   the fast pre-filter / fuzz oracle (the pure-OCaml analog of libbson's bson_validate, and faster). *)
let scan_valid (buf : Bigstringaf.t) : bool = Bson_reader.scan_valid buf

(* ---- encode mirror (SIFT-K3) ----------------------------------------------------------- *)

(* The EXACT wire byte length of encoding [v] through the codec, WITHOUT encoding it — equals
   [String.length] of the BSON the codec would produce. For pre-sizing a buffer, a Content-Length, a
   quota check; and the foundation of straight-to-buffer encode. *)
let size (c : 'a t) (v : 'a) : int = Engine.size c.shape v

(* Encode [v] straight into a freshly-sized buffer, NO Bson.t tree — byte-identical to encoding through
   the tree then serialising. Single pass; each document's length is backpatched once known. *)
let encode_bytes (c : 'a t) (v : 'a) : Bigstringaf.t = Bson_writer.encode_value_bytes c.shape v

(* RELAXED JSON encode (plain JSON for HTTP APIs), straight from the value, no Bson.t tree — distinct
   from {!to_json_string} (the bridge's CANONICAL extended JSON, lossless for BSON/mongosh interchange).
   Round-trips via {!decode_json} / {!of_json_string}. *)
let encode_json (c : 'a t) (v : 'a) : string = Json_writer.encode_json c.shape v

(* RELAXED JSON decode, NATIVE — parse straight to the neutral {!Value} model then {!of_value}, with
   NO Bson hop (distinct from {!of_json_string}, which routes through the extended-JSON/Bson bridge).
   The inverse of {!encode_json}; validates with the same path-collected errors as {!decode}. *)
let decode_json (c : 'a t) (s : string) : ('a, error list) result =
  match Value_json.of_string s with Ok v -> of_value c v | Error m -> Error [ mkerr ~code:"json" m ]

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
  | TDyn -> V_bson
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

(* ---- derived operations (SIFT-K6): equal / compare / hash / default, free from the shape ----------
   Monomorphic, structural — not OCaml's polymorphic runtime compare/hash. *)

let equal (c : 'a t) (x : 'a) (y : 'a) : bool = Derived.equal c.shape x y
let compare (c : 'a t) (x : 'a) (y : 'a) : int = Derived.compare c.shape x y
let hash (c : 'a t) (v : 'a) : int = Derived.hash c.shape v
let default (c : 'a t) : 'a = Derived.default c.shape

(* structural diff (old → new) as Mongo-style $set/$unset — the reactive-sync / minimal-update primitive *)
type delta = Derived.delta = { set : (string * Bson.t) list; unset : string list }

let diff (c : 'a t) (old : 'a) (new_ : 'a) : delta = Derived.diff c.shape old new_

(* apply a {!delta} to a value — the inverse of {!diff}: [$set] replaces/adds fields, [$unset] removes
   them, then the merged document is decoded (and validated). [patch c old (diff c old new) = Ok new]. *)
let patch (c : 'a t) (old : 'a) (d : delta) : ('a, error list) result =
  let kvs = match c.enc old with Bson.Document kvs -> kvs | _ -> [] in
  let touched = d.unset @ List.map fst d.set in
  let kept = List.filter (fun (k, _) -> not (List.mem k touched)) kvs in
  decode c (Bson.Document (kept @ d.set))

(* a random value conforming to the shape (refinements best-effort via rejection) — property testing.
   Pure given the [Random.State.t], so a seeded state reproduces. *)
let arbitrary (c : 'a t) (st : Random.State.t) : 'a = Derived.arbitrary c.shape st

(* ---- positional parameter lists (DDP method params) — unchanged surface ----------------- *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

(* function forms of the (now-sealed) [args] fields — the method layer marshals params through these *)
let encode_args (a : 'a args) (v : 'a) : Bson.t list = a.enc_args v
let decode_args (a : 'a args) (params : Bson.t list) : ('a, string) result = a.dec_args params

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
