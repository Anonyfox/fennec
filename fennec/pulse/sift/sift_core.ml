(* Sift_core — the FORMAT-AGNOSTIC core: the shape language, the combinators, validation, the neutral
   {!Value} model, native relaxed JSON, derived ops, and reflection. A real serde with NO bson
   dependency — usable standalone. The BSON format (decode/to_bson/to_json + the zero-copy byte codecs)
   is the [sift.bson] plugin; the public {!Sift} facade adds it back so fennec's call sites are unchanged.

   The internal modules are re-exported below so the plugin can interpret shapes (it needs the GADT
   constructors); the public {!Sift} mli keeps the representation abstract. *)

module Shape = Shape
module Value = Value
module Engine = Engine
module Serde = Serde
module Value_format = Value_format
module Value_json = Value_json
module Json_writer = Json_writer
module Derived = Derived
module Codec = Codec
module Combinators = Combinators

include Shape
include Codec
include Combinators

(* ---- native relaxed JSON (no Bson) ----------------------------------------------------- *)

(* RELAXED JSON encode — plain JSON straight from the value (no Bson.t tree); the wire form an HTTP
   API sends. *)
let encode_json (c : 'a t) (v : 'a) : string = Json_writer.encode_json c.shape v

(* RELAXED JSON decode, NATIVE — parse straight to the neutral {!Value} model then read + validate,
   NO Bson hop. The inverse of {!encode_json}; same path-collected errors as a decode. *)
let decode_json (c : 'a t) (s : string) : ('a, error list) result =
  match Value_json.of_string s with Ok v -> of_value c v | Error m -> Error [ mkerr ~code:"json" m ]

(* ---- introspection: the neutral reflection renderers consume ----------------------------- *)

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
let field_view (f : 'a field) : view = reflect f.item
let field_required (f : 'a field) : bool = f.needed

(* ---- derived operations: equal / compare / hash / default / arbitrary — free from the shape ----- *)

let equal (c : 'a t) (x : 'a) (y : 'a) : bool = Derived.equal c.shape x y
let compare (c : 'a t) (x : 'a) (y : 'a) : int = Derived.compare c.shape x y
let hash (c : 'a t) (v : 'a) : int = Derived.hash c.shape v
let default (c : 'a t) : 'a = Derived.default c.shape
let arbitrary (c : 'a t) (st : Random.State.t) : 'a = Derived.arbitrary c.shape st
