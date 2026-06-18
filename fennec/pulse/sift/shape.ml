(* The shape language. ONE GADT type representation ([shape]) inside; plain combinators outside.
   Everything else derives from [shape]: the codec (decode collects path-tagged errors; encode is
   total), encode-side validation ([validate] / [encode_checked] — an invalid value cannot pass a
   write boundary), normalizers (run BEFORE checks, on both directions), derived pretty-printing
   ([pp]/[show], nested), and the neutral [view] reflection downstream renderers consume
   ($jsonSchema, OpenAPI, admin) without this module knowing them.

   Refinements carry a [hint] — the machine-readable half a renderer can translate (min_len →
   minLength…); an arbitrary [check] carries [H_none] and is honestly app-side-only. Floats reject
   nan/inf by default (a Bson Float can carry them; silently storing them is how data rots).
   Options: absent OR null decode to [None]; [None] encodes by OMITTING the key (Mongo-idiomatic).

   Back-compat: the original combinator surface (string/int/…/req/opt/obj1-4/a0-a3, and the public
   [enc]/[dec] record fields) is preserved verbatim — every existing call site keeps compiling. *)

(* ---- errors ------------------------------------------------------------------ *)

(* [msg] is the human (English-default) message. [code] is a machine-readable rule id ("required",
   "type", "min_len", "one_of", …) and [params] its interpolation values ([("n","80")]) — together
   they let a downstream translator localize without re-parsing the string. *)
type error = { path : string list; msg : string; code : string; params : (string * string) list }

let mkerr ?(code = "") ?(params = []) ?(path = []) msg = { path; msg; code; params }
let err ?(code = "") ?(params = []) path msg = mkerr ~code ~params ~path msg

let error_to_string e =
  match e.path with [] -> e.msg | p -> String.concat "." p ^ ": " ^ e.msg

let errors_to_string errs = String.concat "; " (List.map error_to_string errs)
let at name e = { e with path = name :: e.path }
let fail ?code ?params msg = Error [ mkerr ?code ?params msg ]

(* re-render an error's [msg] through a translator (using its [code]/[params]/[path]); identity-safe. *)
let translate (tr : error -> string) (e : error) : error = { e with msg = tr e }

(* ---- the type representation -------------------------------------------------- *)

(* the renderable half of a refinement — what $jsonSchema/OpenAPI can translate *)
type hint =
  | H_none
  | H_min_len of int
  | H_max_len of int
  | H_pattern of string
  | H_enum of string list
  | H_min of float
  | H_max of float
  | H_multiple_of of float
  | H_min_items of int
  | H_max_items of int
  | H_unique_items

(* a refinement's machine-readable error code + interpolation params, derived from its hint — so a
   length/range/enum check reports ("max_len", [("n","80")]) without per-combinator wiring *)
let code_of_hint = function
  | H_none -> "check"
  | H_min_len _ -> "min_len"
  | H_max_len _ -> "max_len"
  | H_pattern _ -> "pattern"
  | H_enum _ -> "one_of"
  | H_min _ -> "min"
  | H_max _ -> "max"
  | H_multiple_of _ -> "multiple_of"
  | H_min_items _ -> "min_items"
  | H_max_items _ -> "max_items"
  | H_unique_items -> "unique_items"

let fmt_num f = if Float.is_integer f then string_of_int (int_of_float f) else string_of_float f

let params_of_hint = function
  | H_min_len n | H_max_len n | H_min_items n | H_max_items n -> [ ("n", string_of_int n) ]
  | H_pattern p -> [ ("pattern", p) ]
  | H_enum xs -> [ ("values", String.concat ", " xs) ]
  | H_min f | H_max f | H_multiple_of f -> [ ("n", fmt_num f) ]
  | H_none | H_unique_items -> []

type _ shape =
  | TString : string shape
  | TInt : int shape
  | TFloat : { allow_nonfinite : bool } -> float shape
  | TBool : bool shape
  | TDate : int64 shape (* Bson.Date, ms since epoch *)
  | TId : string shape (* "_id" values: String or ObjectId, surfaced as string *)
  | TBson : Bson.t shape (* the dynamic escape hatch — a raw {!Bson.t} value (any shape) *)
  | TUnit : unit shape
  | TList : 'a shape -> 'a list shape
  | TOption : 'a shape -> 'a option shape
  | TMap : 'a shape -> (string * 'a) list shape (* dynamic-key subdocuments *)
  | TCheck : ('a -> bool) * string * hint * 'a shape -> 'a shape
  | TNorm : ('a -> 'a) * 'a shape -> 'a shape
  | TConv : ('b -> 'a) * ('a -> ('b, string) result) * 'a shape -> 'b shape
  | TObj : 'r record_shape -> 'r shape
  | TVariant : { tag : string; cases : 'r case list } -> 'r shape
  | TLazy : 'a shape Lazy.t -> 'a shape (* a self-referential codec ({!fix}) — forced on demand, finite values terminate *)
  | TCoerce : 'a shape -> 'a shape (* {!from_string}: decode also accepts a [String] and parses it to the inner leaf *)

and 'r record_shape = {
  (* the FORMAT-AGNOSTIC decode: pull each field through a {!field_reader}, threading the record's
     constructor. The SAME builder serves the BSON-tree path (the engine indexes the kvs into a reader),
     the zero-copy buffer path (spans scanned straight from bytes), and every other format — the serde
     "Deserializer drives the visitor" inversion. NO format is named here, so this type is Bson-free. *)
  decode_src : field_reader -> ('r, error list) result;
  (* drives BOTH reflection AND encode: each field's name + shape + omit-rule, so any format's encoder
     (the engine's [write_record], the JSON writer, …) reproduces the wire doc without a format-typed
     field stored here. *)
  members : 'r bound_field list;
  invariants : (('r -> bool) * string) list; (* record-level (cross-field) checks *)
}

(* one field of a record: its wire [key], the leaf [item] shape, whether it is [needed] (required),
   and the [fallback] used when absent (the [None]/[[]]/default that makes a field optional) *)
and 'a field = { key : string; item : 'a shape; needed : bool; fallback : 'a option }

(* a pull source of a document's fields: given a [field] (key + shape), produce its decoded value (or
   collected errors). Rank-2 — ONE reader decodes fields of every type. The tree path captures a kvs
   index in the closure; the buffer path captures the scanned span-tape. This is what lets a record's
   constructor be threaded once and fed from ANY format. *)
and field_reader = { read_field : 'a. 'a field -> ('a, error list) result }

and 'r bound_field =
  (* [omit v] is the encode-side rule: does this field, with value [v], get dropped from the wire? (an
     absent optional / an empty opt_list — Mongo-idiomatic absence). Carried here so EVERY format's
     encoder (the engine's [write_record], the JSON writer, the byte writer) reproduces the omission
     from the members alone. *)
  | Bound_field : { name : string; shape : 'a shape; get : 'r -> 'a; required : bool; omit : 'a -> bool } -> 'r bound_field

and 'r case =
  | Case : { name : string; body : 'a record_shape; inject : 'a -> 'r; project : 'r -> 'a option } -> 'r case

(* ---- decode / encode / checks, derived from shape -------------------------------- *)

