(* The codec value ['a t] — the FORMAT-AGNOSTIC core: a shape, the validation gate, neutral-Value I/O,
   and pretty-printing. NO Bson here — the BSON encode/decode functions (decode/to_bson/to_json/…) live
   in the public {!Sift} facade (the [sift.bson] plugin). *)

open Shape
open Engine

(* The codec IS the shape — nothing more. Per-format encode/decode are functions over it, so no format
   is welded into the value. *)
type 'a t = { shape : 'a shape }

let of_shape shape = { shape }

(* run every check against an in-memory value — the encode-side gate, and the form-feedback primitive *)
let validate c v = match run_checks c.shape v with [] -> Ok () | es -> Error es

(* derived pretty-printing — nested documents, lists, options, variants, all readable *)
let pp c fmt v = pretty c.shape fmt v
let show c v = Format.asprintf "%a" (pp c) v
