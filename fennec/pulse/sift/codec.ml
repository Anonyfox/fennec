(* The codec value ['a t] — just a shape, plus the encode-side validation gate and pretty-printing.
   The BSON encode/decode and JSON I/O are functions over it, in the {!Sift} facade. *)

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
