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

(* ---- neutral data model ({!Value}) — the format-agnostic interchange ----------------------------
   [to_value] projects a typed value to the neutral {!Value.t}; [of_value] reads + validates it back.
   Both drive the {!Serde} interpreter over the {!Value_format} instance — a DIRECT neutral-model walk,
   no BSON hop. [of_value] runs the same check phase as a decode. *)
let to_value c v : Value.t = Serde.write (module Value_format.Writer) c.shape v

let of_value c (value : Value.t) =
  match Serde.read (module Value_format.Reader) c.shape value with
  | Error _ as e -> e
  | Ok v -> if not (needs_checks c.shape) then Ok v else ( match run_checks c.shape v with [] -> Ok v | es -> Error es)
