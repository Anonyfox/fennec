(* The codec value ['a t] — a shape paired with its compiled BSON encode/decode — and the operations
   over it (decode / validate / encode_checked / pp / show) + JSON I/O. *)

open Shape
open Engine

type 'a t = { shape : 'a shape; enc : 'a -> Bson.t; dec : Bson.t -> ('a, string) result }

(* decode = RAW shape decode, then the check phase — so refinement violations COLLECT across
   stacked checks, sibling fields, and record-level rules instead of short-circuiting *)
let decode_value shape b =
  match read shape b with
  | Error es -> Error es
  | Ok v -> if not (needs_checks shape) then Ok v else ( match run_checks shape v with [] -> Ok v | es -> Error es)

let of_shape shape =
  { shape;
    enc = write shape;
    dec = (fun b -> match decode_value shape b with Ok v -> Ok v | Error es -> Error (errors_to_string es)) }

let decode c b = decode_value c.shape b
let validate c v = match run_checks c.shape v with [] -> Ok () | es -> Error es
let encode_checked c v = match run_checks c.shape v with [] -> Ok (c.enc v) | es -> Error es
let pp c fmt v = pretty c.shape fmt v
let show c v = Format.asprintf "%a" (pp c) v

(* ---- JSON I/O — one shape drives BOTH BSON and JSON, via the extended-JSON bridge --------------- *)
(* The shape is described ONCE; [enc]/[dec] speak BSON (the storage form), these speak JSON (the wire
   form for APIs). No separate serializer: DB shape = wire shape. Encode is total (mirrors [enc]);
   decode validates with the same path-collected errors as {!decode}. *)
module Json = Fennec_mongo_json.Json
module Bson_json = Fennec_mongo_bson_json.Bson_json

let to_json c v = Bson_json.to_json (c.enc v)
let of_json c j = decode_value c.shape (Bson_json.of_json j)
let to_json_string c v = Bson_json.to_string (c.enc v)

let of_json_string c s =
  match Bson_json.of_string_opt s with Some b -> decode_value c.shape b | None -> Error [ mkerr ~code:"json" "malformed JSON" ]

(* ---- neutral data model ({!Value}) — the format-agnostic interchange ----------------------------
   [to_value] projects a typed value to the neutral {!Value.t} (the shape's semantics are erased to
   the common denominator: a date becomes an [Int], an id a [String]); [of_value] reads + validates a
   value back from it. Both drive the {!Format} interpreter over the {!Value_format} instance — a
   DIRECT neutral-model walk, no BSON hop. [of_value] runs the same check phase as {!decode}. The
   neutral lens a generic renderer / serializer consumes. *)
let to_value c v : Value.t = Serde.write (module Value_format.Writer) c.shape v

let of_value c (value : Value.t) =
  match Serde.read (module Value_format.Reader) c.shape value with
  | Error _ as e -> e
  | Ok v -> if not (needs_checks c.shape) then Ok v else ( match run_checks c.shape v with [] -> Ok v | es -> Error es)

(* TOTAL BSON encode (mirrors {!to_value}/{!to_json}) — a typed value always serializes; the encode-side
   refinement gate is {!validate}/{!encode_checked}. The function form of the (now-sealed) [enc] field. *)
let to_bson c v : Bson.t = c.enc v

(* ---- primitives ------------------------------------------------------------------ *)
