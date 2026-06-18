(* The BSON codec — shape-directed read / write / size over {!Bson.t}. This IS Sift's encode/decode
   (one tree pass, no intermediate). The helpers (coercion, sizing, oid detection, …) stay private. *)

open Shape

(** Decode a {!Bson.t} into the shape's value. RAW phase: structural + type errors only — refinements
    run in {!Engine.run_checks}. Errors are path-tagged and collect across fields. *)
val read : 'a shape -> Bson.t -> ('a, error list) result

(** Encode a value to {!Bson.t}. TOTAL — a typed value always serializes; the encode-side refinement
    gate lives in {!Engine.run_checks}, not here. *)
val write : 'a shape -> 'a -> Bson.t

(** The EXACT wire byte length {!write} would encode to, without building the bytes (for pre-sizing). *)
val size : 'a shape -> 'a -> int
