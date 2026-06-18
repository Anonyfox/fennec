(* The interpreters over a {!Shape.shape} that are NOT the wire codec: normalization, the
   refinement-check phase, the [needs_checks] gate, and pretty-printing. The recursion helpers stay
   private. *)

open Shape

(** Apply the shape's normalizer chain (idempotent; runs BEFORE checks, on decode and encode alike). *)
val normalize : 'a shape -> 'a -> 'a

(** Every refinement violation against an in-memory value — the encode-side gate. Collects all
    (stacked checks, sibling fields, record-level rules), each path-tagged. *)
val run_checks : 'a shape -> 'a -> error list

(** Could {!run_checks} fire for this shape anywhere in its tree? A property of the SHAPE (O(shape),
    alloc-free), so a clean decode can skip the value-walk. A recursive shape answers [true]. *)
val needs_checks : 'a shape -> bool

(** Derived pretty-printing — nested documents, lists, options, variants, all readable. *)
val pretty : 'a shape -> Format.formatter -> 'a -> unit
