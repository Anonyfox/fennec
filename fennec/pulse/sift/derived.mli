(* Free, shape-derived operations — each composed structurally from the shape, MONOMORPHIC (no
   reliance on OCaml's polymorphic compare/hash). The structural recursion helpers stay private. *)

open Shape

(** Structural equality (a record: all fields equal; a variant: same case + equal bodies). *)
val equal : 'a shape -> 'a -> 'a -> bool

(** Total ordering — lexicographic over fields / list elements; variants by declaration order. *)
val compare : 'a shape -> 'a -> 'a -> int

(** Structural hash (combines field hashes; monomorphic). *)
val hash : 'a shape -> 'a -> int

(** A sensible default/zero: leaves zero out, containers empty, a record built from its fields'
    defaults, a variant taking its first case. *)
val default : 'a shape -> 'a

(** A random value conforming to the shape (refinements met best-effort by rejection sampling). Pure
    given the {!Random.State.t}. *)
val arbitrary : 'a shape -> Random.State.t -> 'a
