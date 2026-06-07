(* Doc-coverage checking for [fennec docs]: parse an interface (or implementation) and report
   exports that lack a [(** ... *)] doc comment. OCaml has no missing-docs lint; this is fennec's. *)

(** A documentable export and whether it carries a doc comment. *)
type item = { kind : string; name : string; line : int; documented : bool }

(** Documentable exports of a parsed [.mli] signature (val / type / exception / module / module
    type). Pure. *)
val interface_items : Ppxlib.signature -> item list

(** Top-level definitions of a parsed [.ml] structure (let / type / exception) — for [--private].
    Pure. *)
val implementation_items : Ppxlib.structure -> item list

(** Run the check over [paths] (files or directories; default: the current project). [strict] →
    exit [1] if any export is undocumented (else [0], warn-only). [private_] → also scan [.ml]
    top-level definitions, not just [.mli] exports. Returns the process exit code. *)
val run : paths:string list -> strict:bool -> private_:bool -> int
