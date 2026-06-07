(* Doc-coverage for [fennec test docs]: parse an interface (or implementation) and report exports
   that lack a [(** ... *)] doc comment. OCaml has no missing-docs lint; this is fennec's. Because
   odoc renders the curated [.mli], an export is documented-in-[.mli] (renders),
   documented-only-in-[.ml] (won't render — "promote" it), or undocumented. *)

(** A documentable export and its doc-comment text ([None] = no doc comment). *)
type item = { kind : string; name : string; line : int; doc : string option }

(** Documentable exports of a parsed [.mli] signature. Pure. *)
val interface_items : Ppxlib.signature -> item list

(** Top-level definitions of a parsed [.ml] structure. Pure. *)
val implementation_items : Ppxlib.structure -> item list

(** Insert a doc comment before each given 1-based line of a source string (the [--promote] core).
    One pass, so line numbers never shift; a doc that would close the comment early is skipped.
    Pure. *)
val promote_source : string -> (int * string) list -> string

(** Run the check over [paths] (files or directories; default: the current project).
    - [strict] → exit [1] if any export is undocumented OR documented only in its [.ml] (both mean
      the public docs are blank); else [0] (warn-only).
    - [private_] → also scan [.ml] top-level definitions, not just [.mli] exports.
    - [promote] → don't report; instead copy each ".ml-only" doc into its [.mli] (idempotent; the
      [.mli] wins on conflict; the [.ml] is never modified). Returns the process exit code. *)
val run : paths:string list -> strict:bool -> private_:bool -> promote:bool -> int
