(** [fennec doctor] — the one command a stuck beginner (or AI agent) runs when a frontend Fennec app
    won't build or the editor's [.mlx] IntelliSense is dead.

    It verifies the whole [.mlx] dialect toolchain reaches the user's machine — now SELF-CONTAINED in
    [fennec-cli] (the mlx parser is vendored): the [fennec] binary itself (the build preprocessor is
    [fennec mlx-pp]), [ocamlmerlin-fennec-mlx] (the editor reader), the JS runtime the bundler can use
    ([node], optional), the OPTIONAL [ocamlmerlin-mlx] (an editor error-recovery upgrade — the reader
    works without it), AND that the project's [dune-project] declares the [(dialect (name mlx) …)]
    stanza. For every MISSING piece it prints the exact remedy to paste; when everything required is
    present it prints a single green "toolchain OK".

    The report-building is pure (given the probe results), so it is unit-tested without touching the
    filesystem or PATH; {!run} wires the real probes and returns a process exit code. *)

(** One checked tool on PATH. *)
type tool = {
  name : string;  (** the executable name probed with [command -v] *)
  found : bool;  (** is it resolvable on PATH? *)
  required : bool;  (** a missing REQUIRED tool fails the check; an optional one only warns *)
  purpose : string;  (** one-liner: what it does in the toolchain *)
  remedy : string;  (** the exact fix to print when it is missing (e.g. "opam install mlx") *)
}

(** The [dune-project] dialect-stanza check for the current project. *)
type project = {
  dune_project : string option;  (** path to the nearest [dune-project], if one was found *)
  dialect_ok : bool;  (** does it declare [(dialect (name mlx) …)]? *)
}

(** [stanza] is the verbatim [(dialect (name mlx) …)] block {!run} tells the user to paste into
    [dune-project] when it is missing — the SAME stanza the workspace and the scaffold use. *)
val stanza : string

(** Does [text] (the contents of a [dune-project]) declare the mlx dialect? True iff it contains a
    [(dialect …)] s-expression whose body names [mlx] (tolerant of whitespace/newlines). Pure;
    exposed for testing. *)
val text_declares_dialect : string -> bool

(** Build the human report from already-gathered probe results, returning [(text, all_ok)] where
    [all_ok] is false iff any REQUIRED tool is missing or the dialect stanza is absent. Pure and
    deterministic given [tty]; the heart of the command, unit-tested directly. *)
val render : Tty.t -> tools:tool list -> project:project -> string * bool

(** Run the real probes (PATH lookups + the [dune-project] read from the cwd up), print the report to
    stdout, and return a process exit code: [0] when the toolchain is OK, [1] when something REQUIRED
    is missing. *)
val run : unit -> int
