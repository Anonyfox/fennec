(** [dune build --watch] as a typed, bullet-proof event stream.

    When dune's stderr is not a terminal (we pipe it), [--watch] emits a clean,
    newline-delimited log with a small, stable grammar:

    {v
      ********** NEW BUILD (main.ml changed) **********      a build (re)started
      File "main.ml", line 1, characters 4-5: ...           compiler diagnostics
      Success, waiting for filesystem changes...            settled — OK
      Had 1 error, waiting for filesystem changes...        settled — failed
    v}

    The "waiting for filesystem changes" line is the load-bearing one: dune emits it
    ONLY when the build queue has fully drained. A burst of edits cancels the in-flight
    build (a fresh "NEW BUILD" banner) but does NOT emit "waiting" until everything
    settles — so dune coalesces an edit storm into ONE settle. Acting only on a settle
    (never on a banner) is what makes a rapid LLM-edit loop produce a single restart
    instead of a thrash.

    Unbreakable by construction: {!classify_line} is pure and total, and the IO layer
    never lets an exception escape. *)

(** The result of a settled build. Warnings are errors in the dev profile, so a
    warning settles as [Errors]. *)
type outcome = Ok | Errors of int

(** A semantically classified dune output line — the only place that knows dune's
    wording. Pure and total; exposed so it can be unit-tested against real samples. *)
type line =
  | Trigger of string  (** a "NEW BUILD (desc)" banner; [desc] is the parenthetical *)
  | Settled of outcome  (** a "…, waiting for filesystem changes…" line *)
  | Other  (** anything else: progress, diagnostics, blanks *)

val classify_line : string -> line

(** Strip ANSI CSI escapes (colour) from a line. Exposed for reuse by the diagnostics parser. *)
val strip_ansi : string -> string

(** [find_sub hay needle]: byte index of the first occurrence of [needle] in [hay]. Exposed for
    reuse by the diagnostics parser. *)
val find_sub : string -> string -> int option

(** A build cycle that has fully settled, or the watcher process exiting. *)
type event =
  | Settled_build of {
      outcome : outcome;
      triggers : string list;  (** the "(… changed)" descriptions seen since the last settle *)
      messages : string;  (** accumulated diagnostics (the error/warning text), for display *)
      duration_ms : float option;  (** first trigger → settle; [None] for the initial build *)
    }
  | Exited  (** the dune process ended (crashed or was killed) *)

type t

(** Spawn [dune build --watch <targets>] with its stderr captured. *)
val start : string list -> t

(** Build a watcher reading from an arbitrary fd WITHOUT spawning dune. Test seam only: it lets
    a test drive the full read/assemble/EOF path over a plain pipe (write dune-shaped bytes to
    the other end, then [poll]). [pid] is 0. Not for production use. *)
val of_fd : Unix.file_descr -> t

(** The dune process id (for supervision / cleanup). *)
val pid : t -> int

(** Stop the watcher: SIGTERM → SIGKILL → reap. Closes the read pipe. Safe to call
    multiple times. *)
val stop : t -> unit

(** Block up to [timeout] seconds for the next event; [None] on timeout (so the caller
    can also poll the server's health). Never raises. *)
val poll : t -> timeout:float -> event option

(** Whether dune is currently mid-build — a build started ("NEW BUILD") that has not yet
    settled. A caller defers acting on a just-settled build while a newer one is in flight: the
    in-flight build is rewriting the artifact, so restarting now would thrash and risk loading a
    half-written image. Its settle will arrive. *)
val is_building : t -> bool
