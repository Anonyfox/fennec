(** System tests — the fourth hunt layer (Unit / Http / Browser / System).

    A typed vocabulary for the things shell scripts do — spawn processes, run one-shot
    commands, watch their output, poke the filesystem, probe ports — but contained and
    deterministic, on Eio. It replaces hand-rolled [.sh] integration tests.

    Each scenario runs in a {b sandbox}: a disposable temp workdir with its own scoped env
    and process registry. Every process spawned in it goes into its own session/process-group,
    so on scenario exit — pass, fail, exception, or timeout — the {b whole tree} is killed
    (no orphans, even a process the tool under test itself leaked) and the temp dir is removed.
    Cross-platform (macOS + Linux), no namespaces required.

    Determinism: there is no [sleep]-and-hope. Every wait is condition-based with a deadline
    and {b raises} {!Timeout} on overrun rather than hanging. Output is captured to a file, so
    it never deadlocks a pipe and is readable at any instant.

    The argv of every command is a {b list}, never a parsed string — no shell, no quoting, no
    injection. *)

(** Raised by a [wait_*] that exceeds its deadline. *)
exception Timeout of string

(** {2 Entry point} *)

(** Call this FIRST in a system-test executable: it intercepts the internal re-exec used to
    put a spawned process in its own session. Otherwise it runs the Eio event loop, runs [f]
    (which calls {!test}), prints a summary, and exits 0 if every scenario passed, 1 if any
    failed. *)
val main : (unit -> unit) -> unit

(** {2 A scenario} *)

type sandbox

(** [test name f] runs scenario [f] in a fresh sandbox. A raised exception (including a failed
    {!check}) marks it failed; later scenarios still run. Teardown is total and guaranteed. *)
val test : string -> (sandbox -> unit) -> unit

(** [check name cond] fails the current scenario (raising) if [cond] is false. *)
val check : string -> bool -> unit

(** {2 Filesystem — real, contained to the sandbox workdir} *)

val write  : sandbox -> string -> string -> unit   (** write a file (creating parent dirs) *)
val read   : sandbox -> string -> string           (** read a file's contents *)
val exists : sandbox -> string -> bool
val rm     : sandbox -> string -> unit             (** remove a file or tree (idempotent) *)

(** {2 Processes} *)

type proc

(** The outcome of a one-shot {!run}: exit status, combined stdout+stderr, wall-clock ms. *)
type result = { status : Unix.process_status; output : string; ms : float }

(** [run sandbox argv] runs a command to completion and returns its result. The multi-turn-CLI
    primitive: call it repeatedly; commands share the sandbox's real working directory.
    [cwd] (absolute) overrides the working directory — e.g. to run a tool against an existing
    project rather than the empty sandbox; defaults to the sandbox workdir. *)
val run : sandbox -> ?env:(string * string) list -> ?cwd:string -> string list -> result

(** [spawn sandbox argv] starts a long-running process (in its own session), output captured.
    Reaped — whole group — on sandbox teardown. The standing-server primitive. [cwd] as for
    {!run}. *)
val spawn : sandbox -> ?env:(string * string) list -> ?cwd:string -> string list -> proc

val output : proc -> string                       (** captured output so far *)
val pid    : proc -> int
val alive  : proc -> bool                          (** has not yet exited *)
val signal : proc -> int -> unit                  (** send a signal to the process *)
val stop   : proc -> unit                         (** SIGTERM → SIGKILL → reap the whole group *)

(** {2 Temporal waits — deadline-bounded, raise {!Timeout} on overrun} *)

(** Block until the process's captured output contains the substring (default 10s). *)
val wait_output : proc -> ?timeout:float -> string -> unit

(** Block until [port] accepts a connection AND the process is still alive (default 30s);
    fails fast if the process exits before binding. *)
val wait_ready : proc -> port:int -> ?timeout:float -> unit -> unit

(** Block until the process exits (default 30s); returns its status. *)
val wait_exit : proc -> ?timeout:float -> unit -> Unix.process_status

(** Block until something is listening on [port] (default 10s). *)
val wait_port : ?timeout:float -> int -> unit

(** Block until [cond ()] returns true (polling, default 10s) — e.g. a port freeing after a
    kill: [wait_until (fun () -> not (port_open p))]. Raises {!Timeout} on overrun. *)
val wait_until : ?timeout:float -> (unit -> bool) -> unit

(** {2 Ports} *)

val free_port : sandbox -> int    (** an ephemeral, currently-free port (parallel-safe) *)
val port_open : int -> bool       (** is something listening on [port] right now? *)
