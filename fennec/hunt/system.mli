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

(** Raised by any [wait_*] call that exceeds its deadline.
    The string payload describes what was being waited on. *)
exception Timeout of string

(** {2 Authoring a scenario}

    Write scenarios with the [let%system] ppx — no entry point, no env wiring:
    {[
      let%system "the dev server frees its port when killed" = fun sb ->
        let dev = Fennec_hunt.System.dev sb in   (* spawns THIS app's `fennec dev` *)
        wait_ready dev ~port:4000 ();
        signal dev Sys.sigkill;
        wait_until (fun () -> not (port_open 4000));
        check "freed" (not (port_open 4000))
    ]}
    [let%system_manual] registers an opt-in scenario (skipped unless [--manual]) — for a
    destructive case like [fennec dev --clean], which wipes the shared [_build].

    [fennec test system] discovers [test/system/], builds the suite, and runs it. There is no
    [main]: the convention is a [-linkall] library of [*_test.ml] plus a one-line runner
    [let () = exit (Fennec_hunt.System.run ())]. *)

(** An isolated test environment: a temporary workdir, a scoped env, and a process registry.
    Created per scenario and fully torn down on exit — whether the scenario passes, fails,
    raises an exception, or is timed out. Every process spawned inside is killed on teardown. *)
type sandbox

(** {2 A scenario (registration)} *)

(** [test name f] registers scenario [f]. Prefer [let%system]; this is the no-ppx form. A raised
    exception (including a failed {!check}) fails it; other scenarios still run. Teardown is total
    and guaranteed. *)
val test : string -> (sandbox -> unit) -> unit

(** ppx-generated registration (with source location); prefer {!test} by hand. *)
val test_loc : name:string -> file:string -> line:int -> (sandbox -> unit) -> unit

(** ppx-generated registration of an opt-in ([@manual]) scenario; prefer {!test} by hand. *)
val test_manual_loc : name:string -> file:string -> line:int -> (sandbox -> unit) -> unit

(** [check name cond] fails the current scenario (raising) if [cond] is false. *)
val check : string -> bool -> unit

(** {2 Entry points} *)

(** Run every registered scenario (honouring [--grep] / [--manual] from argv), print a summary,
    and return [0] if all passed else [1]. The whole body of a suite runner: [let () = exit
    (Fennec_hunt.System.run ())]. Also handles the internal re-exec used to session-isolate a
    spawned process. *)
val run : unit -> int

(** Register via [f] (which calls {!test}) then {!run}, exiting with its code. A convenience for a
    self-contained runner; userland uses [let%system] + {!run} with no [main]. *)
val main : (unit -> unit) -> unit

(** {2 Filesystem — real, contained to the sandbox workdir} *)

(** All four take a sandbox-relative path, OR an absolute path (to touch/read a real file
    outside the sandbox — e.g. a project source, or a sentinel under [_build]). *)

val write  : sandbox -> string -> string -> unit   (** write a file (creating parent dirs) *)
val read   : sandbox -> string -> string           (** read a file's contents *)
val exists : sandbox -> string -> bool
val rm     : sandbox -> string -> unit             (** remove a file or tree (idempotent) *)

(** [with_edit sandbox path transform f] rewrites the file at [path] with [transform], runs [f],
    and ALWAYS restores the original content (even on failure) — for editing a real source file
    under test, like the livereload / error-panel scenarios. *)
val with_edit : sandbox -> string -> (string -> string) -> (unit -> 'a) -> 'a

(** {2 Processes} *)

(** A long-running process spawned inside a sandbox: its output is captured to a file,
    its whole process group is reaped on sandbox teardown, and it can be probed with
    {!alive}, {!wait_ready}, {!wait_output}, etc. *)
type proc

(** The outcome of a one-shot {!run_cmd}: exit status, combined stdout+stderr, wall-clock ms. *)
type result = { status : Unix.process_status; output : string; ms : float }

(** An HTTP response (see {!request}). *)
type response = { status : int; headers : (string * string) list; body : string }

(** [run_cmd sandbox argv] runs a command to completion and returns its result. The multi-turn-CLI
    primitive: call it repeatedly; commands share the sandbox's real working directory.
    [cwd] (absolute) overrides the working directory — e.g. to run a tool against an existing
    project rather than the empty sandbox; defaults to the sandbox workdir. *)
val run_cmd : sandbox -> ?env:(string * string) list -> ?cwd:string -> string list -> result

(** [spawn sandbox argv] starts a long-running process (in its own session), output captured.
    Reaped — whole group — on sandbox teardown. The standing-server primitive. [cwd] as for
    {!run_cmd}. *)
val spawn : sandbox -> ?env:(string * string) list -> ?cwd:string -> string list -> proc

val output : proc -> string                       (** captured output so far *)
val pid    : proc -> int
val alive  : proc -> bool                          (** has not yet exited *)
val signal : proc -> int -> unit                  (** send a signal to the process *)
val stop   : proc -> unit                         (** SIGTERM → SIGKILL → reap the whole group *)

(** {2 Harness context — typed, set by [fennec test system] (sane defaults run by hand)}

    So a suite never hand-rolls [getenv] for the framework binary, the app dir, etc. *)

(** Spawn THIS app's [fennec dev] (extra [args] appended), in {!app_dir}, captured + reaped on
    teardown. The standing-server primitive for System suites. Pair with {!wait_ready}. *)
val dev : ?args:string list -> sandbox -> proc

val fennec    : unit -> string          (** the fennec binary under test ([fennec] on PATH by hand) *)
val app_dir   : unit -> string          (** the project to run [fennec dev] in (cwd by hand) *)
val root      : unit -> string          (** the workspace root *)
val server_bc : unit -> string option   (** the built server bytecode, if the harness provided it *)

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

(** {2 HTTP (for asserting against a spawned server)} *)

(** One-shot HTTP request to [localhost:port][path] (always connects to loopback). [host] sets
    the routing Host header — for testing a host-routed gateway without touching /etc/hosts. *)
val request : ?host:string -> ?headers:(string * string) list -> ?meth:string -> ?body:string -> int -> string -> response

(** Case-insensitive header lookup on a {!response}. *)
val header : response -> string -> string option
