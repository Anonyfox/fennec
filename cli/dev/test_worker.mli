(** Supervisor-side client for the resident warm test worker (cli/dev/worker/fennec_test_worker.ml).

    Owns the worker's lifecycle and the wire, and the FALLBACK DECISION: every entry point returns an
    option and yields [None] on any failure (worker missing, spawn/handshake failed, socket error,
    malformed reply) so the caller can drop to the cold [Dev_tests.run_one] path. A worker absence or
    crash is invisible except for speed.

    Pure transport — the pass/fail tally is parsed by the caller ({!Dev_tests}); here a run only carries
    the child's exit code, captured output, and timings. *)

(** A live worker handle. *)
type t

(** The forked child's exit code, its captured combined output (timing sentinel already stripped), and
    the worker-measured timings. [exit_code <> 0] is a real test failure delivered over a healthy
    transport — NOT a fallback case. *)
type result = {
  exit_code : int;
  output : string;
  total_ms : float;   (** fork → reap *)
  dynlink_ms : float; (** Dynlink of the framework .cma's in the child *)
  run_ms : float;     (** load of the test .cmo — i.e. the test run *)
}

(** The project-local framework lib names the worker links with [-linkall]. Mirrors
    [cli/dev/worker/dune]; {!Test_chain.derive} consults it to decide warm-vs-cold. *)
val preloaded : string list

(** [spawn ?worker_exe ~root ()] locates the worker binary (the [$FENNEC_TEST_WORKER] override, then
    [<root>/_build/default/cli/dev/worker/fennec_test_worker.bc], then next to the running exe),
    launches it, and completes the PING/PONG handshake. [None] if the binary can't be found or the
    handshake fails — the caller stays on the cold path. {!Stublibs.ensure} must have run first (the
    worker dlopens the project C stubs via the inherited [CAML_LD_LIBRARY_PATH]). *)
val spawn : ?worker_exe:string -> root:string -> unit -> t option

(** [run t ~chain] sends the Dynlink [chain] ({!Test_chain.objects}: app [.cma]s then the test [.cmo])
    and reads the captured {!result}. [None] on any transport failure (→ fall back to cold). *)
val run : t -> chain:string list -> result option

(** Liveness: the worker process is alive AND answering PING. A wedged or exited worker is [false] so
    the caller falls back. *)
val healthy : t -> bool

(** Ask the worker to exit, close the connection, reap it, and remove its socket. Idempotent-ish:
    safe to call once at supervisor shutdown. *)
val shutdown : t -> unit

(** The worker's process id (for pidfile bookkeeping). *)
val pid : t -> int

(**/**)

(* exposed for inline tests *)
val encode_run : string list -> string
val decode_result_header : string -> (int * float * float * float * int) option
val candidate_paths : root:string -> string list
