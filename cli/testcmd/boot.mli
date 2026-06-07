(** Boot one app instance for a test suite — spawn the server bytecode with the suite's isolated
    env, wait for its port to respond, and tear it down on exit. Output goes to a per-instance log
    file (no pipe to deadlock; available for failure diagnostics). I/O; proven via the e2e. *)

(** A spawned app instance: its OS pid, the log-file path, and readiness state. *)
type t

(** The inherited environment with [extra] [(name, value)] pairs appended — for spawning a
    child (the server, or a suite) with the isolated instance's env. *)
val env_array : (string * string) list -> string array

(** Spawn [exe] (the server bytecode) with [env] appended to the inherited environment; output
    to a fresh per-instance log file. *)
val spawn : exe:string -> env:(string * string) list -> t

(** Block until the instance accepts a connection on [port], or fail (the server exited, or
    [timeout] seconds elapsed). *)
val wait_ready : t -> port:int -> timeout:float -> (unit, string) result

(** SIGTERM → SIGKILL → reap: a graceful stop that guarantees the port is freed. *)
val stop : t -> unit

(** The captured server output (for diagnostics when an instance fails to start). *)
val read_log : t -> string

(** Remove the instance's log file. *)
val cleanup : t -> unit
