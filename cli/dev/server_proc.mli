(** The dev server child process: its pid, the read end of its merged stdout+stderr pipe, and a
    partial-line carry buffer — bundled so they can't drift apart. The supervisor holds at most one,
    as the [Up] arm of its state machine. The line classifier is pure; acting on each line is the
    supervisor's responsibility. *)

(** An active server child process. *)
type t

(** How a line the server printed is classified — the pure routing decision the supervisor acts on. *)
type parsed =
  | Urls of (string * string) list  (** the server bound and reported its dev URLs, as (name, url) pairs *)
  | Port_busy of int  (** the server could not bind: this port is held *)
  | Chatter  (** the server's own framework noise, or a blank line — ignore *)
  | App_log of string  (** the user's application output — relay verbatim *)

(** Pure, total: classify one line of server output. (Unit-tested in isolation.) *)
val classify_line : string -> parsed

(** Spawn [exe] with [env] appended to the environment, stdout+stderr merged into ONE pipe the
    parent reads (so the supervisor is the sole terminal writer). [None] if the spawn fails. *)
val start : exe:string -> env:string array -> t option

(** The OS process id of the server child. *)
val pid : t -> int

(** Read available output (non-blocking), split into complete lines, and hand each — classified —
    to [on_line]. A partial trailing line is carried to the next call. *)
val drain : t -> on_line:(parsed -> unit) -> unit

(** WNOHANG check: [Some status] (and the pid is reaped) if the server exited on its own; [None] if
    it is still alive. *)
val reap : t -> Unix.process_status option

(** Close the read pipe — after a self-exit already reaped via {!reap}. *)
val close : t -> unit

(** SIGTERM, then SIGKILL if it lingers, then reap, then close the pipe — a graceful stop that
    guarantees the port is actually freed (for a restart or shutdown, vs {!reap}+{!close}). *)
val stop : t -> unit
