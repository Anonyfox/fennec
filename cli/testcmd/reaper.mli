(* A process-wide registry of spawned child PIDs (servers + suite processes) so an interrupt
   (Ctrl-C / SIGTERM) can tear every one of them down — even in parallel mode, where the
   per-suite [Fun.protect] finalizers live on worker threads a signal won't unwind. Without
   this, Ctrl-C mid-run would orphan servers still holding their ports (the next run then trips
   the "port held by another process" guard). Thread-safe; teardown is best-effort SIGKILL. *)

(** Register a live child pid. *)
val track : int -> unit

(** Forget a pid that has been reaped. *)
val untrack : int -> unit

(** SIGKILL every still-tracked pid (best-effort; reading the registry is lock-free so it is
    safe to call from a signal handler regardless of which thread is mid-update). *)
val kill_all : unit -> unit

(** Install SIGINT/SIGTERM handlers that [kill_all ()] then exit (130/143). Idempotent. *)
val install_signal_handlers : unit -> unit
