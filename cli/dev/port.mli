(* Who is listening on a TCP port, and whether a holder is our own dev server. Used by the
   supervisor to self-heal a held dev port: reclaim a leftover of OUR server, name a foreign one. *)

(** [(pid, full command)] of every process LISTENING on [port] right now (via lsof + ps); [] if
    lsof is absent. *)
val listeners : int -> (int * string) list

(** Pure. Does command line [cmd] RUN our server binary [exe]? True iff [argv[0]] is that build
    artifact — matched by its ["_build/…"] tail at a path boundary — so a process that merely
    mentions the path as an argument (an editor, a grep, a log line) is NOT ours. This is the gate
    the reclaim SIGKILL trusts, so it deliberately errs toward NOT-ours. *)
val is_ours : exe:string -> cmd:string -> bool

(** SIGKILL any leftover of our own server [exe] that is holding [port]; [true] iff something was
    killed. Waits briefly after a kill so the port is actually free before the caller retries. *)
val reclaim : exe:string -> int -> bool

(** The first listener on [port] that is NOT ours — a process to name for the user, not kill. *)
val foreign_holder : exe:string -> int -> (int * string) option
