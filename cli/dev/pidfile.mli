(** Cross-run orphan reaping.

    The supervised server self-exits when orphaned, but the external [dune build --watch] daemon
    and the esbuild worker can't — so a force-killed `fennec dev` would leave a stale dune daemon
    that breaks the NEXT run. Each run records its child pids in [_build/.fennec_dev.pids]; the
    next run reaps whatever the previous one left, making startup self-healing. *)

(** Parse a pidfile body into pids (one per line; blanks and garbage ignored). Pure. *)
val parse : string -> int list

(** Is a process command name one of ours (supervisor, dune, server, esbuild worker)? The identity
    gate that makes reaping recycle-safe — matched precisely (exact name / "fennec" prefix / ".bc"
    suffix), not by loose substring. Pure; exposed for testing. *)
val comm_is_ours : string -> bool

(** Render pids as a pidfile body (one per line). Pure. *)
val render : int list -> string

(** The pidfile path for the dune project rooted at [root]. *)
val path_for : root:string -> string

(** Write [pids] to [path] (best-effort). *)
val record : string -> int list -> unit

(** Walk up from [cwd] to the dune project root, SIGKILL any pids its pidfile records, and
    remove the file. A no-op outside a dune project. Best-effort; never raises. *)
val reap_stale : cwd:string -> unit
