(** The `fennec dev` orchestration loop — the composition root.

    It drives entirely off {!Dune_watch}'s settled-build events: restart the server on a good
    backend build, hot-reload on a frontend-only change, keep the last good server on a failed
    build, and survive anything (a total, exception-wrapped loop). The substantive logic lives in
    the tested sibling modules — {!Artifact}, {!Pidfile}, {!Assets}, {!Crash_limiter}, the server
    child + its line classifier ({!Server_proc}), held-port reclaim ({!Port}), and the shared
    CLI<->server wire ({!Fennec_core.Dev_proto}). This module is the glue that wires them to dune:
    it holds the server as a [Down | Up] state (so a live pid can't pair with a dead pipe or a
    stale port), the dev control socket, the esbuild worker, and status logging. *)

(** Watch [targets] with [dune build --watch], supervise the server executable [exe], and serve
    livereload from the [assets] subdirectory of the exe's build dir. Blocks until killed
    (SIGINT/SIGTERM clean up the children). *)
val run : ?port:int -> ?agent_dir:string -> targets:string list -> exe:string -> assets:string -> unit
(* C-stub loading for spawned bytecode servers (CAML_LD_LIBRARY_PATH) lives in {!Stublibs}. *)
