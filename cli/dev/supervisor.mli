(** The `fennec dev` orchestration loop — the composition root.

    It drives entirely off {!Dune_watch}'s settled-build events: restart the server on a good
    backend build, hot-reload on a frontend-only change, keep the last good server on a failed
    build, and survive anything (a total, exception-wrapped loop). The substantive logic lives
    in the tested sibling modules — {!Artifact}, {!Pidfile}, {!Assets}, {!Crash_limiter}; this
    module is the IO glue (process spawning, the dev control socket, the esbuild worker, status
    logging) that wires them to dune and the server. *)

(** Watch [targets] with [dune build --watch], supervise the server executable [exe], and serve
    livereload from the [assets] subdirectory of the exe's build dir. Blocks until killed
    (SIGINT/SIGTERM clean up the children). *)
val run : targets:string list -> exe:string -> assets:string -> unit
