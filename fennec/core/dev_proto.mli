(* The dev-time CLI<->server protocol — environment names, stderr line prefixes, the port-conflict
   exit code, and typed (de)serializers — defined ONCE so the CLI supervisor and the app/server
   reference the same authoritative wire instead of literals duplicated (and silently drifting)
   across files. See dev_proto.ml for the rationale. *)

(** {1 Environment the CLI sets for the server} *)

(** [FENNEC_ENV] — ["development"] | ["production"]. *)
val env_mode : string

(** [FENNEC_LIVERELOAD] — path of the dev control unix socket. *)
val env_livereload : string

(** [FENNEC_DEV_PARENT] — supervisor pid; the server self-exits when it changes. *)
val env_dev_parent : string

(** [FENNEC_DEV_UI] — ["1"] asks the server to report its URLs for the CLI banner. *)
val env_dev_ui : string

(** [FENNEC_DEV_LIVERELOAD] — ["0"] serves the dev root but suppresses livereload. *)
val env_dev_livereload : string

(** [FENNEC_ESBUILD_WORKER] — path of the warm esbuild worker socket. *)
val env_esbuild_worker : string

(** [FENNEC_PORT] — the base port: dev allocates its block from here, prod listens on it. *)
val env_port : string

(** [FENNEC_PARALLELISM] — optional worker-domain (per-core) count; auto by default. *)
val env_parallelism : string

(** [FENNEC_TEST_URL] — the per-suite target URL [fennec test] sets so each suite hits its own
    isolated instance. Mirror of [Fennec_hunt.Test_proto.env_url] (the suite side, in an
    independent package); the equality is guarded by a test so it cannot drift. *)
val env_test_url : string

(** {1 Exit code} *)

(** Distinct exit code the server uses on [EADDRINUSE], so the supervisor self-heals a port
    conflict (reclaim/name the holder) instead of treating it as a generic crash. *)
val port_in_use_exit : int

(** {1 stderr line protocol (server → CLI)} *)

val urls_prefix : string
val port_busy_prefix : string

(** Prefix of the server's own human chatter; the CLI suppresses such lines (its UI says it better). *)
val chatter_prefix : string

(** The dev-URL report the server prints (after a successful bind) for the CLI's banner: a list of
    [(endpoint name, url)] pairs rendered as ["name=url"] tokens. *)
val urls_line : (string * string) list -> string

(** [Some named] iff [line] is a URL report; [None] otherwise. Inverse of {!urls_line}. *)
val parse_urls_line : string -> (string * string) list option

(** The port-conflict line the server prints before exiting {!port_in_use_exit}. *)
val port_busy_line : int -> string

(** [Some port] iff [line] is a port-conflict report; [None] otherwise. Inverse of {!port_busy_line}. *)
val parse_port_busy : string -> int option

(** [starts_with s prefix] — prefix test, exposed for the CLI's line classifier. *)
val starts_with : string -> string -> bool
