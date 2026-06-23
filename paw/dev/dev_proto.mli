(** The dev-time CLI↔server protocol — environment variable names, stderr line prefixes, the
    port-conflict exit code, and typed (de)serializers — defined ONCE here so the CLI supervisor
    and the app server reference the same authoritative wire, never duplicated literals that can
    silently drift. See [dev_proto.ml] for the full rationale.

    {[
      (* server side: report bound URLs, then classify on exit *)
      Printf.eprintf "%s\n%!" (Dev_proto.urls_line [ ("app", "http://localhost:3000") ]);

      (* CLI side: parse a line the server printed to stderr *)
      match Dev_proto.parse_urls_line line with
      | Some named -> ignore named                       (* render the dev banner *)
      | None -> (match Dev_proto.parse_port_busy line with
                 | Some port -> ignore port              (* reclaim the port holder *)
                 | None -> ())
    ]} *)

(** {1 Environment the CLI sets for the server} *)

(** [FENNEC_ENV] — ["development"] | ["production"]. An explicit {e override} for {!is_dev}; normally
    unset, because the default is derived from the build (see {!is_dev}). *)
val env_mode : string

(** [is_dev ()] — the framework's single dev-vs-production decision. A production build should just run
    as production with no env var to remember, so the default is derived from how the binary was
    {e built}: [fennec dev] runs the {e bytecode} server, [fennec release] builds a {e native} one — so
    a bytecode binary defaults to development and a native binary to production ([Sys.backend_type]).
    [FENNEC_ENV] = ["development"] | ["production"] overrides either way (e.g. to run a native build
    locally in dev mode). A [fennec test] / [dune runtest] run is also development: its runner is native
    like a release, so the test {e runtime} ({!Fennec_hunt_unit.run}) sets [FENNEC_ENV=development]
    before any test runs — the robust signal the build can't give. The one home of this rule, so the
    facade, the HTTP server, and the logger never diverge on what "dev" means. *)
val is_dev : unit -> bool

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

(** [FENNEC_CONSOLE] — ["1"] turns the server into a pure console: boot the runtime + the eval engine,
    no HTTP listener. The user's [server.ml] is unchanged; [fennec console] sets this. *)
val env_console : string

(** [FENNEC_CONSOLE_SOCK] — path of the console eval unix socket the in-server engine listens on (dev
    only). With none, the engine stays idle (no console). Set by [fennec dev] / [fennec console]. *)
val env_console_sock : string

(** [FENNEC_PORT] — the base port: dev allocates its block from here, prod listens on it. *)
val env_port : string

(** [FENNEC_PARALLELISM] — optional worker-domain (per-core) count; auto by default. *)
val env_parallelism : string

(** [FENNEC_TEST_URL] — the per-suite target URL [fennec test] sets so each suite hits its own
    isolated instance. Mirror of [Fennec_hunt.Test_proto.env_url] (the suite side, in an
    independent package); the equality is guarded by a test so it cannot drift. *)
val env_test_url : string

(** The System-cut harness contract: [fennec test system] SETS these, a System suite READS them
    (via {!Fennec_hunt.System} / [Test_proto]). Mirrored in the suite-side package; equality
    guarded by a test so it cannot drift. *)

val env_test_bin : string        (** [FENNEC_BIN] — the fennec binary under test *)
val env_test_app_dir : string    (** [FENNEC_APP_DIR] — the project to run [fennec dev] in *)
val env_test_server_bc : string  (** [FENNEC_SERVER_BC] — the built server bytecode *)
val env_test_root : string       (** [FENNEC_ROOT] — the dune workspace root *)

(** {1 Exit code} *)

(** Distinct exit code the server uses on [EADDRINUSE], so the supervisor self-heals a port
    conflict (reclaim/name the holder) instead of treating it as a generic crash. *)
val port_in_use_exit : int

(** {1 stderr line protocol (server → CLI)} *)

(** The fixed prefix that starts a URL report line (the server prints this before the name=url list). *)
val urls_prefix : string

(** The fixed prefix that starts a port-conflict line (the server prints this before the busy port number). *)
val port_busy_prefix : string

(** Prefix of the server's own human chatter; the CLI suppresses such lines (its UI says it better). *)
val chatter_prefix : string

(** Prefix of a structured per-request access line (["[fennec:http] {json}"]), emitted under
    [FENNEC_DEV_UI] for the supervisor's pretty HTTP view. Shares the ["[fennec"] stem with
    {!chatter_prefix} but diverges at [':'], so [starts_with line chatter_prefix] is [false] for an
    http line — the supervisor must try {!parse_http_line} BEFORE the chatter fallthrough. *)
val http_prefix : string

(** The dev-URL report the server prints (after a successful bind) for the CLI's banner: a list of
    [(endpoint name, url)] pairs rendered as ["name=url"] tokens. *)
val urls_line : (string * string) list -> string

(** [Some named] iff [line] is a URL report; [None] otherwise. Inverse of {!urls_line}. *)
val parse_urls_line : string -> (string * string) list option

(** The port-conflict line the server prints before exiting {!port_in_use_exit}. *)
val port_busy_line : int -> string

(** [Some port] iff [line] is a port-conflict report; [None] otherwise. Inverse of {!port_busy_line}. *)
val parse_port_busy : string -> int option

(** The structured access line for one finished request (["[fennec:http] {compact-json}"]); the
    payload is {!Access.to_compact_json}, so the shape lives in {!Access} and this is just the framing. *)
val http_line : Access.t -> string

(** [Some event] iff [line] is a structured access line; [None] otherwise. Inverse of {!http_line}. *)
val parse_http_line : string -> Access.t option

(** [starts_with s prefix] — prefix test, exposed for the CLI's line classifier. *)
val starts_with : string -> string -> bool
