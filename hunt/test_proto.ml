(* The `fennec test` ⟷ suite environment contract. The CLI SETS these per suite; a suite
   (via {!Http.hunt} / {!Run}) READS them. One source of truth — no stringly-typed drift,
   the same discipline as the dev wire (Fennec_core.Dev_proto), but kept here so the testing
   package stays free of any framework dependency. *)

(* the suite's target instance URL — set per-suite by the harness so each suite hits its own
   isolated server *)
let env_url = "FENNEC_TEST_URL"

(* the instance's port — the server reads it (same var the dev/prod server honours) *)
let env_port = "FENNEC_PORT"

let target_url () = match Sys.getenv_opt env_url with Some u when u <> "" -> Some u | _ -> None

let url_for ~port = Printf.sprintf "http://localhost:%d" port

(* pure target resolution — explicit wins, else the harness env, else a clear error. Pure so
   it is unit-testable without touching the environment. *)
let resolve ~explicit ~from_env =
  match (explicit, from_env) with
  | Some u, _ | None, Some u -> Ok u
  | None, None -> Error "no target URL: pass ~url (or ~base_url), or run via `fennec test` (it sets FENNEC_TEST_URL per suite)"

let resolve_url ~explicit = resolve ~explicit ~from_env:(target_url ())

(* ── System cut: the harness contract `fennec test system` sets, a System suite reads. MIRROR of
   Fennec_core.Dev_proto.env_test_* (the CLI side); equality guarded by a test in hunt/test.
   Typed accessors so a suite never hand-rolls getenv for these — see Fennec_hunt.System. *)
let env_bin = "FENNEC_BIN"
let env_app_dir = "FENNEC_APP_DIR"
let env_server_bc = "FENNEC_SERVER_BC"
let env_root = "FENNEC_ROOT"

let getenv_opt k = match Sys.getenv_opt k with Some v when v <> "" -> Some v | _ -> None

(* the fennec binary under test; defaults to "fennec" (on PATH) when run outside `fennec test`. *)
let bin () = Option.value (getenv_opt env_bin) ~default:"fennec"
(* the project dir to run `fennec dev` in; defaults to the cwd. *)
let app_dir () = Option.value (getenv_opt env_app_dir) ~default:(Sys.getcwd ())
(* the built server bytecode, if the harness provided it. *)
let server_bc () = getenv_opt env_server_bc
(* the workspace root; defaults two levels up from the app dir (the example layout). *)
let root () = Option.value (getenv_opt env_root) ~default:(Filename.dirname (Filename.dirname (app_dir ())))

(* ──── env constants ──── *)
let%test "env_url name"  = env_url = "FENNEC_TEST_URL"
let%test "env_port name" = env_port = "FENNEC_PORT"
let%test "env_bin name"        = env_bin = "FENNEC_BIN"
let%test "env_app_dir name"    = env_app_dir = "FENNEC_APP_DIR"
let%test "env_server_bc name"  = env_server_bc = "FENNEC_SERVER_BC"
let%test "env_root name"       = env_root = "FENNEC_ROOT"

(* ──── url_for ──── *)
let%test "url_for: default port" = url_for ~port:8200 = "http://localhost:8200"
let%test "url_for: port 80"      = url_for ~port:80 = "http://localhost:80"
