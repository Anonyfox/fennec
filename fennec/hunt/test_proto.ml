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
