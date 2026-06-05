(* The dev-time CLI<->server protocol, defined ONCE for both sides.

   `fennec dev` (the CLI supervisor) runs the app server as a child process and talks to it across
   a boundary that is, by nature, untyped: environment variables the CLI sets, and lines the server
   prints to stderr that the CLI parses back. With the literals duplicated across files, a change on
   one side (a renamed prefix, a shifted offset, a tweaked exit code) drifts silently — the most
   visible symptom being the dev URL / "ready" banner quietly never appearing while the server runs
   fine. Centralizing the wire here — names, prefixes, exit code, and typed (de)serializers — makes
   any such change a change to THIS module, and lets the parsers be round-tripped in a test.

   Lives in fennec_core (Stdlib only) so the CLI, the app facade, and the server adapter can all
   reference the one definition. *)

(* ── environment the CLI sets for the server ──────────────────────────────────────────── *)
let env_mode = "FENNEC_ENV" (* "development" | "production" *)
let env_livereload = "FENNEC_LIVERELOAD" (* path of the dev control unix socket the CLI pings *)
let env_dev_parent = "FENNEC_DEV_PARENT" (* the supervisor's pid; the server self-exits if it changes *)
let env_dev_ui = "FENNEC_DEV_UI" (* "1" → the server reports its URLs for the CLI's banner *)
let env_dev_livereload = "FENNEC_DEV_LIVERELOAD" (* "0" → serve the dev root but suppress livereload (e2e) *)
let env_esbuild_worker = "FENNEC_ESBUILD_WORKER" (* path of the warm esbuild worker socket *)
let env_port = "FENNEC_PORT" (* the base port: dev allocates the block from here; prod listens on it *)
let env_parallelism = "FENNEC_PARALLELISM" (* optional worker-domain (per-core) override; auto by default *)

(* ── exit code (server → CLI, out of band) ────────────────────────────────────────────── *)

(* the server exits with THIS distinct code when it can't bind its port, so the supervisor can tell
   a port conflict (self-heal: reclaim a leftover / name a foreign holder) from a generic crash
   (rate-limited restart). A bare process status carries no other channel for "why", hence a code. *)
let port_in_use_exit = 98

(* ── stderr line protocol (server → CLI) ──────────────────────────────────────────────── *)
let urls_prefix = "[fennec:urls]"
let port_busy_prefix = "fennec: port "
let chatter_prefix = "[fennec]" (* the server's own human chatter; the CLI suppresses it (its UI says it better) *)

let starts_with s pfx =
  let lp = String.length pfx in
  String.length s >= lp && String.sub s 0 lp = pfx

(* the dev-URL report, as named endpoints: "[fennec:urls] web=http://localhost:8021 admin=http://…".
   Each token is "<name>=<url>"; names carry no '=' or space (Host_router enforces it) and the URL
   no space, so a single split on ' ' then the FIRST '=' round-trips exactly. *)
let urls_line (named : (string * string) list) : string = urls_prefix ^ " " ^ String.concat " " (List.map (fun (n, u) -> n ^ "=" ^ u) named)

let parse_urls_line (line : string) : (string * string) list option =
  if not (starts_with line urls_prefix) then None
  else
    let lp = String.length urls_prefix in
    String.sub line lp (String.length line - lp)
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
    |> List.filter_map (fun tok -> match String.index_opt tok '=' with Some i -> Some (String.sub tok 0 i, String.sub tok (i + 1) (String.length tok - i - 1)) | None -> None)
    |> fun pairs -> Some pairs

(* the port-conflict report: "fennec: port 8200 is already in use — …" *)
let port_busy_line (port : int) : string = Printf.sprintf "%s%d is already in use — another server is holding it." port_busy_prefix port

let parse_port_busy (line : string) : int option =
  if not (starts_with line port_busy_prefix) then None
  else
    let s = String.sub line (String.length port_busy_prefix) (String.length line - String.length port_busy_prefix) in
    let n = String.length s and i = ref 0 in
    while !i < n && s.[!i] >= '0' && s.[!i] <= '9' do
      incr i
    done;
    if !i > 0 then int_of_string_opt (String.sub s 0 !i) else None
