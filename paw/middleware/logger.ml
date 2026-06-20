(* The HTTP access logger — one structured event ({!Access.t}), four ways to render it.

   This is a thin RENDERING + WIRING layer over {!Access}. The structured event (method, path, final
   status, microsecond duration, post-gzip byte count, ip, request id, error) is captured ONCE by the
   server at its per-request chokepoint; this module turns that one value into a line in the format
   the environment calls for:

     - Pretty  — aligned, colourised columns for a human at a dev terminal
     - Json    — NDJSON, one object per line, for a log shipper (the prod default)
     - Logfmt  — key=value pairs, the other machine-readable staple
     - Apache  — the classic combined access-log line, for existing log tooling
     - Dev     — the framed [fennec:http] wire line the dev supervisor parses to draw its own view

   WIRING (how it sees the post-gzip size). A paw runs in the pipeline, BEFORE {!Responder.finalize}
   sets the final Content-Length, so a logging paw cannot itself see the compressed size. So
   [Logger.make] does NOT log from a before_send hook; instead it INSTALLS a per-request sink on the
   conn ({!Conn.request_access_log}), and the server calls that sink with the FINAL {!Access.t} after
   finalize. The middleware stays the opt-in + config surface ([Paw.use (Logger.make ())]); the
   server does the size-accurate emit. (For a server you drive directly, {!sink} gives the same
   renderer as a plain [Access.t -> unit] to pass as [Server.run ~on_access].)

   Hand-rolled JSON (no yojson) keeps the shipped server prod-lean. *)

module Conn = Conn

(* ── format ────────────────────────────────────────────────────────────────────────────────── *)

type format =
  | Pretty   (* aligned + colourised, for a human at a TTY *)
  | Json     (* NDJSON, one object per line, for a log shipper (the prod default) *)
  | Logfmt   (* key=value pairs, space-separated; values with spaces quoted *)
  | Apache   (* the combined access-log line; fields we don't carry render as "-" *)
  | Dev      (* the framed [fennec:http] wire line the dev supervisor parses (see {!Dev_proto}) *)

(* ── human helpers (pretty) ──────────────────────────────────────────────────────────────────── *)

(* a human byte size: B under 1KB, then KB / MB / GB with one decimal. 0 → "0B". *)
let human_bytes (n : int) : string =
  let f = float_of_int n in
  if n < 1024 then Printf.sprintf "%dB" n
  else if f < 1024. *. 1024. then Printf.sprintf "%.1fKB" (f /. 1024.)
  else if f < 1024. *. 1024. *. 1024. then Printf.sprintf "%.1fMB" (f /. (1024. *. 1024.))
  else Printf.sprintf "%.1fGB" (f /. (1024. *. 1024. *. 1024.))

(* a human duration: microseconds under 1ms (e.g. "420µs"), else milliseconds with one decimal
   ("1.4ms") up to a second, else seconds ("2.3s"). Keeps a sub-millisecond request legible instead
   of collapsing it to "0.0ms". *)
let human_dur (dur_us : int) : string =
  if dur_us < 1000 then Printf.sprintf "%dµs" dur_us
  else if dur_us < 1_000_000 then Printf.sprintf "%.1fms" (float_of_int dur_us /. 1000.)
  else Printf.sprintf "%.1fs" (float_of_int dur_us /. 1_000_000.)

(* ── colour (pretty, on a TTY) ───────────────────────────────────────────────────────────────── *)

(* the SGR colour code for a status class — 2xx green, 3xx cyan, 4xx yellow, 5xx red, else default *)
let status_color (status : int) : string =
  if status >= 500 then "31" else if status >= 400 then "33" else if status >= 300 then "36" else if status >= 200 then "32" else "0"

let paint ~on code s = if on then Printf.sprintf "\027[%sm%s\027[0m" code s else s
let dim ~on s = if on then Printf.sprintf "\027[2m%s\027[0m" s else s

(* ── the four renderers ──────────────────────────────────────────────────────────────────────── *)

(* PRETTY: "<status>  <method>  <path>  <dur>  <size>" with the status colourised and a dimmed request
   id / red error appended. Columns are padded so a stream of lines aligns: status in 3, method in 4.
   [color] is threaded so a custom sink / a non-TTY gets the plain text (same layout, no SGR). *)
let render_pretty ~color (a : Access.t) : string =
  let status_s = Printf.sprintf "%3d" a.Access.status in
  let status_s = paint ~on:color (status_color a.Access.status) status_s in
  let meth = Printf.sprintf "%-4s" a.Access.meth in
  let line = Printf.sprintf "%s  %s  %s  %s  %s" status_s meth a.Access.path (human_dur a.Access.dur_us) (human_bytes a.Access.bytes) in
  let line = match a.Access.req_id with Some id -> line ^ "  " ^ dim ~on:color ("#" ^ id) | None -> line in
  match a.Access.error with Some e -> line ^ "  " ^ paint ~on:color "31" ("error: " ^ e) | None -> line

(* JSON: the shared compact object (see {!Access.to_compact_json}) — all fields, dur_us raw, error
   only when present. *)
let render_json (a : Access.t) : string = Access.to_compact_json a

(* LOGFMT: key=value pairs. A value with a space (or empty, or with a quote) is double-quoted with
   inner quotes/backslashes escaped, per the de-facto logfmt rule; bare tokens otherwise. *)
let logfmt_val (v : string) : string =
  let needs_quote = v = "" || String.exists (fun c -> c = ' ' || c = '"' || c = '\\' || c = '=') v in
  if not needs_quote then v
  else begin
    let b = Buffer.create (String.length v + 2) in
    Buffer.add_char b '"';
    String.iter (fun c -> match c with '"' -> Buffer.add_string b {|\"|} | '\\' -> Buffer.add_string b {|\\|} | '\n' -> Buffer.add_string b {|\n|} | c -> Buffer.add_char b c) v;
    Buffer.add_char b '"';
    Buffer.contents b
  end

let render_logfmt (a : Access.t) : string =
  let kv k v = k ^ "=" ^ logfmt_val v in
  let parts =
    [ kv "ts" (Printf.sprintf "%.3f" a.Access.ts);
      kv "method" a.Access.meth;
      kv "path" a.Access.path;
      kv "status" (string_of_int a.Access.status);
      kv "dur_us" (string_of_int a.Access.dur_us);
      kv "bytes" (string_of_int a.Access.bytes) ]
    @ (match a.Access.ip with Some ip -> [ kv "ip" ip ] | None -> [])
    @ (match a.Access.req_id with Some id -> [ kv "req_id" id ] | None -> [])
    @ (match a.Access.error with Some e -> [ kv "error" e ] | None -> [])
  in
  String.concat " " parts

(* APACHE combined log: '<ip> - - [<ts>] "<method> <path> <version>" <status> <bytes> "<referer>" "<ua>"'.
   We don't carry the referer / user-agent in the event, so they render as "-" (documented). The
   "remote logname" + "remote user" identd fields are always "-" (we don't do identd / HTTP auth user
   capture). The timestamp uses the Common Log Format: [dd/Mon/yyyy:HH:MM:SS +0000] (always UTC). *)
let clf_month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let clf_time (ts : float) : string =
  let tm = Unix.gmtime ts in
  Printf.sprintf "%02d/%s/%04d:%02d:%02d:%02d +0000" tm.Unix.tm_mday clf_month.(tm.Unix.tm_mon) (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let render_apache (a : Access.t) : string =
  let ip = Option.value a.Access.ip ~default:"-" in
  (* a 0-byte body is "-" per CLF convention (no body sent) *)
  let size = if a.Access.bytes = 0 then "-" else string_of_int a.Access.bytes in
  Printf.sprintf {|%s - - [%s] "%s %s %s" %d %s "-" "-"|} ip (clf_time a.Access.ts) a.Access.meth a.Access.path a.Access.version a.Access.status size

(* render an event to a single line (NO trailing newline) for the given format. [color] only affects
   Pretty. Dev frames via {!Dev_proto.http_line}. *)
let render ?(color = false) (fmt : format) (a : Access.t) : string =
  match fmt with
  | Pretty -> render_pretty ~color a
  | Json -> render_json a
  | Logfmt -> render_logfmt a
  | Apache -> render_apache a
  | Dev -> Dev_proto.http_line a

(* ── environment-derived defaults ────────────────────────────────────────────────────────────── *)

(* the default format, derived from the environment when [?format] is not given:
     FENNEC_DEV_UI set        → Dev    (the supervisor renders; we emit the framed wire line)
     FENNEC_ENV = production   → Json   (NDJSON for a log shipper)
     otherwise                 → Pretty (a human at a dev terminal)
   A later [?format] always wins over this. *)
let default_format () : format =
  match Sys.getenv_opt Dev_proto.env_dev_ui with
  | Some ("1" | "on" | "true") -> Dev
  | _ -> ( match Sys.getenv_opt Dev_proto.env_mode with Some "production" -> Json | _ -> Pretty)

(* colour only for Pretty, only when the default sink (stderr) is a real terminal and NO_COLOR is
   unset — the same restraint the old logger used, so piped/redirected logs stay clean. A custom
   sink never gets colour (it might be a file / JSON consumer). *)
let stderr_is_tty = lazy (try Unix.isatty Unix.stderr with _ -> false)
let no_color = lazy (match Sys.getenv_opt "NO_COLOR" with Some s when s <> "" -> true | _ -> false)

(* the default sink for a format: structured formats (Json/Logfmt/Apache/Dev) go to STDOUT (where a
   log collector / the dev supervisor's pipe reads them); Pretty goes to STDERR (the conventional
   place for human diagnostics, kept off stdout so it never pollutes piped data). Each writes the
   line + a newline. Documented so a caller knows where unconfigured output lands. *)
let default_sink (fmt : format) : string -> unit =
  match fmt with
  | Pretty -> fun line -> prerr_string line; prerr_newline ()
  | Json | Logfmt | Apache | Dev -> fun line -> print_string line; print_newline ()

(* ── the public surface ──────────────────────────────────────────────────────────────────────── *)

(* a renderer-sink: turn an [Access.t] into a line and write it. Use this directly as
   [Server.run ~on_access:(Logger.sink ())] for a server you drive yourself; [Logger.make] uses it
   under the hood. [?format] overrides the env default; [?sink] overrides the per-format default
   (and disables colour — a custom sink gets plain text). *)
let sink ?format ?sink () : Access.t -> unit =
  let fmt = match format with Some f -> f | None -> default_format () in
  let write = match sink with Some s -> s | None -> default_sink fmt in
  (* colour: only Pretty, only the default stderr TTY sink, only when NO_COLOR is unset *)
  let color = fmt = Pretty && sink = None && Lazy.force stderr_is_tty && not (Lazy.force no_color) in
  fun a -> ( try write (render ~color fmt a) with _ -> ())

(* the logger PAW: installs the per-request sink on the conn; the server invokes it after finalize
   with the final (post-gzip) {!Access.t}. Declines (passes through) — it only marks the conn.
   Back-compat: [Paw.use (Logger.make ())] keeps working and now logs the size + µs in the
   env-appropriate format. *)
let make ?format ?sink:s () : Pipeline.t =
  let emit = sink ?format ?sink:s () in
  fun c -> Conn.request_access_log c emit

(* ──── logger tests ──── *)

let sample : Access.t =
  { Access.ts = 1_700_000_000.0; meth = "GET"; path = "/users/42"; status = 200; dur_us = 1402;
    bytes = 2150; ip = Some "203.0.113.7"; req_id = Some "ab12-7"; error = None; version = "HTTP/1.1"; host = "example.com" }

let err_sample : Access.t =
  { sample with Access.status = 500; dur_us = 980; bytes = 21; error = Some "Failure(\"boom\")"; req_id = None }

(* ──── human helpers ──── *)
let%test "human_bytes: bytes"   = human_bytes 512 = "512B"
let%test "human_bytes: KB"      = human_bytes 2150 = "2.1KB"
let%test "human_bytes: MB"      = human_bytes (3 * 1024 * 1024) = "3.0MB"
let%test "human_dur: µs under 1ms" = human_dur 420 = "420µs"
let%test "human_dur: ms"           = human_dur 1402 = "1.4ms"
let%test "human_dur: s"            = human_dur 2_300_000 = "2.3s"

(* ──── Pretty (plain, no colour) ──── *)
let%test "pretty: exact line (success, no colour)" =
  render ~color:false Pretty sample = "200  GET   /users/42  1.4ms  2.1KB  #ab12-7"

let%test "pretty: an error renders the message (plain)" =
  render ~color:false Pretty err_sample = "500  GET   /users/42  980µs  21B  error: Failure(\"boom\")"

let%test "pretty: colour wraps the status in SGR on a 200" =
  let out = render ~color:true Pretty sample in
  Fennec_hunt_unit.str_contains out "\027[32m200\027[0m"

let%test "pretty: a custom sink is never colourised" =
  let buf = Buffer.create 64 in
  let emit = sink ~format:Pretty ~sink:(Buffer.add_string buf) () in
  emit sample;
  not (Fennec_hunt_unit.str_contains (Buffer.contents buf) "\027[")

(* ──── Json ──── *)
let%test "json: exact line (success)" =
  render Json sample = {|{"ts":1700000000.000000,"method":"GET","path":"/users/42","status":200,"dur_us":1402,"bytes":2150,"ip":"203.0.113.7","req_id":"ab12-7","version":"HTTP/1.1","host":"example.com"}|}

let%test "json: an error event carries the error, omits absent req_id" =
  let j = render Json err_sample in
  Fennec_hunt_unit.str_contains j {|"status":500|}
  && Fennec_hunt_unit.str_contains j {|"error":"Failure(\"boom\")"|}
  && not (Fennec_hunt_unit.str_contains j "req_id")

(* ──── Logfmt ──── *)
let%test "logfmt: exact line (success)" =
  render Logfmt sample = {|ts=1700000000.000 method=GET path=/users/42 status=200 dur_us=1402 bytes=2150 ip=203.0.113.7 req_id=ab12-7|}

let%test "logfmt: a value with a space is quoted" =
  let a = { sample with Access.error = Some "kaboom now"; status = 500 } in
  Fennec_hunt_unit.str_contains (render Logfmt a) {|error="kaboom now"|}

(* ──── Apache ──── *)
let%test "apache: exact combined line (success)" =
  render Apache sample = {|203.0.113.7 - - [14/Nov/2023:22:13:20 +0000] "GET /users/42 HTTP/1.1" 200 2150 "-" "-"|}

let%test "apache: absent ip and zero body render as dashes" =
  let a = { sample with Access.ip = None; bytes = 0 } in
  render Apache a = {|- - - [14/Nov/2023:22:13:20 +0000] "GET /users/42 HTTP/1.1" 200 - "-" "-"|}

(* ──── Dev (framed) ──── *)
let%test "dev format emits the [fennec:http] frame and round-trips" =
  let line = render Dev sample in
  Dev_proto.starts_with line Dev_proto.http_prefix && Dev_proto.parse_http_line line = Some sample

(* ──── env-derived default ──── *)
(* save / restore an env var around a probe so these tests never leak state to other test files in
   the shared runner process *)
let with_env key value f =
  let saved = Sys.getenv_opt key in
  (match value with Some v -> Unix.putenv key v | None -> Unix.putenv key "");
  Fun.protect ~finally:(fun () -> Unix.putenv key (Option.value saved ~default:"")) f

let%test "default_format: FENNEC_DEV_UI → Dev" =
  with_env Dev_proto.env_dev_ui (Some "1") (fun () -> default_format () = Dev)

let%test "default_format: production → Json (DEV_UI unset)" =
  with_env Dev_proto.env_dev_ui None (fun () -> with_env Dev_proto.env_mode (Some "production") (fun () -> default_format () = Json))

let%test "default_format: dev (neither set) → Pretty" =
  with_env Dev_proto.env_dev_ui None (fun () -> with_env Dev_proto.env_mode None (fun () -> default_format () = Pretty))

(* ──── the paw + sink wiring ──── *)
let req_ ?(meth = Http.GET) path = Http.make_request ~meth ~path ()

let%test "make installs a per-request access sink (the paw declines / passes through)" =
  let c = Conn.make (req_ "/x") in
  let c = make ~format:Json ~sink:ignore () c in
  (* the paw must NOT answer — it only marks the conn *)
  (not (Conn.answered c)) && Conn.access_sink c <> None

let%test "the installed sink renders via the chosen format when the server calls it" =
  let buf = Buffer.create 64 in
  let c = make ~format:Logfmt ~sink:(Buffer.add_string buf) () (Conn.make (req_ ~meth:Http.POST "/p")) in
  (match Conn.access_sink c with Some f -> f { sample with Access.meth = "POST"; path = "/p" } | None -> ());
  let out = Buffer.contents buf in
  Fennec_hunt_unit.str_contains out "method=POST" && Fennec_hunt_unit.str_contains out "path=/p"
