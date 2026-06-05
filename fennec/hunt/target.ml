(* The server-under-test lifecycle, shared by BOTH testing layers (Http + Browser).

   A target is a URL. The Http layer makes requests against it; the Browser layer points
   Chrome at it. Either layer may SPAWN the server (start a command, wait for it to accept
   connections, kill it on switch end) or use one that is already RUNNING at that URL.

   This is the single source of truth for: parsing a URL, probing readiness, and
   spawn+teardown. Both layers call it — there is exactly one implementation of each. *)

type net = [ `Generic ] Eio.Net.ty Eio.Resource.t
type clock = float Eio.Time.clock_ty Eio.Resource.t

(* A parsed URL. Every field is read by a caller, so the breakdown is load-bearing:
   - [scheme]    chooses transport (only "http" is supported; "https" is rejected up front)
   - [host]      is what the client CONNECTS to (DNS-resolved) — so remote URLs work, not just localhost
   - [port]      is what readiness probes and the client target
   - [base_path] is prepended to every request path (so [~url:".../api"] scopes a sub-tree) *)
type url = { scheme : string; host : string; port : int; base_path : string }

(* Parse a URL. TOTAL — never raises; missing parts get sensible defaults. A garbage input
   yields a best-effort url (it will simply fail to connect, surfacing as a normal test
   failure, never an internal crash). *)
let parse_url (raw : string) : url =
  let scheme, rest =
    match String.index_opt raw ':' with
    | Some i when i + 2 < String.length raw && raw.[i + 1] = '/' && raw.[i + 2] = '/' ->
      (String.sub raw 0 i, String.sub raw (i + 3) (String.length raw - i - 3))
    | _ -> ("http", raw)
  in
  let host_port, base_path =
    match String.index_opt rest '/' with
    | Some i -> (String.sub rest 0 i, String.sub rest i (String.length rest - i))
    | None -> (rest, "")
  in
  let default_port = if scheme = "https" then 443 else 80 in
  let host, port =
    match String.rindex_opt host_port ':' with
    | Some i ->
      let h = String.sub host_port 0 i in
      let p = match int_of_string_opt (String.sub host_port (i + 1) (String.length host_port - i - 1)) with Some p -> p | None -> default_port in
      ((if h = "" then "localhost" else h), p)
    | None -> ((if host_port = "" then "localhost" else host_port), default_port)
  in
  { scheme; host; port; base_path }

(* Block until [host]:[port] accepts a TCP connection and answers a line, or fail after
   [timeout] seconds. Event-driven retry on the Eio clock — the ONE wait in the lifecycle
   (a setup concern, before any test runs). *)
let wait_ready ~(net : net) ~(clock : clock) ~host ~port ~timeout =
  let deadline = Eio.Time.now clock +. timeout in
  let rec loop () =
    if Eio.Time.now clock > deadline then
      failwith (Printf.sprintf "server at %s:%d never became ready (%.0fs timeout)" host port timeout);
    match
      Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net (fun flow ->
          Eio.Flow.copy_string "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" flow;
          ignore (Eio.Buf_read.line (Eio.Buf_read.of_flow flow ~max_size:4096)))
    with
    | () -> ()
    | exception _ -> Eio.Time.sleep clock 0.05; loop ()
  in
  loop ()

(* Spawn [argv] under [sw] (stdout/stderr discarded), applying [env] (KEY=VALUE entries) to
   the process environment, then wait for the target to accept connections. The process is
   terminated when [sw] finishes (Eio ties the child to the switch — structural teardown,
   no manual kill). *)
let spawn ~sw ~proc_mgr ~fs ~net ~clock ?(env = [||]) ~host ~port ~timeout (argv : string list) =
  Array.iter
    (fun kv -> match String.index_opt kv '=' with
       | Some i -> Unix.putenv (String.sub kv 0 i) (String.sub kv (i + 1) (String.length kv - i - 1))
       | None -> ())
    env;
  let devnull = Eio.Path.open_out ~sw ~create:(`If_missing 0o644) Eio.Path.(fs / "/dev/null") in
  ignore (Eio.Process.spawn ~sw proc_mgr ~stdout:devnull ~stderr:devnull argv);
  wait_ready ~net ~clock ~host ~port ~timeout
