(* See server_proc.mli. Our server child process: its pid, the read-end of its merged stdout+stderr,
   and the partial-line carry buffer — bundled in one value so they move together (the supervisor
   holds at most one, as the [Up] arm of its state, so a live pid can never pair with a dead/absent
   pipe). The line CLASSIFIER is pure and total (unit-tested); the EFFECT of each line stays with
   the supervisor, which passes [drain] an [on_line] callback. *)

module Dev_proto = Fennec_core.Dev_proto

type t = { pid : int; fd : Unix.file_descr; carry : Buffer.t }

type parsed =
  | Urls of (string * string) list (* the server bound and reported its dev URLs, as (name, url) pairs *)
  | Port_busy of int (* the server could not bind: this port is held *)
  | Chatter (* the server's own framework noise (or a blank line) — ignore *)
  | App_log of string (* the user's application output — relay verbatim *)

let classify_line raw =
  let line = String.trim raw in
  match Dev_proto.parse_urls_line line with
  | Some urls -> Urls urls
  | None -> (
    match Dev_proto.parse_port_busy line with
    | Some p -> Port_busy p
    | None -> if line = "" || Dev_proto.starts_with line Dev_proto.chatter_prefix then Chatter else App_log line)

(* ──── tests: classify_line ──── *)

let%test "a urls report -> Urls" =
  classify_line "[fennec:urls] web=http://localhost:8200 admin=http://localhost:8201"
  = Urls [ ("web", "http://localhost:8200"); ("admin", "http://localhost:8201") ]

let%test "a port-busy line -> Port_busy" =
  classify_line "fennec: port 8200 is already in use — another server is holding it."
  = Port_busy 8200

let%test "framework chatter -> Chatter" =
  classify_line "[fennec] serving 2 endpoint(s)" = Chatter

let%test "a blank line -> Chatter" =
  classify_line "" = Chatter

let%test "whitespace-only -> Chatter" =
  classify_line "   " = Chatter

let%test "an app log -> App_log (trimmed)" =
  classify_line "  hello from the app  " = App_log "hello from the app"

let%test "leading/trailing space on a urls line still parses" =
  classify_line "  [fennec:urls] web=http://x  " = Urls [ ("web", "http://x") ]

let start ~exe ~env =
  try
    let rd, wr = Unix.pipe () in
    let pid = Unix.create_process_env exe [| exe |] (Array.append (Unix.environment ()) env) Unix.stdin wr wr in
    Unix.close wr;
    Some { pid; fd = rd; carry = Buffer.create 256 }
  with _ -> None

let pid t = t.pid

let drain t ~on_line =
  let rec go () =
    match (try Unix.select [ t.fd ] [] [] 0.0 with _ -> ([], [], [])) with
    | [], _, _ -> ()
    | _ -> (
      let buf = Bytes.create 4096 in
      match (try Unix.read t.fd buf 0 (Bytes.length buf) with _ -> 0) with
      | 0 -> () (* EOF — the server's gone; the supervisor notices via [reap] *)
      | k ->
        Buffer.add_subbytes t.carry buf 0 k;
        let s = Buffer.contents t.carry in
        Buffer.clear t.carry;
        let n = String.length s and start = ref 0 in
        for i = 0 to n - 1 do
          if s.[i] = '\n' then (on_line (classify_line (String.sub s !start (i - !start))); start := i + 1)
        done;
        if !start < n then Buffer.add_string t.carry (String.sub s !start (n - !start));
        go ())
  in
  go ()

let reap t = match (try Unix.waitpid [ Unix.WNOHANG ] t.pid with _ -> (0, Unix.WEXITED 0)) with 0, _ -> None | _, status -> Some status

let close t = try Unix.close t.fd with _ -> ()

let stop t =
  (try Unix.kill t.pid Sys.sigterm with _ -> ());
  let dead = ref false and i = ref 0 in
  while (not !dead) && !i < 6 do
    (match Unix.waitpid [ Unix.WNOHANG ] t.pid with
    | 0, _ -> Unix.sleepf 0.05 (* still alive: wait a beat *)
    | _ -> dead := true
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> () (* a signal interrupted us: just retry *)
    | exception _ -> dead := true (* ECHILD &c: already reaped/gone *));
    incr i
  done;
  if not !dead then (try Unix.kill t.pid Sys.sigkill with _ -> ());
  (try ignore (Unix.waitpid [] t.pid) with _ -> ());
  close t
