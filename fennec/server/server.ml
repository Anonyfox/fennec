(* The native I/O shell — a compact HTTP/1.1 + WebSocket server over Eio (no
   cohttp, no Lwt). It parses requests, runs the pure [Fennec_core.App.dispatch]
   for HTTP, and upgrades [Upgrade: websocket] connections to a text-message
   channel ([ws]) the caller drives. Single port; the only non-portable part of
   the framework. RFC 6455 framing lives in [Ws]. *)

module CH = Fennec_core.Http

(* a live websocket connection as a text-message channel. The handler sets
   [on_text]/[on_close]; [send] is safe to call from any fiber (serialized). *)
type ws = {
  send : string -> unit;
  mutable on_text : string -> unit;
  mutable on_close : unit -> unit;
}

type parsed = {
  meth : string;
  target : string;
  version : string; (* "HTTP/1.0" | "HTTP/1.1" | … *)
  headers : (string * string) list;
  body : string;
}

(* Request limits. The Buf_read max_size bounds total buffered bytes; these add
   semantic caps so a hostile request gets a clean 4xx, not a silent desync. *)
let max_header_count = 100
let max_body_size = 8 * 1024 * 1024 (* 8 MiB request body *)

(* outcome of parsing one request off the connection *)
type request_result =
  | Req of parsed
  | Conn_eof (* clean end of stream — no (more) requests *)
  | Bad_request of string (* malformed — answer 400 and close *)
  | Too_large of string (* body/headers over a limit — answer 413 and close *)

let header h k = List.assoc_opt (String.lowercase_ascii k) h

(* does [hay] contain [needle]? (small, no regex) *)
let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let read_request (r : Eio.Buf_read.t) : request_result =
  match try `Line (Eio.Buf_read.line r) with End_of_file | Failure _ -> `Eof with
  | `Eof -> Conn_eof
  | `Line "" -> Conn_eof (* leading blank line: treat as end *)
  | `Line reqline -> (
    (* request line: METHOD SP TARGET SP VERSION — reject if malformed *)
    match String.split_on_char ' ' reqline with
    | [ meth; target; version ] when meth <> "" && target <> "" ->
      let rec headers acc n =
        if n > max_header_count then `Too_many
        else
          match Eio.Buf_read.line r with
          | "" -> `Done (List.rev acc)
          | line -> (
            match String.index_opt line ':' with
            | Some i ->
              let k = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
              let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
              headers ((k, v) :: acc) (n + 1)
            | None -> headers acc (n + 1) (* tolerate a fold/garbage header line *))
          | exception (End_of_file | Failure _) -> `Done (List.rev acc)
      in
      (match headers [] 0 with
       | `Too_many -> Too_large "too many headers"
       | `Done hs -> (
         (* body: validate Content-Length strictly *)
         match header hs "content-length" with
         | None -> Req { meth; target; version; headers = hs; body = "" }
         | Some n -> (
           match int_of_string_opt (String.trim n) with
           | None -> Bad_request "invalid content-length"
           | Some len when len < 0 -> Bad_request "negative content-length"
           | Some len when len > max_body_size -> Too_large "request body too large"
           | Some 0 -> Req { meth; target; version; headers = hs; body = "" }
           | Some len -> (
             match Eio.Buf_read.take len r with
             | body -> Req { meth; target; version; headers = hs; body }
             | exception (End_of_file | Failure _) ->
               Bad_request "truncated body" (* fewer bytes than Content-Length *)))))
    | _ -> Bad_request "malformed request line")

let to_request (p : parsed) : CH.request =
  let path, query = CH.split_target p.target in
  { CH.meth = CH.meth_of_string p.meth; path; query; headers = p.headers; body = p.body }

let is_ws_upgrade (p : parsed) =
  match header p.headers "upgrade" with
  | Some v -> contains (String.lowercase_ascii v) "websocket"
  | None -> false

let reason_phrase = function
  | 200 -> "OK"
  | 204 -> "No Content"
  | 206 -> "Partial Content"
  | 301 -> "Moved Permanently"
  | 302 -> "Found"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 403 -> "Forbidden"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 413 -> "Payload Too Large"
  | 416 -> "Range Not Satisfiable"
  | 500 -> "Internal Server Error"
  | _ -> "OK"

let has_header_ci headers k =
  let kl = String.lowercase_ascii k in
  List.exists (fun (hk, _) -> String.lowercase_ascii hk = kl) headers

let write_http (w : Eio.Buf_write.t) (resp : CH.response) ~keep_alive =
  Eio.Buf_write.string w (Printf.sprintf "HTTP/1.1 %d %s\r\n" resp.CH.status (reason_phrase resp.CH.status));
  List.iter
    (fun (k, v) -> Eio.Buf_write.string w (Printf.sprintf "%s: %s\r\n" k v))
    resp.CH.headers;
  (* Responder normally sets Content-Length; add it only if absent (e.g. for the
     bare 404 error path that bypasses the app) *)
  if not (has_header_ci resp.CH.headers "content-length") then
    Eio.Buf_write.string w (Printf.sprintf "content-length: %d\r\n" (String.length resp.CH.body));
  Eio.Buf_write.string w
    (Printf.sprintf "connection: %s\r\n\r\n" (if keep_alive then "keep-alive" else "close"));
  Eio.Buf_write.string w resp.CH.body

(* returns whether permessage-deflate was negotiated for this connection *)
let ws_handshake (w : Eio.Buf_write.t) (p : parsed) : bool =
  let key = match header p.headers "sec-websocket-key" with Some k -> k | None -> "" in
  let pmd =
    match header p.headers "sec-websocket-extensions" with
    | Some v -> contains v "permessage-deflate" && !Deflate.enabled
    | None -> false
  in
  let ext =
    if pmd then
      "Sec-WebSocket-Extensions: permessage-deflate; server_no_context_takeover; \
       client_no_context_takeover\r\n"
    else ""
  in
  Eio.Buf_write.string w
    ("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
      Sec-WebSocket-Accept: " ^ Ws.accept_key key ^ "\r\n" ^ ext ^ "\r\n");
  Eio.Buf_write.flush w;
  pmd

(* drive the websocket: one writer fiber serializes all outgoing frames; the
   reader loop handles ping/pong/close and dispatches text to [ws.on_text]. When
   [pmd] (permessage-deflate) is negotiated, data frames are compressed (RSV1=1)
   and incoming RSV1 frames are inflated; control frames are never compressed. *)
let serve_ws ~sw ~pmd (r : Eio.Buf_read.t) (w : Eio.Buf_write.t) (setup : ws -> unit) =
  let outbox : Ws.frame Eio.Stream.t = Eio.Stream.create 256 in
  let text_frame s =
    if pmd then { Ws.fin = true; rsv1 = true; opcode = Ws.Text; payload = Deflate.compress s }
    else { Ws.fin = true; rsv1 = false; opcode = Ws.Text; payload = s }
  in
  let ws =
    {
      send = (fun s -> Eio.Stream.add outbox (text_frame s));
      on_text = (fun _ -> ());
      on_close = (fun () -> ());
    }
  in
  setup ws;
  Eio.Fiber.fork ~sw (fun () ->
      try
        while true do
          let f = Eio.Stream.take outbox in
          Ws.write_frame w f;
          Eio.Buf_write.flush w
        done
      with _ -> ());
  (* deliver a complete (reassembled) data message to the app *)
  let deliver ~rsv1 (payload : string) =
    let payload = if rsv1 then (try Deflate.decompress payload with _ -> payload) else payload in
    ws.on_text payload
  in
  let close ?code () =
    (match code with
     | Some c -> Eio.Stream.add outbox (Ws.close_frame ~code:c ())
     | None -> Eio.Stream.add outbox (Ws.close_frame ()));
    ws.on_close ()
  in
  (* read loop with fragmentation reassembly. [frag] is Some (opcode_rsv1, buf)
     while a fragmented message is in progress; control frames may interleave. *)
  let rec loop frag =
    match Ws.read_frame r with
    | Ws.Eof -> ws.on_close ()
    | Ws.Protocol_error _ -> close ~code:1002 ()
    | Ws.Frame { Ws.opcode = Ws.Close; _ } -> close ~code:1000 ()
    | Ws.Frame { Ws.opcode = Ws.Ping; payload; _ } ->
      Eio.Stream.add outbox { Ws.fin = true; rsv1 = false; opcode = Ws.Pong; payload };
      loop frag
    | Ws.Frame { Ws.opcode = Ws.Pong; _ } -> loop frag
    | Ws.Frame { Ws.opcode = (Ws.Text | Ws.Binary) as op; fin; rsv1; payload } -> (
      match frag with
      | Some _ -> close ~code:1002 () (* new data frame mid-fragment: protocol error *)
      | None ->
        if fin then (deliver ~rsv1 payload; loop None)
        else (
          let buf = Buffer.create (String.length payload) in
          Buffer.add_string buf payload;
          loop (Some ((op, rsv1), buf))))
    | Ws.Frame { Ws.opcode = Ws.Continuation; fin; payload; _ } -> (
      match frag with
      | None -> close ~code:1002 () (* continuation with no message started *)
      | Some ((_, rsv1), buf) ->
        Buffer.add_string buf payload;
        if Buffer.length buf > Ws.max_message_size then close ~code:1009 ()
        else if fin then (deliver ~rsv1 (Buffer.contents buf); loop None)
        else loop frag)
    | Ws.Frame { Ws.opcode = Ws.Other _; _ } -> close ~code:1002 ()
  in
  loop None

(* keep-alive decision honoring the protocol version (RFC 7230 §6.3):
   - HTTP/1.1 defaults to keep-alive unless "Connection: close"
   - HTTP/1.0 defaults to close unless "Connection: keep-alive" *)
let want_keep_alive (p : parsed) : bool =
  let conn = Option.map String.lowercase_ascii (header p.headers "connection") in
  let is_11 = p.version = "HTTP/1.1" in
  match conn with
  | Some v when contains v "close" -> false
  | Some v when contains v "keep-alive" -> true
  | _ -> is_11

let handle_conn ?on_ws ~now ~timeout (app : Fennec_core.App.t) flow _addr =
  Eio.Switch.run @@ fun sw ->
  let r = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  Eio.Buf_write.with_flow flow @@ fun w ->
  let respond_and_close resp =
    write_http w resp ~keep_alive:false;
    Eio.Buf_write.flush w
  in
  let rec loop () =
    (* read the next request under an idle/header timeout so a slowloris or a
       stalled keep-alive connection can't tie up this fiber forever *)
    match Eio.Time.Timeout.run timeout (fun () -> Ok (read_request r)) with
    | Error `Timeout -> () (* drop a stalled connection silently *)
    | Ok Conn_eof -> ()
    | Ok (Bad_request _) -> respond_and_close (CH.text ~status:400 "Bad Request")
    | Ok (Too_large _) -> respond_and_close (CH.text ~status:413 "Payload Too Large")
    | Ok (Req p) ->
      if is_ws_upgrade p then (
        match on_ws with
        | Some f ->
          let pmd = ws_handshake w p in
          serve_ws ~sw ~pmd r w (f (to_request p))
        | None -> respond_and_close (CH.text ~status:404 "no websocket here"))
      else begin
        let req = to_request p in
        (* run the app + airtight HTTP semantics. A handler exception becomes a
           clean 500 (never a dropped connection / partial write) — we only write
           AFTER finalize succeeds. *)
        let resp =
          try Responder.finalize ~now:(now ()) ~req (Fennec_core.App.dispatch app req)
          with exn ->
            Printf.eprintf "fennec: handler error: %s\n%!" (Printexc.to_string exn);
            Responder.finalize ~now:(now ()) ~req (CH.text ~status:500 "Internal Server Error")
        in
        let keep_alive = want_keep_alive p in
        write_http w resp ~keep_alive;
        Eio.Buf_write.flush w;
        if keep_alive then loop ()
      end
  in
  loop ()

(* run [app] on [port]; [on_ws req ws] sets up a websocket connection. Blocks
   until the process stops. The Date/conditional headers use the env clock.
   @param timeout    per-request idle/header read timeout in seconds (slowloris
                     defense). Default 30s.
   @param max_conns  cap on concurrently-served connections (resource bound).
                     Default 10_000. Excess connections wait for a slot. *)
let run ?(timeout = 30.0) ?(max_conns = 10_000) ~env ?on_ws ~port (app : Fennec_core.App.t) =
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) timeout in
  (* a counting semaphore bounds in-flight connections *)
  let slots = Eio.Semaphore.make max_conns in
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let handle flow addr =
    Eio.Semaphore.acquire slots;
    Fun.protect
      ~finally:(fun () -> Eio.Semaphore.release slots)
      (fun () -> handle_conn ?on_ws ~now ~timeout app flow addr)
  in
  Eio.Net.run_server socket handle ~on_error:(fun e ->
      (* connection-level errors (client resets, parse failures past the response)
         are logged but never crash the server *)
      Printf.eprintf "fennec: connection error: %s\n%!" (Printexc.to_string e))
