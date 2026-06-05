(* The native I/O shell — a compact HTTP/1.1 + WebSocket server over Eio (no
   cohttp, no Lwt). It parses requests, runs each Endpoint's paw pipeline
   ([Paw.run_conn]), writes the HTTP response (or performs the RFC 6455 upgrade
   when a paw requested one), and dispatches across endpoints by Host + port. The
   only non-portable part of the framework. Framing lives in [Ws]. *)

module CH = Fennec_core.Http

(* the live websocket channel type lives in core (Ws_channel) so paws can
   reference it; the server provides the concrete [send] *)
type ws = Fennec_core.Ws_channel.t = {
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

(* Decode a Transfer-Encoding: chunked request body. Each chunk is "<hex-size>[;ext]\r\n
   <bytes>\r\n"; a 0-size chunk ends it (then optional trailers up to a blank line).
   Bounded by [max_body_size]. *)
let read_chunked (r : Eio.Buf_read.t) : (string, string) result =
  let buf = Buffer.create 4096 in
  let rec loop () =
    match Eio.Buf_read.line r with
    | exception (End_of_file | Failure _) -> Error "truncated chunked body"
    | size_line ->
      let hex =
        String.trim (match String.index_opt size_line ';' with Some i -> String.sub size_line 0 i | None -> size_line)
      in
      (match int_of_string_opt ("0x" ^ hex) with
       | None -> Error "bad chunk size"
       | Some 0 ->
         (* consume trailer headers up to the final blank line *)
         let rec drain () =
           match Eio.Buf_read.line r with "" -> () | _ -> drain () | exception _ -> ()
         in
         drain ();
         Ok (Buffer.contents buf)
       | Some n when n < 0 || Buffer.length buf + n > max_body_size -> Error "chunked body too large"
       | Some n -> (
         match Eio.Buf_read.take n r with
         | data ->
           Buffer.add_string buf data;
           (match Eio.Buf_read.line r with _ -> () | exception _ -> ()); (* trailing CRLF *)
           loop ()
         | exception (End_of_file | Failure _) -> Error "truncated chunk data"))
  in
  loop ()

(* [continue] is invoked just before the body is read, to honour Expect: 100-continue. *)
let read_request ~(continue : unit -> unit) (r : Eio.Buf_read.t) : request_result =
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
         let mk body = Req { meth; target; version; headers = hs; body } in
         let te = header hs "transfer-encoding" and cl = header hs "content-length" in
         let chunked = match te with Some v -> contains (String.lowercase_ascii v) "chunked" | None -> false in
         (* Content-Length AND Transfer-Encoding together is a request-smuggling vector *)
         if chunked && cl <> None then Bad_request "content-length with transfer-encoding"
         else begin
           (match header hs "expect" with
            | Some v when contains (String.lowercase_ascii v) "100-continue" -> continue ()
            | _ -> ());
           if chunked then (match read_chunked r with Ok body -> mk body | Error e -> Bad_request e)
           else
             match cl with
             | None -> mk ""
             | Some n -> (
               match int_of_string_opt (String.trim n) with
               | None -> Bad_request "invalid content-length"
               | Some len when len < 0 -> Bad_request "negative content-length"
               | Some len when len > max_body_size -> Too_large "request body too large"
               | Some 0 -> mk ""
               | Some len -> (
                 match Eio.Buf_read.take len r with
                 | body -> mk body
                 | exception (End_of_file | Failure _) -> Bad_request "truncated body"))
         end))
    | _ -> Bad_request "malformed request line")

let to_request ~host ~scheme ~remote_ip (p : parsed) : CH.request =
  let path, query_string = CH.split_target p.target in
  CH.make_request ~meth:(CH.meth_of_string p.meth) ~path ~query_string ~headers:p.headers
    ~body:p.body ~host ~scheme ~remote_ip ~version:p.version ()

let is_ws_upgrade (p : parsed) =
  match header p.headers "upgrade" with
  | Some v -> contains (String.lowercase_ascii v) "websocket"
  | None -> false

let has_header_ci headers k = Fennec_core.Headers.mem headers k

(* write the bytes directly to the buffer — no per-line Printf.sprintf allocation *)
let bw = Eio.Buf_write.string
let write_status_line w status =
  bw w "HTTP/1.1 "; bw w (string_of_int status); bw w " "; bw w (CH.reason_phrase status); bw w "\r\n"
(* defang CR/LF in a header value so no paw can split the response by reflecting client
   input into a header (header injection / response splitting). The common case has no
   control byte, so we only allocate when one is present. *)
let sanitize_header_value v =
  if String.exists (fun c -> c = '\r' || c = '\n') v then
    String.map (fun c -> if c = '\r' || c = '\n' then ' ' else c) v
  else v
let write_header_line w k v = bw w k; bw w ": "; bw w (sanitize_header_value v); bw w "\r\n"
let write_conn_header w ~keep_alive =
  bw w "connection: "; bw w (if keep_alive then "keep-alive" else "close"); bw w "\r\n"

let write_http (w : Eio.Buf_write.t) (resp : CH.response) ~keep_alive =
  write_status_line w resp.CH.status;
  List.iter (fun (k, v) -> write_header_line w k v) resp.CH.headers;
  (* Responder normally sets Content-Length; add it only if absent (e.g. for the
     bare 404 error path that bypasses the app) *)
  if not (has_header_ci resp.CH.headers "content-length") then
    write_header_line w "content-length" (string_of_int (String.length resp.CH.body));
  write_conn_header w ~keep_alive;
  bw w "\r\n";
  bw w resp.CH.body

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

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Headers = Fennec_core.Headers

(* Write a STREAMED response — the body is produced without buffering it in memory.
   [resp] carries the status + headers (after before_send); its body is ignored. *)
let write_stream w flow ~fs ~(resp : CH.response) ~keep_alive (stream : Conn.stream) =
  let status_line () = write_status_line w resp.CH.status in
  let write_headers hs =
    List.iter (fun (k, v) -> write_header_line w k v) hs;
    write_conn_header w ~keep_alive;
    bw w "\r\n";
    Eio.Buf_write.flush w
  in
  match stream with
  | Conn.File (path, ct) -> (
    (* size up front -> a real Content-Length (keep-alive friendly); missing file -> 404 *)
    match try Some (Unix.stat path).Unix.st_size with _ -> None with
    | None -> write_http w (CH.text ~status:404 "Not Found") ~keep_alive:false; Eio.Buf_write.flush w
    | Some size ->
      let hs =
        Headers.put (Headers.put resp.CH.headers "content-type" ct) "content-length"
          (string_of_int size)
      in
      status_line ();
      write_headers hs;
      (* stream the bytes straight from the file to the socket (Eio may use sendfile) *)
      (try Eio.Path.with_open_in Eio.Path.(fs / path) (fun f -> Eio.Flow.copy f flow) with _ -> ()))
  | Conn.Chunked (ct, produce) ->
    let hs =
      ("transfer-encoding", "chunked")
      :: Headers.put (Headers.delete resp.CH.headers "content-length") "content-type" ct
    in
    status_line ();
    write_headers hs;
    let emit s =
      if String.length s > 0 then begin
        Eio.Buf_write.string w (Printf.sprintf "%x\r\n" (String.length s));
        Eio.Buf_write.string w s;
        Eio.Buf_write.string w "\r\n";
        Eio.Buf_write.flush w
      end
    in
    (try produce emit with _ -> ());
    Eio.Buf_write.string w "0\r\n\r\n";
    Eio.Buf_write.flush w

(* select the endpoint whose host pattern matches the request's Host header;
   first match wins, falling back to the first endpoint if none match (so a bare
   IP / missing Host still gets served) *)
let select_endpoint (endpoints : Endpoint.t list) (p : parsed) : Endpoint.t option =
  let host = match header p.headers "host" with Some h -> h | None -> "" in
  match List.find_opt (fun e -> Endpoint.host_matches e host) endpoints with
  | Some e -> Some e
  | None -> ( match endpoints with e :: _ -> Some e | [] -> None)

(* Run a handler under a per-request deadline. On timeout, Eio CANCELS the whole handler
   fiber tree — including any sub-fibers it forked (parallel fetches) and their in-flight
   IO — and we answer 503. A thrown exception becomes a clean 500. This is structured,
   propagating cancellation that BEAM/Node/Go can't do for free. (It bounds the handler's
   logic; a streamed body — send_chunked/SSE — is produced afterwards, so SSE isn't cut.) *)
let run_handler ~clock ~timeout (handler : Paw.t) (req : CH.request) : Conn.t =
  match
    Eio.Time.with_timeout clock timeout (fun () ->
        try Ok (Paw.run_conn handler req) with exn -> Error (`Exn exn))
  with
  | Ok conn -> conn
  | Error `Timeout -> Conn.respond (Conn.make req) (CH.text ~status:503 "Service Unavailable")
  | Error (`Exn exn) ->
    Printf.eprintf "fennec: handler error: %s\n%!" (Printexc.to_string exn);
    Conn.respond (Conn.make req) (CH.text ~status:500 "Internal Server Error")

(* Handle one connection. [endpoints] are those sharing this listen port; each
   request picks its endpoint by Host. A paw pipeline may answer with an HTTP
   response OR a websocket upgrade (the ws is itself a paw). *)
let handle_conn ~now ~clock ~timeout ~request_timeout ~fs (endpoints : Endpoint.t list) flow addr =
  Eio.Switch.run @@ fun sw ->
  (* the peer IP, computed once for the connection (all its requests share it) *)
  let remote_ip =
    match addr with `Tcp (ip, _) -> Some (Format.asprintf "%a" Eio.Net.Ipaddr.pp ip) | _ -> None
  in
  let r = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  Eio.Buf_write.with_flow flow @@ fun w ->
  let respond_and_close resp =
    write_http w resp ~keep_alive:false;
    Eio.Buf_write.flush w
  in
  (* honour Expect: 100-continue: tell the client to go ahead before we read the body *)
  let continue () = Eio.Buf_write.string w "HTTP/1.1 100 Continue\r\n\r\n"; Eio.Buf_write.flush w in
  let rec loop () =
    (* read the next request under an idle/header timeout (slowloris defense) *)
    match Eio.Time.Timeout.run timeout (fun () -> Ok (read_request ~continue r)) with
    | Error `Timeout -> ()
    | Ok Conn_eof -> ()
    | Ok (Bad_request _) -> respond_and_close (CH.text ~status:400 "Bad Request")
    | Ok (Too_large _) -> respond_and_close (CH.text ~status:413 "Payload Too Large")
    | Ok (Req p) -> (
      (* scheme is http at the transport (no in-process TLS); a force-https / proxy
         a force-https paw can rewrite from X-Forwarded-Proto. host is the normalized Host header. *)
      let host = Host.normalize (match header p.headers "host" with Some h -> h | None -> "") in
      let req = to_request ~host ~scheme:"http" ~remote_ip p in
      let endpoint = select_endpoint endpoints p in
      (* run the endpoint's paw pipeline to a conn. A handler exception becomes a
         clean 500 (never a dropped connection / partial write). *)
      let conn =
        match endpoint with
        | None -> Conn.respond (Conn.make req) (CH.text ~status:404 "no endpoint")
        | Some e -> run_handler ~clock ~timeout:request_timeout (Endpoint.handler e) req
      in
      match Conn.upgrade_handler conn with
      | Some setup when is_ws_upgrade p ->
        (* a paw requested a websocket upgrade *)
        let pmd = ws_handshake w p in
        serve_ws ~sw ~pmd r w setup
      | _ -> (
        match Conn.stream conn with
        | Some stream ->
          (* a streamed response (file / chunks): run before_send over the status+headers,
             then stream the body without buffering it (no compression/Responder pass) *)
          let skel = Conn.resp_skeleton conn in
          let resp = try Conn.apply_before_send conn skel with _ -> skel in
          let keep_alive = want_keep_alive p in
          write_stream w flow ~fs ~resp ~keep_alive stream;
          if keep_alive then loop ()
        | None ->
          (* buffered HTTP response: run before_send hooks (e.g. security headers), then
             finalize (compression, ETag/304, Date, Content-Length) *)
          let resp =
            match Conn.resp conn with Some r -> r | None -> CH.text ~status:404 "404 Not Found"
          in
          (* a throwing before_send hook must not kill the connection — fall back to the
             un-hooked response *)
          let resp = try Conn.apply_before_send conn resp with _ -> resp in
          let resp = try Responder.finalize ~now:(now ()) ~req resp with _ -> resp in
          let keep_alive = want_keep_alive p in
          write_http w resp ~keep_alive;
          Eio.Buf_write.flush w;
          if keep_alive then loop ()))
  in
  loop ()

(* Run [endpoints], blocking. Endpoints sharing a listen port are grouped onto one
   socket and selected per-request by Host pattern (prod) — so one process serves
   many subdomains/wildcards. In dev each endpoint listens on its own [dev_port]
   on localhost (no /etc/hosts / proxy needed).
   @param dev             dev mode (use dev_port + lenient host). Default from FENNEC_ENV.
   @param timeout         per-request idle/header read timeout, seconds (default 30).
   @param request_timeout per-request handler deadline; on expiry the handler tree is
                          cancelled and a 503 is returned (default 30).
   @param max_conns       concurrent-connection cap (default 10_000). *)
let run ?(timeout = 30.0) ?(request_timeout = 30.0) ?(max_conns = 10_000) ?domains ?dev ?(on_listen = fun () -> ()) ~env
    (endpoints : Endpoint.t list) =
  let dev =
    match dev with
    | Some d -> d
    | None -> ( try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true)
  in
  (* worker domains for true multicore: each handles whole connections independently (the
     nginx-worker model). Default: 1 in dev (deterministic; the livereload relay is shared),
     all cores in prod. Override with ~domains or FENNEC_DOMAINS. *)
  let domains =
    match domains with
    | Some n -> max 1 n
    | None -> (
      match Option.bind (Sys.getenv_opt "FENNEC_DOMAINS") int_of_string_opt with
      | Some n -> max 1 n
      | None -> if dev then 1 else Domain.recommended_domain_count ())
  in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  let fs = Eio.Stdenv.fs env in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) timeout in
  let slots = Eio.Semaphore.make max_conns in
  (* group endpoints by their listen port for this mode *)
  let ports = Hashtbl.create 8 in
  List.iter
    (fun e ->
      let port = Endpoint.listen_port ~dev e in
      Hashtbl.replace ports port (e :: (try Hashtbl.find ports port with Not_found -> [])))
    endpoints;
  Eio.Switch.run @@ fun sw ->
  Hashtbl.iter
    (fun port group ->
      let group = List.rev group (* preserve declaration order for host matching *) in
      let socket =
        try
          Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env)
            (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
        with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
          (* a clear, actionable message + a distinct exit code the dev supervisor recognizes
             (so it reports "port busy" and self-heals, instead of a generic crash-loop) *)
          Printf.eprintf "fennec: port %d is already in use — another server is holding it.\n%!" port;
          exit 98
      in
      let handle flow addr =
        Eio.Semaphore.acquire slots;
        Fun.protect
          ~finally:(fun () -> Eio.Semaphore.release slots)
          (fun () -> handle_conn ~now ~clock ~timeout ~request_timeout ~fs group flow addr)
      in
      let on_error e =
        Printf.eprintf "fennec: connection error: %s\n%!" (Printexc.to_string e)
      in
      Eio.Fiber.fork ~sw (fun () ->
          if domains > 1 then
            Eio.Net.run_server socket handle ~additional_domains:(domain_mgr, domains - 1) ~on_error
          else Eio.Net.run_server socket handle ~on_error))
    ports;
  (* every port is now bound and serving — announce ONLY here, so a server that failed to bind
     (exit 98 above) never prints a misleading "ready" before it dies *)
  on_listen ()
