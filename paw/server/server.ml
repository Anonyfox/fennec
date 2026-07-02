(* The native I/O shell — a compact HTTP/1.1 + WebSocket server over Eio (no
   cohttp, no Lwt). It parses requests, runs each Endpoint's paw pipeline
   ([Pipeline.run_conn]), writes the HTTP response (or performs the RFC 6455 upgrade
   when a paw requested one), and dispatches across endpoints by Host + port. The
   only non-portable part of the framework. Framing lives in [Ws]. *)

module CH = Http

(* the live websocket channel type lives in core (Ws_channel) so paws can
   reference it; the server provides the concrete [send] *)
type ws = Ws_channel.t = {
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
let max_body_size = 8 * 1024 * 1024 (* 8 MiB request body *)

(* outcome of parsing one request off the connection *)
type request_result =
  | Req of parsed
  | Conn_eof (* clean end of stream — no (more) requests *)
  | Bad_request of string (* malformed — answer 400 and close *)
  | Too_large of string (* body/headers over a limit — answer 413 and close *)

(* case-insensitive lookup via {!Headers} — allocation-free compare (no lowercased copy of the key
   per call), and independent of how the parsed keys are cased *)
let header h k = Headers.get h k

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

(* a request line + headers larger than this is rejected (slowloris / oversized-head bound) *)
let max_head_size = 64 * 1024

(* [continue] is invoked just before the body is read, to honour Expect: 100-continue. [out] is a
   per-connection scratch int array (reused across requests, so parsing the head allocates nothing
   beyond the materialized strings). The head is scanned by the hand-crafted C parser ({!Http_parse});
   the body is read here. The blocking [ensure] that grows the buffer — and ONLY it — is bounded by
   [timeout] (a fully-buffered / pipelined head never arms a timer). *)
let read_request ~(continue : unit -> unit) ~(timeout : Eio.Time.Timeout.t) (r : Eio.Buf_read.t) (out : int array) : request_result =
  let rec parse_head () =
    let cs = Eio.Buf_read.peek r in
    let n = Http_parse.parse cs.Cstruct.buffer cs.Cstruct.off cs.Cstruct.len out Http_parse.max_headers in
    if n >= 0 then `Head (cs, n)
    else if n = Http_parse.malformed then `Bad
    else if n = Http_parse.too_many then `Too_large
    else if cs.Cstruct.len >= max_head_size then `Too_large (* incomplete but already oversized *)
    else
      match Eio.Time.Timeout.run timeout (fun () -> Ok (Eio.Buf_read.ensure r (cs.Cstruct.len + 1))) with
      | Ok () -> parse_head ()
      | Error `Timeout -> `Eof (* idle / slow client — close *)
      | exception (End_of_file | Eio.Buf_read.Buffer_limit_exceeded) -> if cs.Cstruct.len = 0 then `Eof else `Bad
  in
  match parse_head () with
  | `Eof -> Conn_eof
  | `Bad -> Bad_request "malformed request head"
  | `Too_large -> Too_large "request head too large"
  | `Head (cs, n) ->
    let buf = cs.Cstruct.buffer in
    let sub off len = Bigstringaf.substring buf ~off ~len in
    (* materialize from the C-provided offsets while [cs] is still valid (before consume); no
       per-header trim/lowercase — the C parser already trims OWS, and lookups are case-insensitive *)
    let meth = sub out.(0) out.(1) and target = sub out.(2) out.(3) and version = sub out.(4) out.(5) in
    let rec build i acc = if i < 0 then acc else let o = 7 + (4 * i) in build (i - 1) ((sub out.(o) out.(o + 1), sub out.(o + 2) out.(o + 3)) :: acc) in
    let hs = build (out.(6) - 1) [] in
    Eio.Buf_read.consume r n;
    let mk body = Req { meth; target; version; headers = hs; body } in
    let te = header hs "transfer-encoding" and cl = header hs "content-length" in
    let chunked = match te with Some v -> contains (String.lowercase_ascii v) "chunked" | None -> false in
    (* Content-Length AND Transfer-Encoding together is a request-smuggling vector *)
    if chunked && cl <> None then Bad_request "content-length with transfer-encoding"
    else begin
      (match header hs "expect" with Some v when contains (String.lowercase_ascii v) "100-continue" -> continue () | _ -> ());
      if chunked then (match read_chunked r with Ok body -> mk body | Error e -> Bad_request e)
      else
        match cl with
        | None -> mk ""
        | Some v -> (
          match int_of_string_opt (String.trim v) with
          | None -> Bad_request "invalid content-length"
          | Some len when len < 0 -> Bad_request "negative content-length"
          | Some len when len > max_body_size -> Too_large "request body too large"
          | Some 0 -> mk ""
          | Some len -> (match Eio.Buf_read.take len r with body -> mk body | exception (End_of_file | Failure _) -> Bad_request "truncated body"))
    end

let to_request ~host ~scheme ~remote_ip (p : parsed) : CH.request =
  let path, query_string = CH.split_target p.target in
  CH.make_request ~meth:(CH.meth_of_string p.meth) ~path ~query_string ~headers:p.headers
    ~body:p.body ~host ~scheme ~remote_ip ~version:p.version ()

let is_ws_upgrade (p : parsed) =
  match header p.headers "upgrade" with
  | Some v -> contains (String.lowercase_ascii v) "websocket"
  | None -> false

let has_header_ci headers k = Headers.mem headers k

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
  (* fire [on_close] EXACTLY once, however the read loop exits — a clean Close/EOF, OR an abrupt flow
     exception (TCP reset / a non-Eof IO error from the underlying flow) that would otherwise unwind
     the switch WITHOUT running on_close, leaking the session's subscriptions/observers forever. *)
  let closed = ref false in
  let fire_close () = if not !closed then (closed := true; ws.on_close ()) in
  let close ?code () =
    (match code with
     | Some c -> Eio.Stream.add outbox (Ws.close_frame ~code:c ())
     | None -> Eio.Stream.add outbox (Ws.close_frame ()));
    fire_close ()
  in
  (* read loop with fragmentation reassembly. [frag] is Some (opcode_rsv1, buf)
     while a fragmented message is in progress; control frames may interleave. *)
  let rec loop frag =
    match Ws.read_frame r with
    | Ws.Eof -> fire_close ()
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
  (* whatever happens in the read loop (clean close or an abrupt flow exception), guarantee teardown *)
  (try loop None with _ -> ());
  fire_close ()

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

module Conn = Conn
module Headers = Headers

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

(* ---- unified error funnel ----
   ALL request-scoped errors flow through ONE function so the developer has a single place to
   customize error rendering (JSON vs HTML, request IDs, branded pages). The default renders
   plain text; the user overrides via [~on_error] on [Fennec.serve]. *)

type request_error =
  | Handler_exception of exn * CH.request
  | Handler_timeout of CH.request
  | No_route of CH.request
  | Method_not_allowed of CH.request * CH.meth list (* the path exists but not for this method; the methods it DOES serve *)

(* The [Allow] header value for a path's declared methods: HEAD is added whenever GET is served (a GET
   route answers HEAD), OPTIONS is always present (we auto-answer it), and the list is emitted in a
   stable canonical order with any non-standard methods last. *)
let allow_header (declared : CH.meth list) : string =
  let has m = List.mem m declared in
  let std = List.filter (fun m -> has m || (m = CH.HEAD && has CH.GET) || m = CH.OPTIONS) [ CH.GET; CH.HEAD; CH.POST; CH.PUT; CH.PATCH; CH.DELETE; CH.OPTIONS ] in
  let others = List.filter (function CH.Other _ -> true | _ -> false) declared in
  String.concat ", " (List.map CH.string_of_meth (std @ others))

(* ──── read_request ──── *)

let parse_ ?(continue = fun () -> ()) s = read_request ~continue ~timeout:Eio.Time.Timeout.none (Eio.Buf_read.of_string s) (Http_parse.make_out ())
let body_of_ = function Req p -> Some p.body | _ -> None
let is_bad_ = function Bad_request _ -> true | _ -> false

let%test "GET no body"               = body_of_ (parse_ "GET / HTTP/1.1\r\nHost: a\r\n\r\n") = Some ""
let%test "content-length body"       = body_of_ (parse_ "POST /x HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\n\r\nHello") = Some "Hello"
let%test "chunked body decoded"      = body_of_ (parse_ "POST /x HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n World\r\n0\r\n\r\n") = Some "Hello World"
let%test "chunk extension tolerated" = body_of_ (parse_ "POST /x HTTP/1.1\r\nHost: a\r\nTransfer-Encoding: chunked\r\n\r\n3;x=1\r\nabc\r\n0\r\n\r\n") = Some "abc"
let%test "CL + TE rejected as smuggling" = is_bad_ (parse_ "POST /x HTTP/1.1\r\nHost: a\r\nContent-Length: 5\r\nTransfer-Encoding: chunked\r\n\r\nHello")
let%test_unit "100-continue triggers callback" =
  let called = ref false in
  let _ = parse_ ~continue:(fun () -> called := true) "POST /x HTTP/1.1\r\nHost: a\r\nExpect: 100-continue\r\nContent-Length: 2\r\n\r\nhi" in
  Fennec_hunt_unit.check "called" !called
let%test_unit "no Expect: callback not called" =
  let called = ref false in
  let _ = parse_ ~continue:(fun () -> called := true) "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
  Fennec_hunt_unit.check "not called" (not !called)
let%test "malformed request line"    = is_bad_ (parse_ "GARBAGE\r\n\r\n")

let default_on_error : request_error -> CH.response = function
  | Handler_exception (exn, _) ->
    Printf.eprintf "fennec: handler error: %s\n%!" (Printexc.to_string exn);
    CH.text ~status:500 "Internal Server Error"
  | Handler_timeout _ -> CH.text ~status:503 "Service Unavailable"
  | No_route _ -> CH.text ~status:404 "Not Found"
  | Method_not_allowed (_, allowed) -> { (CH.text ~status:405 "Method Not Allowed") with CH.headers = [ ("allow", allow_header allowed) ] }

(* A drop-in [~on_error] that renders framework errors (404/405/500/503) as JSON instead of plain text
   — for an API: [Paw.serve ~on_error:Server.json_on_error apps]. The 405 keeps its [Allow] header. paw
   carries no JSON library (prod-lean), so we hand-roll; the bodies are framework constants, but a tiny
   escaper keeps it correct if a message ever carries a quote. *)
let json_escape s =
  let buf = Buffer.create (String.length s + 2) in
  String.iter (fun c -> match c with '"' -> Buffer.add_string buf {|\"|} | '\\' -> Buffer.add_string buf {|\\|} | '\n' -> Buffer.add_string buf {|\n|} | '\r' -> Buffer.add_string buf {|\r|} | '\t' -> Buffer.add_string buf {|\t|} | c when Char.code c < 0x20 -> Buffer.add_string buf (Printf.sprintf {|\u%04x|} (Char.code c)) | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let json_error ?(extra_headers = []) ~status msg =
  { CH.status; headers = ("content-type", "application/json; charset=utf-8") :: extra_headers; body = Printf.sprintf {|{"error":"%s","status":%d}|} (json_escape msg) status }

let json_on_error : request_error -> CH.response = function
  | Handler_exception (exn, _) -> Printf.eprintf "fennec: handler error: %s\n%!" (Printexc.to_string exn); json_error ~status:500 "Internal Server Error"
  | Handler_timeout _ -> json_error ~status:503 "Service Unavailable"
  | No_route _ -> json_error ~status:404 "Not Found"
  | Method_not_allowed (_, allowed) -> json_error ~status:405 "Method Not Allowed" ~extra_headers:[ ("allow", allow_header allowed) ]

(* ──── json_on_error ──── *)

let jreq_ = CH.make_request ~meth:CH.GET ~path:"/" ()
let%test "json_escape: quote + backslash" = json_escape {|a"b\c|} = {|a\"b\\c|}
let%test "json_on_error: 404 is JSON" =
  let r = json_on_error (No_route jreq_) in
  r.CH.status = 404 && contains r.CH.body {|"error":"Not Found"|} && contains r.CH.body {|"status":404|} && Headers.get r.CH.headers "content-type" = Some "application/json; charset=utf-8"
let%test "json_on_error: 405 stays JSON and keeps Allow" =
  let r = json_on_error (Method_not_allowed (jreq_, [ CH.GET ])) in
  r.CH.status = 405 && Headers.get r.CH.headers "allow" = Some "GET, HEAD, OPTIONS" && contains r.CH.body {|"status":405|}

(* No endpoint answered. [allowed] is the set of methods the path is declared for, unioned across every
   endpoint on the host (empty ⇒ the path doesn't exist). A non-empty set means the request used the
   WRONG method: an OPTIONS probe with no explicit handler gets an automatic 204 + Allow, anything else
   gets a 405 + Allow. Only an empty set is a genuine 404. The Allow header is guaranteed present on the
   405 even when a custom [on_error] renders its own body and forgets it. *)
let resolve_miss ~on_error (req : CH.request) (allowed : CH.meth list) : CH.response =
  match allowed with
  | [] -> on_error (No_route req)
  | _ when req.CH.meth = CH.OPTIONS -> { CH.status = 204; headers = [ ("allow", allow_header allowed) ]; body = "" }
  | _ -> let r = on_error (Method_not_allowed (req, allowed)) in if Headers.mem r.CH.headers "allow" then r else { r with CH.headers = ("allow", allow_header allowed) :: r.CH.headers }

(* ──── allow_header / resolve_miss ──── *)

let amiss_ ?(meth = CH.GET) allowed = resolve_miss ~on_error:default_on_error (CH.make_request ~meth ~path:"/x" ()) allowed
let%test "allow_header adds HEAD for GET and always OPTIONS, canonical order" = allow_header [ CH.POST; CH.GET ] = "GET, HEAD, POST, OPTIONS"
let%test "allow_header: a lone POST gains OPTIONS but not HEAD" = allow_header [ CH.POST ] = "POST, OPTIONS"
let%test "empty allowed → 404 (path absent)" = (amiss_ []).CH.status = 404
let%test "wrong method on an existing path → 405" = (amiss_ ~meth:CH.DELETE [ CH.GET; CH.POST ]).CH.status = 405
let%test "405 carries the Allow header" = Headers.get (amiss_ ~meth:CH.DELETE [ CH.GET ]).CH.headers "allow" = Some "GET, HEAD, OPTIONS"
let%test "OPTIONS on an existing path → automatic 204 + Allow" =
  let r = amiss_ ~meth:CH.OPTIONS [ CH.GET; CH.POST ] in
  r.CH.status = 204 && Headers.get r.CH.headers "allow" = Some "GET, HEAD, POST, OPTIONS"
let%test "OPTIONS on an absent path is still a 404" = (amiss_ ~meth:CH.OPTIONS []).CH.status = 404
let%test_unit "a custom on_error 405 is backstopped with Allow" =
  let on_error = function Method_not_allowed _ -> CH.text ~status:405 "nope" | e -> default_on_error e in
  let r = resolve_miss ~on_error (CH.make_request ~meth:CH.PUT ~path:"/x" ()) [ CH.GET ] in
  Fennec_hunt_unit.check "body is the custom one" (r.CH.body = "nope");
  Fennec_hunt_unit.check "Allow stamped by the server" (Headers.get r.CH.headers "allow" = Some "GET, HEAD, OPTIONS")

(* Run a handler under a per-request deadline. On timeout, Eio CANCELS the whole handler
   fiber tree — including any sub-fibers it forked (parallel fetches) and their in-flight
   IO. A thrown exception is caught. Both flow to the error funnel. (It bounds the handler's
   logic; a streamed body — send_chunked/SSE — is produced afterwards, so SSE isn't cut.) *)
let run_handler ~clock ~timeout ~on_error (handler : Pipeline.t) (req : CH.request) : Conn.t =
  let attempt () = try Ok (Pipeline.run_conn handler req) with exn -> Error (`Exn exn) in
  (* arming an Eio timer + cancellation scope per request is expensive (it dominated the profile), and
     a per-request handler deadline only catches a handler stuck on a slow Eio op — so it is OPT-IN
     ([timeout] <= 0 ⇒ skip it; handlers bound their own slow upstream calls when needed). Slowloris
     is still defended at the read layer (see [loop]). *)
  match (if timeout <= 0. then attempt () else Eio.Time.with_timeout clock timeout attempt) with
  | Ok conn -> conn
  | Error `Timeout -> Conn.set_error (Conn.respond (Conn.make req) (on_error (Handler_timeout req))) "handler timeout"
  | Error (`Exn exn) -> Conn.set_error (Conn.respond (Conn.make req) (on_error (Handler_exception (exn, req)))) (Printexc.to_string exn)

(* ──── run_handler ──── *)

let status_of_ c = match Conn.resp c with Some r -> r.CH.status | None -> 0
let req_ = CH.make_request ~meth:CH.GET ~path:"/" ()

let%test "normal handler → 200" =
  Eio_main.run @@ fun env ->
  status_of_ (run_handler ~clock:(Eio.Stdenv.clock env) ~on_error:default_on_error ~timeout:1.0 (fun c -> Conn.text c "ok") req_) = 200

let%test "hung handler → 503" =
  Eio_main.run @@ fun env ->
  status_of_ (run_handler ~clock:(Eio.Stdenv.clock env) ~on_error:default_on_error ~timeout:0.05 (fun _ -> Eio.Fiber.await_cancel ()) req_) = 503

let%test "throwing handler → 500" =
  Eio_main.run @@ fun env ->
  status_of_ (run_handler ~clock:(Eio.Stdenv.clock env) ~on_error:default_on_error ~timeout:1.0 (fun _ -> failwith "boom") req_) = 500

let%test_unit "sub-fibers cancelled at deadline" =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let t0 = Eio.Time.now clock in
  let c = run_handler ~clock ~on_error:default_on_error ~timeout:0.05
    (fun c -> Eio.Fiber.both (fun () -> Eio.Time.sleep clock 10.0) (fun () -> Eio.Time.sleep clock 10.0); c) req_ in
  Fennec_hunt_unit.check "returns at deadline, not after 10s" (Eio.Time.now clock -. t0 < 1.0);
  Fennec_hunt_unit.check "503" (status_of_ c = 503)

(* Handle one connection. [resolve ~host] returns the precompiled endpoint handler(s) to try for a
   request: in prod (and on the dev gateway) it routes by Host pattern; on a dev convenience port it
   always returns that one endpoint's handler. Handlers are compiled ONCE at serve, never per request.
   [scheme] is the transport scheme — "https" when {!run} terminated TLS in-process for this
   connection, else "http"; it becomes [Conn.scheme] so Secure cookies / force-https work over
   in-process HTTPS with no proxy. A paw pipeline may answer with an HTTP response OR a websocket
   upgrade (the ws is itself a paw). *)
let handle_conn ~now ~clock ~timeout ~request_timeout ~fs ~on_error ?(on_access : (Access.t -> unit) option) ~scheme ?(allowed = fun ~host:(_ : string) ~path:(_ : string) -> []) ~(resolve : host:string -> Pipeline.t list) flow addr =
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
  (* per-connection scratch for the C head parser, reused across this connection's requests *)
  let out = Http_parse.make_out () in
  let rec loop () =
    (* read_request scans the head with the hand-crafted C parser and bounds ONLY its blocking
       buffer-grow by [timeout]; a fully-buffered / pipelined head never arms a timer. *)
    match read_request ~continue ~timeout r out with
    | Conn_eof -> ()
    | Bad_request msg ->
      (* a malformed request never became a conn/req, but operators still want 400 spikes in the
         access log — emit a minimal event (no method/path, the parse error as [error]) *)
      respond_and_close (CH.text ~status:400 "Bad Request");
      (match on_access with
       | Some f -> ( try f (Access.make ~meth:"" ~path:"" ~status:400 ~dur_us:0 ~bytes:(String.length "Bad Request") ~ip:remote_ip ~error:(Some msg) ()) with _ -> ())
       | None -> ())
    | Too_large msg ->
      respond_and_close (CH.text ~status:413 "Payload Too Large");
      (match on_access with
       | Some f -> ( try f (Access.make ~meth:"" ~path:"" ~status:413 ~dur_us:0 ~bytes:(String.length "Payload Too Large") ~ip:remote_ip ~error:(Some msg) ()) with _ -> ())
       | None -> ())
    | Req p -> (
      (* [scheme] is the real transport: "https" when we terminated TLS in-process, else "http" — so
         Secure cookies / force-https Just Work over in-process HTTPS. Behind a TLS-terminating proxy
         (plaintext transport here), a force-https / session paw still reads X-Forwarded-Proto, which
         takes precedence. host is the normalized Host header. *)
      let host = Host_pattern.normalize (match header p.headers "host" with Some h -> h | None -> "") in
      let req = to_request ~host ~scheme ~remote_ip p in
      (* the endpoints matching this Host, most-specific-first (overlap: several may match). Try each
         on a FRESH conn (run_handler makes one from [req]); the first to ANSWER wins, a declining one
         (no route matched — its auth/matched middleware never fired, its conn discarded) falls
         through to the next. None answered / none matched → a clean 404. A handler exception becomes
         a clean 500, never a dropped connection. *)
      let rec try_each = function
        | [] -> Conn.respond (Conn.make req) (resolve_miss ~on_error req (allowed ~host ~path:req.CH.path))
        | handler :: rest ->
          let c = run_handler ~clock ~timeout:request_timeout ~on_error handler req in
          if Conn.answered c then c else try_each rest
      in
      let conn = try_each (resolve ~host) in
      (* Capture the ONE structured access event for this request and hand it to [on_access]. Called
         from each terminal path with the FINAL [status]/[bytes] — for the buffered path that is after
         {!Responder.finalize}, so [bytes] is the real post-gzip body length and [status] the final
         status (e.g. a 304). [dur_us] is read here (after the work), from the conn's single start
         stamp. Effect-isolated: a throwing sink never breaks the connection. *)
      let emit_access ~status ~bytes =
        (* zero-cost when unused: with no [~on_access] AND no Logger sink on the conn, the event is
           never even built — a server that logs nothing pays nothing for the seam *)
        match (on_access, Conn.access_sink conn) with
        | None, None -> ()
        | oa, sink ->
          let a =
            Access.make ~meth:(CH.string_of_meth req.CH.meth) ~path:req.CH.path ~status
              ~dur_us:(Conn.elapsed_us conn) ~bytes ~ip:(Conn.remote_ip conn) ~req_id:(Request_id.current conn)
              ~error:(Conn.error conn) ~version:req.CH.version ~host:req.CH.host ()
          in
          (* the generic structured seam (a metrics shipper / custom sink) AND the per-request sink the
             Logger paw installed (so a [Paw.use (Logger.make ())] line gets the accurate post-gzip
             size with no server wiring). Both effect-isolated. *)
          (match oa with Some f -> (try f a with _ -> ()) | None -> ());
          (match sink with Some f -> (try f a with _ -> ()) | None -> ())
      in
      match Conn.upgrade_handler conn with
      | Some setup when is_ws_upgrade p ->
        (* a paw requested a websocket upgrade — a long-lived ws, not a request/response, so it is
           outside the access log (it never produces a finalized HTTP response) *)
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
          (* the streamed body length isn't known up front (chunked) / isn't buffered here (file), so
             [bytes] is reported as 0 — the status + timing are still logged *)
          emit_access ~status:resp.CH.status ~bytes:0;
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
          (* AFTER finalize: status is final, body is post-gzip — the exact on-the-wire size *)
          emit_access ~status:resp.CH.status ~bytes:(String.length resp.CH.body);
          (* Flush only when the read buffer is drained. A lone request (buffer empty, client waiting)
             flushes immediately — no added latency. A pipelined burst (more requests already buffered)
             keeps batching its responses into the write buffer, so N pipelined requests cost a handful
             of write syscalls instead of N. (Buf_write also auto-flushes when its buffer fills, and
             with_flow flushes on close, so nothing is ever stranded.) *)
          if keep_alive then (
            if Eio.Buf_read.buffered_bytes r = 0 then Eio.Buf_write.flush w;
            loop ())
          else Eio.Buf_write.flush w))
  in
  loop ()

(* ──── in-process TLS, end to end ──── *)

(* The flagship proof: a self-signed cert terminates TLS in-process, and the very HTTPS client the
   ACME client uses round-trips a request whose handler sees [Conn.scheme = "https"]. Over a genuine
   TLS handshake on loopback — no openssl, no proxy — this locks BOTH that scheme=https propagates
   (so Secure cookies / force-https work over in-process HTTPS) AND that {!Tls_termination.self_signed}
   produces a server a real client can talk to. *)
let%test "in-process TLS: real handshake + the handler sees scheme=https" =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env and fs = Eio.Stdenv.fs env in
  let now () = Eio.Time.now clock in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.0 in
  let cfg = Tls_termination.self_signed ~hosts:[ "localhost" ] () in
  (* an endpoint that simply echoes the request's scheme back as the body *)
  let ep = Endpoint.make ~name:"t" ~hosts:[ "*" ] () |> Endpoint.get "/" (fun c -> Conn.text c (Conn.scheme c)) in
  let resolve ~host:_ = [ Endpoint.handler ep ] in
  let sock = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | _ -> 0 in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow addr ->
          (* terminate TLS in-process, then drive the real connection loop with scheme="https" *)
          match (try Some (Tls_eio.server_of_flow cfg flow) with _ -> None) with
          | Some tls_flow -> handle_conn ~now ~clock ~timeout ~request_timeout:5.0 ~fs ~on_error:default_on_error ~scheme:"https" ~resolve tls_flow addr
          | None -> ()));
  (* an inline TLS client to the EXACT bound loopback address — deterministic (no DNS / IPv4-vs-IPv6
     ambiguity). The accept-all authenticator stands in for "trust the dev cert". *)
  let authenticator : X509.Authenticator.t = fun ?ip:_ ~host:_ _ -> Ok None in
  let cconf = match Tls.Config.client ~authenticator () with Ok c -> c | Error (`Msg m) -> failwith m in
  let raw = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let tls = Tls_eio.client_of_flow cconf raw in
  Eio.Flow.copy_string "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" tls;
  let response = Eio.Buf_read.take_all (Eio.Buf_read.of_flow tls ~max_size:65536) in
  (* status line "HTTP/1.1 200 OK" + the handler's scheme echo as the body *)
  contains response " 200 " && String.length response >= 5 && String.sub response (String.length response - 5) 5 = "https"

(* ──── 405 / automatic OPTIONS, end to end ──── *)

(* drive one plaintext request through the real connection loop with the same [allowed] resolver the
   server wires, and return the raw response — so the 405 / auto-OPTIONS path is proven over the wire,
   not just in [resolve_miss]'s unit tests *)
let serve_one_ ~env ~ep request_str =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env and fs = Eio.Stdenv.fs env in
  let now () = Eio.Time.now clock in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.0 in
  let resolve ~host:_ = [ Endpoint.handler ep ] in
  let allowed ~host:_ ~path = Endpoint.allowed_methods ep ~path in
  let sock = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | _ -> 0 in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow addr ->
          handle_conn ~now ~clock ~timeout ~request_timeout:5.0 ~fs ~on_error:default_on_error ~scheme:"http" ~allowed ~resolve flow addr));
  let raw = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string request_str raw;
  Eio.Buf_read.take_all (Eio.Buf_read.of_flow raw ~max_size:65536)

(* a GET+POST endpoint, so the path exists for GET/HEAD/POST/OPTIONS but not DELETE *)
let getpost_ep_ () = Endpoint.make ~name:"t" ~hosts:[ "*" ] () |> Endpoint.get "/" (fun c -> Conn.text c "home") |> Endpoint.post "/" (fun c -> Conn.text c "made")

let%test "wire: wrong method on an existing path → 405 + Allow" =
  Eio_main.run @@ fun env ->
  let resp = serve_one_ ~env ~ep:(getpost_ep_ ()) "DELETE / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" in
  contains resp " 405 " && contains (String.lowercase_ascii resp) "allow:" && contains resp "GET" && contains resp "POST" && contains resp "OPTIONS"

let%test "wire: OPTIONS with no explicit handler → automatic 204 + Allow" =
  Eio_main.run @@ fun env ->
  let resp = serve_one_ ~env ~ep:(getpost_ep_ ()) "OPTIONS / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" in
  contains resp " 204 " && contains (String.lowercase_ascii resp) "allow:"

let%test "wire: an unknown path is still a 404" =
  Eio_main.run @@ fun env ->
  let resp = serve_one_ ~env ~ep:(getpost_ep_ ()) "DELETE /nope HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" in
  contains resp " 404 "

(* ──── on_access capture: the structured event over the wire ──── *)

(* like [serve_one_] but capture the [Access.t] the server emits for the request, so the µs/bytes/
   status/method/path capture is proven over the REAL connection loop (post-finalize), not in
   isolation. Returns the captured event (the last one, if a keep-alive sent several). *)
let access_of_ ~env ~ep request_str : Access.t option =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env and fs = Eio.Stdenv.fs env in
  let now () = Eio.Time.now clock in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.0 in
  let resolve ~host:_ = [ Endpoint.handler ep ] in
  let allowed ~host:_ ~path = Endpoint.allowed_methods ep ~path in
  let captured = ref None in
  let on_access a = captured := Some a in
  let sock = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | _ -> 0 in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow addr ->
          handle_conn ~now ~clock ~timeout ~request_timeout:5.0 ~fs ~on_error:default_on_error ~on_access ~scheme:"http" ~allowed ~resolve flow addr));
  let raw = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string request_str raw;
  let _ = Eio.Buf_read.take_all (Eio.Buf_read.of_flow raw ~max_size:65536) in
  !captured

(* an endpoint that answers a known, large-enough body so we can assert the post-finalize byte count *)
let body_ep_ body = Endpoint.make ~name:"t" ~hosts:[ "*" ] () |> Endpoint.get "/p" (fun c -> Conn.text c body)

let%test_unit "on_access: method/path/status/bytes captured post-finalize" =
  Eio_main.run @@ fun env ->
  (* a small body (< the compress threshold) so post-finalize bytes == the body length exactly *)
  let body = "hello access" in
  match access_of_ ~env ~ep:(body_ep_ body) "GET /p HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" with
  | None -> Fennec_hunt_unit.check "an access event was emitted" false
  | Some a ->
    Fennec_hunt_unit.check_eq "method" ~expected:"GET" ~got:a.Access.meth;
    Fennec_hunt_unit.check_eq "path" ~expected:"/p" ~got:a.Access.path;
    Fennec_hunt_unit.check "status 200" (a.Access.status = 200);
    Fennec_hunt_unit.check_eq "bytes = body length (uncompressed)" ~expected:(string_of_int (String.length body)) ~got:(string_of_int a.Access.bytes);
    Fennec_hunt_unit.check "dur_us is non-negative" (a.Access.dur_us >= 0);
    Fennec_hunt_unit.check "host carried" (a.Access.host = "localhost");
    Fennec_hunt_unit.check "no error on a 200" (a.Access.error = None)

let%test "on_access: a large body reports the POST-GZIP size (smaller than the source)" =
  Eio_main.run @@ fun env ->
  (* a highly compressible body well over the 1KB compress threshold → finalize gzips it; the event's
     bytes must reflect the compressed size, i.e. be much smaller than the source — the headline win
     (the old before_send logger could not see this number at all) *)
  let body = String.make 50_000 'a' in
  match access_of_ ~env ~ep:(body_ep_ body) "GET /p HTTP/1.1\r\nHost: localhost\r\nAccept-Encoding: gzip\r\nConnection: close\r\n\r\n" with
  | None -> false
  | Some a -> a.Access.status = 200 && a.Access.bytes > 0 && a.Access.bytes < String.length body / 10

let%test "on_access: a throwing handler is logged as a 500 with the error message" =
  Eio_main.run @@ fun env ->
  let ep = Endpoint.make ~name:"t" ~hosts:[ "*" ] () |> Endpoint.get "/boom" (fun _ -> failwith "kaboom") in
  match access_of_ ~env ~ep "GET /boom HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" with
  | None -> false
  | Some a -> a.Access.status = 500 && (match a.Access.error with Some m -> contains m "kaboom" | None -> false)

(* ──── back-compat: the Logger PAW logs via the server even with NO ~on_access wired ──── *)

(* drive a request through an endpoint that has [Logger.make ~sink:buf ()] mounted (the SAME shape
   as the 6 call sites: [Paw.use (Paw.Logger.make ())]) and NO server-level ~on_access. The logger
   paw installs a per-conn sink; the server's emit invokes it after finalize. This proves the back-
   compat wiring: a mounted logger still produces a line, now with the size + µs, with zero server
   wiring. *)
let%test_unit "back-compat: a mounted Logger paw logs (post-finalize) with no on_access" =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and clock = Eio.Stdenv.clock env and fs = Eio.Stdenv.fs env in
  let now () = Eio.Time.now clock in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.0 in
  let buf = Buffer.create 128 in
  (* mount the logger exactly as userland does, forcing Logfmt + the test sink so the assertion is
     format-stable and capture-able *)
  let ep =
    Endpoint.make ~name:"t" ~hosts:[ "*" ] ()
    |> Endpoint.use (Logger.make ~format:Logger.Logfmt ~sink:(Buffer.add_string buf) ())
    |> Endpoint.get "/p" (fun c -> Conn.text c "hello logger")
  in
  let resolve ~host:_ = [ Endpoint.handler ep ] in
  let sock = Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr sock with `Tcp (_, p) -> p | _ -> 0 in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork ~sw sock ~on_error:(fun _ -> ()) (fun flow addr ->
          (* NOTE: no ~on_access — the only logging path is the conn sink the paw installed *)
          handle_conn ~now ~clock ~timeout ~request_timeout:5.0 ~fs ~on_error:default_on_error ~scheme:"http" ~resolve flow addr));
  let raw = Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  Eio.Flow.copy_string "GET /p HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n" raw;
  let _ = Eio.Buf_read.take_all (Eio.Buf_read.of_flow raw ~max_size:65536) in
  let out = Buffer.contents buf in
  Fennec_hunt_unit.check "the logger emitted a line" (String.length out > 0);
  Fennec_hunt_unit.check "line carries method=GET" (contains out "method=GET");
  Fennec_hunt_unit.check "line carries path=/p" (contains out "path=/p");
  Fennec_hunt_unit.check "line carries status=200" (contains out "status=200");
  Fennec_hunt_unit.check "line carries the byte count" (contains out (Printf.sprintf "bytes=%d" (String.length "hello logger")))

(* Run a {!Host_router} table, blocking. In PROD the whole table is served on ONE port
   ([FENNEC_PORT], default 80) and selected per request by Host pattern — one process, arbitrary
   subdomains/wildcards. In DEV the same table is served on the GATEWAY port ([FENNEC_PORT] base,
   default 4000) with identical Host routing (prod fidelity), and EACH non-catch-all endpoint also
   gets a forced convenience port ([base + 1 + i], declaration order) so a browser reaches it with
   no /etc/hosts. A different base (--port) shifts the whole block, so instances never collide.

   Method-aware misses (computed only AFTER every endpoint on the host declines, never on the hot
   path): a path declared for OTHER methods yields [405 Method Not Allowed] with an [Allow] header
   (via the [~on_error] funnel, [Allow] guaranteed) rather than a blanket 404, and an [OPTIONS] probe
   with no explicit handler is auto-answered [204 No Content] + [Allow]. Only a truly unknown path is
   a 404. An explicit [OPTIONS] route or a CORS paw still wins (it answers before the dispatch declines).
   @param dev             dev mode. Default from FENNEC_ENV.
   @param timeout         per-request idle/header read timeout, seconds (default 30).
   @param request_timeout per-request handler deadline in seconds; <= 0 (the default) disables it —
          arming an Eio timer per request is costly and only catches a handler stuck on a slow Eio op,
          so it is opt-in. Slowloris is defended at the read layer regardless. Set > 0 for a 503 cap.
   @param max_conns       concurrent-connection cap (default 10_000).
   @param parallelism     worker domains (per-core); auto by default, or FENNEC_PARALLELISM.
   @param on_access       called once per finished request with its {!Access.t} (method, path, final
          status, µs duration, post-gzip body bytes, ip, request id, error). The structured seam the
          access log / a metrics shipper hangs off; default = a no-op (nothing logged). Effect-only.
   @param on_listen       called post-bind with the (endpoint name, url) pairs for the banner. *)
(* peek the SNI host from a connection without consuming it (MSG_PEEK), so an on-demand handler can
   ensure that host's cert before the TLS handshake reads the same ClientHello. Fail-safe → None. *)
let peek_sni flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> None
  | Some fd -> (
    try
      Eio_unix.Fd.use_exn "fennec-sni-peek" fd (fun ufd ->
          (try Eio_unix.await_readable ufd with _ -> ());
          let b = Bytes.create 4096 in
          match Unix.recv ufd b 0 (Bytes.length b) [ Unix.MSG_PEEK ] with n when n > 0 -> Sni.host_of_client_hello (Bytes.sub_string b 0 n) | _ -> None)
    with _ -> None)

let run ?(timeout = 30.0) ?(request_timeout = 0.0) ?(max_conns = 10_000) ?parallelism ?dev ?tls ?on_demand ?(on_error = default_on_error) ?on_access ?(on_listen = fun (_ : (string * string) list) -> ()) ~env (router : Endpoint.t Host_router.t) =
  let dev = match dev with Some d -> d | None -> Dev_proto.is_dev () in
  (* worker domains for true multicore (the nginx-worker model): each handles whole connections.
     Auto — 1 in dev (deterministic; the livereload relay is shared), all cores in prod — or set
     ~parallelism / FENNEC_PARALLELISM. (Named "parallelism", not "domains", which now means hosts.) *)
  let parallelism =
    match parallelism with
    | Some n -> max 1 n
    | None -> ( match Option.bind (Sys.getenv_opt Dev_proto.env_parallelism) int_of_string_opt with Some n -> max 1 n | None -> if dev then 1 else Domain.recommended_domain_count ())
  in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  let clock = Eio.Stdenv.clock env in
  let now () = Eio.Time.now clock in
  let fs = Eio.Stdenv.fs env in
  let timeout = Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) timeout in
  let slots = Eio.Semaphore.make max_conns in
  let entries = Host_router.entries router in
  (* base port, by precedence: explicit FENNEC_PORT; else dev 4000; else (prod) the PaaS-injected
     $PORT (Heroku/Render/Fly/… — you're behind their router, serving plain HTTP on the assigned
     port); else 443 when terminating TLS in-process (HTTPS, with :80 doing redirect + ACME), else 80. *)
  let base =
    match Option.bind (Sys.getenv_opt Dev_proto.env_port) int_of_string_opt with
    | Some p -> p
    | None ->
      if dev then 4000
      else (
        match Option.bind (Sys.getenv_opt "PORT") int_of_string_opt with
        | Some p -> p
        | None -> if tls <> None then 443 else 80)
  in
  match Port_plan.of_base ~base ~count:(List.length entries) with
  | Error msg -> Error (`Bad_plan msg)
  | Ok plan ->
  let exception Port_in_use of int in
  (try
  (* compile EVERY endpoint's handler ONCE here — the route tables are built at boot, never per
     request — and look them up by physical identity (the same Endpoint.t objects the router returns). *)
  let compiled = List.map (fun (en : Endpoint.t Host_router.entry) -> (en.Host_router.ep, Endpoint.handler en.Host_router.ep)) entries in
  let handlers_for (eps : Endpoint.t list) : Pipeline.t list = List.filter_map (fun e -> List.assq_opt e compiled) eps in
  (* the Host-routed resolver: the precompiled handlers of every endpoint matching the Host,
     most-specific-first, with overlap handled by the dispatch (first to answer wins). *)
  let by_host ~(host : string) : Pipeline.t list = handlers_for (Host_router.route_all router ~host) in
  (* the parallel resolver consulted only AFTER every endpoint on the host declines: the methods the
     path is declared for, unioned across those endpoints, so the server can answer 405 / auto-OPTIONS
     instead of a blanket 404. Off the hot path entirely (a matched route never reaches it). *)
  let allowed_for ~(host : string) ~(path : string) : CH.meth list =
    Host_router.route_all router ~host |> List.concat_map (fun ep -> Endpoint.allowed_methods ep ~path) |> List.sort_uniq compare
  in
  (* a concrete host that matches an endpoint's first pattern, so its forced port can resolve EXACTLY
     as the gateway would for that host — including any same-domain overlap peers and the catch-all. *)
  let repr_host (e : Endpoint.t Host_router.entry) =
    let pick = function
      | Host_pattern.Exact h :: _ -> h
      | Host_pattern.Suffix s :: _ -> "app" ^ s (* ".acme.com" -> "app.acme.com", matches "*.acme.com" *)
      | Host_pattern.Any :: _ -> "localhost"
      | [] -> "localhost"
    in
    pick e.Host_router.patterns
  in
  (* (port, resolver) bindings: prod = one routed port; dev = the routed GATEWAY (prod-identical,
     at the base) plus ONE forced port per endpoint at base+1+i — contiguous, in declaration order,
     so the ports read cleanly: the base routes by Host, base+1.. are the named endpoints, no gaps.
     A forced port resolves AS THE GATEWAY WOULD for that endpoint's own host (route_all of a
     representative host), so same-domain overlap peers and catch-all fall-through behave exactly like
     prod — no /etc/hosts and no Host header needed. Several same-domain endpoints each get a port,
     and each shows the same full stack. *)
  let binds =
    if not dev then [ (base, by_host, allowed_for) ]
    else
      let forced =
        List.mapi
          (fun i (e : Endpoint.t Host_router.entry) ->
            let host = repr_host e in
            ( Port_plan.endpoint_port plan ~index:i,
              (fun ~host:(_ : string) -> handlers_for (Host_router.route_all router ~host)),
              fun ~host:(_ : string) ~path -> allowed_for ~host ~path ))
          entries
      in
      (Port_plan.gateway plan, by_host, allowed_for) :: forced
  in
  Eio.Switch.run @@ fun sw ->
  (* graceful shutdown: SIGINT/SIGTERM stops accepting + drains in-flight requests (each already
     bounded by [request_timeout]), then [run_server] returns and the server exits cleanly — what a
     zero-downtime deploy / k8s rolling update needs. The handler only flips an atomic
     (async-signal-safe); a fiber turns that into [run_server]'s [stop] promise. *)
  let shutting_down = Atomic.make false in
  let on_sig (_ : int) = Atomic.set shutting_down true in
  ignore (Sys.signal Sys.sigint (Sys.Signal_handle on_sig));
  ignore (Sys.signal Sys.sigterm (Sys.Signal_handle on_sig));
  let stop_p, stop_r = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let rec wait () =
        if Atomic.get shutting_down then Eio.Promise.resolve stop_r ()
        else (Eio.Time.sleep clock 0.1; wait ())
      in
      wait ());
  List.iter
    (fun (port, resolve, allowed) ->
      let socket =
        try Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env) (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
        with Unix.Unix_error (Unix.EADDRINUSE, _, _) -> raise (Port_in_use port)
      in
      let serve_conn ~scheme flow addr = handle_conn ~now ~clock ~timeout ~request_timeout ~fs ~on_error ?on_access ~scheme ~allowed ~resolve flow addr in
      let handle flow addr =
        Eio.Semaphore.acquire slots;
        Fun.protect ~finally:(fun () -> Eio.Semaphore.release slots) (fun () ->
            (* on-demand TLS: peek the SNI and ensure its certificate BEFORE reading the source, so a
               first connection to a new tenant domain issues the cert then handshakes with it *)
            (match on_demand with Some ensure -> (match peek_sni flow with Some host -> (try ensure host with _ -> ()) | None -> ()) | None -> ());
            (* [tls] is a SOURCE read per connection (not a static config) so ACME renewal can swap
               the live cert with no restart; [None] (no TLS, or ACME hasn't issued yet) serves plain *)
            match (match tls with Some src -> src () | None -> None) with
            | None -> serve_conn ~scheme:"http" flow addr
            | Some cfg -> (
              (* terminate TLS for this connection; a failed handshake (a non-TLS client, an SNI
                 mismatch) drops the connection rather than erroring the whole server. The request
                 scheme is "https" — we are the TLS terminator, so it is ground truth. *)
              match (try Some (Tls_eio.server_of_flow cfg flow) with _ -> None) with
              | Some tls_flow -> serve_conn ~scheme:"https" tls_flow addr
              | None -> ()))
      in
      let on_error e =
        (* a client going away mid-request (reset / broken pipe / EOF) is normal (a reload abandons
           in-flight connections) — not a server error; only genuinely unexpected errors get a line *)
        let s = Printexc.to_string e in
        let client_gone = e = End_of_file || contains s "Connection reset" || contains s "Connection_reset" || contains s "Broken pipe" || contains s "EPIPE" in
        if not client_gone then Printf.eprintf "fennec: connection error: %s\n%!" s
      in
      Eio.Fiber.fork ~sw (fun () ->
          if parallelism > 1 then Eio.Net.run_server socket handle ~additional_domains:(domain_mgr, parallelism - 1) ~stop:stop_p ~on_error
          else Eio.Net.run_server socket handle ~stop:stop_p ~on_error))
    binds;
  (* every port is now bound — announce ONLY here (a failed bind exit 98'd above). Each endpoint is
     announced at its own contiguous forced port (base+1+i); the gateway (base) is the supervisor's
     to show, since it owns the banner and knows the base. *)
  let url p = Printf.sprintf "%s://localhost:%d" (if tls <> None then "https" else "http") p in
  let named = List.mapi (fun i (e : Endpoint.t Host_router.entry) -> (e.Host_router.name, url (Port_plan.endpoint_port plan ~index:i))) entries in
  on_listen named;
  Ok ()
  with Port_in_use port -> Error (`Port_in_use port))
