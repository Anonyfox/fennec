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

type parsed = { meth : string; target : string; headers : (string * string) list; body : string }

let header h k = List.assoc_opt (String.lowercase_ascii k) h

(* does [hay] contain [needle]? (small, no regex) *)
let contains hay needle =
  let nh = String.length hay and nn = String.length needle in
  let rec go i = i + nn <= nh && (String.sub hay i nn = needle || go (i + 1)) in
  nn = 0 || go 0

let read_request (r : Eio.Buf_read.t) : parsed option =
  match try Some (Eio.Buf_read.line r) with End_of_file | Failure _ -> None with
  | None | Some "" -> None
  | Some reqline ->
    let meth, target =
      match String.split_on_char ' ' reqline with m :: t :: _ -> (m, t) | _ -> ("GET", "/")
    in
    let rec headers acc =
      match Eio.Buf_read.line r with
      | "" -> List.rev acc
      | line -> (
        match String.index_opt line ':' with
        | Some i ->
          let k = String.lowercase_ascii (String.trim (String.sub line 0 i)) in
          let v = String.trim (String.sub line (i + 1) (String.length line - i - 1)) in
          headers ((k, v) :: acc)
        | None -> headers acc)
      | exception End_of_file -> List.rev acc
    in
    let hs = headers [] in
    let body =
      match header hs "content-length" with
      | Some n -> ( try Eio.Buf_read.take (int_of_string n) r with _ -> "")
      | None -> ""
    in
    Some { meth; target; headers = hs; body }

let to_request (p : parsed) : CH.request =
  let path, query = CH.split_target p.target in
  { CH.meth = CH.meth_of_string p.meth; path; query; headers = p.headers; body = p.body }

let is_ws_upgrade (p : parsed) =
  match header p.headers "upgrade" with
  | Some v -> contains (String.lowercase_ascii v) "websocket"
  | None -> false

let write_http (w : Eio.Buf_write.t) (resp : CH.response) ~keep_alive =
  let reason =
    match resp.CH.status with
    | 200 -> "OK"
    | 404 -> "Not Found"
    | 403 -> "Forbidden"
    | 500 -> "Internal Server Error"
    | _ -> "OK"
  in
  Eio.Buf_write.string w (Printf.sprintf "HTTP/1.1 %d %s\r\n" resp.CH.status reason);
  List.iter
    (fun (k, v) -> Eio.Buf_write.string w (Printf.sprintf "%s: %s\r\n" k v))
    resp.CH.headers;
  Eio.Buf_write.string w (Printf.sprintf "content-length: %d\r\n" (String.length resp.CH.body));
  Eio.Buf_write.string w
    (Printf.sprintf "connection: %s\r\n\r\n" (if keep_alive then "keep-alive" else "close"));
  Eio.Buf_write.string w resp.CH.body

let ws_handshake (w : Eio.Buf_write.t) (p : parsed) : unit =
  let key = match header p.headers "sec-websocket-key" with Some k -> k | None -> "" in
  Eio.Buf_write.string w
    ("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\
      Sec-WebSocket-Accept: " ^ Ws.accept_key key ^ "\r\n\r\n");
  Eio.Buf_write.flush w

(* drive the websocket: one writer fiber serializes all outgoing frames; the
   reader loop handles ping/pong/close and dispatches text to [ws.on_text] *)
let serve_ws ~sw (r : Eio.Buf_read.t) (w : Eio.Buf_write.t) (setup : ws -> unit) =
  let outbox : Ws.frame Eio.Stream.t = Eio.Stream.create 256 in
  let text_frame s = { Ws.fin = true; opcode = Ws.Text; payload = s } in
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
  let rec loop () =
    match try Ws.read_frame r with _ -> None with
    | None -> ws.on_close ()
    | Some { Ws.opcode = Ws.Close; _ } ->
      Eio.Stream.add outbox { Ws.fin = true; opcode = Ws.Close; payload = "" };
      ws.on_close ()
    | Some { Ws.opcode = Ws.Ping; payload; _ } ->
      Eio.Stream.add outbox { Ws.fin = true; opcode = Ws.Pong; payload };
      loop ()
    | Some { Ws.opcode = Ws.Pong; _ } -> loop ()
    | Some { Ws.opcode = Ws.Text; payload; _ } ->
      ws.on_text payload;
      loop ()
    | Some _ -> loop ()
  in
  loop ()

let handle_conn ?on_ws (app : Fennec_core.App.t) flow _addr =
  Eio.Switch.run @@ fun sw ->
  let r = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  Eio.Buf_write.with_flow flow @@ fun w ->
  let rec loop () =
    match read_request r with
    | None -> ()
    | Some p ->
      if is_ws_upgrade p then (
        match on_ws with
        | Some f ->
          ws_handshake w p;
          serve_ws ~sw r w (f (to_request p))
        | None -> write_http w (CH.text ~status:404 "no websocket here") ~keep_alive:false)
      else begin
        let resp = Fennec_core.App.dispatch app (to_request p) in
        let keep_alive =
          match header p.headers "connection" with
          | Some v -> String.lowercase_ascii v <> "close"
          | None -> true
        in
        write_http w resp ~keep_alive;
        Eio.Buf_write.flush w;
        if keep_alive then loop ()
      end
  in
  loop ()

(* run [app] on [port]; [on_ws req ws] sets up a websocket connection. Blocks
   until the process stops. *)
let run ~env ?on_ws ~port (app : Fennec_core.App.t) =
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen ~sw ~backlog:128 ~reuse_addr:true (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Net.run_server socket (handle_conn ?on_ws app) ~on_error:(fun e ->
      Printf.eprintf "fennec: %s\n%!" (Printexc.to_string e))
