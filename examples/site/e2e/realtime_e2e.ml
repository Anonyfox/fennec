(* End-to-end proof of the realtime stack over a REAL WebSocket — no browser, fully deterministic.
   In one Eio process: a minimal RFC 6455 server (fennec.server's Ws codec + accept_key) wires each
   connection to a DDP session via fennec.pulse.server over a fennec.pulse reactive instance backed by
   fennec-mongo's in-memory engine; the client (fennec-hunt's Cdp WebSocket client) performs a full
   DDP handshake/subscribe/method round-trip and asserts the live server→client push.

   It exercises the whole server stack across a socket: RFC 6455 framing ↔ Ws_channel ↔ Session ↔
   Reactive ↔ Minimongo observe → sub-tagged delta → frame. Run with `dune exec`. Exits 0 on PASS. *)

module R = Fennec_pulse.Reactive.Mini
module D = Fennec_pulse_server.Make (R)
module Msg = Fennec_ddp.Message
module Ws = Fennec_server.Ws
module Wsc = Fennec_core.Ws_channel
module B = Bson

(* a server-side unmasked text frame written immediately (mirrors Cdp.ws_send, server-side) *)
let send_text flow (s : string) =
  let n = String.length s in
  let b = Buffer.create (n + 8) in
  Buffer.add_char b '\x81' (* FIN + text opcode *);
  if n < 126 then Buffer.add_char b (Char.chr n)
  else if n < 65536 then (
    Buffer.add_char b (Char.chr 126);
    Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
    Buffer.add_char b (Char.chr (n land 0xff)))
  else (
    Buffer.add_char b (Char.chr 127);
    for i = 7 downto 0 do Buffer.add_char b (Char.chr ((n lsr (i * 8)) land 0xff)) done);
  Buffer.add_string b s;
  Eio.Flow.copy_string (Buffer.contents b) flow

(* one connection: HTTP upgrade handshake, then a DDP session over the frame loop *)
let handle_conn flow _addr =
  let r = Eio.Buf_read.of_flow flow ~max_size:(16 * 1024 * 1024) in
  let key = ref "" in
  let rec hdrs () =
    match (try Eio.Buf_read.line r with _ -> "") with
    | "" | "\r" -> ()
    | line ->
        (match String.index_opt line ':' with
        | Some i when String.lowercase_ascii (String.trim (String.sub line 0 i)) = "sec-websocket-key" ->
            key := String.trim (String.sub line (i + 1) (String.length line - i - 1))
        | _ -> ());
        hdrs ()
  in
  hdrs ();
  Eio.Flow.copy_string
    (Printf.sprintf
       "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: %s\r\n\r\n"
       (Ws.accept_key !key))
    flow;
  let ch = { Wsc.send = (fun s -> send_text flow s); on_text = (fun _ -> ()); on_close = (fun () -> ()) } in
  D.serve ch;
  let rec loop () =
    match Ws.read_frame r with
    | Ws.Frame { opcode = Ws.Text; payload; _ } -> ch.Wsc.on_text payload; loop ()
    | Ws.Frame { opcode = Ws.Close; _ } | Ws.Eof -> ch.Wsc.on_close ()
    | Ws.Frame _ -> loop () (* ping/pong/binary: ignore *)
    | Ws.Protocol_error _ -> ch.Wsc.on_close ()
  in
  loop ()

let () =
  (* the realtime app: a published "tasks" collection (one initial doc) + an addTask method *)
  let tasks = R.Collection.create ~name:"tasks" (Minimongo.create ()) in
  let _ = R.Collection.insert tasks (B.doc [ ("title", B.str "first") ]) in
  R.publish "tasks" (fun () -> R.Cursor (R.cursor tasks ()));
  R.methods
    [ ("addTask", fun _ args ->
         match args with [ B.String t ] -> B.String (R.Collection.insert tasks (B.doc [ ("title", B.str t) ])) | _ -> B.Null) ];
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let port = 4577 in
  let sock = Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let client () =
    let ws = Fennec_hunt.Cdp.ws_connect ~sw net ~port ~path:"/websocket" in
    let send m = Fennec_hunt.Cdp.ws_send ws (Msg.encode m) in
    let recv () = Msg.decode (Fennec_hunt.Cdp.ws_recv ws) in
    send (Msg.Connect { session = None; version = "1"; support = [ "1" ] });
    let connected = match recv () with Msg.Connected _ -> true | _ -> false in
    send (Msg.Sub { id = "s1"; name = "tasks"; params = [] });
    let added = ref 0 and ready = ref false in
    while not !ready do
      match recv () with Msg.Added _ -> incr added | Msg.Ready _ -> ready := true | _ -> ()
    done;
    let initial_ok = !added = 1 && !ready in
    send (Msg.Method { method_ = "addTask"; params = [ B.str "second" ]; id = "m1"; random_seed = None });
    let got_result = ref false and got_push = ref false in
    while not (!got_result && !got_push) do
      match recv () with
      | Msg.Result { id = "m1"; _ } -> got_result := true
      | Msg.Added _ -> got_push := true
      | _ -> ()
    done;
    let ok = connected && initial_ok && !got_result && !got_push in
    Printf.printf
      "realtime e2e: connected=%b initial_added=%d ready=%b method_result=%b live_push=%b => %s\n%!"
      connected !added !ready !got_result !got_push (if ok then "PASS" else "FAIL");
    ok
  in
  let ok = Eio.Fiber.first (fun () -> Eio.Net.run_server sock ~on_error:(fun _ -> ()) handle_conn) client in
  if not ok then exit 1
