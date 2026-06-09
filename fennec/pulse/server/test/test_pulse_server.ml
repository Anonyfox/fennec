(* DDP server, end to end over a FAKE websocket channel. We construct a [Ws_channel.t] (just the
   send/on_text/on_close callbacks), let [Realtime.Make(R).serve] wire a session onto it, feed it
   encoded DDP frames via [on_text], and assert the frames it emits back via [send] — exercising the
   whole stack (decode → session → glue → reactive observe → encode) without a real socket. *)

module R = Fennec_pulse.Reactive.Mini
module D = Fennec_pulse_server.Make (R)
module Msg = Fennec_ddp.Message
module Ws = Fennec_core.Ws_channel
module C = R.Collection
module B = Bson

let fake_channel () =
  let out = ref [] in
  let ch = { Ws.send = (fun s -> out := s :: !out); on_text = (fun _ -> ()); on_close = (fun () -> ()) } in
  (ch, out)

let emitted out = List.rev_map Msg.decode !out

let%test "connect → connected, sub → sub-tagged added + ready (full stack)" =
  let feed = C.create ~name:"feed" (Minimongo.create ()) in
  let _ = C.insert feed (B.doc [ ("n", B.int 1) ]) in
  R.publish "rt_feed" (fun _ -> R.Cursor (R.cursor feed ()));
  let ch, out = fake_channel () in
  D.serve ~session_id:"S1" ch;
  ch.Ws.on_text (Msg.encode (Msg.Connect { session = None; version = "1"; support = [] }));
  ch.Ws.on_text (Msg.encode (Msg.Sub { id = "sub1"; name = "rt_feed"; params = [] }));
  let ms = emitted out in
  List.exists (function Msg.Connected { session = "S1" } -> true | _ -> false) ms
  && List.exists (function Msg.Added { sub = Some "sub1"; collection = "feed"; _ } -> true | _ -> false) ms
  && List.exists (function Msg.Ready { subs = [ "sub1" ] } -> true | _ -> false) ms

let%test "a live insert after subscribe pushes a sub-tagged added to the channel" =
  let feed = C.create ~name:"feed2" (Minimongo.create ()) in
  R.publish "rt_feed2" (fun _ -> R.Cursor (R.cursor feed ()));
  let ch, out = fake_channel () in
  D.serve ~session_id:"S2" ch;
  ch.Ws.on_text (Msg.encode (Msg.Sub { id = "s2"; name = "rt_feed2"; params = [] }));
  let _ = C.insert feed (B.doc [ ("k", B.int 9) ]) in
  List.exists (function Msg.Added { sub = Some "s2"; collection = "feed2"; _ } -> true | _ -> false) (emitted out)

let%test "method call returns a result over the channel" =
  R.methods [ ("rt_sum", fun _ args -> match args with [ B.Int a; B.Int b ] -> B.Int (a + b) | _ -> B.Null) ];
  let ch, out = fake_channel () in
  D.serve ~session_id:"S3" ch;
  ch.Ws.on_text (Msg.encode (Msg.Method { method_ = "rt_sum"; params = [ B.int 2; B.int 3 ]; id = "m1"; random_seed = None }));
  List.exists (function Msg.Result { id = "m1"; result = Some (B.Int 5); _ } -> true | _ -> false) (emitted out)

let%test "a method error reaches the client with its code, not a generic 500" =
  R.methods [ ("rt_boom", fun _ _ -> raise (R.Error { code = "403"; reason = "no" })) ];
  let ch, out = fake_channel () in
  D.serve ~session_id:"S4" ch;
  ch.Ws.on_text (Msg.encode (Msg.Method { method_ = "rt_boom"; params = []; id = "m2"; random_seed = None }));
  List.exists (function Msg.Result { error = Some e; _ } -> e.Msg.code = "403" | _ -> false) (emitted out)

let%test "sockjs serve opens with 'o' and wraps replies in array frames" =
  R.methods [ ("rt_id", fun _ _ -> B.Int 1) ];
  let ch, out = fake_channel () in
  D.serve_sockjs ~session_id:"S5" ch;
  ch.Ws.on_text (Fennec_ddp.Sockjs.client_frame [ Msg.encode (Msg.Connect { session = None; version = "1"; support = [] }) ]);
  match List.rev !out with
  | first :: _ -> first = Fennec_ddp.Sockjs.open_frame && List.exists (fun f -> Fennec_ddp.Sockjs.is_array_frame f) !out
  | [] -> false

let () = exit (Fennec_hunt_unit.run ())
