(* DDP layer tests: the EJSON wire codec (round-trips + marker escaping), the message codec
   (round-trips + the numeric-error-code coercion that real Meteor needs), the server session state
   machine (connect/ping/sub→ready/method→result/unsub, all sub-tagged), and SockJS framing. *)

open Fennec_ddp
module B = Bson

(* ── EJSON codec ── *)
let%test "EJSON round-trips a mixed document" =
  let d =
    B.doc
      [ ("n", B.int 1); ("s", B.str "x"); ("when", B.Date 1000L);
        ("id", B.Object_id (String.make 24 'a')); ("xs", B.array [ B.int 1; B.bool true ]) ]
  in
  B.equal (Ejson.decode (Ejson.encode d)) d
let%test "EJSON escapes a marker-shaped document and round-trips it" =
  let d = B.doc [ ("$date", B.int 5) ] in
  B.equal (Ejson.decode (Ejson.encode d)) d

(* ── message codec ── *)
let%test "Message round-trips a sub-tagged added" =
  match Message.decode (Message.encode (Message.Added { collection = "c"; id = "1"; fields = [ ("n", B.int 1) ]; sub = Some "s1" })) with
  | Message.Added a ->
      a.collection = "c" && a.id = "1" && a.sub = Some "s1"
      && (match List.assoc_opt "n" a.fields with Some (B.Int 1) -> true | _ -> false)
  | _ -> false
let%test "Message round-trips connect / method / result" =
  let rt m = Message.decode (Message.encode m) in
  (match rt (Message.Connect { session = None; version = "1"; support = [ "1" ] }) with
   | Message.Connect c -> c.version = "1" && c.support = [ "1" ]
   | _ -> false)
  && (match rt (Message.Method { method_ = "m"; params = [ B.int 2 ]; id = "i"; random_seed = None }) with
      | Message.Method m -> m.method_ = "m" && m.id = "i"
      | _ -> false)
  && (match rt (Message.Result { id = "i"; error = None; result = Some (B.int 5) }) with
      | Message.Result r -> r.id = "i" && r.result = Some (B.Int 5)
      | _ -> false)
let%test "Message coerces a numeric error code to a string" =
  match Message.decode {|{"msg":"nosub","id":"x","error":{"error":404,"reason":"nope"}}|} with
  | Message.Nosub { error = Some e; _ } -> e.Message.error = "404"
  | _ -> false

(* ── session state machine ── *)
let%test "session: connect emits connected; ping emits pong" =
  let out = ref [] in
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods:(Hashtbl.create 1) in
  Session.handle s (Message.Connect { session = None; version = "1"; support = [] });
  Session.handle s (Message.Ping { id = Some "p" });
  match !out with
  | [ Message.Pong { id = Some "p" }; Message.Connected { session = "S" } ] -> true
  | _ -> false
let%test "session: sub runs the publication sub-tagged and emits ready" =
  let out = ref [] in
  let pubs = Hashtbl.create 1 in
  Hashtbl.replace pubs "feed" (fun ~params:_ (sink : Session.sink) ->
      sink.added ~collection:"items" ~id:"1" ~fields:[ ("n", B.int 1) ];
      sink.ready ();
      { Session.stop = (fun () -> ()) });
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs ~methods:(Hashtbl.create 1) in
  Session.handle s (Message.Sub { id = "sub1"; name = "feed"; params = [] });
  List.exists (function Message.Added a -> a.sub = Some "sub1" && a.collection = "items" | _ -> false) !out
  && List.exists (function Message.Ready { subs = [ "sub1" ] } -> true | _ -> false) !out
let%test "session: method emits result + updated; unknown pub emits nosub" =
  let out = ref [] in
  let methods = Hashtbl.create 1 in
  Hashtbl.replace methods "sum" (fun args -> match args with [ B.Int a; B.Int b ] -> B.Int (a + b) | _ -> B.Null);
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods in
  Session.handle s (Message.Method { method_ = "sum"; params = [ B.int 2; B.int 3 ]; id = "m1"; random_seed = None });
  Session.handle s (Message.Sub { id = "s2"; name = "nope"; params = [] });
  List.exists (function Message.Result { id = "m1"; result = Some (B.Int 5); _ } -> true | _ -> false) !out
  && List.exists (function Message.Updated { methods = [ "m1" ] } -> true | _ -> false) !out
  && List.exists (function Message.Nosub { id = "s2"; error = Some _ } -> true | _ -> false) !out

(* ── sockjs framing ── *)
let%test "sockjs wrap/unwrap round-trip; path detection" =
  let msgs = [ {|{"msg":"ping"}|}; {|{"msg":"pong"}|} ] in
  Sockjs.unwrap (Sockjs.wrap msgs) = msgs
  && Sockjs.is_sockjs_path "/sockjs/abc/def/websocket"
  && not (Sockjs.is_sockjs_path "/websocket")

let () = exit (Fennec_hunt_unit.run ())
