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
  | Message.Nosub { error = Some e; _ } -> e.Message.code = "404"
  | _ -> false

(* ── session state machine ── *)
let%test "session: connect emits connected; ping emits pong" =
  let out = ref [] in
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods:(Hashtbl.create 1) () in
  Session.dispatch s (Message.Connect { session = None; version = "1"; support = [] });
  Session.dispatch s (Message.Ping { id = Some "p" });
  (* connect now also pushes the connection identity (v2 fennecUser) — anonymous here *)
  match !out with
  | [ Message.Pong { id = Some "p" }; Message.User { id = None }; Message.Connected { session = "S" } ] -> true
  | _ -> false
let%test "session: sub runs the publication sub-tagged and emits ready" =
  let out = ref [] in
  let pubs = Hashtbl.create 1 in
  Hashtbl.replace pubs "feed" (fun ~params:_ (sink : Session.sink) ->
      sink.added ~collection:"items" ~id:"1" ~fields:[ ("n", B.int 1) ];
      sink.ready ();
      { Session.stop = (fun () -> ()) });
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs ~methods:(Hashtbl.create 1) () in
  Session.dispatch s (Message.Sub { id = "sub1"; name = "feed"; params = []; have = None });
  List.exists (function Message.Added a -> a.sub = Some "sub1" && a.collection = "items" | _ -> false) !out
  && List.exists (function Message.Ready { subs = [ "sub1" ] } -> true | _ -> false) !out
let%test "session: method emits result + updated; unknown pub emits nosub" =
  let out = ref [] in
  let methods = Hashtbl.create 1 in
  Hashtbl.replace methods "sum" (fun _ctx args -> match args with [ B.Int a; B.Int b ] -> B.Int (a + b) | _ -> B.Null);
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods () in
  Session.dispatch s (Message.Method { method_ = "sum"; params = [ B.int 2; B.int 3 ]; id = "m1"; random_seed = None });
  Session.dispatch s (Message.Sub { id = "s2"; name = "nope"; params = []; have = None });
  List.exists (function Message.Result { id = "m1"; result = Some (B.Int 5); _ } -> true | _ -> false) !out
  && List.exists (function Message.Updated { methods = [ "m1" ] } -> true | _ -> false) !out
  && List.exists (function Message.Nosub { id = "s2"; error = Some _ } -> true | _ -> false) !out

(* ── sockjs framing ── *)
let%test "sockjs wrap/unwrap round-trip; path detection" =
  let msgs = [ {|{"msg":"ping"}|}; {|{"msg":"pong"}|} ] in
  Sockjs.unwrap (Sockjs.wrap msgs) = msgs
  && Sockjs.is_sockjs_path "/sockjs/abc/def/websocket"
  && not (Sockjs.is_sockjs_path "/websocket")

(* ── audit regressions ── *)
let%test "EJSON escapes a $value-first marker-shaped document (no silent type corruption)" =
  let d = B.doc [ ("$value", B.str "x"); ("$type", B.str "oid") ] in
  B.equal (Ejson.decode (Ejson.encode d)) d
let%test "JSON serializes non-finite numbers as null (valid wire bytes)" =
  Json.to_string (Json.Number (1.0 /. 0.0)) = "null" && Json.to_string (Json.Number nan) = "null"
let%test "JSON rejects trailing garbage and a lone minus" =
  (match Json.parse_opt "1 xyz" with None -> true | Some _ -> false)
  && (match Json.parse_opt "-" with None -> true | Some _ -> false)
  && (match Json.parse_opt "[1,2]" with Some (Json.List _) -> true | _ -> false)
let%test "session: a method's Method_error is reported, not collapsed to 500" =
  let out = ref [] in
  let methods = Hashtbl.create 1 in
  Hashtbl.replace methods "boom" (fun _ctx _ -> raise (Session.Method_error { code = "403"; reason = "Not authorized" }));
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods () in
  Session.dispatch s (Message.Method { method_ = "boom"; params = []; id = "m"; random_seed = None });
  List.exists
    (function
      | Message.Result { error = Some e; _ } -> e.Message.code = "403" && e.Message.reason = Some "Not authorized"
      | _ -> false)
    !out

let%test "session: a throwing publication emits Nosub, not a hang" =
  let out = ref [] in
  let pubs = Hashtbl.create 1 in
  Hashtbl.replace pubs "boom" (fun ~params:_ (_ : Session.sink) -> failwith "publication boom");
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs ~methods:(Hashtbl.create 1) () in
  Session.dispatch s (Message.Sub { id = "sub1"; name = "boom"; params = []; have = None });
  List.exists (function Message.Nosub { id = "sub1"; error = Some _ } -> true | _ -> false) !out

let%test "session: set_user_id rebinds the connection's user for subsequent methods" =
  let out = ref [] in
  let methods = Hashtbl.create 2 in
  Hashtbl.replace methods "login" (fun ctx _ -> ctx.Session.set_user_id (Some "alice"); B.Bool true);
  Hashtbl.replace methods "whoami" (fun ctx _ ->
      match ctx.Session.user_id with Some u -> B.str u | None -> B.Null);
  let s = Session.create ~session_id:"S" ~emit:(fun m -> out := m :: !out) ~pubs:(Hashtbl.create 1) ~methods () in
  Session.dispatch s (Message.Method { method_ = "whoami"; params = []; id = "m1"; random_seed = None });
  Session.dispatch s (Message.Method { method_ = "login"; params = []; id = "m2"; random_seed = None });
  Session.dispatch s (Message.Method { method_ = "whoami"; params = []; id = "m3"; random_seed = None });
  List.exists (function Message.Result { id = "m1"; result = Some B.Null; _ } -> true | _ -> false) !out
  && List.exists (function Message.Result { id = "m3"; result = Some (B.String "alice"); _ } -> true | _ -> false) !out
  && Session.user_id s = Some "alice"

let%test "session: initial user_id is visible to the first method" =
  let out = ref [] in
  let methods = Hashtbl.create 1 in
  Hashtbl.replace methods "whoami" (fun ctx _ ->
      match ctx.Session.user_id with Some u -> B.str u | None -> B.Null);
  let s =
    Session.create ~user_id:"cookie-user" ~session_id:"S" ~emit:(fun m -> out := m :: !out)
      ~pubs:(Hashtbl.create 1) ~methods ()
  in
  Session.dispatch s (Message.Method { method_ = "whoami"; params = []; id = "m1"; random_seed = None });
  Session.user_id s = Some "cookie-user"
  && List.exists (function Message.Result { id = "m1"; result = Some (B.String "cookie-user"); _ } -> true | _ -> false) !out

(* ── interop: REAL DDP frames captured off a live Meteor 3.x server (raw /websocket) must decode
   losslessly — the proof the wire stays Meteor-compatible (V1 drop-in), pinning the quirks (numeric
   error codes, nested number/array fields). Ported from the reference repo so fennec's own CI guards
   it. ── *)
let%test "interop: real Meteor connected frame" =
  Message.decode {|{"msg":"connected","session":"bxdCChEZxQFERBpRk"}|}
  = Message.Connected { session = "bxdCChEZxQFERBpRk" }
let%test "interop: real Meteor ready frame" =
  Message.decode {|{"msg":"ready","subs":["auto"]}|} = Message.Ready { subs = [ "auto" ] }
let%test "interop: real Meteor added (simple fields) — untagged, sub=None" =
  Message.decode
    {|{"msg":"added","collection":"meteor_autoupdate_clientVersions","id":"version","fields":{"version":"outdated"}}|}
  = Message.Added
      { collection = "meteor_autoupdate_clientVersions"; id = "version";
        fields = [ ("version", B.String "outdated") ]; sub = None }
let%test "interop: real Meteor added with nested number + empty array fields" =
  match
    Message.decode
      {|{"msg":"added","collection":"meteor_autoupdate_clientVersions","id":"web.browser","fields":{"version":"2a7399e79b7bf06e401b39cd9a52242e2d264dd7","versionHmr":1780169750118,"assets":[]}}|}
  with
  | Message.Added { id = "web.browser"; fields; _ } ->
      List.assoc_opt "version" fields = Some (B.String "2a7399e79b7bf06e401b39cd9a52242e2d264dd7")
      && List.assoc_opt "versionHmr" fields = Some (B.Int 1780169750118)
      && List.assoc_opt "assets" fields = Some (B.Array [])
  | _ -> false
let%test "interop: real Meteor nosub with NUMERIC error code (404 coerced to string)" =
  match
    Message.decode
      {|{"msg":"nosub","id":"bad","error":{"isClientSafe":true,"error":404,"reason":"Subscription '__no_such_pub__' not found","message":"Subscription '__no_such_pub__' not found [404]","errorType":"Meteor.Error"}}|}
  with
  | Message.Nosub { id = "bad"; error = Some e } ->
      e.Message.code = "404"
      && e.Message.reason = Some "Subscription '__no_such_pub__' not found"
      && e.Message.error_type = "Meteor.Error"
  | _ -> false
let%test "interop: encode produces exact Meteor wire bytes (V1 byte-identity, sub omitted when None)" =
  Message.encode (Message.Connect { session = None; version = "1"; support = [ "1" ] })
  = {|{"msg":"connect","version":"1","support":["1"]}|}
  && Message.encode
       (Message.Added { collection = "tasks"; id = "1"; fields = [ ("title", B.str "hi") ]; sub = None })
     = {|{"msg":"added","collection":"tasks","id":"1","fields":{"title":"hi"}}|}

(* ── delta resync (v2): the sink wrapper turns a full replay into a difference ── *)
let%test "resync_wrap: skips matching docs, passes changed/new ones, removes the dead at ready" =
  let out = ref [] in
  let sink =
    { Session.added = (fun ~collection ~id ~fields:_ -> out := ("added", collection, id) :: !out);
      changed = (fun ~collection ~id ~fields:_ ~cleared:_ -> out := ("changed", collection, id) :: !out);
      removed = (fun ~collection ~id -> out := ("removed", collection, id) :: !out);
      ready = (fun () -> out := ("ready", "", "") :: !out) }
  in
  let f_same = [ ("n", B.int 1) ] and f_diff = [ ("n", B.int 2) ] in
  let have = [ ("c", [ ("same", Doc_hash.fields f_same); ("stale", Doc_hash.fields f_same); ("dead", "x") ]) ] in
  let w = Session.resync_wrap ~have sink in
  w.Session.added ~collection:"c" ~id:"same" ~fields:f_same; (* matching → skipped *)
  w.Session.added ~collection:"c" ~id:"stale" ~fields:f_diff; (* held but different → passes *)
  w.Session.added ~collection:"c" ~id:"new" ~fields:f_same; (* not held → passes *)
  w.Session.ready ();
  w.Session.added ~collection:"c" ~id:"live" ~fields:f_same; (* post-ready → inert passthrough *)
  let ms = List.rev !out in
  ms = [ ("added", "c", "stale"); ("added", "c", "new"); ("removed", "c", "dead"); ("ready", "", "");
         ("added", "c", "live") ]

let%test "doc hash: field order does not matter; values do" =
  Doc_hash.fields [ ("a", B.int 1); ("b", B.str "x") ] = Doc_hash.fields [ ("b", B.str "x"); ("a", B.int 1) ]
  && Doc_hash.fields [ ("a", B.int 1) ] <> Doc_hash.fields [ ("a", B.int 2) ]

let%test "wire: Sub round-trips the have payload; absent stays None" =
  (match Message.decode (Message.encode (Message.Sub { id = "s"; name = "n"; params = []; have = Some [ ("c", [ ("1", "abc") ]) ] })) with
   | Message.Sub { have = Some [ ("c", [ ("1", "abc") ]) ]; _ } -> true
   | _ -> false)
  && (match Message.decode {|{"msg":"sub","id":"s","name":"n","params":[]}|} with
      | Message.Sub { have = None; _ } -> true
      | _ -> false)

let () = exit (Fennec_hunt_unit.run ())
