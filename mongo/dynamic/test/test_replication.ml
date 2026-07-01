(* Wire-backed replication: a follower converges to a REMOTE source over the MongoDB wire. Expose a source
   engine over the wire (unauthenticated, on a Unix socket), write to it THROUGH the wire, then a follower
   (a local engine + Burrow.Replica) pulls the source's oplog via the oplogFetch command and applies it,
   converging exactly — an insert batch, then an update + delete delta. Exercises the whole transport:
   Wire_client, the oplogFetch command, Oplog.entry_of_bson, Replica. *)
module Eng = Burrow.Engine
module Rep = Burrow.Replica
module WC = Fennec_mongo_dynamic.Wire_client
module Repl = Fennec_mongo_dynamic.Replication
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_wrepl_%d_%s" (Unix.getpid ()) suffix) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []
let run_cmd client fields = ignore (WC.run client (B.Document fields))

let v_of eng c id =
  match Eng.find_one eng c ~selector:(doc [ ("_id", s id) ]) ~sort:empty ~skip:0 ~fields:empty with
  | Some d -> B.get_int d "v"
  | None -> None

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let path = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "bwr_%d.sock" (Unix.getpid ())) in
  (try Sys.remove path with _ -> ());
  let addr = `Unix path in

  (* source: expose an (empty) engine over the wire, unauthenticated, on the unix socket *)
  Fennec_mongo_dynamic.expose ~sw ~net ~addr ~base_dir:(temp "src") ();
  let client = WC.connect ~sw ~net addr in

  (* write to the source THROUGH the wire: insert a=1, b=2 into db "shop" *)
  run_cmd client
    [ ("insert", s "t"); ("$db", s "shop");
      ("documents", B.Array [ doc [ ("_id", s "a"); ("v", i 1) ]; doc [ ("_id", s "b"); ("v", i 2) ] ]) ];

  (* follower: a local empty engine + a Replica; pull the source's oplog over the wire and apply *)
  let follower = Eng.open_ ~sw ~durability:S.No_sync (temp "flw") in
  let fc = Eng.collection follower "t" in
  let rep = Rep.create follower in
  let pull = Repl.pull client ~db:"shop" ~limit:1000 in
  assert (Rep.step rep ~pull = 2);
  assert (Eng.count follower fc ~selector:empty = 2);
  assert (v_of follower fc "a" = Some 1 && v_of follower fc "b" = Some 2);

  (* the source changes more, over the wire: a -> 9, remove b *)
  run_cmd client
    [ ("update", s "t"); ("$db", s "shop");
      ("updates", B.Array [ doc [ ("q", doc [ ("_id", s "a") ]); ("u", doc [ ("$set", doc [ ("v", i 9) ]) ]) ] ]) ];
  run_cmd client
    [ ("delete", s "t"); ("$db", s "shop");
      ("deletes", B.Array [ doc [ ("q", doc [ ("_id", s "b") ]); ("limit", i 1) ] ]) ];

  (* the follower pulls the delta + applies -> converged to the source *)
  assert (Rep.step rep ~pull = 2);
  assert (Eng.count follower fc ~selector:empty = 1);
  assert (v_of follower fc "a" = Some 9);
  assert (v_of follower fc "b" = None);

  Eng.close follower;

  (* ---- the same, but the source REQUIRES SCRAM-SHA-256 and the follower authenticates ---- *)
  let path2 = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "bwr_%d_a.sock" (Unix.getpid ())) in
  (try Sys.remove path2 with _ -> ());
  let user = Fennec_mongo_dynamic.wire_user ~user:"repl" ~password:"s3cret" () in
  Fennec_mongo_dynamic.expose ~sw ~net ~addr:(`Unix path2) ~base_dir:(temp "src2") ~users:[ user ] ~require_auth:true ();
  let aclient = WC.connect ~auth:("repl", "s3cret") ~sw ~net (`Unix path2) in
  run_cmd aclient
    [ ("insert", s "t"); ("$db", s "shop"); ("documents", B.Array [ doc [ ("_id", s "x"); ("v", i 7) ] ]) ];
  let follower2 = Eng.open_ ~sw ~durability:S.No_sync (temp "flw2") in
  let fc2 = Eng.collection follower2 "t" in
  let rep2 = Rep.create follower2 in
  assert (Rep.step rep2 ~pull:(Repl.pull aclient ~db:"shop" ~limit:1000) = 1);
  assert (v_of follower2 fc2 "x" = Some 7);
  Eng.close follower2;

  (* the socket paths are unlinked by Eio when the switch closes the listening sockets — don't remove here *)
  print_string "replication (wire, unauth + SCRAM): OK\n"
