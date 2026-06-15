(* The moment of truth for the mongosh/driver exposure: point the REAL libmongoc driver (a conformant
   MongoDB wire-protocol CLIENT) at our wire server over loopback, authenticate with SCRAM-SHA-256, and
   run CRUD through it. The driver is the oracle — if it can handshake, auth, and operate against us
   exactly as it would against a hosted mongod, then our framing, spec BSON, SCRAM, and command dispatch
   are all correct end to end. The server fronts the embedded Burrow engine. Skips when the native
   driver wasn't compiled in. *)

module Mongo = Fennec_pulse_mongo
module Backend = Fennec_pulse.Backend
module B = Bson

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_wire_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let%test "real libmongoc driver authenticates (SCRAM-SHA-256) + runs CRUD against the embedded wire server" =
  if not (Mongo.available ()) then true
  else begin
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let port = 20000 + (Unix.getpid () mod 10000) in
    Mongo.expose ~sw ~net
      ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      ~base_dir:(tmpdir ())
      ~users:[ Mongo.wire_user ~user:"admin" ~password:"s3cret" ]
      ();
    (* a REAL driver client, authenticating over SCRAM exactly as against a hosted mongod *)
    let conn = Mongo.connect (Printf.sprintf "mongodb://admin:s3cret@127.0.0.1:%d/?authSource=admin" port) in
    let c = Mongo.collection ~sw conn ~db:"shop" ~name:"items" in

    (* insert *)
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "a"); ("name", B.str "apple"); ("price", B.int 10) ]));
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "b"); ("name", B.str "pear"); ("price", B.int 20) ]));
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "c"); ("name", B.str "plum"); ("price", B.int 30) ]));

    (* find all + filtered + sorted *)
    assert (List.length (Mongo.find c (Backend.query ())) = 3);
    let cheap = Mongo.find c (Backend.query ~selector:(B.doc [ ("price", B.doc [ ("$lte", B.int 20) ]) ]) ~sort:(B.doc [ ("price", B.int 1) ]) ()) in
    assert (List.map (fun d -> B.get_string d "_id") cheap = [ Some "a"; Some "b" ]);

    (* count *)
    assert (Mongo.count c (B.doc [ ("price", B.doc [ ("$gte", B.int 20) ]) ]) = 2);

    (* index + indexed query *)
    Mongo.ensure_index c ~name:"price_1" ~keys:(B.doc [ ("price", B.int 1) ]) ~unique:false ~sparse:false;
    assert (Mongo.count c (B.doc [ ("price", B.int 30) ]) = 1);

    (* update one *)
    assert (Mongo.update c ~multi:false ~upsert:false (B.doc [ ("_id", B.str "a") ]) (B.doc [ ("$set", B.doc [ ("price", B.int 99) ]) ]) = 1);
    (match Mongo.find_one c (Backend.query ~selector:(B.doc [ ("_id", B.str "a") ]) ()) with
     | Some d -> assert (B.get_int d "price" = Some 99)
     | None -> assert false);

    (* distinct *)
    let names = Mongo.distinct c "name" (B.Document []) in
    assert (List.length names = 3);

    (* aggregate ($match -> $group sum) *)
    let agg = Mongo.aggregate c [ B.doc [ ("$group", B.doc [ ("_id", B.Null); ("total", B.doc [ ("$sum", B.str "$price") ]) ]) ] ] in
    (match agg with [ d ] -> assert (B.get_int d "total" = Some (99 + 20 + 30)) | _ -> assert false);

    (* delete one *)
    assert (Mongo.remove c (B.doc [ ("_id", B.str "c") ]) = 1);
    assert (List.length (Mongo.find c (Backend.query ())) = 2);

    true
  end

let%test "wrong password is rejected by the SCRAM handshake (no anonymous access)" =
  if not (Mongo.available ()) then true
  else begin
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let port = 30000 + (Unix.getpid () mod 10000) in
    Mongo.expose ~sw ~net
      ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      ~base_dir:(tmpdir ())
      ~users:[ Mongo.wire_user ~user:"admin" ~password:"right" ]
      ();
    (* short server-selection timeout so a rejected handshake surfaces in seconds, not the 30s default *)
    let conn = Mongo.connect (Printf.sprintf "mongodb://admin:WRONG@127.0.0.1:%d/?authSource=admin&serverSelectionTimeoutMS=3000" port) in
    let c = Mongo.collection ~sw conn ~db:"shop" ~name:"items" in
    (* the first operation drives the handshake; a bad password must make it fail, not succeed *)
    match Mongo.insert c (B.doc [ ("x", B.int 1) ]) with
    | (_ : string) -> false
    | exception _ -> true
  end

(* the URL-driven auto path: a burrow:// URL with a loopback authority (no creds) makes expose_from_env
   open an UNAUTHENTICATED endpoint on that port, fronting the engine the path points at; the db is the
   path's trailing segment. Runs last — it sets the process-global MONGO_URL the earlier subtests skip. *)
let%test "expose_from_env opens an unauthenticated endpoint from a burrow:// authority" =
  if not (Mongo.available ()) then true
  else begin
    let port = 50000 + (Unix.getpid () mod 5000) in
    let dir = tmpdir () in
    Unix.putenv "MONGO_URL" (Printf.sprintf "burrow://localhost:%d%s" port (Filename.concat dir "devdb"));
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Mongo.expose_from_env ~sw ~net:(Eio.Stdenv.net env) ();
    (* connect with NO credentials — the endpoint is open on loopback *)
    let conn = Mongo.connect (Printf.sprintf "mongodb://127.0.0.1:%d/?serverSelectionTimeoutMS=5000" port) in
    let c = Mongo.collection ~sw conn ~db:"devdb" ~name:"k" in
    ignore (Mongo.insert c (B.doc [ ("_id", B.str "x"); ("n", B.int 7) ]));
    match Mongo.find c (Backend.query ()) with [ d ] -> B.get_int d "n" = Some 7 | _ -> false
  end

let () = exit (Fennec_hunt_unit.run ())
