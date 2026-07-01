(* The literal use case: a real `mongosh` process connects to the embedded wire endpoint exactly as it
   would to a hosted mongod — authenticates with SCRAM-SHA-256 over the URI, then runs ad-hoc CRUD — and
   we assert on what mongosh printed. This needs NO libmongoc driver (our server is pure OCaml over the
   Burrow engine); it only needs `mongosh` on PATH, and skips (passes) otherwise. mongosh runs as a
   subprocess via Eio.Process so the scheduler stays free to let the server fiber answer it. *)

module Mongo = Fennec_mongo_dynamic

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_mongosh_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let find_exe name =
  String.split_on_char ':' (try Sys.getenv "PATH" with Not_found -> "")
  |> List.find_map (fun d -> let p = Filename.concat d name in if Sys.file_exists p then Some p else None)

let contains hay sub =
  let hl = String.length hay and sl = String.length sub in
  let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
  sl = 0 || go 0

let%test "real mongosh authenticates (SCRAM-SHA-256) and runs CRUD against the embedded wire endpoint" =
  match find_exe "mongosh" with
  | None -> true
  | Some mongosh ->
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let port = 40000 + (Unix.getpid () mod 10000) in
    Mongo.expose ~sw ~net
      ~addr:(`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
      ~base_dir:(tmpdir ())
      ~users:[ Mongo.wire_user ~user:"ada" ~password:"lovelace" () ]
      ();
    let uri = Printf.sprintf "mongodb://ada:lovelace@127.0.0.1:%d/?authSource=admin&serverSelectionTimeoutMS=5000" port in
    (* a small mongosh script: insert, aggregate-sum, filtered count, sorted find — printing markers *)
    let script =
      String.concat " "
        [ "db = db.getSiblingDB('lab');";
          "db.nums.insertMany([{v:1},{v:2},{v:3},{v:4}]);";
          "print('SUM=' + db.nums.aggregate([{$group:{_id:null,s:{$sum:'$v'}}}]).toArray()[0].s);";
          "print('CNT=' + db.nums.countDocuments({v:{$gte:3}}));";
          "print('FIRST=' + db.nums.find().sort({v:-1}).limit(1).toArray()[0].v);";
          "print('INS=' + (db.serverStatus().opcounters.insert >= 1));";
          "print('CONN=' + (db.serverStatus().connections.current >= 1));";
          (* a resumable change stream: open it, insert, then a non-blocking tryNext sees the insert event *)
          "cs = db.nums.watch();";
          "db.nums.insertOne({v:99});";
          "e = cs.tryNext();";
          "print('CS=' + (e ? e.operationType : 'none'));" ]
    in
    let script_file = Filename.temp_file "mongosh_script" ".js" in
    Out_channel.with_open_text script_file (fun oc -> Out_channel.output_string oc script);
    let out_file = Filename.temp_file "mongosh_out" ".txt" in
    (* capture stdout+stderr so an auth/connection failure is visible if the assertions trip *)
    let cmd = Printf.sprintf "%s '%s' --quiet %s > %s 2>&1" (Filename.quote mongosh) uri (Filename.quote script_file) (Filename.quote out_file) in
    let mgr = Eio.Stdenv.process_mgr env in
    ignore (Eio.Process.await (Eio.Process.spawn mgr ~sw [ "sh"; "-c"; cmd ]));
    let out = In_channel.with_open_text out_file In_channel.input_all in
    let ok =
      contains out "SUM=10" && contains out "CNT=2" && contains out "FIRST=4" && contains out "INS=true"
      && contains out "CONN=true" && contains out "CS=insert"
    in
    if not ok then Printf.eprintf "mongosh output was:\n%s\n%!" out;
    ok

let () = exit (Fennec_hunt_unit.run ())
