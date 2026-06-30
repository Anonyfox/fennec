(* Online hot backup: a consistent, zero-downtime copy of the whole database to a fresh directory. Proves
   the copy is a standalone, openable database (documents + secondary indexes survive backup -> reopen),
   and that it reflects the SNAPSHOT at backup time (writes afterwards don't leak in). No_sync durability
   for speed — the copy reads committed pages regardless of the fsync mode. *)
module Eng = Burrow.Engine
module B = Bson

let temp name =
  Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_backup_%d_%s" (Unix.getpid ()) name)

(* remove a prior run's database files so counts are exact and the hot-copy target starts clean *)
let clean dir = List.iter (fun f -> try Sys.remove (Filename.concat dir f) with _ -> ()) [ "data.mdb"; "lock.mdb" ]

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let src = temp "src" in
  (try Unix.mkdir src 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  clean src;
  let eng = Eng.open_ ~sw ~durability:Burrow_store.Store.No_sync src in
  let users = Eng.collection eng "users" in

  ignore (Eng.insert eng users (doc [ ("_id", s "u1"); ("name", s "ada"); ("email", s "ada@x") ]));
  ignore (Eng.insert eng users (doc [ ("_id", s "u2"); ("name", s "bob"); ("email", s "bob@x") ]));
  ignore (Eng.insert eng users (doc [ ("_id", s "u3"); ("name", s "cleo"); ("email", s "cleo@x") ]));
  Eng.ensure_index eng users ~name:"email_1" ~keys:(doc [ ("email", i 1) ]) ~unique:true ~sparse:false;

  (* back up the 3-document snapshot to a fresh directory the backup creates itself (compacting default) *)
  let bk = temp "bk" in
  clean bk;
  (try Unix.rmdir bk with _ -> ());
  Eng.backup eng ~dir:bk ();
  assert (Sys.file_exists (Filename.concat bk "data.mdb"));

  (* a write AFTER the backup must not appear in the copy (snapshot consistency); the source is undisturbed *)
  ignore (Eng.insert eng users (doc [ ("_id", s "u4"); ("name", s "dan"); ("email", s "dan@x") ]));
  assert (Eng.count eng users ~selector:empty = 4);

  (* open the backup AS A DATABASE — this is "restore" *)
  let eng2 = Eng.open_ ~sw ~durability:Burrow_store.Store.No_sync bk in
  let users2 = Eng.collection eng2 "users" in

  (* exactly the 3 pre-backup documents — u4 is absent *)
  assert (Eng.count eng2 users2 ~selector:empty = 3);
  assert (Eng.find_one eng2 users2 ~selector:(doc [ ("_id", s "u4") ]) ~sort:empty ~skip:0 ~fields:empty = None);
  (match Eng.find_one eng2 users2 ~selector:(doc [ ("_id", s "u1") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_string d "name" = Some "ada")
   | None -> assert false);

  (* the secondary index survived: its spec is in the restored catalog AND it serves a lookup *)
  assert (List.mem "email_1" (Eng.index_names users2));
  (match Eng.find_one eng2 users2 ~selector:(doc [ ("email", s "cleo@x") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_string d "name" = Some "cleo")
   | None -> assert false);

  Eng.close eng;
  Eng.close eng2;
  print_string "backup: OK\n"
