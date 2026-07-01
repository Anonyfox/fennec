(* PITR (point-in-time recovery): back up at LSN X, write more, then RESTORE the backup (frozen at X) and
   REPLAY the source's oplog (X, now] onto it via oplog_apply. The idempotent replay reconstructs the
   source's current state — an update, a delete, and an insert applied on top of the frozen backup — and
   replaying the same tail twice is a no-op. This is the primitive an async replica follower reuses. *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_pitr_%d_%s" (Unix.getpid ()) suffix) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []

let v_of eng c id =
  match Eng.find_one eng c ~selector:(doc [ ("_id", s id) ]) ~sort:empty ~skip:0 ~fields:empty with
  | Some d -> B.get_int d "v"
  | None -> None

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.No_sync (temp "src") in
  let c = Eng.collection eng "t" in

  (* initial state: a=1, b=2, c=3 *)
  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("v", i 1) ]));
  ignore (Eng.insert eng c (doc [ ("_id", s "b"); ("v", i 2) ]));
  ignore (Eng.insert eng c (doc [ ("_id", s "c"); ("v", i 3) ]));
  let backup_lsn = Eng.oplog_lsn eng in

  (* hot backup at this point (the frozen restore target) *)
  let bk = temp "bk" in
  Eng.backup eng ~dir:bk ();

  (* writes AFTER the backup: update a->99, delete b, insert d=4 *)
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("v", i 99) ]) ]));
  ignore (Eng.remove eng c (doc [ ("_id", s "b") ]));
  ignore (Eng.insert eng c (doc [ ("_id", s "d"); ("v", i 4) ]));

  (* RESTORE: the backup is the frozen state at backup_lsn (a=1, b=2, c=3) *)
  let restored = Eng.open_ ~sw ~durability:S.No_sync bk in
  let rc = Eng.collection restored "t" in
  assert (Eng.count restored rc ~selector:empty = 3);
  assert (v_of restored rc "a" = Some 1 && v_of restored rc "b" = Some 2);

  (* REPLAY the source's oplog (backup_lsn, now] onto the restored engine *)
  let tail = Eng.oplog_tail eng ~from_lsn:backup_lsn ~limit:1000 in
  assert (List.length tail = 3) (* the update, the delete, the insert *);
  List.iter (Eng.oplog_apply restored) tail;

  (* the restored engine now matches the source: a=99, b gone, c=3, d=4 *)
  assert (Eng.count restored rc ~selector:empty = 3);
  assert (v_of restored rc "a" = Some 99);
  assert (v_of restored rc "b" = None);
  assert (v_of restored rc "c" = Some 3);
  assert (v_of restored rc "d" = Some 4);

  (* idempotent: replaying the same tail again is a no-op *)
  List.iter (Eng.oplog_apply restored) tail;
  assert (Eng.count restored rc ~selector:empty = 3);
  assert (v_of restored rc "a" = Some 99 && v_of restored rc "d" = Some 4);

  Eng.close eng;
  Eng.close restored;
  print_string "pitr: OK\n"
