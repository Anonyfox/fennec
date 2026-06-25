(* Workflow transactions: [begin_txn] opens ONE parent LMDB txn; the session's [*_in] reads/writes route
   through it (read-your-writes); [commit_txn] persists atomically, [abort_txn] rolls back — and the
   non-transactional ops keep working (the write lock is released either way). *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let tmp label =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_" ^ label ^ "_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.No_sync (tmp "txn") in
  let c = Eng.collection eng "t" in
  let one sel = Eng.find_one eng c ~selector:sel ~sort:empty ~skip:0 ~fields:empty in
  let count () = Eng.count eng c ~selector:empty in

  (* seed one doc the normal (non-tx) way *)
  ignore (Eng.insert eng c (doc [ ("_id", s "seed"); ("n", i 1) ]));
  assert (count () = 1);

  (* --- ABORT: writes roll back; read-your-writes holds WITHIN the session --- *)
  let sess = Eng.begin_txn eng in
  ignore (Eng.insert_in sess c (doc [ ("_id", s "a"); ("n", i 10) ]));
  ignore (Eng.insert_in sess c (doc [ ("_id", s "b"); ("n", i 20) ]));
  ignore (Eng.update_in sess c ~multi:false ~upsert:false (doc [ ("_id", s "seed") ]) (doc [ ("$set", doc [ ("n", i 99) ]) ]));
  assert (Eng.count_in sess c ~selector:empty = 3) (* the session sees its own pending inserts *);
  (match Eng.find_one_in sess c ~selector:(doc [ ("_id", s "a") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_int d "n" = Some 10)
   | None -> assert false);
  (match Eng.find_one_in sess c ~selector:(doc [ ("_id", s "seed") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_int d "n" = Some 99) (* read-your-WRITE: the pending $set is visible *)
   | None -> assert false);
  Eng.abort_txn eng sess;
  assert (count () = 1) (* none of the session's writes persisted *);
  assert (one (doc [ ("_id", s "a") ]) = None);
  (match one (doc [ ("_id", s "seed") ]) with Some d -> assert (B.get_int d "n" = Some 1) (* the $set rolled back *) | None -> assert false);

  (* --- non-tx ops still work after the aborted session (lock released) --- *)
  ignore (Eng.insert eng c (doc [ ("_id", s "z"); ("n", i 5) ]));
  assert (count () = 2);

  (* --- COMMIT: the whole session persists atomically --- *)
  let sess2 = Eng.begin_txn eng in
  ignore (Eng.insert_in sess2 c (doc [ ("_id", s "p"); ("n", i 1) ]));
  ignore (Eng.insert_in sess2 c (doc [ ("_id", s "q"); ("n", i 2) ]));
  ignore (Eng.remove_in sess2 c (doc [ ("_id", s "z") ]));
  Eng.commit_txn eng sess2;
  assert (count () = 3) (* seed + p + q (z removed) *);
  (match one (doc [ ("_id", s "p") ]) with Some d -> assert (B.get_int d "n" = Some 1) | None -> assert false);
  assert (one (doc [ ("_id", s "z") ]) = None);

  Eng.close eng;
  print_string "engine workflow-txn (commit/abort/read-your-writes): OK\n"
