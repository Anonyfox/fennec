(* Oplog — the engine-wide change log. Every committed write appends one idempotent entry per affected
   document (the resulting doc for insert/update, the _id for delete) into the capped _oplog, in the same
   txn. Consumers tail it from an LSN. Checks: append + tail, the entry shape, LSN monotonicity +
   crash-resume, the retention cap, and that a workflow transaction logs on COMMIT but not on ABORT. *)
module Eng = Burrow.Engine
module Op = Burrow.Oplog
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_oplog_%d_%s" (Unix.getpid ()) suffix) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp "main" in
  let eng = Eng.open_ ~sw ~durability:S.No_sync dir in
  let c = Eng.collection eng "t" in

  (* two inserts, an update, a delete -> one oplog entry each, LSN 1..4 in order *)
  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("n", i 1) ]));
  ignore (Eng.insert eng c (doc [ ("_id", s "b"); ("n", i 2) ]));
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("n", i 9) ]) ]));
  ignore (Eng.remove eng c (doc [ ("_id", s "b") ]));

  let entries = Eng.oplog_tail eng ~from_lsn:0L ~limit:100 in
  assert (List.map (fun (e : Op.entry) -> e.Op.lsn) entries = [ 1L; 2L; 3L; 4L ]);
  assert (Eng.oplog_lsn eng = 4L);
  (match entries with
   | [ e1; e2; e3; e4 ] ->
     assert (e1.Op.op = Op.Insert && e2.Op.op = Op.Insert);
     (* the update entry carries the RESULTING document (idempotent replace by _id) *)
     assert (e3.Op.op = Op.Update);
     (match e3.Op.doc with Some d -> assert (B.get_int d "n" = Some 9 && B.get_string d "_id" = Some "a") | None -> assert false);
     (* the delete entry carries just the _id, no doc *)
     assert (e4.Op.op = Op.Delete && e4.Op.doc = None && e4.Op.id = s "b")
   | _ -> assert false);

  (* tail from a mid LSN returns only newer entries *)
  assert (List.length (Eng.oplog_tail eng ~from_lsn:2L ~limit:100) = 2);
  Eng.close eng;

  (* crash-resume: reopen resumes the LSN from the log; a new write gets lsn = 5 *)
  let eng2 = Eng.open_ ~sw ~durability:S.No_sync dir in
  assert (Eng.oplog_lsn eng2 = 4L);
  let c2 = Eng.collection eng2 "t" in
  ignore (Eng.insert eng2 c2 (doc [ ("_id", s "z") ]));
  assert (Eng.oplog_lsn eng2 = 5L);
  Eng.close eng2;

  (* the retention cap: with keep=3, the oldest entries are trimmed to the most recent 3 *)
  let engc = Eng.open_ ~sw ~durability:S.No_sync ~oplog_keep:3 (temp "cap") in
  let cc = Eng.collection engc "t" in
  for k = 1 to 6 do ignore (Eng.insert engc cc (doc [ ("_id", s (Printf.sprintf "k%d" k)) ])) done;
  let kept = Eng.oplog_tail engc ~from_lsn:0L ~limit:100 in
  assert (List.length kept = 3);
  assert (List.map (fun (e : Op.entry) -> e.Op.lsn) kept = [ 4L; 5L; 6L ]) (* the oldest (1..3) trimmed *);
  Eng.close engc;

  (* a workflow transaction logs its writes on COMMIT, and logs NOTHING on ABORT *)
  let engw = Eng.open_ ~sw ~durability:S.No_sync (temp "wf") in
  let cw = Eng.collection engw "t" in
  let s1 = Eng.begin_txn engw in
  ignore (Eng.insert_in s1 cw (doc [ ("_id", s "w1") ]));
  ignore (Eng.insert_in s1 cw (doc [ ("_id", s "w2") ]));
  Eng.commit_txn engw s1;
  assert (List.length (Eng.oplog_tail engw ~from_lsn:0L ~limit:100) = 2);
  let s2 = Eng.begin_txn engw in
  ignore (Eng.insert_in s2 cw (doc [ ("_id", s "w3") ]));
  Eng.abort_txn engw s2;
  (* w3's entry was appended inside the aborted txn, so it is NOT in the log *)
  assert (List.length (Eng.oplog_tail engw ~from_lsn:0L ~limit:100) = 2);
  Eng.close engw;

  print_string "oplog: OK\n"
