(* Replica: a follower converges to a source by pulling + applying its oplog. Initial sync from a hot backup
   (the follower starts at the backup's LSN), then continuous steps catch up as the source writes; lag
   reports the gap; a step with nothing new applies nothing. In-process transport (Engine.oplog_tail). *)
module Eng = Burrow.Engine
module Rep = Burrow.Replica
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_repl_%d_%s" (Unix.getpid ()) suffix) in
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
  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("v", i 1) ]));
  ignore (Eng.insert eng c (doc [ ("_id", s "b"); ("v", i 2) ]));

  (* initial sync: hot backup -> open the follower from it, seeded at the backup's LSN *)
  let bk = temp "bk" in
  Eng.backup eng ~dir:bk ();
  let follower = Eng.open_ ~sw ~durability:S.No_sync bk in
  let fc = Eng.collection follower "t" in
  let rep = Rep.create follower in
  assert (Rep.last_applied rep = Eng.oplog_lsn eng) (* the backup pinned the exact LSN *);
  assert (Eng.count follower fc ~selector:empty = 2) (* a, b from the snapshot *);

  (* the source writes more; the follower is now behind by 3 ops *)
  ignore (Eng.insert eng c (doc [ ("_id", s "c"); ("v", i 3) ]));
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("v", i 9) ]) ]));
  ignore (Eng.remove eng c (doc [ ("_id", s "b") ]));
  assert (Rep.lag rep ~source_lsn:(Eng.oplog_lsn eng) = 3L);

  (* one step pulls + applies the backlog -> converged, lag 0 *)
  let pull ~from_lsn = Eng.oplog_tail eng ~from_lsn ~limit:1000 in
  assert (Rep.step rep ~pull = 3);
  assert (Rep.lag rep ~source_lsn:(Eng.oplog_lsn eng) = 0L);

  (* the follower now matches the source: c=3, a=9, b gone *)
  assert (Eng.count follower fc ~selector:empty = 2) (* a, c (b removed) *);
  assert (v_of follower fc "a" = Some 9);
  assert (v_of follower fc "c" = Some 3);
  assert (v_of follower fc "b" = None);

  (* a step with nothing new applies nothing *)
  assert (Rep.step rep ~pull = 0);

  (* ---- too-stale detection: a follower below the source's retention floor must re-sync ---- *)
  let capped = Eng.open_ ~sw ~durability:S.No_sync ~oplog_keep:3 (temp "cap") in
  let cc = Eng.collection capped "t" in
  for k = 1 to 6 do ignore (Eng.insert capped cc (doc [ ("_id", s (Printf.sprintf "k%d" k)) ])) done;
  let floor = Eng.oplog_floor capped in
  assert (floor = 4L) (* LSNs 1..3 trimmed, oldest retained is 4 *);

  let flw2 = Eng.open_ ~sw ~durability:S.No_sync (temp "flw2") in
  let r2 = Rep.create flw2 in
  let entry lsn id =
    Burrow.Oplog.entry_of_bson
      (doc [ ("lsn", B.Int64 lsn); ("op", B.String "i"); ("ns", B.String "t"); ("id", s id); ("o", doc [ ("_id", s id) ]) ])
  in
  assert (Rep.too_stale r2 ~source_floor:floor) (* fresh (0): needs 1, trimmed -> stale *);
  ignore (Rep.apply r2 [ entry 2L "k2" ]);
  assert (Rep.too_stale r2 ~source_floor:floor) (* at 2: needs 3, trimmed -> still stale *);
  ignore (Rep.apply r2 [ entry 3L "k3" ]);
  assert (not (Rep.too_stale r2 ~source_floor:floor)) (* at 3 (== floor-1): needs 4, retained -> OK *);
  assert (Rep.last_applied r2 = 3L);
  Eng.close capped;
  Eng.close flw2;

  Eng.close eng;
  Eng.close follower;
  print_string "replica: OK\n"
