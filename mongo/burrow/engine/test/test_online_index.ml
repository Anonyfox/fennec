(* Online index builds: a NON-UNIQUE index builds without holding the write lock for the whole walk —
   registered query-invisible (ready=false), backfilled in CHUNKS through the writer (live writes
   interleave + maintain it), then flipped ready. We build over >1000 records to exercise the multi-chunk
   resume, confirm the index is used AND complete, that a post-build write is maintained, that it survives
   reopen, and — crash recovery — that a ready=false (incomplete) index is dropped on reopen. *)
module Eng = Burrow.Engine
module Cat = Burrow.Catalog
module Planner = Burrow.Planner
module Plan = Burrow.Plan
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_oidx_%d_%s" (Unix.getpid ()) suffix) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let i = B.int
let str = B.str
let empty = B.Document []
let n7 = doc [ ("n", i 7) ]

let uses_index (c : Cat.collection) sel =
  match Planner.plan c.Cat.indexes ~selector:sel ~sort:empty with Plan.Index_scan _ -> true | _ -> false

let find_n7 eng c = Eng.find eng c ~selector:n7 ~sort:empty ~skip:0 ~limit:0 ~fields:empty

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  (* ---- online build over >1000 records (two chunks), with a post-build write ---- *)
  let dir = temp "e2e" in
  let eng = Eng.open_ ~sw ~durability:S.No_sync dir in
  let c = Eng.collection eng "t" in
  (* 1100 records, n = k mod 100 — so 11 of them have n = 7 (k = 7, 107, ..., 1007) *)
  for k = 1 to 1100 do
    ignore (Eng.insert eng c (doc [ ("_id", str (Printf.sprintf "u%05d" k)); ("n", i (k mod 100)) ]))
  done;

  (* before the index exists, a query on n scans *)
  assert (not (uses_index c n7));
  (* the online build spans two chunks (1100 > the 1000-record chunk) and flips the index ready *)
  Eng.ensure_index eng c ~name:"n_1" ~keys:(doc [ ("n", i 1) ]) ~unique:false ~sparse:false;
  (* the planner now USES the index, and it is COMPLETE: all 11 n=7 records are found through it *)
  assert (uses_index c n7);
  assert (List.length (find_n7 eng c) = 11);

  (* a write AFTER the build is maintained in the index too *)
  ignore (Eng.insert eng c (doc [ ("_id", str "u90001"); ("n", i 7) ]));
  assert (List.length (find_n7 eng c) = 12);
  Eng.close eng;

  (* reopen: ready persisted, the index is still used + complete *)
  let eng2 = Eng.open_ ~sw ~durability:S.No_sync dir in
  let c2 = Eng.collection eng2 "t" in
  assert (uses_index c2 n7);
  assert (List.length (find_n7 eng2 c2) = 12);
  Eng.close eng2;

  (* ---- crash recovery: a ready=false (incomplete) index is dropped on reopen ---- *)
  let dir2 = temp "wb" in
  let st = S.open_ ~durability:S.No_sync dir2 in
  let cat = Cat.open_ st in
  let col = Cat.collection cat "x" in
  (* a built index: register ready=false then flip ready (the online build's phases 1 + 3) *)
  ignore (Cat.ensure_index cat col ~name:"done_1" ~keys:(doc [ ("b", i 1) ]) ~unique:false ~sparse:false ~ready:false);
  (match List.find_opt (fun ix -> ix.Cat.iname = "done_1") col.Cat.indexes with
   | Some ix -> Cat.set_index_ready cat col ix
   | None -> assert false);
  (* an index left mid-build (ready=false): simulates a crash before phase 3 *)
  ignore (Cat.ensure_index cat col ~name:"partial_1" ~keys:(doc [ ("b", i 1) ]) ~unique:false ~sparse:false ~ready:false);
  S.close st;

  let st2 = S.open_ ~durability:S.No_sync dir2 in
  let col2 = Cat.collection (Cat.open_ st2) "x" in
  assert (List.mem "done_1" (Cat.index_names col2)) (* the readied one survives *);
  assert (not (List.mem "partial_1" (Cat.index_names col2))) (* the incomplete one was dropped *);
  S.close st2;

  print_string "online index: OK\n"
