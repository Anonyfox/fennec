(* Concurrency: many writer fibers run at once. Without serialization, a contended LMDB write-txn begin
   would block the whole Eio domain and deadlock; the engine's write lock must make them queue cleanly
   and apply every write. Reads run lock-free (MVCC) during writes. *)
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
  let eng = Eng.open_ ~sw ~durability:S.No_sync (tmp "concurrent") in
  let c = Eng.collection eng "t" in
  let n_fibers = 8 and per = 50 in

  (* concurrent inserts across fibers *)
  Eio.Fiber.all
    (List.init n_fibers (fun f () ->
         for j = 0 to per - 1 do
           ignore (Eng.insert eng c (doc [ ("_id", s (Printf.sprintf "f%d_%d" f j)); ("f", i f) ]))
         done));
  assert (Eng.count eng c ~selector:empty = n_fibers * per);

  (* readers query lock-free while a writer runs; never observe a torn/negative state *)
  let reader_ok = ref true in
  Eio.Fiber.all
    [ (fun () -> for j = 0 to 99 do ignore (Eng.insert eng c (doc [ ("_id", s (Printf.sprintf "w_%d" j)); ("w", B.bool true) ])) done);
      (fun () ->
        for _ = 0 to 299 do
          if Eng.count eng c ~selector:empty < n_fibers * per then reader_ok := false;
          Eio.Fiber.yield ()
        done) ];
  assert !reader_ok;
  assert (Eng.count eng c ~selector:(doc [ ("w", B.bool true) ]) = 100);

  (* concurrent updates to disjoint docs *)
  Eio.Fiber.all
    (List.init n_fibers (fun f () ->
         for j = 0 to per - 1 do
           ignore
             (Eng.update eng c ~multi:false ~upsert:false
                (doc [ ("_id", s (Printf.sprintf "f%d_%d" f j)) ])
                (doc [ ("$set", doc [ ("touched", B.bool true) ]) ]))
         done));
  assert (Eng.count eng c ~selector:(doc [ ("touched", B.bool true) ]) = n_fibers * per);

  Eng.close eng;
  print_string "engine concurrency: OK\n"
