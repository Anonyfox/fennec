(* Micro-benchmark for the engine's access paths over a 100k-document collection. Contrasts the
   optimized paths (index equality/range, sort-via-index streaming with a limit, storage-layer count)
   against their unoptimized equivalents (collection scan, full sort) so the wins are visible. No_sync
   durability so insert throughput isn't dominated by fsync. *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let tmp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_bench_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []

let time name (f : unit -> string) =
  let t0 = Unix.gettimeofday () in
  let info = f () in
  let dt = (Unix.gettimeofday () -. t0) *. 1000. in
  Printf.printf "  %-46s %9.2f ms   %s\n%!" name dt info

let () =
  Eio_main.run @@ fun _env ->
  let eng = Eng.open_ ~durability:S.No_sync (tmp ()) in
  let c = Eng.collection eng "t" in
  Eng.ensure_index eng c ~name:"n_1" ~keys:(doc [ ("n", i 1) ]) ~unique:false;
  Eng.ensure_index eng c ~name:"g_1" ~keys:(doc [ ("g", i 1) ]) ~unique:false;
  let n = 100_000 in
  Random.init 1;
  Printf.printf "Burrow engine bench — %d documents, indexes on n (0..%d) and g (0..9)\n%!" n n;

  time (Printf.sprintf "insert %d docs (record + 2 indexes each)" n) (fun () ->
      for k = 0 to n - 1 do
        ignore
          (Eng.insert eng c
             (doc [ ("_id", s (string_of_int k)); ("n", i (Random.int n)); ("g", i (Random.int 10)); ("name", s "fixed") ]))
      done;
      Printf.sprintf "%d inserted" n);

  let find ?(sort = empty) ?(skip = 0) ?(limit = 0) sel =
    Eng.find eng c ~selector:sel ~sort ~skip ~limit ~fields:empty
  in
  let hits l = Printf.sprintf "%d hits" (List.length l) in

  Printf.printf "-- point/equality --\n";
  time "find {n = 42}  (via n_1 index)" (fun () -> hits (find (doc [ ("n", i 42) ])));
  time "find {name = \"fixed\"}  (collection scan, all match)" (fun () -> hits (find (doc [ ("name", s "fixed") ])));
  time "find {g = 5}  (via g_1 index, ~10%)" (fun () -> hits (find (doc [ ("g", i 5) ])));

  Printf.printf "-- range / $in --\n";
  time "find {n: {$gte: 50000, $lt: 50100}}  (index range)" (fun () -> hits (find (doc [ ("n", doc [ ("$gte", i 50000); ("$lt", i 50100) ]) ])));
  time "find {n: {$in: [1;2;3;4;5]}}  (index $in)" (fun () -> hits (find (doc [ ("n", doc [ ("$in", B.array [ i 1; i 2; i 3; i 4; i 5 ]) ]) ])));

  Printf.printf "-- sorted + limit (pagination) --\n";
  time "sort {n:1,_id:1} limit 10  (sort-via-index, streaming)" (fun () -> hits (find ~sort:(doc [ ("n", i 1); ("_id", i 1) ]) ~limit:10 empty));
  time "sort {n:1,_id:1} skip 90000 limit 10  (streaming)" (fun () -> hits (find ~sort:(doc [ ("n", i 1); ("_id", i 1) ]) ~skip:90000 ~limit:10 empty));
  time "sort {name:1} limit 10  (no index -> scan + full sort)" (fun () -> hits (find ~sort:(doc [ ("name", i 1) ]) ~limit:10 empty));

  Printf.printf "-- count --\n";
  time "count {}  (storage-layer, no decode)" (fun () -> Printf.sprintf "%d" (Eng.count eng c ~selector:empty));
  time "count {g = 5}  (index + match)" (fun () -> Printf.sprintf "%d" (Eng.count eng c ~selector:(doc [ ("g", i 5) ])));

  Eng.close eng
