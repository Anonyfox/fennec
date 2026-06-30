(* Query result governance: a configurable cap on the documents a read MATERIALIZES (the matched set,
   pre-window), so an unbounded scan raises a clean Result_too_large instead of exhausting memory. The
   cap is on what the query loads, so production sets it generously; here a tiny cap exercises the path.
   A SELECTIVE query whose matched set is within the cap passes — even over a larger collection. *)
module Eng = Burrow.Engine
module Ex = Burrow.Executor
module S = Burrow_store.Store
module B = Bson

let temp suffix =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_gov_%d_%s" (Unix.getpid ()) suffix) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []
let too_large f = try ignore (f ()); false with Ex.Result_too_large _ -> true

let seed eng c =
  for k = 1 to 10 do
    ignore (Eng.insert eng c (doc [ ("_id", s (Printf.sprintf "u%d" k)); ("n", i k); ("grp", s "x") ]))
  done

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  (* a capped engine: at most 5 materialized documents per read *)
  let eng = Eng.open_ ~sw ~durability:S.No_sync ~query_limit:5 (temp "capped") in
  let c = Eng.collection eng "t" in
  seed eng c;

  (* an unbounded scan (10 matched > cap 5) raises a clean Result_too_large *)
  assert (too_large (fun () -> Eng.find eng c ~selector:empty ~sort:empty ~skip:0 ~limit:0 ~fields:empty));
  (* the cap is on the MATERIALIZED set, so even a query limit doesn't dodge it (it signals: add an index) *)
  assert (too_large (fun () -> Eng.find eng c ~selector:empty ~sort:empty ~skip:0 ~limit:3 ~fields:empty));
  (* distinct + aggregate over the whole collection also raise *)
  assert (too_large (fun () -> Eng.distinct eng c ~key:"grp" ~selector:empty));
  assert (too_large (fun () -> Eng.aggregate eng c [ doc [ ("$count", s "n") ] ]));

  (* a SELECTIVE query whose matched set is within the cap is fine, though the collection is larger *)
  assert (List.length (Eng.find eng c ~selector:(doc [ ("n", i 3) ]) ~sort:empty ~skip:0 ~limit:0 ~fields:empty) = 1);
  (* a point lookup is bounded regardless of the cap *)
  (match Eng.find_one eng c ~selector:(doc [ ("_id", s "u7") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_int d "n" = Some 7)
   | None -> assert false);

  (* the default is UNLIMITED: a fresh engine with no query_limit scans the whole collection freely *)
  let eng2 = Eng.open_ ~sw ~durability:S.No_sync (temp "uncapped") in
  let c2 = Eng.collection eng2 "t" in
  seed eng2 c2;
  assert (Eng.count eng2 c2 ~selector:empty = 10);
  assert (List.length (Eng.find eng2 c2 ~selector:empty ~sort:empty ~skip:0 ~limit:0 ~fields:empty) = 10);

  Eng.close eng;
  Eng.close eng2;
  print_string "governance: OK\n"
