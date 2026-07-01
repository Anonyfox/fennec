(* explain: the winningPlan the planner would choose for a query, WITHOUT executing it — COLLSCAN for an
   unindexed field, IDHACK for an _id point, IXSCAN (with the index name) once an index covers the field. *)
module Eng = Burrow.Engine
module B = Bson

let temp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_explain_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int
let empty = B.Document []
let stage d = match B.get d "stage" with Some (B.String x) -> x | _ -> "?"

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:Burrow_store.Store.No_sync (temp ()) in
  let c = Eng.collection eng "t" in
  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("n", i 1) ]));

  (* an unindexed field -> a collection scan *)
  assert (stage (Eng.explain eng c ~selector:(doc [ ("n", i 1) ]) ~sort:empty) = "COLLSCAN");
  (* an _id point -> IDHACK *)
  assert (stage (Eng.explain eng c ~selector:(doc [ ("_id", s "a") ]) ~sort:empty) = "IDHACK");

  (* after indexing n, the same query -> IXSCAN naming the index *)
  Eng.ensure_index eng c ~name:"n_1" ~keys:(doc [ ("n", i 1) ]) ~unique:false ~sparse:false;
  let e = Eng.explain eng c ~selector:(doc [ ("n", i 1) ]) ~sort:empty in
  assert (stage e = "IXSCAN");
  assert (B.get_string e "indexName" = Some "n_1");

  Eng.close eng;
  print_string "explain: OK\n"
