(* Secondary indexes: equality + range scans, multikey (array) indexes, index maintenance across
   update/remove, unique enforcement, backfill on create, and persistence + reload across reopen.
   Each index result is checked against the known-correct set the collection scan would return. *)
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
let ids docs = List.sort compare (List.filter_map (fun d -> B.get_string d "_id") docs)

let () =
  Eio_main.run @@ fun _env ->
  let dir = tmp "index" in
  let eng = Eng.open_ ~durability:S.No_sync dir in
  let c = Eng.collection eng "t" in
  let insert d = ignore (Eng.insert eng c d) in
  let q sel = Eng.find eng c ~selector:sel ~sort:empty ~skip:0 ~limit:0 ~fields:empty in

  insert (doc [ ("_id", s "a"); ("age", i 20); ("tags", B.array [ s "x"; s "y" ]) ]);
  insert (doc [ ("_id", s "b"); ("age", i 30); ("tags", B.array [ s "y" ]) ]);
  insert (doc [ ("_id", s "c"); ("age", i 30); ("tags", B.array [ s "z" ]) ]);
  insert (doc [ ("_id", s "d"); ("age", i 40) ]);

  (* full-scan baseline before any index *)
  assert (ids (q (doc [ ("age", i 30) ])) = [ "b"; "c" ]);

  Eng.ensure_index eng c ~name:"age_1" ~keys:(doc [ ("age", i 1) ]) ~unique:false;
  Eng.ensure_index eng c ~name:"tags_1" ~keys:(doc [ ("tags", i 1) ]) ~unique:false;

  (* equality + ranges via the index match the scan answers *)
  assert (ids (q (doc [ ("age", i 30) ])) = [ "b"; "c" ]);
  assert (ids (q (doc [ ("age", doc [ ("$gte", i 30) ]) ])) = [ "b"; "c"; "d" ]);
  assert (ids (q (doc [ ("age", doc [ ("$gt", i 20); ("$lt", i 40) ]) ])) = [ "b"; "c" ]);
  assert (ids (q (doc [ ("age", doc [ ("$lte", i 20) ]) ])) = [ "a" ]);
  assert (ids (q (doc [ ("age", doc [ ("$gt", i 40) ]) ])) = []);
  (* multikey: a scalar query against an array field, served by the index *)
  assert (ids (q (doc [ ("tags", s "y") ])) = [ "a"; "b" ]);
  assert (ids (q (doc [ ("tags", s "z") ])) = [ "c" ]);
  assert (ids (q (doc [ ("tags", s "missing") ])) = []);

  (* update maintains the index: move b from 30 to 99 *)
  ignore (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "b") ]) (doc [ ("$set", doc [ ("age", i 99) ]) ]));
  assert (ids (q (doc [ ("age", i 30) ])) = [ "c" ]);
  assert (ids (q (doc [ ("age", i 99) ])) = [ "b" ]);

  (* remove maintains the index *)
  ignore (Eng.remove eng c (doc [ ("_id", s "c") ]));
  assert (ids (q (doc [ ("age", i 30) ])) = []);

  (* unique index *)
  Eng.ensure_index eng c ~name:"u_1" ~keys:(doc [ ("u", i 1) ]) ~unique:true;
  ignore (Eng.insert eng c (doc [ ("_id", s "p"); ("u", i 1) ]));
  let dup =
    try
      ignore (Eng.insert eng c (doc [ ("_id", s "q"); ("u", i 1) ]));
      false
    with Burrow.Write.Duplicate_key _ -> true
  in
  assert dup;
  (* a distinct value is fine; and the rejected insert left nothing behind *)
  ignore (Eng.insert eng c (doc [ ("_id", s "r"); ("u", i 2) ]));
  assert (ids (q (doc [ ("u", i 1) ])) = [ "p" ]);

  (* reopen: indexes persist + reload, still correct, still enforcing uniqueness *)
  Eng.close eng;
  let eng2 = Eng.open_ ~durability:S.No_sync dir in
  let c2 = Eng.collection eng2 "t" in
  let q2 sel = Eng.find eng2 c2 ~selector:sel ~sort:empty ~skip:0 ~limit:0 ~fields:empty in
  assert (ids (q2 (doc [ ("age", i 99) ])) = [ "b" ]);
  (* b's tags were untouched by the age update, so y still matches both a and b *)
  assert (ids (q2 (doc [ ("tags", s "y") ])) = [ "a"; "b" ]);
  let dup2 =
    try
      ignore (Eng.insert eng2 c2 (doc [ ("_id", s "z"); ("u", i 2) ]));
      false
    with Burrow.Write.Duplicate_key _ -> true
  in
  assert dup2;
  Eng.close eng2;
  print_string "engine indexes: OK\n"
