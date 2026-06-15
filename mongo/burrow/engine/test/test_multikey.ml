(* Focused reproduction of a multikey-index maintenance bug found by the differential test: a doc with
   an array field, indexed, then updated on OTHER fields (whose selector is served by other indexes) —
   the array index entries must survive. *)
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
  Eio.Switch.run @@ fun sw ->
  let eng = Eng.open_ ~sw ~durability:S.No_sync (tmp "mk") in
  let c = Eng.collection eng "t" in
  Eng.ensure_index eng c ~name:"a_1" ~keys:(doc [ ("a", i 1) ]) ~unique:false ~sparse:false;
  Eng.ensure_index eng c ~name:"b_1" ~keys:(doc [ ("b", i 1) ]) ~unique:false ~sparse:false;
  Eng.ensure_index eng c ~name:"arr_1" ~keys:(doc [ ("arr", i 1) ]) ~unique:false ~sparse:false;
  let q sel = ids (Eng.find eng c ~selector:sel ~sort:empty ~skip:0 ~limit:0 ~fields:empty) in
  ignore (Eng.insert eng c (doc [ ("_id", s "X"); ("a", i 0); ("b", s "x"); ("arr", B.array [ i 1; i 2 ]) ]));

  assert (q (doc [ ("arr", i 1) ]) = [ "X" ]);
  Printf.printf "after insert: arr:1 -> %s\n%!" (String.concat "," (q (doc [ ("arr", i 1) ])));

  ignore (Eng.update eng c ~multi:true ~upsert:false (doc [ ("a", i 0) ]) (doc [ ("$inc", doc [ ("a", i 1) ]) ]));
  Printf.printf "after $inc a (a:0->1): arr:1 -> [%s]\n%!" (String.concat "," (q (doc [ ("arr", i 1) ])));

  ignore (Eng.update eng c ~multi:true ~upsert:false (doc [ ("b", s "x") ]) (doc [ ("$set", doc [ ("b", s "w") ]) ]));
  Printf.printf "after $set b=w: arr:1 -> [%s]\n%!" (String.concat "," (q (doc [ ("arr", i 1) ])));

  assert (q (doc [ ("arr", i 1) ]) = [ "X" ]);
  assert (q (doc [ ("arr", i 2) ]) = [ "X" ]);
  assert (Eng.remove eng c (doc [ ("arr", i 1) ]) = 1);
  Eng.close eng;
  print_string "multikey repro: OK\n"
