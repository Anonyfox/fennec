(* The mutation path: single/multi update, $set/$inc, no-match, upsert insert, remove. *)
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
  let eng = Eng.open_ ~sw ~durability:S.No_sync (tmp "writes") in
  let c = Eng.collection eng "t" in
  let insert d = ignore (Eng.insert eng c d) in
  let one sel = Eng.find_one eng c ~selector:sel ~sort:empty ~skip:0 ~fields:empty in

  insert (doc [ ("_id", s "a"); ("n", i 1); ("g", s "x") ]);
  insert (doc [ ("_id", s "b"); ("n", i 2); ("g", s "x") ]);
  insert (doc [ ("_id", s "c"); ("n", i 3); ("g", s "y") ]);

  (* single update with $set *)
  assert (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "a") ]) (doc [ ("$set", doc [ ("n", i 10) ]) ]) = 1);
  (match one (doc [ ("_id", s "a") ]) with Some d -> assert (B.get_int d "n" = Some 10) | None -> assert false);

  (* multi update with $inc over g=x -> a,b *)
  assert (Eng.update eng c ~multi:true ~upsert:false (doc [ ("g", s "x") ]) (doc [ ("$inc", doc [ ("n", i 100) ]) ]) = 2);
  (match one (doc [ ("_id", s "a") ]) with Some d -> assert (B.get_int d "n" = Some 110) | None -> assert false);
  (match one (doc [ ("_id", s "b") ]) with Some d -> assert (B.get_int d "n" = Some 102) | None -> assert false);

  (* non-multi update touches at most one even if many match *)
  assert (Eng.update eng c ~multi:false ~upsert:false (doc [ ("g", s "x") ]) (doc [ ("$set", doc [ ("k", i 1) ]) ]) = 1);

  (* no match, no upsert -> 0 *)
  assert (Eng.update eng c ~multi:false ~upsert:false (doc [ ("_id", s "zzz") ]) (doc [ ("$set", doc [ ("n", i 0) ]) ]) = 0);

  (* upsert insert: seeds from selector equality + modifier *)
  assert (Eng.update eng c ~multi:false ~upsert:true (doc [ ("_id", s "d"); ("g", s "z") ]) (doc [ ("$set", doc [ ("n", i 7) ]) ]) = 1);
  (match one (doc [ ("_id", s "d") ]) with
   | Some d -> assert (B.get_int d "n" = Some 7 && B.get_string d "g" = Some "z")
   | None -> assert false);
  assert (Eng.count eng c ~selector:empty = 4);

  (* remove all matching *)
  assert (Eng.remove eng c (doc [ ("g", s "x") ]) = 2);
  assert (Eng.count eng c ~selector:empty = 2);
  assert (one (doc [ ("_id", s "a") ]) = None);

  Eng.close eng;
  print_string "engine writes: OK\n"
