(* Phase-A end-to-end: the storage stack -> planner -> executor -> query evaluators pipeline, plus
   durability across reopen. Uses No_sync durability for test speed. *)
module Eng = Burrow.Engine
module B = Bson

let tmp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_eng_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let s = B.str
let i = B.int

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let dir = tmp () in
  let eng = Eng.open_ ~sw ~durability:Burrow_store.Store.No_sync dir in
  let users = Eng.collection eng "users" in

  ignore
    (Eng.insert eng users
       (doc [ ("_id", s "u1"); ("name", s "ada"); ("age", i 36); ("tags", B.array [ s "vip"; s "math" ]) ]));
  ignore (Eng.insert eng users (doc [ ("_id", s "u2"); ("name", s "bob"); ("age", i 20) ]));
  ignore (Eng.insert eng users (doc [ ("_id", s "u3"); ("name", s "cleo"); ("age", i 36); ("tags", B.array [ s "vip" ]) ]));
  let gen = Eng.insert eng users (doc [ ("name", s "dan"); ("age", i 50) ]) in
  assert (String.length gen = 24);

  (* a missing _id is generated; collection now has 4 *)
  assert (Eng.count eng users ~selector:(B.Document []) = 4);

  let empty = B.Document [] in
  let find ?(sel = empty) ?(sort = empty) ?(skip = 0) ?(limit = 0) ?(fields = empty) () =
    Eng.find eng users ~selector:sel ~sort ~skip ~limit ~fields
  in

  (* point lookup by _id *)
  (match Eng.find_one eng users ~selector:(doc [ ("_id", s "u2") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_string d "name" = Some "bob")
   | None -> assert false);

  (* equality on a field (collection scan + matcher): age = 36 -> u1, u3 *)
  assert (List.length (find ~sel:(doc [ ("age", i 36) ]) ()) = 2);
  (* operator: age >= 36 -> u1, u3, dan *)
  assert (List.length (find ~sel:(doc [ ("age", doc [ ("$gte", i 36) ]) ]) ()) = 3);
  (* scalar-against-array: tags = "vip" -> u1, u3 *)
  assert (List.length (find ~sel:(doc [ ("tags", s "vip") ]) ()) = 2);

  (* sort by age asc + projection (name only, drop _id) *)
  let sorted = find ~sort:(doc [ ("age", i 1) ]) ~fields:(doc [ ("name", i 1); ("_id", i 0) ]) () in
  let names = List.map (fun d -> Option.value ~default:"?" (B.get_string d "name")) sorted in
  assert (List.hd names = "bob");
  (* age 20 *)
  assert (List.nth names 3 = "dan");
  (* age 50 *)
  (match sorted with d :: _ -> assert (B.get d "_id" = None && B.get d "age" = None) | [] -> assert false);

  (* skip/limit on the sorted set *)
  assert (List.length (find ~sort:(doc [ ("age", i 1) ]) ~skip:1 ~limit:2 ()) = 2);

  Eng.close eng;

  (* reopen: catalog + records persist *)
  let eng2 = Eng.open_ ~sw ~durability:Burrow_store.Store.No_sync dir in
  let users2 = Eng.collection eng2 "users" in
  assert (Eng.count eng2 users2 ~selector:empty = 4);
  (match Eng.find_one eng2 users2 ~selector:(doc [ ("_id", s "u1") ]) ~sort:empty ~skip:0 ~fields:empty with
   | Some d -> assert (B.get_int d "age" = Some 36)
   | None -> assert false);
  Eng.close eng2;
  print_string "engine phase A: OK\n"
