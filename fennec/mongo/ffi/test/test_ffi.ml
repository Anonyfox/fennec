(* The native libmongoc driver, end to end against a REAL mongod (launched by the lifecycle manager)
   through the statically-linked archives. Skips (and passes) when the native driver wasn't built or
   no mongod is installed, so CI without either stays green. *)

module Ffi = Fennec_mongo_ffi.Mongo_ffi
module BJ = Fennec_mongo_bson_json.Bson_json
module M = Fennec_mongo_mongod.Mongod
module B = Bson

let%test "connect + ping + insert + find against a real mongod (static libmongoc)" =
  if not (Ffi.available ()) then true (* native driver not built — skip *)
  else
    match M.find () with
    | None -> true (* no mongod — skip *)
    | Some _ ->
        M.with_ephemeral (fun t ->
            Ffi.init ();
            let pool = Ffi.pool_new (M.uri t) in
            let ping_ok = Ffi.ping pool "admin" in
            let insert doc = ignore (Ffi.insert_one pool "testdb" "tasks" (BJ.to_string doc)) in
            insert (B.doc [ ("title", B.str "hello"); ("n", B.int 1) ]);
            insert (B.doc [ ("title", B.str "world"); ("n", B.int 2) ]);
            let all = BJ.list_of_string (Ffi.find pool "testdb" "tasks" "{}" "{}") in
            let just_2 = BJ.list_of_string (Ffi.find pool "testdb" "tasks" (BJ.to_string (B.doc [ ("n", B.int 2) ])) "{}") in
            ping_ok
            && List.length all = 2
            && (match just_2 with [ d ] -> B.get d "title" = Some (B.String "world") | _ -> false))

let () = exit (Fennec_hunt_unit.run ())
