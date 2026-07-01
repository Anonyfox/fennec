(* change_event: format a raw oplog entry ({lsn, op, ns, id, o?}) as a MongoDB change-stream event — the
   resume token (_id._data = the LSN), operationType, ns, documentKey, and (insert/update) fullDocument.
   The pure formatting the wire $changeStream tail uses; the end-to-end tail is a mongosh smoke. *)
module M = Fennec_mongo_dynamic
module B = Bson

let doc fields = B.Document fields

let () =
  (* an insert entry -> operationType "insert", full ns, documentKey, fullDocument, resume token = LSN *)
  let e =
    M.change_event ~db:"shop"
      (doc
         [ ("lsn", B.Int64 7L); ("op", B.String "i"); ("ns", B.String "items"); ("id", B.str "a");
           ("o", doc [ ("_id", B.str "a"); ("n", B.int 1) ]) ])
  in
  assert (B.get_string e "operationType" = Some "insert");
  (match B.get e "ns" with
   | Some ns -> assert (B.get_string ns "db" = Some "shop" && B.get_string ns "coll" = Some "items")
   | None -> assert false);
  (match B.get e "documentKey" with Some dk -> assert (B.get dk "_id" = Some (B.str "a")) | None -> assert false);
  (match B.get e "fullDocument" with Some fd -> assert (B.get_int fd "n" = Some 1) | None -> assert false);
  (match B.get e "_id" with Some rid -> assert (B.get_string rid "_data" = Some "7") | None -> assert false);

  (* a delete entry -> operationType "delete", no fullDocument *)
  let d =
    M.change_event ~db:"shop"
      (doc [ ("lsn", B.Int64 8L); ("op", B.String "d"); ("ns", B.String "items"); ("id", B.str "a") ])
  in
  assert (B.get_string d "operationType" = Some "delete");
  assert (B.get d "fullDocument" = None);

  print_string "change_event: OK\n"
