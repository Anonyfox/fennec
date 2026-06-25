(* The transparent transaction over the REAL burrow (embedded LMDB) backend — the native-rollback path
   ([begin_txn]/[commit_txn]/[abort_txn] holding one LMDB parent txn for the whole workflow): commit on
   return, roll back on raise (insert / update / remove), read-your-writes, and nested-flatten. Runs
   UNDER an Eio scheduler (the engine's writer fiber needs the switch), MONGO_URL pointed at a temp
   burrow:// directory — so this exercises the durable backend, not :memory:. *)

let tmp =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("fennec_txburrow_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let () = Unix.putenv "MONGO_URL" ("burrow://" ^ tmp ^ "/db")

module Dyn = Fennec_mongo_dynamic
module Tx = Dyn.Tx
module D = Dyn.Dynamic
module B = Bson

let q = Fennec_mongo_backend.query ()
let person name = B.doc [ ("name", B.str name) ]

exception Boom

let check msg cond =
  if cond then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  D.set_switch sw;
  let coll n = D.collection ~name:n () in
  let count c = List.length (D.find c q) in
  let names c =
    D.find c q
    |> List.filter_map (fun d -> match B.get d "name" with Some (B.String s) -> Some s | _ -> None)
    |> List.sort compare
  in

  (* 1. commit: writes persist after a successful transaction *)
  let c = coll "p1" in
  Tx.run (fun () ->
      ignore (D.insert c (person "ada"));
      ignore (D.insert c (person "bob")));
  check "burrow: commit persists writes" (count c = 2);

  (* 2. read-your-writes: inside the transaction, reads see the pending writes (the held LMDB txn) *)
  let c = coll "p2" in
  Tx.run (fun () ->
      ignore (D.insert c (person "ada"));
      check "burrow: read-your-writes" (count c = 1));

  (* 3. rollback on raise: inserts vanish, pre-transaction data survives *)
  let c = coll "p3" in
  ignore (D.insert c (person "seed"));
  (try Tx.run (fun () -> ignore (D.insert c (person "ada")); ignore (D.insert c (person "bob")); raise Boom) with Boom -> ());
  check "burrow: rollback reverts inserts (pre-tx data kept)" (names c = [ "seed" ]);

  (* 4. update rollback: a modified document returns to its pre-image *)
  let c = coll "p4" in
  ignore (D.insert c (person "ada"));
  (try
     Tx.run (fun () ->
         ignore (D.update c ~multi:true ~upsert:false (person "ada") (B.doc [ ("$set", B.doc [ ("name", B.str "eve") ]) ]));
         check "burrow: update visible in-tx" (names c = [ "eve" ]);
         raise Boom)
   with Boom -> ());
  check "burrow: rollback reverts update" (names c = [ "ada" ]);

  (* 5. remove rollback: a removed document comes back *)
  let c = coll "p5" in
  ignore (D.insert c (person "ada"));
  (try
     Tx.run (fun () ->
         ignore (D.remove c (person "ada"));
         check "burrow: remove visible in-tx" (count c = 0);
         raise Boom)
   with Boom -> ());
  check "burrow: rollback reverts remove" (names c = [ "ada" ]);

  (* 6. nested transactions flatten onto ONE parent txn: the OUTER rollback reverts the inner too *)
  let c = coll "p6" in
  (try
     Tx.run (fun () ->
         ignore (D.insert c (person "outer"));
         Tx.run (fun () -> ignore (D.insert c (person "inner")));
         check "burrow: nested both visible" (count c = 2);
         raise Boom)
   with Boom -> ());
  check "burrow: outer rollback reverts nested inner writes" (count c = 0);

  Printf.printf "all burrow transaction tests passed\n%!"
