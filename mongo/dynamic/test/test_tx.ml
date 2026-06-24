(* The transparent transaction context ({!Fennec_mongo_dynamic.Tx}) over the in-memory backend:
   commit-on-return, rollback-on-raise, read-your-writes, nested-flatten, update/remove rollback,
   live-observer convergence on rollback, and the untouched non-transactional path. Runs outside an
   Eio scheduler on purpose — that exercises the global-fallback arm of the ambient seam. *)

module Dyn = Fennec_mongo_dynamic
module Tx = Dyn.Tx
module D = Dyn.Dynamic
module B = Bson

let q = Fennec_mongo_backend.query ()
let count c = List.length (D.find c q)

let names c =
  D.find c q
  |> List.filter_map (fun d -> match B.get d "name" with Some (B.String s) -> Some s | _ -> None)
  |> List.sort compare

let person name = B.doc [ ("name", B.str name) ]

exception Boom

let check msg cond =
  if cond then Printf.printf "  ok: %s\n%!" msg
  else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  (* 1. commit: writes persist after a successful transaction *)
  let c = D.mem (Minimongo.create ()) in
  Tx.run (fun () ->
      ignore (D.insert c (person "ada"));
      ignore (D.insert c (person "bob")));
  check "commit persists writes" (count c = 2);

  (* 2. read-your-writes: inside the transaction, reads see the writes immediately *)
  let c = D.mem (Minimongo.create ()) in
  Tx.run (fun () ->
      ignore (D.insert c (person "ada"));
      check "read-your-writes" (count c = 1));

  (* 3. rollback on raise: inserts vanish, pre-transaction data survives *)
  let c = D.mem (Minimongo.create ()) in
  ignore (D.insert c (person "seed"));
  (try Tx.run (fun () ->
         ignore (D.insert c (person "ada"));
         ignore (D.insert c (person "bob"));
         raise Boom)
   with Boom -> ());
  check "rollback reverts inserts (pre-tx data kept)" (names c = [ "seed" ]);

  (* 4. update rollback: a modified document returns to its pre-image *)
  let c = D.mem (Minimongo.create ()) in
  ignore (D.insert c (person "ada"));
  (try Tx.run (fun () ->
         ignore (D.update c ~multi:true ~upsert:false (person "ada")
                   (B.doc [ ("$set", B.doc [ ("name", B.str "eve") ]) ]));
         check "update visible in-tx" (names c = [ "eve" ]);
         raise Boom)
   with Boom -> ());
  check "rollback reverts update" (names c = [ "ada" ]);

  (* 5. remove rollback: a removed document comes back *)
  let c = D.mem (Minimongo.create ()) in
  ignore (D.insert c (person "ada"));
  (try Tx.run (fun () ->
         ignore (D.remove c (person "ada"));
         check "remove visible in-tx" (count c = 0);
         raise Boom)
   with Boom -> ());
  check "rollback reverts remove" (names c = [ "ada" ]);

  (* 6. nested transactions flatten: the OUTER rollback reverts the inner run's writes too *)
  let c = D.mem (Minimongo.create ()) in
  (try Tx.run (fun () ->
         ignore (D.insert c (person "outer"));
         Tx.run (fun () -> ignore (D.insert c (person "inner")));
         check "nested both visible" (count c = 2);
         raise Boom)
   with Boom -> ());
  check "outer rollback reverts nested inner writes" (count c = 0);

  (* 7. a committed nested run does NOT commit early — only the outer return commits *)
  let c = D.mem (Minimongo.create ()) in
  (try Tx.run (fun () ->
         Tx.run (fun () -> ignore (D.insert c (person "inner")));
         raise Boom)
   with Boom -> ());
  check "inner commit does not escape the outer rollback" (count c = 0);

  (* 8. live observer convergence: a watcher that saw the doomed insert is corrected on rollback *)
  let m = Minimongo.create () in
  let c = D.mem m in
  let seen = Hashtbl.create 8 in
  let _h =
    D.observe_changes c q
      ~added:(fun id _ -> Hashtbl.replace seen id ())
      ~changed:(fun _ _ _ -> ())
      ~removed:(fun id -> Hashtbl.remove seen id)
  in
  (try Tx.run (fun () -> ignore (D.insert c (person "ghost")); raise Boom) with Boom -> ());
  check "observer converged after rollback (ghost gone)" (Hashtbl.length seen = 0);

  (* 9. the non-transactional path is untouched: writes outside Tx.run persist normally *)
  let c = D.mem (Minimongo.create ()) in
  ignore (D.insert c (person "ada"));
  check "no-tx write persists (byte-identical path)" (count c = 1);

  Printf.printf "all transaction-context tests passed\n%!"
