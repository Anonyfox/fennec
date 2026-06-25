(* The transparent transaction over REAL replica-set MongoDB — the libmongoc client-session path
   (session_start / *_s ops / session_commit|abort): commit on return, roll back on raise
   (insert / update / remove), and read-your-writes. Stands up a managed single-node replica set
   (Mongo_rs); SKIPS cleanly (exit 0) when no mongod is installed, so CI without mongo stays green. *)

module Rs = Fennec_dev.Mongo_rs
module Dyn = Fennec_mongo_dynamic
module Tx = Dyn.Tx
module D = Dyn.Dynamic
module B = Bson

let q = Fennec_mongo_backend.query ()
let person name = B.doc [ ("name", B.str name) ]

exception Boom

(* raise on failure (not exit) so the Fun.protect below still stops the mongod *)
let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else failwith ("FAIL: " ^ msg)

let run rs =
  (* the replica-set URI (transactions need topology awareness, not a directConnection) *)
  Unix.putenv "MONGO_URL" (Printf.sprintf "mongodb://127.0.0.1:%d/?replicaSet=rs0" (Rs.port rs));
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
  let fresh n = let c = coll n in ignore (D.remove c (B.Document [])); c in

  (* 1. commit: writes persist after a successful transaction *)
  let c = fresh "p1" in
  Tx.run (fun () -> ignore (D.insert c (person "ada")); ignore (D.insert c (person "bob")));
  check "mongo: commit persists writes" (count c = 2);

  (* 2. read-your-writes: inside the transaction, reads see the pending writes (via the session) *)
  let c = fresh "p2" in
  Tx.run (fun () -> ignore (D.insert c (person "ada")); check "mongo: read-your-writes" (count c = 1));

  (* 3. rollback on raise: inserts vanish, pre-transaction data survives *)
  let c = fresh "p3" in
  ignore (D.insert c (person "seed"));
  (try Tx.run (fun () -> ignore (D.insert c (person "ada")); ignore (D.insert c (person "bob")); raise Boom) with Boom -> ());
  check "mongo: rollback reverts inserts (pre-tx data kept)" (names c = [ "seed" ]);

  (* 4. update rollback: a modified document returns to its pre-image *)
  let c = fresh "p4" in
  ignore (D.insert c (person "ada"));
  (try
     Tx.run (fun () ->
         ignore (D.update c ~multi:true ~upsert:false (person "ada") (B.doc [ ("$set", B.doc [ ("name", B.str "eve") ]) ]));
         check "mongo: update visible in-tx" (names c = [ "eve" ]);
         raise Boom)
   with Boom -> ());
  check "mongo: rollback reverts update" (names c = [ "ada" ]);

  (* 5. remove rollback: a removed document comes back *)
  let c = fresh "p5" in
  ignore (D.insert c (person "ada"));
  (try
     Tx.run (fun () ->
         ignore (D.remove c (person "ada"));
         check "mongo: remove visible in-tx" (count c = 0);
         raise Boom)
   with Boom -> ());
  check "mongo: rollback reverts remove" (names c = [ "ada" ]);

  (* 6. aggregate read-your-writes: inside the transaction, an aggregate sees the pending inserts too *)
  let c = fresh "p6" in
  Tx.run (fun () ->
      ignore (D.insert c (person "ada"));
      ignore (D.insert c (person "bob"));
      let agg = D.aggregate c [ B.doc [ ("$count", B.str "n") ] ] in
      check "mongo: aggregate read-your-writes (sees pending inserts)"
        (match agg with [ d ] -> B.get_int d "n" = Some 2 | _ -> false));
  check "mongo: aggregate's writes committed" (count c = 2);

  Printf.printf "all mongo transaction tests passed\n%!"

let () =
  match Rs.start () with
  | Error msg -> Printf.printf "  SKIP: no replica-set mongod available (%s)\n%!" msg
  | Ok rs -> Fun.protect ~finally:(fun () -> Rs.stop rs) (fun () -> run rs)
