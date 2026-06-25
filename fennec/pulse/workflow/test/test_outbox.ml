(* The effects outbox: tx-aware enqueue (atomic with a workflow — a rolled-back workflow sends nothing)
   and the worker's exactly-once delivery (claim + delete) with retry when a handler fails. Over the
   in-memory backend under an Eio switch. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module D = Fennec_mongo_dynamic.Dynamic
module Tx = Fennec_mongo_dynamic.Tx
module Outbox = Fennec_pulse_workflow.Outbox
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let tag p = B.get_string p "tag"
let payload t = B.doc [ ("tag", B.str t) ]

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  D.set_switch sw;
  Outbox.reset ();

  let delivered = ref [] in
  Outbox.register ~kind:"test" (fun ~id:_ p -> match tag p with Some t -> delivered := t :: !delivered | None -> ());

  let coll = D.collection ~name:Outbox.coll_name () in
  let tick () = Outbox.tick coll ~now:1000 in

  (* 1. a committed workflow's effect delivers exactly once *)
  Tx.run (fun () -> Outbox.enqueue ~kind:"test" ~payload:(payload "a"));
  tick ();
  check "committed effect delivered" (!delivered = [ "a" ]);
  tick ();
  check "exactly-once: a second tick does not re-deliver" (!delivered = [ "a" ]);

  (* 2. a ROLLED-BACK workflow's effect is never delivered (enqueue joined the transaction) *)
  delivered := [];
  (try Tx.run (fun () -> Outbox.enqueue ~kind:"test" ~payload:(payload "ghost"); failwith "boom") with _ -> ());
  tick ();
  check "rolled-back effect never delivered (enqueue is atomic with the tx)" (!delivered = []);

  (* 3. retry: a failing handler leaves the intent pending; it delivers once the handler recovers *)
  delivered := [];
  let fail = ref true in
  Outbox.register ~kind:"flaky" (fun ~id:_ p ->
      if !fail then failwith "transient" else match tag p with Some t -> delivered := t :: !delivered | None -> ());
  Outbox.enqueue ~kind:"flaky" ~payload:(payload "r") (* outside a tx: a direct durable insert *);
  tick () (* handler fails -> reverts to pending, not lost *);
  check "a failed delivery is not lost and not delivered" (!delivered = []);
  fail := false;
  tick () (* handler recovers -> delivered *);
  check "retry delivers once the handler recovers" (!delivered = [ "r" ]);
  tick ();
  check "exactly-once after retry (no duplicate)" (!delivered = [ "r" ]);

  Printf.printf "all outbox tests passed\n%!"
