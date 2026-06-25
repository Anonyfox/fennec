(* The Mail effects-outbox bridge MECHANISM: a [Mail.send] inside a workflow body (a transaction is open)
   or a reaction ([@after], [Workflow.in_effect]) is DEFERRED to the durable outbox and delivered by the
   worker; a direct send is inline; a rolled-back workflow's send is never delivered. This proves the
   seams compose (Mail.set_deferred_send + Tx.current / Workflow.in_effect + Outbox.enqueue/tick) — the
   concrete Mail<->Bson payload lives in Fennec's Mail_outbox (compile-checked, exercised at boot). Over
   :memory: under an Eio switch. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Mail = Fennec_mail
module Outbox = Fennec_pulse_workflow.Outbox
module Workflow = Fennec_pulse_workflow.Workflow
module Tx = Fennec_mongo_dynamic.Tx
module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let mk subj = Mail.make ~from:(Mail.Address.v "a@x.com") ~to_:[ Mail.Address.v "b@x.com" ] ~subject:subj ~text:"hi" ()

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  D.set_switch sw;
  Outbox.reset ();

  (* the bridge wiring, exactly as Fennec.serve installs it (here with a capturing transport as the
     deliverer and a subject-only payload — enough to prove the mechanism) *)
  let transport, captured = Mail.capture () in
  Mail.set_transport transport (* the ambient transport (direct sends) AND the outbox deliverer below *);
  Outbox.register ~kind:"mail" (fun ~id:_ payload ->
      match transport (mk (Option.value ~default:"" (B.get_string payload "subject"))) with Ok () -> () | Error _ -> failwith "x");
  Mail.set_deferred_send (fun m ->
      if Tx.current () <> None || Workflow.in_effect () then (
        Outbox.enqueue ~kind:"mail" ~payload:(B.doc [ ("subject", B.str m.Mail.subject) ]);
        true)
      else false);
  let coll = D.collection ~name:Outbox.coll_name () in
  let sent () = List.length (captured ()) in

  (* 1. a direct send (no workflow context) is inline — delivered immediately *)
  ignore (Mail.send (mk "direct"));
  check "a direct send is inline" (sent () = 1);

  (* 2. a send in a workflow BODY (tx open) defers — NOT inline; the worker then delivers it *)
  Tx.run (fun () -> ignore (Mail.send (mk "in-body")));
  check "a workflow-body send did not go inline (deferred)" (sent () = 1);
  Outbox.tick coll ~now:1000;
  check "the worker delivered the deferred body send" (sent () = 2);

  (* 3. a send in a REACTION ([@after], in_effect) defers + delivers *)
  ignore (Workflow.exec ~name:"w" ~befores:[] ~afters:[ (fun () -> ignore (Mail.send (mk "in-after"))) ] () (fun () -> ()));
  check "a reaction send did not go inline (deferred)" (sent () = 2);
  Outbox.tick coll ~now:1000;
  check "the worker delivered the reaction send" (sent () = 3);

  (* 4. a ROLLED-BACK workflow's send is never delivered (the enqueue joined the transaction) *)
  (try Tx.run (fun () -> ignore (Mail.send (mk "ghost")); failwith "boom") with _ -> ());
  Outbox.tick coll ~now:1000;
  check "a rolled-back workflow send is never delivered" (sent () = 3);

  Printf.printf "all mail-outbox bridge tests passed\n%!"
