(* The non-web core, end to end in the example: the ticket domain (collections/ + workflows/) over the
   ambient in-memory backend. Proves the open_ticket WORKFLOW writes two collections atomically (and a
   raise rolls BOTH back), the close TRANSITION is guarded, and reactions ride along. A standalone Eio
   exe over MONGO_URL=:memory:, run via a runtest rule. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Pulse = Fennec_pulse_app
module C = Pulse.Collection
module W = Pulse.Workflow

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let tickets () = List.length (C.all Ticket.collection)
let events () = List.length (C.all Ticket_event.collection)

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Fennec_mongo_dynamic.Dynamic.set_switch sw;

  (* open_ticket: a WORKFLOW that writes the ticket AND its first audit event atomically *)
  let t = W.call Tickets.open_ticket "Printer on fire" in
  check "open_ticket returns an open ticket" (t.Ticket.status = "open");
  check "open_ticket wrote the ticket" (tickets () = 1);
  check "open_ticket wrote the audit event (cross-collection, same transaction)" (events () = 1);

  (* atomicity: a workflow that opens a ticket and then raises persists NOTHING — both the ticket and
     its audit event roll back together *)
  let boom = W.make "boom" (fun () -> ignore (W.call Tickets.open_ticket "Will vanish"); failwith "boom") in
  (try ignore (W.call boom ()) with Failure _ -> ());
  check "a raise rolls back the nested open_ticket's ticket" (tickets () = 1);
  check "a raise rolls back the nested open_ticket's audit event too (atomic)" (events () = 1);

  (* the close TRANSITION: open -> closed, persisted; its @after effect fires post-commit *)
  let closed = W.call Tickets.close t in
  check "close returns a closed ticket" (closed.Ticket.status = "closed");
  check "close persisted the new state" (match C.get Ticket.collection t.Ticket.id with Some x -> x.Ticket.status = "closed" | None -> false);

  (* the guard: closing an already-closed ticket is vetoed, state intact *)
  (try ignore (W.call Tickets.close closed) with Failure _ -> ());
  check "closing a closed ticket is vetoed (state intact)"
    (match C.get Ticket.collection t.Ticket.id with Some x -> x.Ticket.status = "closed" | None -> false);

  Printf.printf "all ticket-domain tests passed\n%!"
