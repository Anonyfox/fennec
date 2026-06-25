(* The non-web core, end to end in the example: the ticket domain (collections/ + workflows/) over the
   ambient in-memory backend. The workflows are called like ORDINARY functions; reads/writes are the
   collection's own methods (Ticket.all, etc.). Proves open_ticket writes two collections atomically
   (and a raise rolls BOTH back), close is a guarded transition, and the @after effect rides along. A
   standalone Eio exe over MONGO_URL=:memory:, run via a runtest rule. *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Pulse = Fennec_pulse_app (* referenced (transaction) so the facade's module-init installs the write seam *)

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let tickets () = List.length (Ticket.all ())
let events () = List.length (Ticket_event.all ())
let status_of id = match List.find_opt (fun (x : Ticket.t) -> x.Ticket.id = id) (Ticket.all ()) with Some x -> Some x.Ticket.status | None -> None

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Fennec_mongo_dynamic.Dynamic.set_switch sw;

  (* open_ticket — a plain call. It writes the ticket AND its first audit event atomically. *)
  let t = Tickets.open_ticket "Printer on fire" in
  check "open_ticket returns an open ticket" (t.Ticket.status = "open");
  check "open_ticket wrote the ticket (Ticket.all)" (tickets () = 1);
  check "open_ticket wrote the audit event (cross-collection, same transaction)" (events () = 1);

  (* atomicity: open a ticket inside a transaction that then raises — the nested open_ticket JOINS it,
     so both the ticket and its audit event roll back together (nothing persists) *)
  (try Pulse.transaction (fun () -> ignore (Tickets.open_ticket "Will vanish"); failwith "boom") with Failure _ -> ());
  check "a raise rolls back the nested open_ticket's ticket" (tickets () = 1);
  check "a raise rolls back the nested open_ticket's audit event too (atomic)" (events () = 1);

  (* close — a plain call. Transitions open -> closed, persists, and its @after effect fires post-commit. *)
  let closed = Tickets.close t in
  check "close returns a closed ticket" (closed.Ticket.status = "closed");
  check "close persisted the new state" (status_of t.Ticket.id = Some "closed");

  (* the guard: closing an already-closed ticket is vetoed, state intact *)
  (try ignore (Tickets.close closed) with Failure _ -> ());
  check "closing a closed ticket is vetoed (state intact)" (status_of t.Ticket.id = Some "closed");

  Printf.printf "all ticket-domain tests passed\n%!"
