(* The tickets write API — workflows, transitions, and reactions over the Ticket / Ticket_event
   collections (peer of web/ and collections/). Every change goes through a named function here, so the
   data can only change in explicit code and @after catches it. This is "nice imperative OCaml top to
   bottom": ordinary functions over ambient data, control flow by raise, no db threaded anywhere. *)

module Pulse = Fennec_pulse_app
module C = Pulse.Collection
module W = Pulse.Workflow

(* illustrative timestamp — a real app would format the clock *)
let stamp () = "2026-06-25"

(* A cross-collection WORKFLOW: create the ticket AND its first audit event in ONE transaction. If
   either step raises (e.g. an invalid subject), NEITHER is written — the transparent transaction rolls
   back. Note there is no `db` anywhere: data just exists (ambient). *)
let open_ticket =
  W.make "open_ticket" (fun subject ->
      let t =
        W.call (C.create Ticket.collection) { Ticket.id = ""; subject; status = "open"; opened_at = stamp () }
      in
      ignore
        (W.call (C.create Ticket_event.collection)
           { Ticket_event.id = ""; ticket_id = t.Ticket.id; action = "opened"; at = stamp () });
      t)

(* The guarded TRANSITION open -> closed. The pure RULE is a plain function (so it unit-tests with no
   backend); [close] wraps it as the persisted transition — calling it validates, persists by _id, and
   fires the @after below, all in the transaction. Control flow is just [raise]. *)
let close_rule (t : Ticket.t) =
  if t.Ticket.status <> "open" then failwith "ticket is not open";
  { t with Ticket.status = "closed" }

let close = C.transition Ticket.collection "close" close_rule

(* A post-commit EFFECT reaction: when a ticket closes, notify the reporter (illustrative — here a log
   line; a real app would send mail). It runs AFTER the close commits, isolated, so a failure here can
   never roll back the close. *)
let[@after close] notify_closed (t : Ticket.t) =
  Printf.printf "[tickets] ticket %s closed — notifying the reporter\n%!" t.Ticket.id

(* A SCHEDULED job: hourly housekeeping that closes anything left open. Runs at-most-once across
   replicas (the unique-insert claim), and the [unit -> unit] shape is forced by [@cron]. *)
let[@cron "0 * * * *"] auto_close_stale () =
  C.find Ticket.collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "open") ]) ]
  |> List.iter (fun t -> try ignore (W.call close t) with _ -> ())

(* ---- inline tests of the pure transition rule (no backend needed) ----------------------------- *)

let%test "close: an open ticket becomes closed" =
  (close_rule { Ticket.id = "1"; subject = "x"; status = "open"; opened_at = "" }).Ticket.status = "closed"

let%test "close: refuses a ticket that is not open" =
  try
    ignore (close_rule { Ticket.id = "1"; subject = "x"; status = "closed"; opened_at = "" });
    false
  with Failure _ -> true
