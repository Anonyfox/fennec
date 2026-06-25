(* The tickets write API — workflows over the Ticket / Ticket_event collections (a peer of web/ and
   collections/). These are ORDINARY functions: tag a business function [@workflow] and it runs in one
   transaction with its reactions; the data verbs (create / save / find) are plain calls; control flow
   is just `raise`. There is no `db` anywhere, no wrappers, no ceremony — flat imperative OCaml, top to
   bottom, with the machinery transparent. *)

open Fennec_pulse_app

(* illustrative timestamp — a real app would format the clock *)
let stamp () = "2026-06-25"

(* Open a ticket AND record its first audit event — in ONE transaction. If either step raises (e.g. an
   invalid subject), NEITHER is written: the transparent transaction rolls back. *)
let[@workflow] open_ticket subject =
  let t = create Ticket.collection { Ticket.id = ""; subject; status = "open"; opened_at = stamp () } in
  ignore (create Ticket_event.collection { Ticket_event.id = ""; ticket_id = t.Ticket.id; action = "opened"; at = stamp () });
  t

(* The guarded transition open -> closed: read, change, save. A `raise` vetoes the whole call. *)
let[@workflow] close (t : Ticket.t) =
  if t.Ticket.status <> "open" then failwith "ticket is not open";
  save Ticket.collection { t with Ticket.status = "closed" }

(* A post-commit EFFECT: when a ticket closes, notify the reporter (illustrative — a log line here; a
   real app would send mail). It runs AFTER the close commits, isolated, so a failure can't roll it back. *)
let[@after close] notify_closed (t : Ticket.t) =
  Printf.printf "[tickets] ticket %s closed — notifying the reporter\n%!" t.Ticket.id

(* A SCHEDULED job: hourly housekeeping that closes anything left open. Runs at-most-once across
   replicas; the [unit -> unit] shape is forced by [@cron]. *)
let[@cron "0 * * * *"] auto_close_stale () =
  find Ticket.collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "open") ]) ]
  |> List.iter (fun t -> try ignore (close t) with _ -> ())

(* an inline test of the guard (it raises before touching the backend, so it needs no database) *)
let%test "close refuses a ticket that is not open" =
  try ignore (close { Ticket.id = "1"; subject = "x"; status = "closed"; opened_at = "" }); false
  with Failure _ -> true
