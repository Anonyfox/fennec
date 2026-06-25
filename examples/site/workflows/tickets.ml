(* The tickets write API — workflows over the Ticket / Ticket_event collections (a peer of web/ and
   collections/). Ordinary functions tagged [@workflow]; the data changes through the collection's OWN
   methods (`Ticket.create` / `save` / `where`, Meteor-style), not free-floating verbs. `open Ticket`
   brings its fields + methods into scope, exactly like a component does — so the body reads like plain
   imperative OCaml with the transaction invisible. Control flow is just `raise`. *)

open Ticket

(* illustrative timestamp — a real app would format the clock *)
let stamp () = "2026-06-25"

(* Open a ticket AND record its first audit event — in ONE transaction. If either step raises (e.g. an
   invalid subject), NEITHER is written: the transparent transaction rolls back. *)
let[@workflow] open_ticket subject =
  let t = create { id = ""; subject; status = "open"; opened_at = stamp () } in
  ignore (Ticket_event.create { Ticket_event.id = ""; ticket_id = t.id; action = "opened"; at = stamp () });
  t

(* The guarded transition open -> closed: read, change, save. A `raise` vetoes the whole call. *)
let[@workflow] close (t : t) =
  if t.status <> "open" then failwith "ticket is not open";
  save { t with status = "closed" }

(* A post-commit EFFECT: when a ticket closes, notify the reporter (illustrative — a log line here; a
   real app would send mail). Runs AFTER the close commits, isolated. *)
let[@after close] notify_closed (t : t) =
  Printf.printf "[tickets] ticket %s closed — notifying the reporter\n%!" t.id

(* A SCHEDULED job: hourly housekeeping that closes anything left open. The query is the typed Meteor
   syntax ([%q] over the model's Fields); the [unit -> unit] shape is forced by [@cron]. *)
let[@cron "0 * * * *"] auto_close_stale () =
  where [%q status = "open"] |> List.iter (fun t -> try ignore (close t) with _ -> ())

(* an inline test of the guard (it raises before touching the backend, so it needs no database) *)
let%test "close refuses a ticket that is not open" =
  try ignore (close { id = "1"; subject = "x"; status = "closed"; opened_at = "" }); false
  with Failure _ -> true
