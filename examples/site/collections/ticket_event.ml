(* An audit-log entry for a ticket — a second collection, so open_ticket can demonstrate a
   cross-collection ATOMIC write (the ticket and its first event commit together or not at all). *)

type t = {
  id : string;
  ticket_id : string;
  action : string;
  at : string;
}
[@@deriving model]

let collection = Def.v ~indexes:Index.[ asc Fields.ticket_id ] "ticket_events" codec
