(* A support ticket — data ground truth, one concept per file. Its only writes are the named
   transitions in workflows/tickets.ml (open_ticket / close); userland never sets fields directly, so
   every change goes through an explicit function and @after catches it. Server-only: the SSR binary
   links this; clients see tickets through a publication, not by linking the model.

   The validation catalog lives inline as attributes — [@@deriving model] turns it into the codec +
   the typed Fields handles, and the codec is the validation (an invalid ticket cannot be written). *)

type t = {
  id : string;
  subject : string; [@trim] [@non_empty] [@max_len 120]
  status : string; [@one_of [ "open"; "closed" ]]
  opened_at : string;
}
[@@deriving model]

(* the collection (name + indexes), reconciled at boot. [Def.v] instead of [@@deriving collection]
   because this model is server-only — it needs no client reactive cursor. *)
let collection = Def.v ~indexes:Index.[ asc Fields.status ] "tickets" codec
