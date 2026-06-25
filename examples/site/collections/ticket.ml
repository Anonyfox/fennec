(* A support ticket — data ground truth, one concept per file. Its only writes are the named workflows
   in workflows/tickets.ml (open_ticket / close); userland never sets fields directly, so every change
   goes through an explicit function and @after catches it.

   The validation catalog lives inline as attributes; [@@deriving collection] turns it into the codec,
   the typed Fields handles, and the collection (+ its $jsonSchema validator), and the codec IS the
   validation (an invalid ticket cannot be written). Every collection is declared the same way — there
   is no "server-only" variant — so a ticket can become client-relevant any time at no extra cost. *)

type t = {
  id : string;
  subject : string; [@trim] [@non_empty] [@max_len 120]
  status : string; [@one_of [ "open"; "closed" ]]
  opened_at : string;
}
[@@deriving collection ~name:"tickets"]

(* declared indexes, co-located, reconciled at boot *)
let () = [%index status]
