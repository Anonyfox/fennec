(* The ambient client facade over the one page connection (Ddp_client.default ()). Lives above the
   virtual ddp_client boundary, so it's identical on the browser and SSR builds. *)
let connect ?path ?persist ?chrome () = ignore (Ddp_client.connect ?path ?persist ?chrome ())
let subscribe ~name ?params () = Ddp_client.use_subscribe (Ddp_client.default ()) ~name ?params ()
let call m a = Ddp_client.call_m (Ddp_client.default ()) m a
let status () = Ddp_client.status (Ddp_client.default ())
let pending_writes () = Ddp_client.pending_writes (Ddp_client.default ())

(* the per-model client view, branded (Meteor's collection object): module Tasks = Pulse.Collection (Task) *)
module Collection = Ddp_client.Collection
