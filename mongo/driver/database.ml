(* A database-scoped handle: the everyday entry point. You connect a [Client.t] once, name a
   database, and from there reach collections without repeating the db name on every call. This is
   the seam where "just works" lives — a local managed mongod and a remote authenticated cluster look
   identical from here, because the only thing that differs is the URI handed to [Client.connect].

   Kept deliberately thin: a db is just a client plus a name. No cycle with [Client] (Client knows
   nothing about Database), so collections are built via [Collection.create]. *)

type t = { client : Client.t; name : string }

let create client name = { client; name }
let collection t name = Collection.create t.client ~db:t.name ~name
let command t cmd = Client.command t.client ~db:t.name cmd

(* Drop the whole database — the heaviest reset. For a per-collection clean slate prefer
   [Collection.clear]/[Collection.drop]. *)
let drop t = Client.drop_database t.client ~db:t.name
