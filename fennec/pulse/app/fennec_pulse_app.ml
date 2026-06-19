(* The server data facade. Wraps the Reactive/server/Typed functors over the production Dynamic
   backend (mem-or-mongo, chosen by the global Mongo env) into ONE ambient module. A server file
   does: declare collections' publications/methods → done. No functor aliases, no
   per-collection backend threading, no double-declared SSR publication. *)

module D = Fennec_mongo_dynamic.Dynamic
module R = Fennec_pulse.Reactive.Make (D)
module RT = Fennec_pulse_server.Make (R)
module T = Fennec_pulse.Typed.Make (R)

(* No app-level lifecycle: [Fennec.serve] installs the ambient Eio switch and (for a burrow:// authority)
   opens the mongosh wire endpoint at boot — all from MONGO_URL. Collections resolve via [D.collection]. *)

(* one reactive collection per name (stable mux uid; indexes reconciled once on creation), re-wrapped
   per call as a cheap typed handle *)
let _reactives : (string, R.Collection.t) Hashtbl.t = Hashtbl.create 16

let collection (def : 'a Def.t) : 'a T.t =
  let name = Def.name def in
  let coll =
    match Hashtbl.find_opt _reactives name with
    | Some c -> c
    | None ->
        let c = R.Collection.create ~name (D.collection ~name ()) in
        Hashtbl.replace _reactives name c;
        T.reconcile c (Def.all_indexes def);
        T.install_validator c (Def.validator def);
        c
  in
  T.of_reactive def coll

(* server writes (used inside method handlers): validating, ambient *)
let insert def v = T.insert (collection def) v
let seed def vs = List.iter (fun v -> ignore (insert def v)) vs
let update def ?multi ~where m = T.update (collection def) ?multi ~where m
let upsert def ?multi ~where m = T.upsert (collection def) ?multi ~where m
let remove def ~where = T.remove (collection def) ~where

(* ONE publish call: the live DDP publication AND the SSR seed, both keyed by the collection's name.
   [~where] (params → typed clauses) filters; default publishes the whole collection. *)
let publish ?(where = fun _ -> []) (def : 'a Def.t) =
  let name = Def.name def in
  let h = collection def in
  R.publish name (fun (pub : R.publication) -> R.Cursor (T.cursor h ~where:(where pub.params) ()));
  Ddp_client.publish ~name (fun params ->
      let selector = Filter.to_bson (Filter.all (where params)) in
      [ (name, R.Collection.fetch (R.Collection.find ~selector (T.collection h) ())) ])

(* register a typed method handler (the one client write path) *)
let method_ m handler = R.handle m handler

(* ---- the always-on current-user publication -------------------------------------------------

   [__currentUser] keeps the client's [Accounts.user]/[user_id] signals live over the user DOCUMENT
   — profile / roles / emails / status changes, not merely the auth transition. It is the fennec
   equivalent of Meteor's "null publication" that the accounts package auto-creates: transparent,
   zero userland wiring. Registering it here (module init of the app facade) means any server that
   links {!serve_ddp} installs it for free; the DDP session's [registries ()] rebuild reads
   [R.publications ()] per connection, so it is present on every socket.

   Scope is [pub.user_id] — the cookie-seeded session user the websocket paw threads in. [None]
   (anonymous) yields a never-matching cursor (empty). [Some uid] yields a one-document cursor over
   the SHARED accounts users collection (the SAME name + handle the Accounts store opens, so a
   server-side write through Accounts is visible here — STEP 1 made that true on [:memory:] too).

   A [fields] inclusion projection whitelists EXACTLY the safe public set (the field list of
   {!Accounts_session.public_user_to_doc}: username/emails/roles/status/createdAt/updatedAt/profile,
   [_id] implicit). [services] and [passwordHash] are NEVER listed — minimongo applies the projection
   on the live deltas before the observe callbacks fire (the backend seam), so no secret ever enters
   the beat / merge pipeline even in memory. Belt and suspenders. *)
let current_user_publication = "__currentUser"

let accounts_users_collection = "accounts_users"

(* the safe public projection — an inclusion spec; [_id] rides along by Mongo convention *)
let current_user_fields =
  Bson.doc
    [ ("username", Bson.int 1);
      ("emails", Bson.int 1);
      ("roles", Bson.int 1);
      ("status", Bson.int 1);
      ("createdAt", Bson.int 1);
      ("updatedAt", Bson.int 1);
      ("profile", Bson.int 1) ]

(* a selector that matches no document — the anonymous (user_id = None) case *)
let never_match = Bson.doc [ ("_id", Bson.doc [ ("$in", Bson.array []) ]) ]

(* the reactive handle over the shared accounts users collection, built once *)
let users_reactive : R.Collection.t Lazy.t =
  lazy (R.Collection.create ~name:accounts_users_collection (D.collection ~name:accounts_users_collection ()))

let () =
  R.publish current_user_publication (fun (pub : R.publication) ->
      let selector =
        match pub.user_id with Some uid -> Bson.doc [ ("_id", Bson.str uid) ] | None -> never_match
      in
      R.Cursor (R.cursor (Lazy.force users_reactive) ~selector ~fields:current_user_fields ()))

(* the DDP websocket paw for the endpoint pipeline *)
let serve_ddp ?path () = RT.paw ?path ()
