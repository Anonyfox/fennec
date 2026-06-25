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

(* Seed-if-empty: insert the bootstrap documents only when the collection currently has none.
   Idempotent across restarts, which is what makes the two database lifecycles both behave correctly
   from the SAME [on_start] call:
   - a DURABLE dev DB (burrow) is seeded ONCE — a later `fennec dev` reboot sees the persisted docs
     (count > 0) and skips, so your edits survive instead of being buried under duplicate seed rows;
   - a FRESH `:memory:` / temp e2e DB is always empty at boot, so every test run re-seeds it for a
     clean, deterministic slate.
   [T.count] reads the backing store synchronously server-side (the server IS the source of truth, not
   an async mirror), so it reflects persisted documents. *)
let seed def vs = if T.count (collection def) () = 0 then List.iter (fun v -> ignore (insert def v)) vs
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

(* ---- the non-web core, re-exported so a server / collections / workflows file opens ONE module ----
   [transaction] runs a block in the transparent transaction (commit on return, roll back on raise);
   [Workflow]/[Schedule] are the reactions runtime the ppx targets. *)
let transaction f = Fennec_mongo_dynamic.Tx.run f

module Workflow = Fennec_pulse_workflow.Workflow
module Schedule = Fennec_pulse_workflow.Schedule

(* ---- the data verbs — ambient, validating, plain functions ---------------------------------------
   create / save / delete are the write API: ordinary functions you call inside a [@workflow] (so they
   run in that workflow's transparent transaction — a raise anywhere rolls them all back). A "transition"
   is just a [@workflow] function that reads a value, changes it, and [save]s it; there is no special
   transition type. Reads (get/all/find) are one-shot; live data is a {!publish}ed publication. *)
let _by_id id = [ Filter.raw (Bson.doc [ ("_id", Bson.str id) ]) ]

let _string_id (def : 'a Def.t) (v : 'a) : string =
  match Sift.encode_checked (Def.codec def) v with
  | Ok bson -> (
      match Bson.get bson "_id" with
      | Some (Bson.String s) -> s
      | _ -> invalid_arg (Def.name def ^ ": by-id ops require a string id field"))
  | Error _ -> invalid_arg (Def.name def ^ ": cannot address an invalid value by id")

(** [get def id] — the aggregate with [_id = id], or [None]. *)
let get (def : 'a Def.t) (id : string) : 'a option =
  match T.find (collection def) ~where:(_by_id id) ~limit:1 () with x :: _ -> Some x | [] -> None

(** All aggregates (a one-shot server read; live data is a publication). *)
let all (def : 'a Def.t) : 'a list = T.find (collection def) ()

(** [find def ~where] — aggregates matching [where]. *)
let find (def : 'a Def.t) ~where : 'a list = T.find (collection def) ~where ()

(** [create def v] — store a fresh aggregate (mints its [_id]) and return it. Validates; raises
    {!T.Invalid} on a bad value (rolling back the enclosing transaction). *)
let create (def : 'a Def.t) (v : 'a) : 'a =
  let id = insert def v in
  match get def id with Some stored -> stored | None -> v

(** [save def v] — persist a full aggregate by its [_id] (the transition mechanism: read, change,
    save) and return it. Validates; raises {!T.Invalid} on a bad value (rolling back the transaction). *)
let save (def : 'a Def.t) (v : 'a) : 'a =
  (match Sift.encode_checked (Def.codec def) v with
  | Error es -> raise (T.Invalid es)
  | Ok bson ->
      let id = match Bson.get bson "_id" with Some i -> i | None -> invalid_arg (Def.name def ^ ": no _id") in
      let fields =
        match bson with
        | Bson.Document kvs -> Bson.Document (List.filter (fun (k, _) -> k <> "_id") kvs)
        | x -> x
      in
      ignore
        (update def ~multi:false
           ~where:[ Filter.raw (Bson.doc [ ("_id", id) ]) ]
           (Update.raw (Bson.doc [ ("$set", fields) ]))));
  v

(** [delete def v] — remove the aggregate by its [_id]. *)
let delete (def : 'a Def.t) (v : 'a) : unit = ignore (remove def ~where:(_by_id (_string_id def v)))

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
