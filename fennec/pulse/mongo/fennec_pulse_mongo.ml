(* Native MongoDB backend — a {!Fennec_pulse.Backend.S} over the ported driver layer
   ({!Fennec_mongo_driver}: Client/Collection/Live), so the reactive/DDP/realtime stack runs over a
   real mongod with no other change (it is the same seam Minimongo implements in memory).

   Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
   calling fiber, not the scheduler. [observe_changes] is REAL CHANGE STREAMS via {!Live} (one
   stream per collection, fanned out to per-query views) — not polling — which is why it needs a
   replica set (the managed mongod is launched as one by {!Fennec_mongo_driver.Server}). *)

module Driver = Fennec_mongo_driver
module Client = Driver.Client
module Coll = Driver.Collection
module Live = Driver.Live
module Runtime = Driver.Runtime
module Ffi = Fennec_mongo_ffi.Mongo_ffi
module Diff = Query.Diff
module Id = Query.Id
module Backend = Fennec_pulse.Backend
module B = Bson

let available = Ffi.available

type connection = Client.t

let connect uri = Client.connect ~uri ()

(* A native collection IS the driver collection. The ambient Eio switch that {!Live}'s change-stream
   daemons fork into is set when the collection is created. *)
type collection = Coll.t

let collection ?poll ~sw conn ~db ~name : collection =
  Live.set_switch sw;
  (match poll with Some p -> Live.set_poll_interval p | None -> ());
  Coll.create conn ~db ~name

let int_field reply k =
  match B.get reply k with Some v -> ( match B.as_float v with Some f -> int_of_float f | None -> 0) | None -> 0

let native_opts (q : Backend.query) =
  B.Document
    (List.filter_map Fun.id
       [ (match q.Backend.sort with B.Document [] -> None | s -> Some ("sort", s));
         (if q.Backend.skip > 0 then Some ("skip", B.int q.Backend.skip) else None);
         (if q.Backend.limit > 0 then Some ("limit", B.int q.Backend.limit) else None);
         (match q.Backend.fields with B.Document [] -> None | f -> Some ("projection", f)) ])

(* ---- Backend.S ---------------------------------------------------------- *)

let insert c (d : B.t) =
  (* the reactive layer mints an [_id] before calling here; mint one if a direct caller didn't, so
     the returned id always matches the stored document *)
  let kvs = Diff.kvs_of d in
  let id, doc =
    match List.assoc_opt "_id" kvs with
    | Some v -> (Diff.id_to_string v, d)
    | None -> let id = Id.random_id () in (id, B.Document (("_id", B.String id) :: kvs))
  in
  ignore (Coll.insert_one c doc);
  id

let update c ~multi ~upsert sel m =
  let reply =
    Client.command c.Coll.client ~db:c.Coll.db
      (B.doc
         [ ("update", B.str c.Coll.name);
           ("updates", B.array [ B.doc [ ("q", sel); ("u", m); ("multi", B.bool multi); ("upsert", B.bool upsert) ] ]) ])
  in
  int_field reply "n"

let remove c sel = int_field (Coll.delete_many c ~filter:sel) "n"
let find c (q : Backend.query) = Coll.find c ~filter:q.Backend.selector ~opts:(native_opts q) ()
let find_one c (q : Backend.query) = match find c { q with Backend.limit = 1 } with x :: _ -> Some x | [] -> None
let count c sel = Coll.count c ~filter:sel ()
let aggregate c ?lookup (pipeline : B.t list) =
  ignore lookup (* a real mongod resolves $lookup itself; the in-memory resolver is not needed here *);
  Coll.aggregate c ~pipeline:(B.Array pipeline) ()
let distinct c key sel = Coll.distinct c ~key ~filter:sel ()

(* BEST-EFFORT fence on the native driver: mongod's change-stream delivery is asynchronous (network)
   and v1 carries no resume-token plumbing, so the fence runs immediately — a method's [updated] may
   precede its stream deltas under lag (an optimistic client then briefly shows the pre-method state;
   Meteor fences via oplog positions — the resume-token equivalent is the marked seam here). *)
let fence _c k = k ()

(* index ops on the native driver — name-explicit so reconcile matches; list returns names *)
let ensure_index c ~name ~keys ~unique =
  let opts = if unique then [ ("unique", B.Bool true) ] else [] in
  ignore (Coll.create_index c ~keys ~opts ~name ())
let drop_index c ~name = Coll.drop_index c ~name
let index_names c =
  List.filter_map (fun d -> match B.get d "name" with Some (B.String s) -> Some s | _ -> None) (Coll.list_indexes c)

(* real change streams: Live keeps ONE stream per collection, replays the initial set synchronously
   (ready-after-data), then routes per-query field-level deltas *)
let observe_changes c (q : Backend.query) ~added ~changed ~removed : Backend.handle =
  let h =
    Live.observe_changes
      (Live.query c ~selector:q.Backend.selector ~sort:q.Backend.sort ~skip:q.Backend.skip ~limit:q.Backend.limit
         ~fields:q.Backend.fields)
      ~added ~changed ~removed ()
  in
  { Backend.stop = h.Live.stop }

(* A runtime-selectable backend: in-memory OR this native driver, behind ONE Backend.S, so an app
   picks at boot (real mongo if configured, else :memory:) with no type change downstream. The
   per-op dispatch references the outer (native) ops — non-recursive [let], so no shadowing. *)
module Dynamic = struct
  module Mini = Backend.Mini

  type collection = Mem of Minimongo.t | Native of Coll.t | Missing of string

  let mem m = Mem m
  let real ?poll ~sw conn ~db ~name = Native (collection ?poll ~sw conn ~db ~name)
  let missing message = Missing message
  let unavailable message = failwith ("Fennec.Pulse.Mongo: " ^ message)

  (* The convention the fennec CLI speaks: MONGO_URL is the one database location. `fennec dev`
     auto-starts/adopts a local mongod when possible; `fennec test` sets :memory: by default and
     `fennec test --mongo` supplies a per-suite real URL. Missing MONGO_URL is not a hidden memory
     fallback; operations fail clearly. Build it in [Fennec.serve ~on_start] so the [sw] it captures
     drives Live's change-stream daemons. *)
  let mongo_url_env = Runtime.mongo_url_env

  let from_env ?poll ~sw ~db ~name () =
    match Runtime.state () with
    | Runtime.Missing -> missing (Runtime.unavailable_message ())
    | Runtime.Memory -> mem (Minimongo.create ())
    | Runtime.Mongo { uri; db = _ } -> real ?poll ~sw (connect uri) ~db ~name

  let insert c d = match c with Mem m -> Mini.insert m d | Native r -> insert r d | Missing message -> unavailable message
  let update c ~multi ~upsert s m =
    match c with Mem mm -> Mini.update mm ~multi ~upsert s m | Native r -> update r ~multi ~upsert s m | Missing message -> unavailable message
  let remove c s = match c with Mem m -> Mini.remove m s | Native r -> remove r s | Missing message -> unavailable message
  let find c q = match c with Mem m -> Mini.find m q | Native r -> find r q | Missing message -> unavailable message
  let find_one c q = match c with Mem m -> Mini.find_one m q | Native r -> find_one r q | Missing message -> unavailable message
  let count c s = match c with Mem m -> Mini.count m s | Native r -> count r s | Missing message -> unavailable message
  let aggregate c ?(lookup = fun _ -> []) p =
    match c with Mem m -> Mini.aggregate m ~lookup p | Native r -> aggregate r ~lookup p | Missing message -> unavailable message
  let distinct c k s = match c with Mem m -> Mini.distinct m k s | Native r -> distinct r k s | Missing message -> unavailable message

  let observe_changes c q ~added ~changed ~removed =
    match c with
    | Mem m -> Mini.observe_changes m q ~added ~changed ~removed
    | Native r -> observe_changes r q ~added ~changed ~removed
    | Missing message -> unavailable message

  let fence c k = match c with Mem m -> Mini.fence m k | Native r -> fence r k | Missing message -> unavailable message
  let ensure_index c ~name ~keys ~unique =
    match c with Mem m -> Mini.ensure_index m ~name ~keys ~unique | Native r -> ensure_index r ~name ~keys ~unique | Missing message -> unavailable message
  let drop_index c ~name =
    match c with Mem m -> Mini.drop_index m ~name | Native r -> drop_index r ~name | Missing message -> unavailable message
  let index_names c =
    match c with Mem m -> Mini.index_names m | Native r -> index_names r | Missing message -> unavailable message
end
