(* Native MongoDB backend — a {!Fennec_mongo_backend.S} over the ported driver layer
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
module Backend = Fennec_mongo_backend (* the PURE storage seam lib (Backend.S + query + Mini) *)
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

(* The same Backend.S ops, but issued inside a Mongo TRANSACTION (a held client session): the Dynamic
   dispatch routes Native ops through these when a transaction is in flight, so a workflow's writes commit
   or roll back together, and every read (find / find_one / count / distinct / aggregate) sees its own
   pending writes — read-your-writes, complete. *)
let insert_s sess c (d : B.t) =
  let kvs = Diff.kvs_of d in
  let id, doc =
    match List.assoc_opt "_id" kvs with
    | Some v -> (Diff.id_to_string v, d)
    | None -> let id = Id.random_id () in (id, B.Document (("_id", B.String id) :: kvs))
  in
  ignore (Coll.insert_one_s sess c doc);
  id

let update_s sess (c : Coll.t) ~multi ~upsert sel m =
  int_field
    (Coll.command_s sess ~db:c.Coll.db
       (B.doc
          [ ("update", B.str c.Coll.name);
            ("updates", B.array [ B.doc [ ("q", sel); ("u", m); ("multi", B.bool multi); ("upsert", B.bool upsert) ] ]) ]))
    "n"

let remove_s sess (c : Coll.t) sel =
  int_field
    (Coll.command_s sess ~db:c.Coll.db
       (B.doc [ ("delete", B.str c.Coll.name); ("deletes", B.array [ B.doc [ ("q", sel); ("limit", B.int 0) ] ]) ]))
    "n"

let find_s sess c (q : Backend.query) = Coll.find_s sess c ~filter:q.Backend.selector ~opts:(native_opts q) ()
let find_one_s sess c (q : Backend.query) = match find_s sess c { q with Backend.limit = 1 } with x :: _ -> Some x | [] -> None
let count_s sess (c : Coll.t) sel = int_field (Coll.command_s sess ~db:c.Coll.db (B.doc [ ("count", B.str c.Coll.name); ("query", sel) ])) "n"

let distinct_s sess (c : Coll.t) key sel =
  match B.get (Coll.command_s sess ~db:c.Coll.db (B.doc [ ("distinct", B.str c.Coll.name); ("key", B.str key); ("query", sel) ])) "values" with
  | Some (B.Array xs) -> xs
  | _ -> []

let aggregate_s sess c ?lookup (pipeline : B.t list) =
  ignore lookup (* a real mongod resolves $lookup itself *);
  Coll.aggregate_s sess c ~pipeline:(B.Array pipeline) ()

(* BEST-EFFORT fence on the native driver: mongod's change-stream delivery is asynchronous (network)
   and v1 carries no resume-token plumbing, so the fence runs immediately — a method's [updated] may
   precede its stream deltas under lag (an optimistic client then briefly shows the pre-method state;
   Meteor fences via oplog positions — the resume-token equivalent is the marked seam here). *)
let fence _c k = k ()

(* index ops on the native driver — name-explicit so reconcile matches; list returns names *)
let ensure_index c ~name ~keys ~unique ~sparse =
  let opts =
    (if unique then [ ("unique", B.Bool true) ] else []) @ (if sparse then [ ("sparse", B.Bool true) ] else [])
  in
  ignore (Coll.create_index c ~keys ~opts ~name ())
let drop_index c ~name = Coll.drop_index c ~name
let index_names c =
  List.filter_map (fun d -> match B.get d "name" with Some (B.String s) -> Some s | _ -> None) (Coll.list_indexes c)

(* install the model's $jsonSchema so mongod itself rejects foreign writes that violate the shape:
   create-with-validator on a fresh collection, else collMod when it already exists. validationLevel
   "moderate" keeps legacy/invalid docs updatable (the evolution-tolerance stance in Schema). *)
let ensure_validator c = function
  | None -> ()
  | Some v ->
      let opts = [ ("validator", v); ("validationLevel", B.str "moderate") ] in
      ( try ignore (Client.command c.Coll.client ~db:c.Coll.db (B.doc (("create", B.str c.Coll.name) :: opts)))
        with _ -> ignore (Client.command c.Coll.client ~db:c.Coll.db (B.doc (("collMod", B.str c.Coll.name) :: opts))) )

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

(* ---- Embedded (Burrow) backend ------------------------------------------ *)

(* Burrow — the native, in-process, on-disk MongoDB-compatible engine (fennec-mongo.burrow). One
   engine per on-disk database directory, shared across that database's collections; selected by a
   burrow:// MONGO_URL (the path's trailing segment is the db). Durable by default (group-committed F_FULLFSYNC). *)
module Burrow_engine = Burrow.Engine

let embedded_engines : (string, Burrow_engine.t) Hashtbl.t = Hashtbl.create 8

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  end

let embedded_engine ~sw dir =
  match Hashtbl.find_opt embedded_engines dir with
  | Some e -> e
  | None ->
    mkdir_p dir;
    (* the writer fiber runs under [sw] — the server's long-lived switch, captured at first open *)
    let e = Burrow_engine.open_ ~sw dir in
    Hashtbl.replace embedded_engines dir e;
    e

type embedded = { eng : Burrow_engine.t; ecoll : Burrow_engine.collection }

let embedded_collection ~sw ~dir ~name =
  let eng = embedded_engine ~sw dir in
  { eng; ecoll = Burrow_engine.collection eng name }

let bw_find e (q : Backend.query) =
  Burrow_engine.find e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort ~skip:q.Backend.skip
    ~limit:q.Backend.limit ~fields:q.Backend.fields

let bw_find_one e (q : Backend.query) =
  Burrow_engine.find_one e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort
    ~skip:q.Backend.skip ~fields:q.Backend.fields

(* the same two reads routed through an open workflow txn (read-your-writes within the transaction) *)
let bw_find_in sess e (q : Backend.query) =
  Burrow_engine.find_in sess e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort ~skip:q.Backend.skip
    ~limit:q.Backend.limit ~fields:q.Backend.fields

let bw_find_one_in sess e (q : Backend.query) =
  Burrow_engine.find_one_in sess e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort
    ~skip:q.Backend.skip ~fields:q.Backend.fields

let bw_observe e (q : Backend.query) ~added ~changed ~removed =
  { Backend.stop =
      Burrow_engine.observe_changes e.eng e.ecoll ~selector:q.Backend.selector ~sort:q.Backend.sort
        ~skip:q.Backend.skip ~limit:q.Backend.limit ~fields:q.Backend.fields ~added ~changed ~removed }

(* ---- runtime-selectable backend ----------------------------------------- *)

(* A runtime-selectable backend: in-memory, this native driver, OR the embedded Burrow engine, behind
   ONE Backend.S, so an app picks at boot (real mongo / burrow:// / :memory:) with no type change
   downstream. The per-op dispatch references the outer (native / burrow) ops — non-recursive [let],
   so no shadowing. *)

(* ---- the transparent transaction context ----------------------------------
   A workflow (or any code wrapped in [Tx.run]) runs inside ONE transaction: writes commit when it
   returns and roll back when it raises, with read-your-writes throughout (writes apply eagerly to
   the live store). The context is AMBIENT — fiber-local via the same Eio-key + global-fallback idiom
   the seeded-id seam uses, so it works inside the server's fibers AND in plain unit tests (no
   scheduler). The dispatcher's in-memory write ops consult [Tx.current]: the first write to a
   collection snapshots it; [commit] drops the snapshots, [rollback] restores them.

   Atomicity + rollback are implemented for the in-memory ([:memory:]) backend — what `fennec test`
   and the dev seed exercise. On burrow/mongo the bracket is transparent and writes commit-on-success
   (durable as they happen); backend-native rollback (an LMDB parent-txn / a mongo session) is a
   scoped follow-on, so a workflow that RAISES on those backends may leave the writes it already made.
   Outermost transactions serialize under one Eio mutex (so two in-flight in-memory transactions
   cannot corrupt each other's snapshots); a nested [run] joins the enclosing transaction — the whole
   call tree is one atomic unit, matching "a workflow calling a workflow is still one transaction". *)
module Tx = struct
  type entry = { coll : Minimongo.t; snap : Minimongo.snapshot }

  type t = {
    mutable touched : entry list; (* in-memory collections snapshotted for rollback *)
    mutable on_commit : (unit -> unit) list; (* post-commit reactions/effects, fired after the OUTERMOST commit *)
    mutable burrow : (Burrow_engine.t * Burrow_engine.session) option;
        (* a burrow backend's native parent txn, opened lazily on the first burrow write and
           committed/aborted with the transaction (the durable analogue of [touched]) *)
    mutable mongo : Coll.session option;
        (* a real-Mongo backend's native client-session transaction, opened lazily on the first native
           write and committed/aborted with the transaction *)
  }

  let _key : t Eio.Fiber.key = Eio.Fiber.create_key ()
  let _fallback : t option ref = ref None

  (* the active transaction for this fiber (or the global fallback outside an Eio scheduler), if any *)
  let current () : t option =
    match (try Eio.Fiber.get _key with Stdlib.Effect.Unhandled _ -> None) with
    | Some _ as v -> v
    | None -> !_fallback

  (* snapshot a collection the first time this transaction writes to it (idempotent per collection) *)
  let touch (tx : t) (m : Minimongo.t) : unit =
    if not (List.exists (fun e -> e.coll == m) tx.touched) then
      tx.touched <- { coll = m; snap = Minimongo.tx_snapshot m } :: tx.touched

  let commit (tx : t) : unit =
    (* commit the native backend transactions (burrow parent txn / Mongo client session) before dropping
       the in-memory snapshots; all backends are now committed *)
    (match tx.burrow with Some (eng, s) -> Burrow_engine.commit_txn eng s | None -> ());
    tx.burrow <- None;
    (match tx.mongo with Some s -> Coll.commit_session s | None -> ());
    tx.mongo <- None;
    tx.touched <- [] (* in-memory writes already applied + delivered; drop snapshots *)

  let rollback (tx : t) : unit =
    (* discard the native backend transactions (their pending writes were never made visible) and restore
       the in-memory snapshots (emitting compensating change events so live observers converge) *)
    (match tx.burrow with Some (eng, s) -> Burrow_engine.abort_txn eng s | None -> ());
    tx.burrow <- None;
    (match tx.mongo with Some s -> Coll.abort_session s | None -> ());
    tx.mongo <- None;
    List.iter (fun e -> Minimongo.tx_restore e.coll e.snap) tx.touched;
    tx.touched <- [];
    tx.on_commit <- [] (* a rolled-back transaction fires no post-commit reactions *)

  (* [on_commit thunk] runs [thunk] after the OUTERMOST transaction commits — the seam post-commit
     reactions and exactly-once effects ride on. A nested workflow's after-hooks register here and all
     fire together once the whole transaction is durable. Outside any transaction, runs immediately. *)
  let on_commit (thunk : unit -> unit) : unit =
    match current () with Some tx -> tx.on_commit <- tx.on_commit @ [ thunk ] | None -> thunk ()

  let _serialize = Eio.Mutex.create ()

  (* run the post-commit callbacks, isolated (one failing reaction must not starve the rest) *)
  let _run_post cbs =
    List.iter (fun f -> try f () with (Stack_overflow | Out_of_memory) as e -> raise e | _ -> ()) cbs

  (* [run f] executes [f] in one transaction (commit on return, rollback on raise) and then fires the
     post-commit callbacks. Re-entrant: a nested [run] joins the outer transaction rather than opening
     a new one (so its on_commit callbacks defer to the outermost commit). *)
  let run : type a. (unit -> a) -> a =
   fun f ->
    match current () with
    | Some _ -> f () (* already inside a transaction — join it (flatten to one atomic unit) *)
    | None ->
        let tx = { touched = []; on_commit = []; burrow = None; mongo = None } in
        let go () = match f () with v -> commit tx; v | exception e -> rollback tx; raise e in
        (* serialize + bind ambient under Eio; outside a scheduler (unit tests) a global ref stands in *)
        let result =
          match (try `Eio (ignore (Eio.Fiber.get _key : t option)) with Stdlib.Effect.Unhandled _ -> `Plain) with
          (* use_ro (not use_rw): a raising transaction is normal — [go] has already rolled back, so the
             resource is consistent; we want the mutex UNLOCKED + the exception re-raised, never disabled *)
          | `Eio () -> Eio.Mutex.use_ro _serialize (fun () -> Eio.Fiber.with_binding _key tx go)
          | `Plain ->
              _fallback := Some tx;
              Fun.protect go ~finally:(fun () -> _fallback := None)
        in
        _run_post tx.on_commit; (* outermost committed — fire reactions OUTSIDE the tx binding (fresh txns) *)
        result
end

module Dynamic = struct
  module Mini = Backend.Mini

  type collection = Mem of Minimongo.t | Native of Coll.t | Embedded of embedded | Missing of string

  let mem m = Mem m
  let real ?poll ~sw conn ~db ~name = Native (collection ?poll ~sw conn ~db ~name)
  let missing message = Missing message
  let unavailable message = failwith ("Fennec.Mongo: " ^ message)

  (* The convention the fennec CLI speaks: MONGO_URL is the one database location. `fennec dev`
     auto-starts/adopts a local mongod when possible; `fennec test` sets :memory: by default and
     `fennec test --mongo` supplies a per-suite real URL. Missing MONGO_URL is not a hidden memory
     fallback; operations fail clearly. Build it in [Fennec.serve ~on_start] so the [sw] it captures
     drives Live's change-stream daemons. *)
  let mongo_url_env = Runtime.mongo_url_env

  (* Shared in-memory collections, keyed by the SAME (db, name) identity the on-disk/native backends
     alias by — so two [collection ~name] opens of "accounts_users" return ONE [Minimongo.t], exactly
     as burrow (one engine per dir, one sub-coll per name) and mongod (one server, one namespace) do.
     Without this, [:memory:] minted a fresh isolated store per open, so an observer's handle and the
     writer's handle were different objects and writes were invisible across them — a dev/test-only
     divergence from prod. The db comes from {!Runtime.db} ([FENNEC_DB] or "fennec"), so a per-suite
     [FENNEC_DB] still isolates (its own key space), matching the other backends precisely. *)
  let mem_collections : (string * string, Minimongo.t) Hashtbl.t = Hashtbl.create 16

  let mem_collection ~name =
    let key = (Runtime.db (), name) in
    match Hashtbl.find_opt mem_collections key with
    | Some m -> m
    | None ->
      let m = Minimongo.create () in
      Hashtbl.replace mem_collections key m;
      m

  (* the single MONGO_URL parser ({!Runtime.backend}) picks the engine: minimongo (:memory:), the
     embedded Burrow engine (burrow://, data dir = base/db from the URL path), or the native driver
     (mongodb://). The db comes from the URL, not from app code. *)
  let from_env ?poll ~sw ~name () =
    match Runtime.backend () with
    | Runtime.Missing -> missing (Runtime.unavailable_message ())
    | Runtime.Memory -> mem (mem_collection ~name)
    | Runtime.Burrow { base; db; _ } -> Embedded (embedded_collection ~sw ~dir:(Filename.concat base db) ~name)
    | Runtime.Mongo { uri; db } -> real ?poll ~sw (connect uri) ~db ~name

  (* The ambient Eio switch the data layer forks its long-lived daemons into (Live's change-streams, the
     embedded engine, the wire endpoint). [Fennec.serve] installs it ONCE, inside its switch, at startup
     (see {!Fennec.serve}); then app and accounts code open a backend collection by name via [collection]
     without threading [sw] through every call. Set before any collection is opened, so a plain ref set
     once on the boot fiber is enough — no cross-domain sharing. *)
  let ambient_switch : Eio.Switch.t option ref = ref None
  let set_switch sw = ambient_switch := Some sw

  (* Open the MONGO_URL-selected collection [name] on the ambient switch — the no-thread entry point for
     [Fennec.serve]-hosted code. Before the switch is installed (only possible by reaching the data layer
     outside [serve]) it yields a [Missing] collection whose ops fail with a clear message, never silently. *)
  let collection ?poll ~name () =
    match !ambient_switch with
    | Some sw -> from_env ?poll ~sw ~name ()
    | None -> missing "Fennec data layer is not booted — Fennec.serve installs the Eio switch at startup"

  (* before an in-memory write, let the active transaction (if any) snapshot the collection so a
     later [raise] can roll it back. A no-op when no transaction is in flight — so the
     non-transactional path stays byte-identical to before. *)
  let in_tx_touch (m : Minimongo.t) = match Tx.current () with Some tx -> Tx.touch tx m | None -> ()

  (* the burrow analogue: if a transaction is in flight, lazily open the engine's parent txn (held for
     the whole workflow, stored on the Tx so commit/rollback can finalize it) and return it, so the
     embedded read/write routes through it. A no-op outside a transaction — the non-tx path is unchanged.
     One session per Tx (an app has one burrow engine); the first burrow write opens it. *)
  let in_tx_burrow (e : embedded) : Burrow_engine.session option =
    match Tx.current () with
    | None -> None
    | Some tx -> (
      match tx.Tx.burrow with
      | Some (_, s) -> Some s
      | None ->
        let s = Burrow_engine.begin_txn e.eng in
        tx.Tx.burrow <- Some (e.eng, s);
        Some s)

  (* the real-Mongo analogue: if a transaction is in flight, lazily start a client-session transaction
     (held for the whole workflow, stored on the Tx so commit/rollback finalize it) and route the native
     op through it. A no-op outside a transaction — the non-tx path is unchanged. *)
  let in_tx_mongo (r : Coll.t) : Coll.session option =
    match Tx.current () with
    | None -> None
    | Some tx -> (
      match tx.Tx.mongo with
      | Some _ as s -> s
      | None ->
        let s = Coll.start_session r in
        tx.Tx.mongo <- Some s;
        Some s)

  let insert c d =
    match c with
    | Mem m -> in_tx_touch m; Mini.insert m d
    | Native r -> (match in_tx_mongo r with Some sess -> insert_s sess r d | None -> insert r d)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.insert_in sess e.ecoll d | None -> Burrow_engine.insert e.eng e.ecoll d)
    | Missing message -> unavailable message
  let update c ~multi ~upsert s m =
    match c with
    | Mem mm -> in_tx_touch mm; Mini.update mm ~multi ~upsert s m
    | Native r -> (match in_tx_mongo r with Some sess -> update_s sess r ~multi ~upsert s m | None -> update r ~multi ~upsert s m)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.update_in sess e.ecoll ~multi ~upsert s m | None -> Burrow_engine.update e.eng e.ecoll ~multi ~upsert s m)
    | Missing message -> unavailable message
  let remove c s =
    match c with
    | Mem m -> in_tx_touch m; Mini.remove m s
    | Native r -> (match in_tx_mongo r with Some sess -> remove_s sess r s | None -> remove r s)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.remove_in sess e.ecoll s | None -> Burrow_engine.remove e.eng e.ecoll s)
    | Missing message -> unavailable message
  let find c q =
    match c with Mem m -> Mini.find m q | Native r -> (match in_tx_mongo r with Some sess -> find_s sess r q | None -> find r q) | Embedded e -> (match in_tx_burrow e with Some sess -> bw_find_in sess e q | None -> bw_find e q) | Missing message -> unavailable message
  let find_one c q =
    match c with Mem m -> Mini.find_one m q | Native r -> (match in_tx_mongo r with Some sess -> find_one_s sess r q | None -> find_one r q) | Embedded e -> (match in_tx_burrow e with Some sess -> bw_find_one_in sess e q | None -> bw_find_one e q) | Missing message -> unavailable message
  let count c s =
    match c with
    | Mem m -> Mini.count m s
    | Native r -> (match in_tx_mongo r with Some sess -> count_s sess r s | None -> count r s)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.count_in sess e.ecoll ~selector:s | None -> Burrow_engine.count e.eng e.ecoll ~selector:s)
    | Missing message -> unavailable message
  let aggregate c ?(lookup = fun _ -> []) p =
    match c with
    | Mem m -> Mini.aggregate m ~lookup p
    | Native r -> (match in_tx_mongo r with Some sess -> aggregate_s sess r ~lookup p | None -> aggregate r ~lookup p)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.aggregate_in sess e.ecoll ~lookup p | None -> Burrow_engine.aggregate e.eng e.ecoll ~lookup p)
    | Missing message -> unavailable message
  let distinct c k s =
    match c with
    | Mem m -> Mini.distinct m k s
    | Native r -> (match in_tx_mongo r with Some sess -> distinct_s sess r k s | None -> distinct r k s)
    | Embedded e -> (match in_tx_burrow e with Some sess -> Burrow_engine.distinct_in sess e.ecoll ~key:k ~selector:s | None -> Burrow_engine.distinct e.eng e.ecoll ~key:k ~selector:s)
    | Missing message -> unavailable message

  let observe_changes c q ~added ~changed ~removed =
    match c with
    | Mem m -> Mini.observe_changes m q ~added ~changed ~removed
    | Native r -> observe_changes r q ~added ~changed ~removed
    | Embedded e -> bw_observe e q ~added ~changed ~removed
    | Missing message -> unavailable message

  let fence c k =
    match c with
    | Mem m -> Mini.fence m k
    | Native r -> fence r k
    | Embedded e -> Burrow_engine.fence e.eng e.ecoll k
    | Missing message -> unavailable message
  let ensure_index c ~name ~keys ~unique ~sparse =
    match c with
    | Mem m -> Mini.ensure_index m ~name ~keys ~unique ~sparse
    | Native r -> ensure_index r ~name ~keys ~unique ~sparse
    | Embedded e -> Burrow_engine.ensure_index e.eng e.ecoll ~name ~keys ~unique ~sparse
    | Missing message -> unavailable message
  let drop_index c ~name =
    match c with
    | Mem m -> Mini.drop_index m ~name
    | Native r -> drop_index r ~name
    | Embedded e -> Burrow_engine.drop_index e.eng e.ecoll ~name
    | Missing message -> unavailable message
  let index_names c =
    match c with
    | Mem m -> Mini.index_names m
    | Native r -> index_names r
    | Embedded e -> Burrow_engine.index_names e.ecoll
    | Missing message -> unavailable message

  let ensure_validator c v =
    match c with
    | Mem m -> Mini.ensure_validator m v
    | Native r -> ensure_validator r v
    | Embedded e -> Burrow_engine.set_validator e.eng e.ecoll v
    | Missing message -> unavailable message
end

(* ---- mongosh / driver exposure: the embedded engine over the MongoDB wire protocol ----------

   {!expose} opens a MongoDB wire-protocol TCP listener in front of the embedded Burrow engine, so any
   real client — mongosh, Compass, a driver — connects exactly as it would to a hosted mongod and runs
   ad-hoc queries. It shares the SAME engine cache as the burrow:// backend (keyed by directory), so
   it sees the app's live data and its writes funnel through the same group-committing writer (no
   concurrent-writer hazard). Secure by default: binds loopback, requires SCRAM-SHA-256 when any user is
   configured, never speaks [$where]/JS, caps message size, and offers a read-only mode + TLS. *)

let wire_rng_ready = ref false
let ensure_wire_rng () = if not !wire_rng_ready then (Mirage_crypto_rng_unix.use_default (); wire_rng_ready := true)

type wire_user = { wu_name : string; wu_password : string }

let wire_user ~user ~password = { wu_name = user; wu_password = password }

(* a burrow:// authority host -> an Eio bind address (0.0.0.0/empty => all interfaces; else loopback or
   a dotted-quad) *)
let ipaddr_of_host h =
  match h with
  | "" | "0.0.0.0" | "::" -> Eio.Net.Ipaddr.V4.any
  | "localhost" | "127.0.0.1" -> Eio.Net.Ipaddr.V4.loopback
  | _ -> (
    match List.map int_of_string_opt (String.split_on_char '.' h) with
    | [ Some a; Some b; Some c; Some d ] when a < 256 && b < 256 && c < 256 && d < 256 ->
      Eio.Net.Ipaddr.of_raw (String.init 4 (fun i -> Char.chr (List.nth [ a; b; c; d ] i)))
    | _ -> Eio.Net.Ipaddr.V4.loopback)

let expose ~sw ~net ?(addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, Runtime.default_wire_port)) ?(base_dir = "./.fennec/burrow")
    ?(users = []) ?require_auth ?(read_only = false) ?tls ?(max_message_bytes = 48_000_000) ?(max_connections = 1000) () =
  ensure_wire_rng ();
  let require_auth = match require_auth with Some b -> b | None -> users <> [] in
  if require_auth && users = [] then
    invalid_arg "Fennec_mongo.expose: require_auth is set but no users were configured (nobody could connect)";
  (* derive a SCRAM-SHA-256 verifier per user once; the plaintext password is never retained *)
  let table = Hashtbl.create 8 in
  List.iter
    (fun u ->
      (* MongoDB's SCRAM salt length is (hash_size - 4): SHA-256 -> 28, SHA-1 -> 16. The server reserves
         the trailing 4 bytes for PBKDF2's block index, and drivers (libmongoc, mongosh) enforce exactly
         this length — a 16- or 32-byte salt is rejected outright. *)
      let salt = Mirage_crypto_rng.generate 28 in
      Hashtbl.replace table u.wu_name (Mongo_wire.Scram.make_credential ~salt ~iterations:15000 ~password:u.wu_password))
    users;
  let dir_of db = Filename.concat base_dir db in
  let module Db = struct
    type coll = embedded

    let collection ~db ~name = embedded_collection ~sw ~dir:(dir_of db) ~name
    let collection_names ~db = Burrow_engine.collection_names (embedded_engine ~sw (dir_of db))

    let database_names () =
      if (try Sys.is_directory base_dir with _ -> false) then
        Sys.readdir base_dir |> Array.to_list
        |> List.filter (fun n -> try Sys.is_directory (Filename.concat base_dir n) with _ -> false)
      else []

    let insert e d = Burrow_engine.insert e.eng e.ecoll d
    let find e ~selector ~sort ~skip ~limit ~fields = Burrow_engine.find e.eng e.ecoll ~selector ~sort ~skip ~limit ~fields
    let count e sel = Burrow_engine.count e.eng e.ecoll ~selector:sel
    let update e ~multi ~upsert q m = Burrow_engine.update e.eng e.ecoll ~multi ~upsert q m
    let remove e sel = Burrow_engine.remove e.eng e.ecoll sel

    (* $lookup / $unionWith resolve sibling collections in the same engine *)
    let aggregate e p =
      let empty = Bson.Document [] in
      let lookup name =
        let c = Burrow_engine.collection e.eng name in
        Burrow_engine.find e.eng c ~selector:empty ~sort:empty ~skip:0 ~limit:0 ~fields:empty
      in
      Burrow_engine.aggregate e.eng e.ecoll ~lookup p

    let distinct e key sel = Burrow_engine.distinct e.eng e.ecoll ~key ~selector:sel
    let ensure_index e ~name ~keys ~unique ~sparse = Burrow_engine.ensure_index e.eng e.ecoll ~name ~keys ~unique ~sparse
    let drop_index e ~name = Burrow_engine.drop_index e.eng e.ecoll ~name
    let index_specs e = Burrow_engine.index_specs e.ecoll

    (* v1 drop empties the collection (its catalog entry/sub-DBs remain); a true drop awaits engine support *)
    let drop_collection ~db ~name =
      let e = embedded_collection ~sw ~dir:(dir_of db) ~name in
      ignore (Burrow_engine.remove e.eng e.ecoll (Bson.Document []))
  end in
  let module Server = Wire_server.Make (Db) in
  let config : Wire_server.config =
    { require_auth;
      lookup = (fun name -> Hashtbl.find_opt table name);
      read_only;
      max_message_bytes;
      max_connections;
      server_nonce = (fun () -> Base64.encode_string (Mirage_crypto_rng.generate 18));
      tls }
  in
  let where = match addr with `Tcp (ip, port) -> Format.asprintf "%a:%d" Eio.Net.Ipaddr.pp ip port | `Unix p -> p in
  (* binding the side-channel endpoint is best-effort: a clash (another dev server already holding the
     port) or any listen error must never take down the app's web server — log and carry on *)
  try
    Server.run ~sw ~net ~addr config;
    (* "embedded" makes clear this is burrow speaking the mongo wire protocol for mongosh access — NOT a
       real external mongod (which only runs with an explicit MONGO_URL) *)
    Printf.printf "fennec: embedded mongo-wire endpoint on %s (auth=%b, read_only=%b, tls=%b)\n%!" where require_auth read_only (tls <> None)
  with e -> Printf.eprintf "fennec: embedded mongo-wire endpoint NOT started on %s — %s\n%!" where (Printexc.to_string e)

(* The auto path the framework calls ({!boot}, from [Fennec.serve]). The MONGO_URL alone decides everything:
   if it is a burrow:// URL WITH an authority, open the mongosh wire endpoint there — bind host:port,
   SCRAM when the URL carries user:pass, read-only when ?readonly. No authority (or any other backend)
   => no endpoint. So dev's zero-config endpoint comes from `fennec dev` generating a loopback authority,
   not from a dev branch here. (?tls=true is parsed but TLS reuse for the wire endpoint is not yet wired
   — use an explicit {!expose} ~tls for a TLS wire endpoint; we log and serve plaintext.) *)
let expose_from_env ~sw ~net () =
  match Runtime.backend () with
  | Runtime.Burrow { base; expose = Some s; _ } ->
    if s.tls then
      Printf.eprintf "fennec: mongo-wire ?tls=true is not yet wired for the auto endpoint — serving plaintext on %s:%d\n%!" s.host s.port;
    let users = match (s.user, s.pass) with Some u, Some p -> [ wire_user ~user:u ~password:p ] | _ -> [] in
    expose ~sw ~net ~addr:(`Tcp (ipaddr_of_host s.host, s.port)) ~base_dir:base ~users ~require_auth:(s.user <> None)
      ~read_only:s.read_only ()
  | _ -> ()

(* The one data-layer boot the framework runs inside [Fennec.serve]'s switch, before any connection is
   served: install the ambient Eio switch (so app + accounts collections open by name, no [sw] threading)
   and — for a burrow:// URL with an authority — front the embedded engine over the MongoDB wire protocol
   so `mongosh` connects. MONGO_URL alone decides everything; there is no app-level "start". *)
let boot ~sw ~net () =
  Dynamic.set_switch sw;
  expose_from_env ~sw ~net ()
