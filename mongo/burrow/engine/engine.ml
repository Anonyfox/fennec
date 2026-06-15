(* Engine — the public Burrow API: open/close lifecycle, the catalog, and the document operations.
   Reads run on MVCC snapshots; writes commit durably (group-committing writer fiber lands later).
   The thin {!Fennec_pulse.Backend.S} adapter wrapping this lives in [fennec/pulse/mongo/]. *)

module Store = Burrow_store.Store
module B = Bson

type t = { store : Store.t; cat : Catalog.t; observers : Observe.t; write_lock : Eio.Mutex.t }
type collection = Catalog.collection

(* All writes (DML + DDL) serialize through [write_lock]: LMDB allows a single write txn at a time, and
   a contended [mdb_txn_begin] would block the whole Eio domain (not just the calling fiber). An Eio
   mutex suspends the fiber instead, so concurrent writers queue cleanly — the single-writer design.
   Reads need no lock: they run on immutable MVCC snapshots. *)
let open_ ?(durability = Store.Full) ?(map_size_gb = 128) path =
  let store = Store.open_ ~map_size_gb ~max_dbs:1024 ~durability path in
  { store; cat = Catalog.open_ store; observers = Observe.create (); write_lock = Eio.Mutex.create () }

(* Plain lock/unlock (not [Mutex.use_rw], which poisons the mutex on any exception): an expected write
   failure — Duplicate_key / Validation_failed — leaves the store consistent (Store.write already
   aborted the txn), so we just release the lock and re-raise. Poisoning would break every later write. *)
let with_write t f =
  Eio.Mutex.lock t.write_lock;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.write_lock) f
let close t = Store.close t.store
let store t = t.store
let collection t name = with_write t (fun () -> Catalog.collection t.cat name)
let collection_opt t name = Catalog.collection_opt t.cat name

(* ---- ids ---------------------------------------------------------------------------------- *)

let id_to_string = function B.Object_id h -> h | B.String s -> s | other -> B.to_string other

(* the inserted document with an [_id] guaranteed (generated as an ObjectId when absent, matching the
   mongod path Burrow replaces); returns the id value and the (possibly extended) document *)
let ensure_id (doc : B.t) : B.t * B.t =
  match doc with
  | B.Document fields -> (
    match List.assoc_opt "_id" fields with
    | Some id -> (id, doc)
    | None ->
      let id = B.Object_id (Query.Id.object_id ()) in
      (id, B.Document (("_id", id) :: fields)))
  | _ -> invalid_arg "Burrow: insert expects a Document"

(* ---- writes ------------------------------------------------------------------------------- *)

let insert t (c : collection) doc =
  let id, doc = ensure_id doc in
  with_write t (fun () ->
      Store.write t.store (fun txn -> Write.insert txn c doc);
      Observe.notify t.observers ~coll:c.name);
  id_to_string id

let update t (c : collection) ~multi ~upsert selector modifier =
  with_write t (fun () ->
      let n = Store.write t.store (fun txn -> Write.update txn c ~multi ~upsert selector modifier) in
      Observe.notify t.observers ~coll:c.name;
      n)

let remove t (c : collection) selector =
  with_write t (fun () ->
      let n = Store.write t.store (fun txn -> Write.remove txn c selector) in
      Observe.notify t.observers ~coll:c.name;
      n)

(* ---- reads -------------------------------------------------------------------------------- *)

let plan_for (c : collection) ~selector ~sort = Planner.plan c.indexes ~selector ~sort

let find t (c : collection) ~selector ~sort ~skip ~limit ~fields =
  Store.read t.store (fun txn ->
      let plan = plan_for c ~selector ~sort in
      Executor.find txn c plan ~selector ~sort ~skip ~limit ~fields)

let find_one t (c : collection) ~selector ~sort ~skip ~fields =
  Store.read t.store (fun txn ->
      let plan = plan_for c ~selector ~sort in
      Executor.find_one txn c plan ~selector ~sort ~skip ~fields)

let count t (c : collection) ~selector =
  Store.read t.store (fun txn ->
      let plan = plan_for c ~selector ~sort:(B.Document []) in
      Executor.count txn c plan ~selector)

(* distinct values of [key] (dotted path) across documents matching [selector]; array values are
   unwrapped and the result deduped by the total Bson order *)
let distinct t (c : collection) ~key ~selector =
  let docs =
    Store.read t.store (fun txn ->
        let plan = plan_for c ~selector ~sort:(B.Document []) in
        Executor.matched txn c plan ~selector)
  in
  List.sort_uniq B.compare
    (List.concat_map
       (fun d -> match Query.Matcher.get_path d key with None -> [] | Some (B.Array els) -> els | Some v -> [ v ])
       docs)

(* one-shot aggregation pipeline over the collection's documents (reuses the pure pipeline engine);
   [lookup] resolves a foreign collection for $lookup / $unionWith *)
let aggregate t (c : collection) ?(lookup = fun _ -> []) pipeline =
  let docs =
    Store.read t.store (fun txn ->
        let acc = ref [] in
        Record.iter txn c.records (fun ~id_key:_ ~doc -> acc := doc :: !acc; true);
        List.rev !acc)
  in
  Query.Aggregate.run ~lookup pipeline docs

(* ---- live observation (observeChanges) ---------------------------------------------------- *)

let strip_id = function
  | B.Document fs -> B.Document (List.filter (fun (k, _) -> k <> "_id") fs)
  | d -> d

(* Register a live observer over the query. The recompute closure re-runs the query (membership via a
   full recompute, so sort/skip/limit are honored), keying by the document's [_id] and emitting the
   projected fields with [_id] stripped. Returns the unregister function. *)
let observe_changes t (c : collection) ~selector ~sort ~skip ~limit ~fields ~added ~changed ~removed =
  let proj = if fields = B.Document [] then None else Some (Query.Projection.of_fields fields) in
  let recompute () =
    Store.read t.store (fun txn ->
        let plan = plan_for c ~selector ~sort in
        let docs = Executor.find txn c plan ~selector ~sort ~skip ~limit ~fields:(B.Document []) in
        List.map
          (fun d ->
            let id = id_to_string (Option.value ~default:B.Null (B.get d "_id")) in
            let f = match proj with None -> d | Some p -> Query.Projection.apply p d in
            (id, strip_id f))
          docs)
  in
  Observe.add t.observers ~coll:c.name ~recompute ~added ~changed ~removed

(* Notification is synchronous within each write, so by the time a write returns its deltas are
   delivered; the fence runs immediately. *)
let fence _t (_c : collection) k = k ()

(* ---- index / validator DDL ---------------------------------------------------------------- *)

let ensure_index t (c : collection) ~name ~keys ~unique =
  with_write t (fun () ->
      match Catalog.ensure_index t.cat c ~name ~keys ~unique with
      | None -> ()
      | Some idx -> Store.write t.store (fun txn -> Write.backfill_index txn c idx))

let drop_index t (c : collection) ~name = with_write t (fun () -> Catalog.drop_index t.cat c ~name)
let index_names (c : collection) = Catalog.index_names c
let set_validator t (c : collection) v = with_write t (fun () -> Catalog.set_validator t.cat c v)
