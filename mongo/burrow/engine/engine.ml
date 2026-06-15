(* Engine — the public Burrow API: open/close lifecycle, the catalog, and the document operations.
   Reads run on immutable MVCC snapshots, lock-free. Writes go through a SINGLE group-committing writer
   fiber: submitters enqueue a job and await; the writer drains all queued jobs, runs each in an LMDB
   child transaction (per-write failure isolation), and commits the parent ONCE — so one ~8 ms
   F_FULLFSYNC is amortized across the whole batch (≈125 → 100k+ durable writes/s under load). DDL
   (catalog/index/validator) takes the same write lock the writer holds, so no two write transactions
   ever overlap. The {!Fennec_pulse.Backend.S} adapter wrapping this lives in [fennec/pulse/mongo/]. *)

module Store = Burrow_store.Store
module B = Bson

(* a submitted write: [body] runs in a child txn; [reply] resolves the submitter's promise; [coll] is
   notified to observers after the batch commits. The result type is existential (erased per job). *)
type job = Job : { body : [ `W ] Store.txn -> 'a; reply : ('a, exn) result -> unit; coll : string } -> job
type qitem = Do of job | Quit

type t = {
  store : Store.t;
  cat : Catalog.t;
  observers : Observe.t;
  write_lock : Eio.Mutex.t; (* held during any write txn (the writer's batch OR a DDL Store.write) *)
  queue : qitem Eio.Stream.t;
  writer_done : unit Eio.Promise.t;
  writer_done_u : unit Eio.Promise.u;
}

type collection = Catalog.collection

(* DDL path: plain lock/unlock (not [Mutex.use_rw], which poisons on any exception — an expected write
   failure leaves the store consistent, so we just release + re-raise). *)
let with_write t f =
  Eio.Mutex.lock t.write_lock;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.write_lock) f

(* Run a batch in ONE parent txn — each job in its own child (commit on success, abort on failure, so a
   Duplicate_key/Validation_failed in one job can't sink the batch), then commit the parent once. A
   later job sees earlier ones' changes (children merge into the parent), so intra-batch semantics match
   sequential writes. Observers are notified once per affected collection AFTER the durable commit and
   BEFORE replies resolve, so a caller's fence sees its deltas delivered by the time its write returns. *)
let process_batch t (jobs : job list) =
  Eio.Mutex.lock t.write_lock;
  let outcome =
    try
      let parent = Store.begin_write t.store in
      let run (Job { body; reply; coll = _ }) =
        let ch = Store.child parent in
        let r = match body ch with v -> (match Store.commit_child ch with () -> Ok v | exception e -> Error e) | exception e -> (Store.abort ch; Error e) in
        fun pe -> reply (match (r, pe) with Ok v, None -> Ok v | Ok _, Some e -> Error e | Error e, _ -> Error e)
      in
      let deferred = List.map run jobs in
      let parent_err = match Store.commit_durable t.store parent with () -> None | exception e -> Some e in
      `Done (parent_err, deferred)
    with fatal -> `Fatal fatal
  in
  Eio.Mutex.unlock t.write_lock;
  match outcome with
  | `Done (parent_err, deferred) ->
    (if parent_err = None then
       List.sort_uniq String.compare (List.map (fun (Job { coll; _ }) -> coll) jobs)
       |> List.iter (fun coll -> Observe.notify t.observers ~coll));
    List.iter (fun f -> f parent_err) deferred
  | `Fatal fatal -> List.iter (fun (Job { reply; _ }) -> reply (Error fatal)) jobs

(* the writer fiber: block for the first job, drain everything else queued right now into one batch *)
let rec writer_loop t =
  let first = Eio.Stream.take t.queue in
  let rec drain jobs quit =
    match Eio.Stream.take_nonblocking t.queue with
    | Some (Do j) -> drain (j :: jobs) quit
    | Some Quit -> drain jobs true
    | None -> (List.rev jobs, quit)
  in
  let jobs, quit = match first with Do j -> drain [ j ] false | Quit -> ([], true) in
  if jobs <> [] then process_batch t jobs;
  if quit then Eio.Promise.resolve t.writer_done_u () else writer_loop t

let open_ ~sw ?(durability = Store.Full) ?(map_size_gb = 128) path =
  let store = Store.open_ ~map_size_gb ~max_dbs:1024 ~durability path in
  let writer_done, writer_done_u = Eio.Promise.create () in
  let t =
    { store; cat = Catalog.open_ store; observers = Observe.create (); write_lock = Eio.Mutex.create ();
      queue = Eio.Stream.create 4096; writer_done; writer_done_u }
  in
  Eio.Fiber.fork_daemon ~sw (fun () -> writer_loop t; `Stop_daemon);
  t

(* enqueue a write and await its durable result (re-raising the body's exception on failure) *)
let submit t ~coll body =
  let p, r = Eio.Promise.create () in
  Eio.Stream.add t.queue (Do (Job { body; reply = Eio.Promise.resolve r; coll }));
  match Eio.Promise.await p with Ok v -> v | Error e -> raise e

let close t =
  Eio.Stream.add t.queue Quit;
  Eio.Promise.await t.writer_done;
  Store.close t.store

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

(* ---- writes (through the group-committing writer fiber) ----------------------------------- *)

let insert t (c : collection) doc =
  let id, doc = ensure_id doc in
  submit t ~coll:c.name (fun txn -> Write.insert txn c doc);
  id_to_string id

let update t (c : collection) ~multi ~upsert selector modifier =
  submit t ~coll:c.name (fun txn -> Write.update txn c ~multi ~upsert selector modifier)

let remove t (c : collection) selector = submit t ~coll:c.name (fun txn -> Write.remove txn c selector)

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
