(* Engine — the public Burrow API: open/close lifecycle, the catalog, and the document operations.
   Reads run on MVCC snapshots; writes commit durably (group-committing writer fiber lands later).
   The thin {!Fennec_pulse.Backend.S} adapter wrapping this lives in [fennec/pulse/mongo/]. *)

module Store = Burrow_store.Store
module B = Bson

type t = { store : Store.t; cat : Catalog.t }
type collection = Catalog.collection

let open_ ?(durability = Store.Full) ?(map_size_gb = 64) path =
  let store = Store.open_ ~map_size_gb ~max_dbs:1024 ~durability path in
  { store; cat = Catalog.open_ store }

let close t = Store.close t.store
let store t = t.store
let collection t name = Catalog.collection t.cat name
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
  Store.write t.store (fun txn -> Record.put txn c.records ~id doc);
  id_to_string id

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

(* ---- index / validator DDL ---------------------------------------------------------------- *)

let ensure_index t (c : collection) ~name ~keys ~unique =
  (* registers + persists; backfill + unique enforcement arrive with the indexing phase *)
  ignore (Catalog.ensure_index t.cat c ~name ~keys ~unique)

let drop_index t (c : collection) ~name = Catalog.drop_index t.cat c ~name
let index_names (c : collection) = Catalog.index_names c
let set_validator t (c : collection) v = Catalog.set_validator t.cat c v
