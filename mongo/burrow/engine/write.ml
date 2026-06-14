module Store = Burrow_store.Store
module B = Bson
module Mod = Query.Modifier

exception Duplicate_key of string
exception Validation_failed of string

let id_of doc = match B.get doc "_id" with Some id -> id | None -> invalid_arg "Burrow: document without _id"

(* reject a write whose document violates the collection's installed $jsonSchema validator — the same
   rule mongod enforces, so Burrow and the database refuse the same writes *)
let check_validator (c : Catalog.collection) doc =
  match c.validator with
  | Some schema when not (Query.Matcher.json_schema_matches schema doc) -> raise (Validation_failed c.name)
  | _ -> ()

(* a unique index is violated if any of [doc]'s key tuples already maps to a *different* record *)
let check_unique txn (c : Catalog.collection) ~record_key ~doc =
  List.iter
    (fun idx ->
      if idx.Catalog.unique then
        List.iter
          (fun k ->
            Store.iter txn idx.Catalog.db ~from:k (fun ~key ~data ->
                if String.starts_with ~prefix:k key then (
                  if data <> record_key then raise (Duplicate_key idx.Catalog.iname);
                  true)
                else false))
          (Index.keys_for_doc idx doc))
    c.indexes

(* force [_id] to be [id] and first — guards against a modifier dropping/altering it *)
let force_id id = function
  | B.Document fields -> B.Document (("_id", id) :: List.filter (fun (k, _) -> k <> "_id") fields)
  | other -> other

(* ---- index maintenance (over the collection's indexes) ------------------------------------- *)

let index_all txn (c : Catalog.collection) ~record_key ~doc =
  List.iter (fun idx -> Index.add txn idx ~doc ~record_key) c.indexes

let unindex_all txn (c : Catalog.collection) ~record_key ~doc =
  List.iter (fun idx -> Index.remove txn idx ~doc ~record_key) c.indexes

let reindex txn (c : Catalog.collection) ~record_key ~old_doc ~new_doc =
  List.iter
    (fun idx ->
      Index.remove txn idx ~doc:old_doc ~record_key;
      Index.add txn idx ~doc:new_doc ~record_key)
    c.indexes

(* ---- insert -------------------------------------------------------------------------------- *)

let insert txn (c : Catalog.collection) (doc : B.t) =
  let id = id_of doc in
  let rk = Record.key_of_id id in
  check_validator c doc;
  check_unique txn c ~record_key:rk ~doc;
  Record.put txn c.records ~id doc;
  index_all txn c ~record_key:rk ~doc

(* ---- upsert seed --------------------------------------------------------------------------- *)

let upsert_seed selector modifier : B.t =
  let base_fields =
    match selector with
    | B.Document kvs ->
      List.filter_map
        (fun (k, v) ->
          if String.length k > 0 && k.[0] = '$' then None
          else match v with B.Document _ | B.Array _ -> None | _ -> Some (k, v))
        kvs
    | _ -> []
  in
  let base = B.Document base_fields in
  let seeded = if Mod.is_operator_doc modifier then Mod.apply ~insert:true base modifier else modifier in
  match B.get seeded "_id" with
  | Some _ -> seeded
  | None ->
    let id = match B.get base "_id" with Some i -> i | None -> B.Object_id (Query.Id.object_id ()) in
    force_id id seeded

(* ---- update -------------------------------------------------------------------------------- *)

let update txn (c : Catalog.collection) ~multi ~upsert selector modifier : int =
  let plan = Planner.plan c.indexes ~selector ~sort:(B.Document []) in
  let all = Executor.matched txn c plan ~selector in
  let targets = if multi then all else match all with [] -> [] | d :: _ -> [ d ] in
  match targets with
  | [] when upsert ->
    insert txn c (upsert_seed selector modifier);
    1
  | [] -> 0
  | ds ->
    List.iter
      (fun old_doc ->
        let id = id_of old_doc in
        let rk = Record.key_of_id id in
        let new_doc = force_id id (Mod.apply old_doc modifier) in
        check_validator c new_doc;
        check_unique txn c ~record_key:rk ~doc:new_doc;
        Record.put txn c.records ~id new_doc;
        reindex txn c ~record_key:rk ~old_doc ~new_doc)
      ds;
    List.length ds

(* ---- remove -------------------------------------------------------------------------------- *)

let remove txn (c : Catalog.collection) selector : int =
  let plan = Planner.plan c.indexes ~selector ~sort:(B.Document []) in
  let targets = Executor.matched txn c plan ~selector in
  List.iter
    (fun old_doc ->
      let id = id_of old_doc in
      let rk = Record.key_of_id id in
      ignore (Record.delete txn c.records ~id);
      unindex_all txn c ~record_key:rk ~doc:old_doc)
    targets;
  List.length targets
