(* Executor — interpret a {!Plan.t} into results: gather candidate documents via the chosen access
   path, apply the residual selector (the plan may over-approximate), then sort, window (skip/limit)
   and project. The query semantics themselves are the pure {!Query} evaluators (Matcher / Sorter /
   Projection) — the executor only sequences storage access and those evaluators. *)

module Store = Burrow_store.Store

val matched : _ Store.txn -> Catalog.collection -> Plan.t -> selector:Bson.t -> Bson.t list
(** Candidate documents from the access path, filtered by the residual selector. No sort/window. *)

val find :
  _ Store.txn ->
  Catalog.collection ->
  Plan.t ->
  selector:Bson.t ->
  sort:Bson.t ->
  skip:int ->
  limit:int ->
  fields:Bson.t ->
  Bson.t list

val find_one :
  _ Store.txn ->
  Catalog.collection ->
  Plan.t ->
  selector:Bson.t ->
  sort:Bson.t ->
  skip:int ->
  fields:Bson.t ->
  Bson.t option

val count : _ Store.txn -> Catalog.collection -> Plan.t -> selector:Bson.t -> int
