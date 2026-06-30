(* Executor — interpret a {!Plan.t} into results: gather candidate documents via the chosen access
   path, apply the residual selector (the plan may over-approximate), then sort, window (skip/limit)
   and project. The query semantics themselves are the pure {!Query} evaluators (Matcher / Sorter /
   Projection) — the executor only sequences storage access and those evaluators. *)

module Store = Burrow_store.Store

exception Result_too_large of int
(** Raised by [find] / [find_one] / [matched] when the documents a query would materialize exceed the
    [?cap] (the engine's [~query_limit]) — governance against an unbounded scan exhausting memory. *)

val matched : ?cap:int -> _ Store.txn -> Catalog.collection -> Plan.t -> selector:Bson.t -> Bson.t list
(** Candidate documents from the access path, filtered by the residual selector. No sort/window. With
    [?cap], raises {!Result_too_large} if the matched set exceeds it. *)

val find :
  ?cap:int ->
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
  ?cap:int ->
  _ Store.txn ->
  Catalog.collection ->
  Plan.t ->
  selector:Bson.t ->
  sort:Bson.t ->
  skip:int ->
  fields:Bson.t ->
  Bson.t option

val count : _ Store.txn -> Catalog.collection -> Plan.t -> selector:Bson.t -> int
