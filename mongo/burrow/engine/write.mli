(* Write — the mutation path: insert / update / remove / upsert, each maintaining every secondary index
   inside the caller's single write transaction, so a document and its index entries commit atomically.
   Matching reuses the {!Executor} within that same transaction, so a write sees a consistent snapshot.
   ([update] / [remove] return the affected documents so the engine appends the oplog in the same txn.) *)

module Store = Burrow_store.Store

exception Duplicate_key of string
(** Raised (with the index name) when an insert/update would violate a unique index. The enclosing
    write transaction aborts, so no partial change is committed. *)

exception Validation_failed of string
(** Raised (with the collection name) when a write violates the collection's installed [$jsonSchema]
    validator — the same writes mongod rejects. The enclosing write transaction aborts. *)

val insert : [ `W ] Store.txn -> Catalog.collection -> Bson.t -> unit
(** Insert a document that already carries an [_id]; adds its index entries. *)

val update :
  [ `W ] Store.txn -> Catalog.collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> (Bson.t * Bson.t) list
(** Apply the modifier to documents matching the selector (one unless [multi]); on no match with [upsert],
    seed a document from the selector's equality fields + the modifier. Returns [(_id, resulting-doc)] for
    each affected document (so the engine logs an idempotent oplog entry per doc), maintaining indexes. *)

val remove : [ `W ] Store.txn -> Catalog.collection -> Bson.t -> Bson.t list
(** Remove all documents matching the selector (and their index entries); returns their [_id]s. *)

val backfill_index : [ `W ] Store.txn -> Catalog.collection -> Catalog.index -> unit
(** Populate a freshly-created index from the existing records, setting its multikey flag as needed. *)

val backfill_chunk :
  [ `W ] Store.txn -> Catalog.collection -> Catalog.index -> from:string option -> limit:int -> int * string option
(** One chunk of an online backfill: index up to [limit] records from [from] (resuming a chunked walk),
    returning [(count, last_key)]. A [count < limit] means the scan reached the end. *)
