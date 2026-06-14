(* Write — the mutation path: insert / update / remove / upsert, each maintaining every secondary index
   inside the caller's single write transaction, so a document and its index entries commit atomically.
   Matching reuses the {!Executor} within that same transaction, so a write sees a consistent snapshot.
   (Oplog append for change streams layers on here later.) *)

module Store = Burrow_store.Store

exception Duplicate_key of string
(** Raised (with the index name) when an insert/update would violate a unique index. The enclosing
    write transaction aborts, so no partial change is committed. *)

exception Validation_failed of string
(** Raised (with the collection name) when a write violates the collection's installed [$jsonSchema]
    validator — the same writes mongod rejects. The enclosing write transaction aborts. *)

val insert : [ `W ] Store.txn -> Catalog.collection -> Bson.t -> unit
(** Insert a document that already carries an [_id]; adds its index entries. *)

val update : [ `W ] Store.txn -> Catalog.collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
(** Apply the modifier to documents matching the selector (one unless [multi]); on no match with
    [upsert], seed a document from the selector's equality fields + the modifier. Returns the number of
    documents matched (1 for an upsert insert), maintaining indexes for every change. *)

val remove : [ `W ] Store.txn -> Catalog.collection -> Bson.t -> int
(** Remove all documents matching the selector (and their index entries); returns the count. *)
