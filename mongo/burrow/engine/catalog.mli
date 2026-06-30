(* Catalog — the engine's metadata: which collections exist, their secondary indexes, and their
   structural validators. Persisted in a dedicated [meta] sub-DB (a tagged-key layout) and fully
   rebuilt in memory on {!open_}, so an engine reopen restores every collection, index handle, and
   validator. Collections and indexes each own an LMDB sub-DB; those handles live here. *)

module Store = Burrow_store.Store

type index = {
  iname : string;
  keys : (string * int) list;  (** field path, direction (1 asc | -1 desc) *)
  unique : bool;
  sparse : bool;  (** skip a document that omits ALL key fields (MongoDB sparse) *)
  mutable multikey : bool;  (** set once any document indexes an array field — gates sort-via-index *)
  mutable ready : bool;  (** false while an online build backfills it — the planner skips a non-ready index *)
  db : Store.db;  (** the index sub-DB: encoded-key ++ _id -> record key *)
}

type collection = {
  name : string;
  records : Record.t;
  meta : Store.db;  (** the shared metadata sub-DB (so a write can persist index flags in its txn) *)
  mutable indexes : index list;
  mutable validator : Bson.t option;
}

type t

val persist_index : [ `W ] Store.txn -> collection -> index -> unit
(** Persist [idx]'s current spec (keys / unique / multikey) into the metadata DB, within the caller's
    write txn — used to durably record an index flipping to multikey. *)

val open_ : Store.t -> t
(** Open the catalog over a store, rebuilding all in-memory metadata + sub-DB handles from [meta]. *)

val store : t -> Store.t

val collection : t -> string -> collection
(** Get-or-create a collection (opens/persists its record sub-DB on first use). Call outside a txn. *)

val collection_opt : t -> string -> collection option
val collections : t -> collection list
val index_names : collection -> string list

val ensure_index :
  t -> collection -> name:string -> keys:Bson.t -> unique:bool -> sparse:bool -> ready:bool -> index option
(** Idempotent by [name]. Returns [Some idx] when newly created (the caller backfills existing records
    into it), or [None] if an index of that name already existed. [~ready:false] registers it
    query-invisible for an online build; flip it with {!set_index_ready} once backfilled. *)

val set_index_ready : t -> collection -> index -> unit
(** Flip a backfilled online index to ready (the planner will use it) and persist the flag. *)

val drop_index : t -> collection -> name:string -> unit
(** Idempotent: unregister the index, remove its metadata, and empty its sub-DB. *)

val validator : collection -> Bson.t option
val set_validator : t -> collection -> Bson.t option -> unit
