(* Catalog — the engine's metadata: which collections exist, their secondary indexes, and their
   structural validators. Persisted in a dedicated [meta] sub-DB (a tagged-key layout) and fully
   rebuilt in memory on {!open_}, so an engine reopen restores every collection, index handle, and
   validator. Collections and indexes each own an LMDB sub-DB; those handles live here. *)

module Store = Burrow_store.Store

type index = {
  iname : string;
  keys : (string * int) list;  (** field path, direction (1 asc | -1 desc) *)
  unique : bool;
  db : Store.db;  (** the index sub-DB: encoded-key ++ _id -> record key *)
}

type collection = {
  name : string;
  records : Record.t;
  mutable indexes : index list;
  mutable validator : Bson.t option;
}

type t

val open_ : Store.t -> t
(** Open the catalog over a store, rebuilding all in-memory metadata + sub-DB handles from [meta]. *)

val store : t -> Store.t

val collection : t -> string -> collection
(** Get-or-create a collection (opens/persists its record sub-DB on first use). Call outside a txn. *)

val collection_opt : t -> string -> collection option
val collections : t -> collection list
val index_names : collection -> string list

val ensure_index : t -> collection -> name:string -> keys:Bson.t -> unique:bool -> index option
(** Idempotent by [name]. Returns [Some idx] when newly created (the caller backfills existing records
    into it), or [None] if an index of that name already existed. *)

val drop_index : t -> collection -> name:string -> unit
(** Idempotent: unregister the index, remove its metadata, and empty its sub-DB. *)

val validator : collection -> Bson.t option
val set_validator : t -> collection -> Bson.t option -> unit
