(* Engine — the public Burrow API: lifecycle, collections, document operations, indexing, validation,
   and live observation. Reads run on immutable MVCC snapshots; writes serialize through a single-writer
   lock and commit durably off the Eio scheduler. The {!Fennec_mongo_backend.S} adapter in
   [mongo/dynamic/] wraps this (mapping its [query] record to the labeled arguments here). *)

module Store = Burrow_store.Store

type t
type collection = Catalog.collection

val open_ : sw:Eio.Switch.t -> ?durability:Store.durability -> ?map_size_gb:int -> string -> t
(** Open (creating if absent) the engine over an on-disk directory; the catalog is rebuilt from it. The
    group-committing writer fiber is forked into [sw], so it must outlive the engine; {!close} stops it
    cleanly (flushing queued writes) before the switch ends. *)

val close : t -> unit
val store : t -> Store.t

val backup : t -> dir:string -> ?compact:bool -> unit -> unit
(** Online consistent hot-copy of the whole database to [dir] (created if absent; must not already
    contain a [data.mdb]). The copy reflects a single MVCC snapshot at call time and runs OFF the write
    path — writers are never paused, so it is a zero-downtime backup. [~compact:true] (default) copies
    only live pages, defragmenting. The result is itself a complete, openable Burrow database: to RESTORE,
    point {!open_} at [dir] (or, offline, swap it into the data directory and reopen). The copy runs on a
    systhread, so the calling fiber suspends without stalling the domain. *)

val collection : t -> string -> collection
(** Get-or-create a collection by name. *)

val collection_opt : t -> string -> collection option

val collection_names : t -> string list
(** The names of all collections currently in the catalog (for [listCollections] / [listDatabases]). *)

val insert : t -> collection -> Bson.t -> string
(** Insert a document (minting an ObjectId [_id] when absent); returns the [_id] as a string. *)

val update : t -> collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
val remove : t -> collection -> Bson.t -> int

val find :
  t -> collection -> selector:Bson.t -> sort:Bson.t -> skip:int -> limit:int -> fields:Bson.t -> Bson.t list

val find_one :
  t -> collection -> selector:Bson.t -> sort:Bson.t -> skip:int -> fields:Bson.t -> Bson.t option

val count : t -> collection -> selector:Bson.t -> int
val distinct : t -> collection -> key:string -> selector:Bson.t -> Bson.t list
val aggregate : t -> collection -> ?lookup:(string -> Bson.t list) -> Bson.t list -> Bson.t list

(** Workflow transactions — the native-rollback backend for {!Fennec_mongo_dynamic.Tx}. [begin_txn]
    opens one parent LMDB write txn (holding the single write lock) for a whole workflow; the workflow's
    reads and writes route through it via the [*_in] ops (read-your-writes is free — an LMDB write txn
    reads its own pending writes); [commit_txn] commits once and reveals the deltas to observers,
    [abort_txn] discards them. The Tx layer opens lazily on the first burrow write and commits/aborts at
    the workflow boundary. The non-transactional ops above are unaffected. *)
type session

val begin_txn : t -> session
val commit_txn : t -> session -> unit
val abort_txn : t -> session -> unit
val insert_in : session -> collection -> Bson.t -> string
val update_in : session -> collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
val remove_in : session -> collection -> Bson.t -> int

val find_in :
  session -> collection -> selector:Bson.t -> sort:Bson.t -> skip:int -> limit:int -> fields:Bson.t -> Bson.t list

val find_one_in :
  session -> collection -> selector:Bson.t -> sort:Bson.t -> skip:int -> fields:Bson.t -> Bson.t option

val count_in : session -> collection -> selector:Bson.t -> int
val distinct_in : session -> collection -> key:string -> selector:Bson.t -> Bson.t list
val aggregate_in : session -> collection -> ?lookup:(string -> Bson.t list) -> Bson.t list -> Bson.t list

val observe_changes :
  t ->
  collection ->
  selector:Bson.t ->
  sort:Bson.t ->
  skip:int ->
  limit:int ->
  fields:Bson.t ->
  added:(string -> Bson.t -> unit) ->
  changed:(string -> Bson.t -> string list -> unit) ->
  removed:(string -> unit) ->
  (unit -> unit)
(** Register a live observer; returns its unregister function. The initial [added] burst fires for the
    current matches, then writes deliver field-level deltas synchronously. *)

val fence : t -> collection -> (unit -> unit) -> unit
(** Run [k] once all changes committed so far are delivered — immediate, since delivery is synchronous. *)

val ensure_index : t -> collection -> name:string -> keys:Bson.t -> unique:bool -> sparse:bool -> unit
val drop_index : t -> collection -> name:string -> unit
val index_names : collection -> string list

val index_specs : collection -> (string * (string * int) list * bool) list
(** Each secondary index as [(name, keys, unique)] with [keys = (field, 1|-1) list] — for
    [listIndexes] / [createIndexes] bookkeeping over the wire (the clustered [_id_] index is implicit
    and added by the caller). *)

val set_validator : t -> collection -> Bson.t option -> unit
