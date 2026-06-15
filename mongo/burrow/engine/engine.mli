(* Engine — the public Burrow API: lifecycle, collections, document operations, indexing, validation,
   and live observation. Reads run on immutable MVCC snapshots; writes serialize through a single-writer
   lock and commit durably off the Eio scheduler. The {!Fennec_pulse.Backend.S} adapter in
   [fennec/pulse/mongo/] wraps this (mapping its [query] record to the labeled arguments here). *)

module Store = Burrow_store.Store

type t
type collection = Catalog.collection

val open_ : sw:Eio.Switch.t -> ?durability:Store.durability -> ?map_size_gb:int -> string -> t
(** Open (creating if absent) the engine over an on-disk directory; the catalog is rebuilt from it. The
    group-committing writer fiber is forked into [sw], so it must outlive the engine; {!close} stops it
    cleanly (flushing queued writes) before the switch ends. *)

val close : t -> unit
val store : t -> Store.t

val collection : t -> string -> collection
(** Get-or-create a collection by name. *)

val collection_opt : t -> string -> collection option

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

val ensure_index : t -> collection -> name:string -> keys:Bson.t -> unique:bool -> unit
val drop_index : t -> collection -> name:string -> unit
val index_names : collection -> string list
val set_validator : t -> collection -> Bson.t option -> unit
