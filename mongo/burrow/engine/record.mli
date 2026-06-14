(* Record — one collection's clustered [_id] store: each document keyed by the memcomparable encoding
   of its [_id], valued by its {!Burrow_binary} record bytes. The access method behind point [_id]
   lookups and full collection scans. *)

module Store = Burrow_store.Store

type t
(** A collection's record store — a typed view over one LMDB sub-DB. *)

val make : Store.db -> t

val key_of_id : Bson.t -> string
(** The record key for an [_id]: its memcomparable encoding (so [_id]-ordered scans and ranges work).
    Raises [Invalid_argument] for a composite [_id] (Document / Array) — not yet supported. *)

val get : _ Store.txn -> t -> id:Bson.t -> Bson.t option
val get_by_key : _ Store.txn -> t -> string -> Bson.t option
val mem : _ Store.txn -> t -> id:Bson.t -> bool
val put : [ `W ] Store.txn -> t -> id:Bson.t -> Bson.t -> unit
val delete : [ `W ] Store.txn -> t -> id:Bson.t -> bool

val iter : _ Store.txn -> t -> (id_key:string -> doc:Bson.t -> bool) -> unit
(** Full scan in [_id] (key) order; [f] returns [true] to continue, [false] to stop. *)
