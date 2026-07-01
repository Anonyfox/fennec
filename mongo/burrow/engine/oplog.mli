(* The engine-wide change log — see oplog.ml. Each committed write appends one entry per affected document,
   inside the write txn (atomic, no extra fsync); consumers (change streams, PITR, replication) tail it. *)

module Store = Burrow_store.Store

type op = Insert | Update | Delete

type entry = { lsn : int64; ts : float; op : op; ns : string; id : Bson.t; doc : Bson.t option }
(** [doc] is the RESULTING document (insert / update) or [None] (delete), so replaying an entry is an
    idempotent put-or-delete by [id]. *)

type t

val op_str : op -> string
(** the wire code for an operation: ["i"] / ["u"] / ["d"]. *)

val entry_of_bson : Bson.t -> entry
(** Parse an entry from its Bson form — the internal record, or the wire's raw [{lsn,op,ns,id,o?}] (where
    [ts] is absent and defaults to 0). For a replica rebuilding entries pulled over the wire. *)

val make : ?keep:int -> Store.t -> t
(** Open/create the [_oplog] sub-DB and resume the LSN from its highest key. [keep] caps the retained
    entries (default 1_000_000); older ones are trimmed on append. *)

val current_lsn : t -> int64
(** The highest assigned LSN (0 when empty). *)

val append : t -> [ `W ] Store.txn -> op:op -> ns:string -> id:Bson.t -> doc:Bson.t option -> unit
(** Append one entry in the caller's write txn (atomic with the data change), assigning the next LSN and
    trimming to the retention cap. *)

val tail : t -> _ Store.txn -> from_lsn:int64 -> limit:int -> entry list
(** Entries with [lsn > from_lsn], up to [limit], in LSN order — the consumer's tail. *)

val oldest_lsn : t -> _ Store.txn -> int64
(** The oldest retained LSN (the smallest key), or 0 when empty — the floor below which a follower can no
    longer catch up by tailing (its needed entries were trimmed) and must re-initial-sync. *)
