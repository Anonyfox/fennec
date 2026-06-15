(* Store — the typed-safe facade over the raw LMDB FFI ({!Burrow_lmdb}). Every storage access in the
   engine goes through here, so memory safety and the transaction discipline are auditable in one
   module.

   - **Phantom transaction modes.** A [`W] transaction permits writes; a [`R] one does not —
     [put]/[del] demand [`W], so "write in a read transaction" is a type error.
   - **Named sub-databases** ([db]) are opened (created if absent) once and cached for the env's
     lifetime; call {!db} OUTSIDE a transaction (it opens its own short write txn on first request).
   - **Read** transactions are short MVCC snapshots. **Write** commits when the body returns, aborts
     if it raises, and with [~blocking] runs the commit's fsync off the Eio scheduler (a systhread),
     so an ~8 ms [F_FULLFSYNC] never stalls the domain. *)

type t
(** An open environment (one on-disk directory). *)

type +'m txn constraint 'm = [< `R | `W ]
(** A live transaction, tagged with its mode. *)

type db
(** A named sub-database handle, valid for the env's lifetime. *)

type durability =
  | Full          (** F_FULLFSYNC of data + meta — power-loss safe (group-commit for throughput) *)
  | No_meta_sync  (** sync data only; meta recovered on crash — ~2x faster *)
  | No_sync       (** no fsync; periodic flush — fastest, dev/test *)

val open_ : ?map_size_gb:int -> ?max_dbs:int -> ?durability:durability -> string -> t
val close : t -> unit
val durability : t -> durability

val db : t -> string -> db
(** [db t name] — the named sub-database, opened (and created) under a short write txn on first
    request and cached. Call outside any open transaction. *)

val read : t -> ([ `R ] txn -> 'a) -> 'a
(** A read-only MVCC snapshot; the txn is closed when [f] returns or raises. *)

val write : t -> ?blocking:bool -> ([ `W ] txn -> 'a) -> 'a
(** A write transaction: committed when [f] returns, aborted if it raises. [~blocking:true] (the
    default for the engine's group-committing writer) runs the commit off the scheduler. *)

val get : _ txn -> db -> string -> string option
val put : [ `W ] txn -> db -> string -> string -> unit
val del : [ `W ] txn -> db -> string -> bool
(** [del] returns whether a value was present. *)

val clear : [ `W ] txn -> db -> unit
(** Empty a sub-database (drop all entries; the handle stays valid) — index drop/recreate. *)

val iter : _ txn -> db -> ?from:string -> ?rev:bool -> (key:string -> data:string -> bool) -> unit
(** Iterate key/value pairs in key (byte) order, optionally starting at the first key >= [from].
    [f ~key ~data] returns [true] to continue, [false] to stop. With [~rev:true] iterate back-to-front
    (LAST -> PREV); [from] is ignored in that case (the scan starts at the last key). *)
