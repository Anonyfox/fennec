(* The LMDB FFI — the ONLY unsafe code in the Burrow stack, and this interface is its contract. Handles
   are abstract here so they cannot be forged from OCaml (a raw nativeint cast segfaults); the typed
   {!Burrow_store.Store} facade above adds phantom read/write txn types and is what the engine consumes.

   Ownership + threading rules the C side assumes (violations are undefined behavior, not exceptions):
   - ONE write txn at a time per env ({!txn_begin} with [rdonly:false] BLOCKS the whole OS thread — and
     with it the Eio domain — until the writer lock frees; the engine serializes writers above).
   - A txn/cursor must be finished (commit/abort/close) on the thread that uses it, and never used after.
   - {!txn_commit_blocking} releases the OCaml runtime lock around the fsync, so other domains keep
     running — call it from a systhread for durable commits off the Eio scheduler.
   - Read txns are MVCC snapshots and never block ([flag_notls] lets them roam across domains). *)

type env
type txn
type cursor
type dbi

val flag_durable : int

val flag_nometasync : int
(** MDB_NOMETASYNC — sync data, not meta (meta recovered on crash). *)

val flag_nosync : int
(** MDB_NOSYNC — no fsync; flush periodically via {!env_sync}. *)

val flag_notls : int
(** MDB_NOTLS — read txns not pinned to OS-thread TLS; we own reader domains. *)

external env_open : string -> int -> int64 -> int -> env = "ml_env_open"
(** [env_open dir flags map_size max_dbs] — the directory must exist; [flags] ORs the [flag_*] values. *)

external env_close : env -> unit = "ml_env_close"
external env_sync : env -> bool -> unit = "ml_env_sync"
(** [env_sync env force] — releases the runtime lock around the fsync. *)

external env_copy2 : env -> string -> bool -> unit = "ml_env_copy2"
(** [env_copy2 env dir compact] — online hot backup (one MVCC snapshot) into [dir]; writers never pause. *)

external env_usage : env -> int64 * int64 = "ml_env_usage"
(** [(bytes_in_use, map_ceiling)] of the memory map. *)

external txn_begin : env -> bool -> txn = "ml_txn_begin"
(** [txn_begin env rdonly] — a write txn ([rdonly:false]) blocks the calling OS THREAD until the single
    writer lock frees; serialize writers above (see the header). *)

external txn_begin_child : txn -> txn = "ml_txn_begin_child"
(** A nested write txn under a parent — commit applies into the parent, abort discards (per-job isolation
    inside a group commit). *)

external txn_commit : txn -> unit = "ml_txn_commit"
external txn_abort : txn -> unit = "ml_txn_abort"

external txn_commit_blocking : txn -> unit = "ml_txn_commit_blocking"
(** Commit with the runtime lock released around the fsync — run on a systhread so the durable commit
    never stalls the Eio domain. *)

external dbi_open : txn -> string -> bool -> dbi = "ml_dbi_open_named"
(** [dbi_open txn name create] — named sub-DB handles are env-wide once committed. ⚠ [name] is a C string:
    it MUST be NUL-free or it silently truncates and sub-DBs collide (the engine length-prefixes). *)

external drop : txn -> dbi -> bool -> unit = "ml_drop"
(** [drop txn dbi delete] — empty ([false]) or delete ([true]) a sub-DB. *)

external put : txn -> dbi -> string -> string -> unit = "ml_put"
external get : txn -> dbi -> string -> string option = "ml_get"
external del : txn -> dbi -> string -> bool = "ml_del"

external cursor_open : txn -> dbi -> cursor = "ml_cursor_open"
external cursor_close : cursor -> unit = "ml_cursor_close"

external cursor_move : cursor -> int -> (string * string) option = "ml_cursor_move"
(** Step a cursor with one of {!first} / {!next} / {!last} / {!prev}; [None] at the end. *)

external cursor_seek : cursor -> string -> (string * string) option = "ml_cursor_seek"
(** Position at the first key [>=] the probe (LMDB [MDB_SET_RANGE]); [None] past the end. *)

(** the {!cursor_move} op codes *)

val first : int
val next : int
val last : int
val prev : int
