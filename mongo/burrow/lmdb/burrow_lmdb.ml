(* The owned LMDB binding — the single, deliberately-thin, unsafe FFI surface. Handles are boxed
   [nativeint] pointers. The safe, typed layer (phantom [`r|`w] txn modes, cached named handles,
   grow-and-retry) lives one level up in {!Store}; nothing else in the engine touches this module.
   Every potentially-blocking primitive (commit, sync) has a variant that releases the runtime lock so
   {!Store} can run it off the Eio scheduler. Values cross the boundary as copies — never pointers
   into a transaction's mmap — so use-after-free across a closed txn is impossible by construction. *)

type env = nativeint
type txn = nativeint
type cursor = nativeint
type dbi = int

(* path, env_flags, map_size_bytes, max_named_dbs *)
external env_open : string -> int -> int64 -> int -> env = "ml_env_open"
external env_close : env -> unit = "ml_env_close"
external env_sync : env -> bool -> unit = "ml_env_sync" (* force? — releases the lock around fsync *)

external txn_begin : env -> bool -> txn = "ml_txn_begin" (* rdonly? *)
external txn_begin_child : txn -> txn = "ml_txn_begin_child" (* nested write txn under a parent *)
external txn_commit : txn -> unit = "ml_txn_commit"
external txn_abort : txn -> unit = "ml_txn_abort"

(* commit with the runtime lock released around the fsync — drive via Eio_unix.run_in_systhread *)
external txn_commit_blocking : txn -> unit = "ml_txn_commit_blocking"

(* open/create a named sub-DB; the handle is private to [txn] until it commits, then env-wide *)
external dbi_open : txn -> string -> bool -> dbi = "ml_dbi_open_named" (* name, create? *)
external drop : txn -> dbi -> bool -> unit = "ml_drop" (* empty (false) or delete (true) a sub-DB *)

external put : txn -> dbi -> string -> string -> unit = "ml_put"
external get : txn -> dbi -> string -> string option = "ml_get"
external del : txn -> dbi -> string -> bool = "ml_del"

external cursor_open : txn -> dbi -> cursor = "ml_cursor_open"
external cursor_close : cursor -> unit = "ml_cursor_close"
external cursor_move : cursor -> int -> (string * string) option = "ml_cursor_move"
external cursor_seek : cursor -> string -> (string * string) option = "ml_cursor_seek"

(* [cursor_move] ops *)
let first = 0
let next = 1
let last = 2
let prev = 3

(* env flags (subset of LMDB's; see lmdb.h) *)
let flag_durable = 0
let flag_nometasync = 0x40000 (* MDB_NOMETASYNC — sync data, not meta (meta recovered on crash) *)
let flag_nosync = 0x10000 (* MDB_NOSYNC — no fsync; flush periodically via [env_sync] *)
let flag_notls = 0x200000 (* MDB_NOTLS — read txns not pinned to OS-thread TLS; we own reader domains *)
