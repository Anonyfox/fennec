(* See store.mli for the contract. The phantom ['m txn] tag is erased at runtime — a [txn] is just the
   raw [Burrow_lmdb.txn] nativeint; the type checker alone enforces that [put]/[del] need a [`W]. *)

module L = Burrow_lmdb

type durability = Full | No_meta_sync | No_sync

type t = {
  env : L.env;
  durability : durability;
  dbis : (string, L.dbi) Hashtbl.t; (* name -> handle, opened once and cached env-wide *)
  dbis_mutex : Mutex.t;             (* guards [dbis] and serializes first-open write txns *)
}

type +'m txn = L.txn constraint 'm = [< `R | `W ]

type db = L.dbi

let gib = 1073741824L
let gib_bytes gb = Int64.mul (Int64.of_int gb) gib

let env_flags_of = function
  | Full -> L.flag_durable
  | No_meta_sync -> L.flag_nometasync
  | No_sync -> L.flag_nosync

(* MDB_NOTLS is always on: read txns aren't pinned to OS-thread TLS, so a reader domain (or a fiber
   that migrates threads) can hold a snapshot safely — the basis of the lock-free multicore reads. *)
let open_ ?(map_size_gb = 64) ?(max_dbs = 128) ?(durability = Full) path =
  let flags = env_flags_of durability lor L.flag_notls in
  let env = L.env_open path flags (gib_bytes map_size_gb) max_dbs in
  { env; durability; dbis = Hashtbl.create 32; dbis_mutex = Mutex.create () }

let close t = L.env_close t.env
let durability t = t.durability

(* Open-or-fetch a named sub-DB handle. The common (cached) path takes no transaction. A first open
   runs a short standalone write txn (LMDB requires a txn to open a dbi, and MDB_CREATE a write one);
   the handle is cached only after that txn commits, when LMDB makes it valid env-wide. MUST be called
   outside any open transaction on this fiber — LMDB forbids nesting write txns on one thread. *)
let db t name =
  Mutex.lock t.dbis_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.dbis_mutex) @@ fun () ->
  match Hashtbl.find_opt t.dbis name with
  | Some d -> d
  | None ->
    let txn = L.txn_begin t.env false in
    let d =
      match L.dbi_open txn name true with
      | d -> L.txn_commit txn; d
      | exception e -> L.txn_abort txn; raise e
    in
    Hashtbl.replace t.dbis name d;
    d

let read t f =
  let txn = L.txn_begin t.env true in
  (* a read txn only takes a snapshot; abort releases it (cheaper than commit, same effect) *)
  match f txn with
  | x -> L.txn_abort txn; x
  | exception e -> L.txn_abort txn; raise e

let write t ?(blocking = true) f =
  let txn = L.txn_begin t.env false in
  match f txn with
  | x ->
    (* No_sync commits do no fsync, so a systhread hand-off is pure overhead — commit inline. Full /
       No_meta_sync do an ~8 ms F_FULLFSYNC, so run that off the scheduler when [blocking]. *)
    (if blocking && t.durability <> No_sync then Eio_unix.run_in_systhread (fun () -> L.txn_commit_blocking txn)
     else L.txn_commit txn);
    x
  | exception e ->
    L.txn_abort txn;
    raise e

(* group-commit primitives: open ONE parent write txn, run each write in a child txn (commit or abort
   it independently — failure isolation), then commit the parent ONCE (one fsync for the whole batch). *)
let begin_write t : [ `W ] txn = L.txn_begin t.env false
let child (txn : [ `W ] txn) : [ `W ] txn = L.txn_begin_child txn
let commit_child (txn : [ `W ] txn) = L.txn_commit txn (* merge the child's changes into the parent *)
let abort (txn : [ `W ] txn) = L.txn_abort txn

let commit_durable t (txn : [ `W ] txn) =
  if t.durability = No_sync then L.txn_commit txn
  else Eio_unix.run_in_systhread (fun () -> L.txn_commit_blocking txn)

let get txn db key = L.get txn db key
let put txn db key data = L.put txn db key data
let del txn db key = L.del txn db key
let clear txn db = L.drop txn db false

(* Forward scan from an optional lower bound. Opening/closing the cursor brackets the loop; [f]
   decides when to stop (e.g. once the key leaves the index range). *)
let iter txn db ?from ?(rev = false) f =
  let cur = L.cursor_open txn db in
  Fun.protect ~finally:(fun () -> L.cursor_close cur) @@ fun () ->
  let start =
    if rev then L.cursor_move cur L.last
    else match from with Some k -> L.cursor_seek cur k | None -> L.cursor_move cur L.first
  in
  let step = if rev then L.prev else L.next in
  let rec loop = function None -> () | Some (key, data) -> if f ~key ~data then loop (L.cursor_move cur step) in
  loop start
