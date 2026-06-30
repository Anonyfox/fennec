/* OCaml <-> LMDB glue — the single audited FFI surface for Burrow. Handles are boxed nativeint
   pointers; every value crosses as a COPY out of the mmap, so the safe layer ({!Store}) can never
   hold a dangling pointer into a closed transaction. Commit and sync have lock-releasing variants
   (caml_enter/leave_blocking_section) so the safe layer can run the ~8 ms F_FULLFSYNC off the Eio
   scheduler via Eio_unix.run_in_systhread: the calling fiber suspends, the worker thread runs the
   sync with the runtime lock released, and the domain keeps scheduling other fibers meanwhile. */
#include "lmdb.h"
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <string.h>

#define Env_val(v) ((MDB_env *)Nativeint_val(v))
#define Txn_val(v) ((MDB_txn *)Nativeint_val(v))
#define Cur_val(v) ((MDB_cursor *)Nativeint_val(v))

static void ck(int rc) { if (rc != MDB_SUCCESS) caml_failwith(mdb_strerror(rc)); }

/* --- environment ---------------------------------------------------------------------------- */

CAMLprim value ml_env_open(value path, value flags, value mapsize, value maxdbs) {
  CAMLparam4(path, flags, mapsize, maxdbs);
  MDB_env *env;
  ck(mdb_env_create(&env));
  ck(mdb_env_set_mapsize(env, (size_t)Int64_val(mapsize)));
  ck(mdb_env_set_maxdbs(env, (MDB_dbi)Int_val(maxdbs)));
  int rc = mdb_env_open(env, String_val(path), (unsigned)Int_val(flags), 0664);
  if (rc != MDB_SUCCESS) { mdb_env_close(env); caml_failwith(mdb_strerror(rc)); }
  CAMLreturn(caml_copy_nativeint((intnat)env));
}

CAMLprim value ml_env_close(value env) {
  CAMLparam1(env);
  mdb_env_close(Env_val(env));
  CAMLreturn(Val_unit);
}

/* Flush the mmap to disk explicitly (the No_sync durability mode's periodic flush). Lock released. */
CAMLprim value ml_env_sync(value env, value force) {
  CAMLparam2(env, force);
  MDB_env *e = Env_val(env);
  int f = Bool_val(force);
  caml_enter_blocking_section();
  int rc = mdb_env_sync(e, f);
  caml_leave_blocking_section();
  ck(rc);
  CAMLreturn(Val_unit);
}

/* Online hot-backup: copy the whole environment to [path] (an existing directory) as a standalone,
   openable database. mdb_env_copy2 reads a consistent MVCC snapshot via its own read txn, so WRITERS
   ARE NOT PAUSED — a zero-downtime backup; [compact] (MDB_CP_COMPACT) copies only live pages. The path
   is read into a C buffer BEFORE the blocking section (no OCaml value is touched inside it, as the GC
   may move the heap), then the lock is released around the (long) copy — drive via run_in_systhread. */
CAMLprim value ml_env_copy2(value env, value path, value compact) {
  CAMLparam3(env, path, compact);
  MDB_env *e = Env_val(env);
  unsigned flags = Bool_val(compact) ? MDB_CP_COMPACT : 0;
  char buf[4096];
  size_t n = caml_string_length(path);
  if (n >= sizeof(buf)) caml_failwith("burrow: backup path too long");
  memcpy(buf, String_val(path), n);
  buf[n] = '\0';
  caml_enter_blocking_section();
  int rc = mdb_env_copy2(e, buf, flags);
  caml_leave_blocking_section();
  if (rc != MDB_SUCCESS) caml_failwith(mdb_strerror(rc));
  CAMLreturn(Val_unit);
}

/* Map usage: bytes in use ((last used page + 1) * page size) and the configured map ceiling — for
   alarming before MDB_MAP_FULL. Both reads are in-memory env metadata (no I/O), so no blocking section. */
CAMLprim value ml_env_usage(value env) {
  CAMLparam1(env);
  CAMLlocal3(res, used_v, map_v);
  MDB_env *e = Env_val(env);
  MDB_envinfo info;
  MDB_stat st;
  if (mdb_env_info(e, &info) != MDB_SUCCESS || mdb_env_stat(e, &st) != MDB_SUCCESS)
    caml_failwith("burrow: env usage query failed");
  size_t used = ((size_t)info.me_last_pgno + 1) * (size_t)st.ms_psize;
  used_v = caml_copy_int64((int64_t)used);
  map_v = caml_copy_int64((int64_t)info.me_mapsize);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, used_v);
  Store_field(res, 1, map_v);
  CAMLreturn(res);
}

/* --- transactions --------------------------------------------------------------------------- */

CAMLprim value ml_txn_begin(value env, value rdonly) {
  CAMLparam2(env, rdonly);
  MDB_txn *txn;
  ck(mdb_txn_begin(Env_val(env), NULL, Bool_val(rdonly) ? MDB_RDONLY : 0, &txn));
  CAMLreturn(caml_copy_nativeint((intnat)txn));
}

CAMLprim value ml_txn_commit(value txn) { CAMLparam1(txn); ck(mdb_txn_commit(Txn_val(txn))); CAMLreturn(Val_unit); }
CAMLprim value ml_txn_abort(value txn)  { CAMLparam1(txn); mdb_txn_abort(Txn_val(txn));  CAMLreturn(Val_unit); }

/* Begin a nested (child) write transaction under [parent]. Committing the child merges its changes
   into the parent; aborting discards just the child's — so each write in a group commit is isolated
   (one failure doesn't sink the batch), and a single parent commit fsyncs the whole batch at once.
   Only one child may be open per parent at a time (the group-committing writer runs them serially). */
CAMLprim value ml_txn_begin_child(value parent) {
  CAMLparam1(parent);
  MDB_txn *p = Txn_val(parent);
  MDB_txn *child;
  ck(mdb_txn_begin(mdb_txn_env(p), p, 0, &child));
  CAMLreturn(caml_copy_nativeint((intnat)child));
}

/* Commit with the runtime lock RELEASED around the (~8 ms F_FULLFSYNC) commit. The txn pointer is
   read out BEFORE the blocking section; no OCaml value is touched inside it. Drive via
   Eio_unix.run_in_systhread so the writer fiber suspends and the domain keeps running. */
CAMLprim value ml_txn_commit_blocking(value txn) {
  CAMLparam1(txn);
  MDB_txn *t = Txn_val(txn);
  caml_enter_blocking_section();
  int rc = mdb_txn_commit(t);
  caml_leave_blocking_section();
  if (rc != MDB_SUCCESS) caml_failwith(mdb_strerror(rc));
  CAMLreturn(Val_unit);
}

/* --- named sub-databases -------------------------------------------------------------------- */

/* Open (create when [create]) a named sub-DB; returns its dbi handle. Per LMDB the handle is private
   to this txn until it commits, then shared env-wide — so the caller must only cache it on commit. */
CAMLprim value ml_dbi_open_named(value txn, value name, value create) {
  CAMLparam3(txn, name, create);
  MDB_dbi dbi;
  unsigned flags = Bool_val(create) ? MDB_CREATE : 0;
  ck(mdb_dbi_open(Txn_val(txn), String_val(name), flags, &dbi));
  CAMLreturn(Val_int((int)dbi));
}

/* Empty a sub-DB ([del]=false: keep the handle) or delete it entirely ([del]=true). Used to clear an
   index's entries on drop/recreate so stale entries can't survive. */
CAMLprim value ml_drop(value txn, value dbi, value del) {
  CAMLparam3(txn, dbi, del);
  ck(mdb_drop(Txn_val(txn), (MDB_dbi)Int_val(dbi), Bool_val(del) ? 1 : 0));
  CAMLreturn(Val_unit);
}

/* --- point operations ----------------------------------------------------------------------- */

CAMLprim value ml_put(value txn, value dbi, value key, value data) {
  CAMLparam4(txn, dbi, key, data);
  MDB_val k = { caml_string_length(key), (void *)String_val(key) };
  MDB_val v = { caml_string_length(data), (void *)String_val(data) };
  ck(mdb_put(Txn_val(txn), (MDB_dbi)Int_val(dbi), &k, &v, 0));
  CAMLreturn(Val_unit);
}

CAMLprim value ml_get(value txn, value dbi, value key) {
  CAMLparam3(txn, dbi, key);
  CAMLlocal2(res, some);
  MDB_val k = { caml_string_length(key), (void *)String_val(key) }, v;
  int rc = mdb_get(Txn_val(txn), (MDB_dbi)Int_val(dbi), &k, &v);
  if (rc == MDB_NOTFOUND) CAMLreturn(Val_int(0)); /* None */
  ck(rc);
  some = caml_alloc_string(v.mv_size);
  memcpy((void *)Bytes_val(some), v.mv_data, v.mv_size);
  res = caml_alloc(1, 0); /* Some _ */
  Store_field(res, 0, some);
  CAMLreturn(res);
}

CAMLprim value ml_del(value txn, value dbi, value key) {
  CAMLparam3(txn, dbi, key);
  MDB_val k = { caml_string_length(key), (void *)String_val(key) };
  int rc = mdb_del(Txn_val(txn), (MDB_dbi)Int_val(dbi), &k, NULL);
  if (rc == MDB_NOTFOUND) CAMLreturn(Val_false);
  ck(rc);
  CAMLreturn(Val_true);
}

/* --- cursors (ordered range scan) ----------------------------------------------------------- */

CAMLprim value ml_cursor_open(value txn, value dbi) {
  CAMLparam2(txn, dbi);
  MDB_cursor *cur;
  ck(mdb_cursor_open(Txn_val(txn), (MDB_dbi)Int_val(dbi), &cur));
  CAMLreturn(caml_copy_nativeint((intnat)cur));
}

CAMLprim value ml_cursor_close(value cursor) {
  CAMLparam1(cursor);
  mdb_cursor_close(Cur_val(cursor));
  CAMLreturn(Val_unit);
}

/* Build [Some (key, data)] copying both out of the mmap, or [None] on NOTFOUND. [k]/[v] point into
   LMDB pages (outside the OCaml heap), so the allocations here can't invalidate them; [mk] is rooted
   across the [mv] allocation by CAMLlocal. */
static value cursor_result(int rc, MDB_val *k, MDB_val *v) {
  CAMLparam0();
  CAMLlocal4(res, mk, mv, pair);
  if (rc == MDB_NOTFOUND) CAMLreturn(Val_int(0)); /* None */
  ck(rc);
  mk = caml_alloc_string(k->mv_size);
  memcpy((void *)Bytes_val(mk), k->mv_data, k->mv_size);
  mv = caml_alloc_string(v->mv_size);
  memcpy((void *)Bytes_val(mv), v->mv_data, v->mv_size);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, mk);
  Store_field(pair, 1, mv);
  res = caml_alloc(1, 0); /* Some _ */
  Store_field(res, 0, pair);
  CAMLreturn(res);
}

/* op: 0=FIRST 1=NEXT 2=LAST 3=PREV — forward and (for sort-desc) reverse traversal. */
CAMLprim value ml_cursor_move(value cursor, value opv) {
  CAMLparam2(cursor, opv);
  MDB_cursor_op op;
  switch (Int_val(opv)) {
    case 0: op = MDB_FIRST; break;
    case 1: op = MDB_NEXT;  break;
    case 2: op = MDB_LAST;  break;
    case 3: op = MDB_PREV;  break;
    default: op = MDB_NEXT; break;
  }
  MDB_val k, v;
  int rc = mdb_cursor_get(Cur_val(cursor), &k, &v, op);
  CAMLreturn(cursor_result(rc, &k, &v));
}

/* Position at the first key >= [key] (MDB_SET_RANGE) — the start of an index range scan. */
CAMLprim value ml_cursor_seek(value cursor, value key) {
  CAMLparam2(cursor, key);
  MDB_val k = { caml_string_length(key), (void *)String_val(key) }, v;
  int rc = mdb_cursor_get(Cur_val(cursor), &k, &v, MDB_SET_RANGE);
  CAMLreturn(cursor_result(rc, &k, &v));
}
