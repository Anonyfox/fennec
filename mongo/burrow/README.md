# Burrow — module map & status

The native, in-process, on-disk MongoDB-compatible engine inside `fennec-mongo` ("SQLite for
MongoDB"). Full design in [`docs/internal/EMBEDDED.md`](../../docs/internal/EMBEDDED.md). This file is
the concrete module map: a strict layered DAG where dependencies point **down only** (dune-enforced —
a cycle is a compile error). All unsafe code and all IO live in the two bottom layers; everything above
is pure values and typed handles.

**Status: implemented and wired in.** Burrow is a runtime-selectable `Backend.S` backend (select with a
`burrow://` `MONGO_URL`), validated by a differential test against minimongo and a concurrency test.

## Libraries (bottom-up)

| lib | dir | role | deps |
|---|---|---|---|
| `burrow_lmdb` | `lmdb/` | vendored LMDB 1.0 + thin owned FFI — the **only** unsafe code | (C) |
| `burrow_codec` | `codec/` | memcomparable key encoding (byte order = `Bson.compare`), prefix-free. **Pure.** | bson |
| `burrow_binary` | `binary/` | `Bson.t` ⇄ record bytes (all 19 BSON types) + zero-copy field reader. **Pure.** | bson |
| `burrow_store` | `store/` | typed-safe LMDB facade: phantom `[`r\|`w]` txn modes, cached named sub-DBs, off-scheduler commit | burrow_lmdb, eio |
| `burrow` | `engine/` | the engine (internal modules below) | store, codec, binary, bson, query, eio |

## `burrow` engine — internal modules (each with its `.mli`)

| module | role | status |
|---|---|---|
| `Catalog` | collection / index / validator metadata (persisted in a meta sub-DB), rebuilt on open | ✅ |
| `Record` | the clustered `_id` record store — record bytes keyed by encoded `_id` | ✅ |
| `Index` | secondary index access method (key++`_id` entries); maintenance + multikey/compound; ascending & descending | ✅ |
| `Plan` | the plan IR — point (`_id`) / index-range list / index-union / index-intersect / collection-scan, with a `reverse` flag | ✅ |
| `Planner` | `(selector, sort) → Plan` — `_id` point, equality/range/compound/`$in`/`$or`/intersection index selection, selectivity scoring, forward+reverse sort-via-index | ✅ |
| `Executor` | interpret a `Plan` → docs; residual `Matcher`; sort/skip/limit; projection; streaming early-termination for sorted+limit; index-only count | ✅ |
| `Write` | insert / update / remove / upsert (`Modifier`) + index maintenance + unique + validator (one txn) | ✅ |
| `Observe` | live observeChanges: post-write recompute + diff → added/changed/removed | ✅ (in-process) |
| `Engine` | open/close lifecycle, single-writer lock, the public API | ✅ |
| `Oplog` | capped change log (`seq → change`); resume tokens for cross-process/resumable change streams | ⬜ deferred |

Query semantics (selector operators, update modifiers, projection, sort, aggregation) are **reused**
from `fennec-mongo.query` — Burrow's job is durable storage, indexing, and access-path selection, so it
inherits minimongo's operator coverage exactly (the differential test proves it).

## The `burrow://` Backend.S adapter — built

`Fennec_mongo_backend.S` is in the `fennec` package; `fennec` depends on `fennec-mongo`, so Burrow
(in `fennec-mongo`) cannot depend *up* on it. The engine therefore exposes a Backend-shaped API, and the
adapter that wraps it lives in **`fennec/pulse/mongo/`** (`fennec_pulse_mongo.ml`), beside the Mini and
Native adapters: an `Embedded` op set + a `Dynamic.Embedded` case. The single `MONGO_URL` parser
(`Fennec_mongo_driver.Runtime.backend`) turns a `burrow://[auth]/<abs-path>` URL into the engine
directory (= base/db, the path's trailing segment is the db) + an optional wire-endpoint spec. One engine
per on-disk database directory (cached), shared across that db's collections; durable by default. (The
five burrow libs are public — `fennec-mongo.burrow{,.store,…}` — so the public pulse backend can depend
on them.)

## Invariants

- Deps point **down only**; the two storage libs own all unsafe + all IO.
- Planner and codecs are **pure** — unit/property-testable with no disk.
- **Single group-committing writer**: a dedicated writer fiber drains all queued writes into one
  transaction (each in an LMDB child txn for per-write failure isolation) and commits **once** — so one
  `F_FULLFSYNC` is amortized across the whole wave (sequential 125 → concurrent 20k+ durable writes/s).
  The fsync runs off-scheduler (`run_in_systhread`); a batch of one skips the child. Readers run on
  **immutable MVCC snapshots** (`MDB_NOTLS`) with no lock — no data races by construction.
- Correctness is proven by **differential testing** (the same ops through Burrow and minimongo must
  agree, with index probes after every step) + property tests for the codecs + a concurrency test.

## Performance (three-way stress test — `fennec/pulse/mongo/bench`)

Identical data + indexes + operations through Burrow (`burrow://`), a real **mongod** (libmongoc),
and **minimongo** (in-memory). 20k docs, release build, `µs/op` (lower is better):

| scenario | Burrow | mongod | minimongo | Burrow vs mongod |
|---|---:|---:|---:|---:|
| point by `_id` | 1.5 | 46 | 0.1 | **30×** |
| equality on uid (~10 hits) | 5.6 | 83 | 1360 | **15×** |
| equality on status (~4000 hits) | 3464 | 11882 | 1646 | **3×** |
| range on score (~1000 hits) | 785 | 2952 | 2164 | **4×** |
| sort `created` desc, limit 20 | 6.9 | 96 | 5883 | **14×** |
| feed: `status=k` sort `created` desc, limit 20 | 8.4 | 104 | 2472 | **12×** |
| count `status=k` | 123 | 541 | 1529 | **4×** |
| update one by `_id` (`$inc`) | 16 | 59 | 0.5 | **4×** |
| bulk insert (one-by-one) | 44.5k/s | 20.3k/s | 1.46M/s | **2.2×** |

Burrow beats real mongod on **every** scenario (no wire/serialization — it's in-process). It also beats
minimongo wherever a real index beats an in-memory scan (selective equality 240×, sorted-limit 860×,
the feed 280×, count 12×). minimongo wins on broad scans / point / update because it's a pure in-memory
store with no record decode or durability — a different tier; Burrow trades that for persistence and
still outruns the database it replaces. (Query durability is irrelevant; the writes above use `No_sync`,
comparable to mongod's default `w:1` async-journal.)

**Durable writes (`Full` = `F_FULLFSYNC`, `engine/bench/bench_durable`):** a sequential writer pays one
fsync per write (~125/s), but the group-committing writer fiber amortizes one fsync across each
concurrent wave — **20k+ durable writes/s at 200 concurrent writers (≈160× the sequential rate)**, all
verified durable. So the embedded-as-prod tier scales vertically under write load by default.

## Connecting with mongosh (the wire endpoint)

The embedded engine can be **exposed over the MongoDB wire protocol** so any real client — mongosh,
Compass, an official driver — connects exactly as it would to a hosted mongod and runs ad-hoc queries
against the app's live data. It is built as a clean separate layer, [`fennec-mongo.wire`](../wire/README.md)
(pure: spec BSON codec + OP_MSG/OP_QUERY framing + SCRAM-SHA-256) with the Eio TCP server + the one-liner
in `fennec.pulse.mongo`:

```ocaml
Fennec_mongo_dynamic.expose ~sw ~net:(Eio.Stdenv.net env)
  ~users:[ Fennec_mongo_dynamic.wire_user ~user:"admin" ~password:secret () ] ()
  (* scope a user with roles: ~roles:[ Role.Read "reports"; Role.Read_write "app" ] () — default is Root *)
(* then: mongosh "mongodb://admin:secret@127.0.0.1:27017" *)
```

Secure by default (loopback bind, SCRAM-SHA-256 required when any user is set, no `$where`/JS, capped
message size, optional read-only + TLS, per-database role-based authorization enforced at dispatch, and a
security-audit sink (`expose ~audit`) for authentication + denial events). It shares this engine's per-directory cache, so writes from
mongosh funnel through the same group-committing writer (no concurrent-writer hazard). Validated against
the **real libmongoc driver**, **real mongosh**, and **over TLS** (full CRUD + SCRAM). See the wire
README for details.

**Encryption at rest:** the data directory is a plain LMDB file, so the recommended zero-overhead approach
is **volume / filesystem-level encryption** (LUKS / FileVault / cloud-volume EBS/PD) — the file is opaque on
disk with no engine cost. Engine-level page encryption (the database custodying its own keys) is a deferred
opt-in for compliance regimes that specifically require app-level key custody.

In **dev**, this is automatic: `fennec dev` defaults `MONGO_URL` to a `burrow://localhost:27017/…` URL
(no mongod), and the loopback authority makes the framework auto-open an unauthenticated mongosh endpoint
— `mongosh "mongodb://localhost:27017"` just works. The **e2e/system harness** runs against the embedded
engine for free, isolated per scenario: it pins `MONGO_URL` to a storage-only `burrow:///<sandbox>/…` URL
(no authority ⇒ no endpoint to fight for a port). Production sets `MONGO_URL` explicitly — including the
authority + credentials when it wants a remote mongosh endpoint. The full grammar is in the
[wire README](../wire/README.md).

## Backup & restore

`Engine.backup db ~dir ()` takes a **consistent, online hot copy** of the whole database into a fresh
directory — LMDB's `mdb_env_copy2`, compacting by default. The copy reads its own MVCC snapshot, so
**writers are never paused**: it is a zero-downtime backup, and the snapshot is exactly the state at call
time (writes that land mid-copy don't appear). The long copy runs off the Eio scheduler on a systhread, so
the calling fiber suspends without stalling the domain — the same discipline as the durable commit's fsync.

The result is **a complete, openable Burrow database**, not an opaque dump — so *restore* needs no special
path: point `Engine.open_` at the backup directory to read it directly, or (offline) swap it into the data
directory and reopen. Because compaction copies only live pages, a backup is also a defrag.

```ocaml
Engine.backup db ~dir:"/backups/2026-06-30" () ;      (* compacting hot copy — writers keep running *)
let restored = Engine.open_ ~sw "/backups/2026-06-30"  (* the copy is just a database *)
```

Scheduling, retention, off-host shipping, and periodic restore verification are operational policy that
layer on this primitive (see `PRODUCTION.md`); point-in-time recovery beyond the snapshot awaits the oplog.

## Known limitations (future work)

- **GridFS** — done, as `fennec-mongo.gridfs`: a pure functor (standard `fs.files`/`fs.chunks`, 255 KiB
  chunks, wire-compatible with mongod) over the same minimal store seam, so it runs over minimongo /
  Burrow / mongod and cross-compiles to JS. Client pattern: file metadata syncs reactively to the
  browser; bytes are served from the server's store over HTTP (not pumped through browser minimongo).
- **Oplog + change streams + PITR** — DONE: a capped, LSN-ordered change log (`Engine.oplog_tail` /
  `oplog_lsn`), every committed write logged idempotently in its own txn, crash-resumed; the wire
  `$changeStream` (`db.coll.watch()`) tailing it with LSN resume tokens; and point-in-time recovery (restore
  a backup + replay the oplog forward via `Engine.oplog_apply`, within the retention window); and async
  replication — the `Burrow.Replica` follower plus a wire transport (the `oplogFetch` command + `Wire_client`
  + `Replication.pull`), so a follower converges to a remote source over the MongoDB wire — authenticating
  with SCRAM-SHA-256 (mutual auth) or plaintext on a trusted network. A lagged follower detects it is too
  stale to tail (`Replica.too_stale` against the `oplogFetch` retention floor) and must re-sync. Remaining:
  read-preference routing, and DDL logging (see
  `PRODUCTION.md` §5).
- **GridFS** — large-blob storage is a driver-level convention; the engine stores large values fine.
- **Planner** — equality / range / compound / `$in` / `$or` (index-union) / intersection selection,
  selectivity scoring, forward+reverse sort-via-index (streaming early-termination for sorted+limit),
  and index-only count are all done. Special index types (text / geo) fall back to a collection scan
  (still correct via the residual matcher).
- **Covering-index reads** (serving a projection straight from the index without fetching the record)
  are a deliberate NON-goal: index keys are memcomparable-encoded (lossy for numbers — the sortable
  double can't be turned back into the exact Int/Int64/Float), so the original values aren't
  reconstructable. Serving them would mean storing redundant uncompressed values in every index entry —
  a write/space cost paid on every write for a modest read win (the clustered record store is a single
  cheap point-get away), so it isn't worth it for this design.
- A multi-field / operator-residual `count` still fetches matching records; a single-field count fully
  captured by a non-multikey index is served from index entries (no fetch). (See `engine/bench`.)
- **Map growth** — fixed (default 128 GB) virtual map (sparse; grows on disk as used). `Engine.usage`
  reports bytes-in-use vs the ceiling so an operator can alarm and resize at `open_`; online
  `MDB_MAP_FULL` grow-and-retry is deferred (blocked by LMDB's no-active-txns constraint).
- **`Decimal128`** stored as its canonical string (round-trips losslessly; not the 16-byte wire form);
  composite (`Document`/`Array`) `_id` raises a clear error (scalar `_id`s fully supported).
