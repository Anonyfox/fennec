# Burrow — module map & status

The native, in-process, on-disk MongoDB-compatible engine inside `fennec-mongo` ("SQLite for
MongoDB"). Full design in [`docs/internal/EMBEDDED.md`](../../docs/internal/EMBEDDED.md). This file is
the concrete module map: a strict layered DAG where dependencies point **down only** (dune-enforced —
a cycle is a compile error). All unsafe code and all IO live in the two bottom layers; everything above
is pure values and typed handles.

**Status: implemented and wired in.** Burrow is a runtime-selectable `Backend.S` backend (select with a
`:embedded:` `MONGO_URL`), validated by a differential test against minimongo and a concurrency test.

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

## The `:embedded:` Backend.S adapter — built

`Fennec_pulse.Backend.S` is in the `fennec` package; `fennec` depends on `fennec-mongo`, so Burrow
(in `fennec-mongo`) cannot depend *up* on it. The engine therefore exposes a Backend-shaped API, and the
adapter that wraps it lives in **`fennec/pulse/mongo/`** (`fennec_pulse_mongo.ml`), beside the Mini and
Native adapters: an `Embedded` op set + a `Dynamic.Embedded` case + `:embedded:` `MONGO_URL` parsing in
`Dynamic.from_env`. One engine per on-disk database directory (cached), shared across that db's
collections; durable by default. (The five burrow libs are public — `fennec-mongo.burrow{,.store,…}` —
so the public pulse backend can depend on them.)

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

Identical data + indexes + operations through Burrow (`:embedded:`), a real **mongod** (libmongoc),
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

## Known limitations (future work)

- **GridFS** — done, as `fennec-mongo.gridfs`: a pure functor (standard `fs.files`/`fs.chunks`, 255 KiB
  chunks, wire-compatible with mongod) over the same minimal store seam, so it runs over minimongo /
  Burrow / mongod and cross-compiles to JS. Client pattern: file metadata syncs reactively to the
  browser; bytes are served from the server's store over HTTP (not pumped through browser minimongo).
- **Oplog / resumable change streams** — in-process `observe_changes` works; resume-token-based change
  streams across reconnects/restarts are deferred.
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
- **Map growth** — fixed 128 GB virtual map (sparse; grows on disk as used); online `MDB_MAP_FULL`
  grow-and-retry is deferred (blocked by LMDB's no-active-txns constraint).
- **`Decimal128`** stored as its canonical string (round-trips losslessly; not the 16-byte wire form);
  composite (`Document`/`Array`) `_id` raises a clear error (scalar `_id`s fully supported).
