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
| `Index` | secondary index access method (key++`_id` entries); maintenance + multikey/compound | ✅ |
| `Plan` | the plan IR — point (`_id`) / index-range / collection-scan | ✅ |
| `Planner` | `(selector, sort) → Plan` — `_id` point + equality/range index selection | ✅ (sort-via-index, selectivity heuristic, `$or`/intersection: future) |
| `Executor` | interpret a `Plan` → docs; residual `Matcher`; sort/skip/limit; projection | ✅ |
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
- **Single writer**: all writes serialize through an Eio mutex (a contended LMDB write-txn begin would
  block the whole domain); commit runs off-scheduler via `run_in_systhread`. Readers run on **immutable
  MVCC snapshots** (`MDB_NOTLS`) with no lock — no data races by construction.
- Correctness is proven by **differential testing** (the same ops through Burrow and minimongo must
  agree, with index probes after every step) + property tests for the codecs + a concurrency test.

## Known limitations (future work)

- **Oplog / resumable change streams** — in-process `observe_changes` works; resume-token-based change
  streams across reconnects/restarts are deferred.
- **GridFS** — large-blob storage is a driver-level convention; the engine stores large values fine.
- **Planner** — sort-via-index, `$or`/index-intersection, covering indexes, selectivity costing.
- **Index** — descending-field *range* (asc range + any-direction equality work); special index types
  (text/geo) fall back to collection scan (still correct via the residual matcher).
- **Map growth** — fixed 128 GB virtual map (sparse; grows on disk as used); online `MDB_MAP_FULL`
  grow-and-retry is deferred (blocked by LMDB's no-active-txns constraint).
- **`Decimal128`** stored as its canonical string (round-trips losslessly; not the 16-byte wire form);
  composite (`Document`/`Array`) `_id` raises a clear error (scalar `_id`s fully supported).
