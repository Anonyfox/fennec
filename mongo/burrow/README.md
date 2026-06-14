# Burrow — module map (the anti-spaghetti guideline)

The native, in-process, on-disk MongoDB-compatible engine, inside `fennec-mongo`. Full design in
[`docs/internal/EMBEDDED.md`](../../docs/internal/EMBEDDED.md). This file is the concrete module map:
a strict layered DAG where dependencies point **down only** (dune-enforced — a cycle is a compile
error). All unsafe code and all IO live in the two bottom layers; everything above is pure values and
typed handles.

## Libraries (bottom-up)

| lib | dir | role | deps |
|---|---|---|---|
| `burrow_lmdb` | `lmdb/` | vendored LMDB + thin owned FFI — the **only** unsafe code | (C) |
| `burrow_codec` | `codec/` | memcomparable key encoding (byte order = `Bson.compare`). **Pure.** | bson |
| `burrow_binary` | `binary/` | `Bson.t` ⇄ BSON bytes + zero-copy field reader. **Pure.** | bson |
| `burrow_store` | `store/` | typed-safe LMDB facade: phantom `[`r\|`w]` txn modes, typed `('k,'v) dbi`, grow-and-retry | burrow_lmdb |
| `burrow` | `engine/` | the engine (internal modules below) | store, codec, binary, bson, query, eio |

## `burrow` engine — internal modules (each with its `.mli`)

| module | role | purity |
|---|---|---|
| `Catalog` | collection / index / option metadata (persisted in a meta db), rebuilt on open | IO |
| `Record` | the clustered `_id` record store — BSON bytes keyed by encoded `_id` | IO |
| `Index` | secondary index access method (`Key_codec` keys → `_id`); maintenance + range scan | IO |
| `Plan` | the plan IR — the chosen access path (point / index-range / collection-scan) | pure |
| `Planner` | `(selector, sort) → Plan` — index selection, sort-via-index, selectivity heuristic | **pure** |
| `Executor` | interpret a `Plan` → doc stream; residual `Matcher`; sort/skip/limit; projection | IO |
| `Write` | insert / update / remove / upsert (`Modifier`) + index maintenance + oplog append (one txn) | IO |
| `Oplog` | capped change log (`seq → change`); resume tokens = `seq` | IO |
| `Observe` | change streams over the oplog (`Fanout`) — the reactive primitive | IO |
| `Engine` | open/close lifecycle, the single group-committing writer fiber, the public API | IO |

## Where the `:embedded:` Backend.S adapter lives

**Not here.** `Fennec_pulse.Backend.S` is in the `fennec` package, and `fennec` depends on
`fennec-mongo` — so `fennec-mongo` (where Burrow lives) cannot depend *up* on it. The engine exposes a
Backend-shaped API (insert/update/remove/find/find_one/count/aggregate/distinct/observe_changes/
ensure_index/…); the thin adapter that wraps it as a `Backend.S` (and the `Dynamic.Embedded` case +
`Runtime` `:embedded:` parsing) lives in `fennec/pulse/mongo/`, beside the Mini and Native adapters.

## Invariants

- Deps point **down only**; the two storage libs own all unsafe + all IO.
- Planner and codecs are **pure** — unit/property-testable with no disk.
- **Single writer** (group-commit, off-scheduler via `run_in_systhread`); readers on **immutable MVCC
  snapshots** (`MDB_NOTLS`) — no data races by construction.
- Correctness is proven by **differential testing** (the same ops through minimongo, Burrow, and a
  real mongod must agree) + property tests for the codecs.
