# EMBEDDED — a native, in-process MongoDB engine (codename: Burrow)

> Status: **built and shipped.** Burrow is implemented, differential-tested vs minimongo + mongod, and
> wired in behind `Backend.S`. A mongosh/driver wire endpoint and GridFS exist too.
>
> **URL scheme update (supersedes this doc's `:embedded:` / `:port:` names):** the implemented selector
> is a single `MONGO_URL` with three schemes — `:memory:` (minimongo), `burrow://[user:pass@][host:port]
> /<abs-path>[?tls&readonly]` (this engine; the path's trailing segment is the db, and an authority opens
> the mongosh wire endpoint there — so `:embedded:` and `:port:` are now ONE `burrow://` URL), and
> `mongodb://…` (real server). Parsed once in `Fennec_mongo_driver.Runtime.backend`. The grammar lives in
> [`mongo/wire/README.md`](../../mongo/wire/README.md); the rest of this doc keeps the original
> `:embedded:`/`:port:` terminology for the design narrative.
>
> **Module relocation:** the seam and the runtime selector now live in the mongo package — `Backend.S` in
> `fennec-mongo.backend` (`mongo/backend/`) and `Fennec_mongo_dynamic.Dynamic` (with the `Embedded` case)
> plus the wire server in `fennec-mongo.dynamic` (`mongo/dynamic/`). `Fennec.serve` auto-boots the data
> layer (no `Pulse.start`). Where this doc says `Fennec_pulse.Backend` / `Fennec_pulse_mongo`, read
> `Fennec_mongo_backend` / `Fennec_mongo_dynamic`.

## 1. What this is

The middle gap in Fennec's Mongo story. We already have the two ends and the semantics:

- **minimongo** (`mongo/mem/`) — in-memory, pure, compiles to JS for the browser cache. Test/dev only.
- **the libmongoc driver** (`mongo/driver/`, `mongo/ffi/`) — talks to a *real* mongod over the wire;
  full fidelity, but an external process to install, manage, and serialize to over a socket.
- **the pure engine** (`mongo/query/`) — matcher / modifier / projection / sorter / aggregate / expr /
  geo / diff. The *document semantics*, already written and tested.

**Burrow** is a durable, on-disk, **in-process** MongoDB-compatible storage engine: SQLite-for-MongoDB.
It owns its files, persists across restarts, runs inside the app's Eio domains, and — because Mongo is
a far simpler target than SQL and we already own the query semantics — can be **faster than a real
mongod for local/single-node workloads** by eliminating IPC and the wire round-trip entirely.

Two front doors, one engine:

- **`:embedded:`** (the primary use case) — `MONGO_URL=:embedded:/path/to/data`. In-process, direct
  engine calls, no socket, no protocol, `Bson.t` stays native end to end. Fastest, simplest.
- **`:port:`** — wrap the engine in a process that speaks the **MongoDB wire protocol**, so *any*
  official driver (and our own libmongoc path) connects to it unchanged. This is the
  client-compatibility deliverable; it is a thin server over the same engine.

It drops in behind the existing `Fennec_mongo_backend.S` seam: `Runtime` learns `:embedded:`/`:port:`
sentinels next to `:memory:`, `Fennec_mongo_dynamic.Dynamic` gains an `Embedded` case, and **Pulse /
Reactive / Typed do not change at all.**

## 2. Locked requirements (the non-negotiables)

1. **Official-client wire compatibility is a hard requirement.** In `:port:` mode, the real
   `mongosh`, the Node/Python/Go/Rust/Java drivers, Compass, and our own libmongoc must connect, run
   CRUD + aggregation + indexes + change streams + transactions, and not be able to tell (within our
   supported surface) that they aren't talking to mongod. This is validated by a **driver-conformance
   suite** run against the real drivers (§14).
2. **Faithful single-node semantics.** Same results as mongod for every operation we support —
   enforced by **differential testing** against a real mongod (we already do this in `test_diff.ml`;
   Burrow becomes a third backend). Topology features (replication, sharding, elections) are
   presented as a **single-node replica-set facade**, not implemented for real.
3. **Embedded-first.** `:embedded:` is the design center; `:port:` is the same engine with a protocol
   head. Nothing in the embedded path pays for the wire.
4. **We own the storage binding.** LMDB is vendored and statically linked under our control (the
   libmongoc pattern), with our own thin OCaml FFI — no third-party opam binding in the trust path.
   The result must be **airtight**: crash-safe, leak-free, and correct under Eio multicore.
5. **Dependency hygiene.** Burrow is a *standalone submodule* in the `fennec-mongo` package. It
   **reuses** the pure libs (`bson`, `query`) but **nothing JS-bound ever depends on it** — minimongo
   and the browser bundle must never pull in LMDB or the engine. The C dependency is a physical
   backstop; the dune graph is the enforced contract (§3).
6. **Standalone testability.** Correctness is provable without a network: differential vs mongod,
   property tests for the order-preserving key codec, crash-recovery tests, and the portability check
   (self-contained binary) — all under `dune test`.

## 3. Module & dependency architecture

### Naming & packaging (decided)

**Burrow** is the flagship embedded engine *inside* the `fennec-mongo` package — branded the way
`fennec` brands **Pulse**, and matching the house rule (themed names for fennec-originals; literal
names for the Mongo-compatible plumbing, so `bson`/`query`/`minimongo`/`driver` stay literal). The
package stays `fennec-mongo`; Burrow is foregrounded everywhere (README, docs). **Modes** live one
level down, at the `MONGO_URL` selection — `:memory:` (minimongo, also the JS cache) / `:embedded:`
(Burrow on disk) / `:port:` (Burrow as a wire server) / a real URI (remote mongod) — so fennec-mongo
is *one* MongoDB-compatible data layer with several backends, two of them powered by Burrow. The
`burrow.*` libs are a self-contained stack over the pure `bson`/`query` cores (§5), so Burrow can be
**extracted into a standalone `burrow` package later** — the way `fennec-hunt` stands alone — if it
earns its own identity ("SQLite-for-Mongo in OCaml"). We design for that extraction now; we don't do
it yet.

Current pure/JS-safe libs (must stay free of any Burrow dep): `fennec-mongo.bson`,
`fennec-mongo.json`, `fennec-mongo.query`, `fennec-mongo.minimongo`, `fennec-mongo.bson_json`.
Current native libs: `fennec-mongo.ffi` (libmongoc), `fennec-mongo.driver`, `fennec-mongo.mongod`.

New, under `mongo/burrow/` — **native only**:

```
fennec-mongo.bson  ──(reused)──┐         fennec-mongo.query ──(reused)──┐
                               ▼                                         ▼
  fennec-mongo.bson_binary  (NEW, pure: Bson.t <-> BSON wire bytes — also usable by bson_json/JS)
                               │
  fennec-mongo.burrow.lmdb   (NEW, native: OUR vendored LMDB + thin FFI)
                               │   deps: (none OCaml-heavy)
                               ▼
  fennec-mongo.burrow        (NEW, native: catalog, storage, indexes, planner, executor,
                               │   aggregate, oplog, txn/session)  deps: bson, bson_binary, query,
                               │   burrow.lmdb, eio, eio.unix
                               ▼
  fennec-mongo.burrow.wire   (NEW, native, OPTIONAL: OP_MSG server + command layer + RS facade)
                               │   deps: burrow, eio, eio.unix
                               ▼
  (selection)  Fennec_mongo_dynamic.Dynamic gains `Embedded` → Backend.S over burrow
```

**The JS-safety boundary (enforced):**
- `fennec-mongo.minimongo` stays `(libraries bson query)` — it never lists `burrow`. The browser
  client and any `(modes js)` executable never link a Burrow lib.
- Burrow's `(foreign_archives lmdb)` makes it *physically unlinkable* into js_of_ocaml — a hard
  backstop on top of the dune-graph contract.
- A guard (a `(rule (alias runtest) ...)` like `portability/`) asserts no JS target's transitive deps
  include `burrow.*`.
- `bson_binary` is pure (Stdlib only) and lives next to `bson` so it stays JS-safe and is reusable;
  Burrow needs it for the wire + on-disk format.

**What Burrow reuses vs. builds new:**
- *Reuse:* `Bson.t`, `Bson.compare/equal/as_float`, the `query` engine (matcher/modifier/projection/
  sorter/diff), the `$jsonSchema` validator (`Query.Matcher.json_schema_matches`), the `Fanout`
  delivery discipline (port the pattern), the vendor/static-link/portability build infra.
- *New:* the LMDB binding, the BSON binary codec, the storage catalog, the index access methods +
  order-preserving key codec, the query **planner**, the durable execution engine, a **full**
  aggregation engine (superset of the pure one), the **oplog**, the session/transaction manager, and
  the wire-protocol server + replica-set facade.

### Convergence across backends (the retrofit)

One pure semantics core, several execution backends: **minimongo** (in-memory, JS), **Burrow**
(on-disk, in-process), and **libmongoc** (remote mongod). They agree because they share the pure
`query` layer, and the differential test (§15) makes the agreement a gate, not a wish.

Designing Burrow forces us to pin down the *full, faithful* semantics — every operator, every edge
case. The retrofit, taken deliberately once the spec settles, is to lift the **pure, shareable** parts
into the shared `query` layer and point minimongo at them, so the browser cache stops surprising
people with "works on the server, not in the client." The discipline that keeps it graceful:

- **Pure operators can converge; heavy machinery cannot.** Operator/expression *evaluation* is pure
  and small — a candidate for the shared layer. Storage, indexing, planning, spilling, and the wire
  stay native in Burrow and never approach minimongo.
- **Promotion is deliberate, because JS size is real.** A dispatch `match` references every branch, so
  js_of_ocaml cannot tree-shake an operator the shared layer mentions — each promotion has a bundle
  cost. The comprehensive expression set therefore lives in `burrow.expr` by default; an operator
  graduates into the JS-linked `query` core only when the client genuinely needs it.
- **The differential harness is the safety net.** minimongo, Burrow, and mongod run the same ops and
  must agree; convergence work that breaks parity fails the gate.

Net: the architecture is *built* for graceful convergence — the pure layer is the single source of
truth, each backend is a thin execution shell over it, and minimongo upgrades by depending on more of
that layer only where it pays.

## 4. Storage substrate: a vendored, owned LMDB

LMDB is the substrate: an mmap'd, copy-on-write **B+-tree**, full **ACID**, **MVCC** (readers never
block writers or each other; exactly one writer at a time), **crash-proof without a WAL** (atomic
double-buffered meta page), ~10K LOC of dependency-free C, near-zero-copy reads from the page cache.
It is the smallest, leanest thing that does the disk + mmap heavy lifting correctly, and it hands us
the single hardest part of a database — durable crash recovery — for free.

**We vendor and own it**, mirroring `mongo/vendor/` + `mongo/config/discover.ml` +
`mongo/portability/`:
- `mongo/burrow/vendor/` — the LMDB source tarball (SHA256-pinned) + `build.sh` building a **static**
  `liblmdb.a` into the per-OS cache (keyed by version/os/arch/libc, idempotent stamp), exactly like
  the mongo-c-driver build.
- `mongo/burrow/config/discover.ml` — a dune-configurator that builds/locates the archive and emits
  `c_flags.sexp`/`c_library_flags.sexp`; degrade-safe (LMDB is tiny and portable, so degradation
  should be rare, but the path exists).
- `mongo/burrow/lmdb/lmdb_stubs.c` + `lmdb_ffi.ml` — **our own thin binding** to the small LMDB C
  API: `env_create/open/set_mapsize/set_maxdbs/sync/close`, `txn_begin/commit/abort`,
  `dbi_open/drop`, `get/put/del`, `cursor_open/get(_op)/put/del/close`. No third-party binding in the
  trust path.
- `mongo/burrow/portability/check.sh` — extends the "no non-system shared libs" guarantee to the
  LMDB-linked artifact.

**Airtight binding rules** (same discipline as `mongoc_stubs.c`, made stricter):
- Every potentially-blocking call (a commit that `fsync`s, a page fault that hits disk) releases the
  OCaml runtime lock so it can run in an Eio systhread without stalling the scheduler; we touch no
  OCaml value while the lock is dropped (copy keys/values to/from the C heap or use the mmap'd
  pointers under a held txn only).
- Handles (`env`, `txn`, `cursor`) are GC custom blocks with finalizers, NULL-initialized before the
  real pointer lands, closed deterministically; a finalizer never touches a half-built handle.
- `MDB_NOTLS` is set so read txns are **not** pinned to OS-thread-local slots — we manage txn↔fiber
  affinity ourselves (critical for Eio; §13).
- Durability is a tunable (the SQLite `PRAGMA synchronous` analog): `sync` (fsync every commit, the
  safe default), `nometasync`, `nosync` (fast, crash-may-lose-last-commits, opt-in). `MDB_WRITEMAP`
  optional for write throughput.
- Map size grows in chunks; "map full" is handled by growing + retrying, never by crashing. The
  long-lived-reader file-growth pitfall is mitigated by the short-read-txn rule (§13).

**First numbers (measured — vendored LMDB 1.0, Apple Silicon, 1 M × 112-byte records, through our own
binding).**

- *Reads are effectively free.* Random point-get ~1.9 M/s in raw C and **~1.7 M/s zero-copy through
  the OCaml binding** (the FFI boundary costs ~9%); a count/aggregate scan runs at the full C rate
  **~160 M recs/s (16 GB/s)** because the cursor loop stays in C. Materializing a value into the OCaml
  heap costs ~13% — the case for the zero-copy `Bson_reader` (§5).
- *Writes are fast, and the FFI is not the bottleneck.* A single-txn bulk load is ~6.3 M recs/s in C
  (with `MDB_APPEND`, the sorted-bulk fast path), ~3.6 M/s through the binding. The spike *checked* the
  obvious hypothesis: a `put_batch` (one FFI call looping the puts in C) does **not** help — even with
  keys pre-built it stays ~3.8 M/s. So the OCaml↔C boundary is cheap; the writer needs no batch-FFI
  API. The gap to the C number is `MDB_APPEND` (a flag, for clustered/sorted `_id` loads) + per-key
  allocation (reused buffers), not call overhead — and it barely matters: 3.6 M writes/s dwarfs any
  real rate, and group commit decouples *durable* throughput from per-put speed entirely.
- *Durability — investigated carefully, because the per-commit number looked alarming.* A full durable
  single-record commit ran at **124/s (8.04 ms)**. That is **correct, not a bug or contention**: macOS
  LMDB defines its sync as `fcntl(F_FULLFSYNC)` (`mdb.c:191`) — a true flush to stable NAND,
  power-loss-safe — and a durable commit issues **two** of them (data file `mdb.c:3133`, meta page
  `mdb.c:4964`). Calibrated here: `F_FULLFSYNC` = **4.02 ms**, plain `fsync` = **0.022 ms** (a 186×
  gap), so 2 × 4.02 = 8.04 ms exactly (and `nometasync` measured 4.02 ms — one sync — confirming it).
  The famous "LMDB ≈ 50k synchronous commits/s" figures are Linux `fdatasync`/`fsync` (flush to drive
  *cache*) — which is **46k/s here too**; we were simply measuring the stronger guarantee.
- *The throughput answer is group commit, and it works.* Under *full* F_FULLFSYNC durability, batching
  recovers linearly — **125 / 1.2k / 12k / 114k / 874k recs/s** at batch 1/10/100/1000/10000. The
  single writer accumulates writes and commits once per short window, so even paranoid power-loss-safe
  durability gives 100k+ writes/s.
- *Durability is a tunable* (SQLite-`PRAGMA`-style): `nosync` (~207k/s, dev/test) · **`fsync`**
  (drive-cache durable, ~46k/s — the sensible default, what other engines measure; we expose it by
  making the sync primitive runtime-selectable in our vendored `mdb.c`, since stock LMDB gates an
  fsync-only mode behind a Linux-only flag) · `nometasync` (one F_FULLFSYNC, 249/s) · `full` (two
  F_FULLFSYNC, 124/s ungrouped). The B-tree was depth 3 for 1 M entries.

*Implication for §13:* an 8 ms commit **must run off the Eio scheduler** (release the runtime lock /
systhread), or it stalls every fiber for 8 ms — so the single writer commits in a blocking section,
and group commit means it does so once per window, not per write.

## 5. Internal architecture — the submodule decomposition

Burrow is not one library; it is a strict, acyclic stack of small libraries, each with a sharp job, a
typed interface, and its own tests. The split is chosen so that (a) the clever, allocation-tight parts
are *pure* and provable in isolation, (b) all unsafe code and all IO are confined to one place, and
(c) any hot layer can be optimized without disturbing the others. No layer reaches sideways or up.

### Design principles

1. **Pure where possible; IO only at the edges.** Codecs, the key encoder, the planner, and
   expression compilation are pure `value → value` functions — unit/property-tested with no disk and
   trivially parallel-safe. LMDB lives behind one module; everything above is values + typed handles.
2. **A strict layered DAG, enforced by dune.** Dependencies point only downward; a cycle is a compile
   error, not a code-review note.
3. **A typed IR between planner and executor.** The planner emits a `plan` GADT; the executor
   interprets it. Illegal plans are unrepresentable, the planner is tested as `query → plan` with zero
   IO, and the executor as `plan → rows` over a temp store — bugs localize to one side.
4. **Zero-copy, low-allocation hot path.** A `Bson_reader` walks fields over the mmap'd bytes without
   materializing a `Bson.t` tree (predicate eval and projection read one field, not the whole doc);
   index keys encode into reused buffers; results stream rather than accumulate. The common query
   barely touches the GC.
5. **Compile-time safety for the sharp edges.** Phantom-typed transaction modes (`[`r] | [`w]`) make
   "write in a read txn" a type error; typed sub-DB handles (`('k,'v) dbi`) tie key/value codecs to
   the store at the type level; the plan/stage IRs are GADTs with exhaustive matches. Illegal states
   don't compile.
6. **Parallel-safe by construction.** Readers run on immutable MVCC snapshots and share nothing; there
   is exactly one writer; no module holds mutable hot-path state across a fiber yield. This is
   minimongo's fanout discipline, scaled to disk and domains.

### The library stack (downward dependencies only)

```
PURE FOUNDATION  (no IO; property-tested; JS-safe where marked)
  bson*           the value type                                    [JS]
  bson_binary*    Bson.t <-> BSON bytes + a zero-copy Bson_reader   [JS-safe]
  query*          pure operator semantics (lean core; §3, §15)      [JS]
  burrow.codec    memcomparable key encoder + byte/varint utils     (pure)
       ▲
STORAGE  (the only unsafe code, the only IO)
  burrow.lmdb     owned thin FFI over vendored LMDB (unsafe internally)
  burrow.store    typed-safe facade: [`r|`w] txn, ('k,'v) dbi, grow-and-retry, the airtight rules
       ▲
CATALOG & ACCESS METHODS
  burrow.catalog  namespaces, collection/index/option metadata (rebuilt on open)
  burrow.record   the clustered _id record store (returns BSON slices)
  burrow.access   index maintenance + range scans behind one Access_method module-type
                  (btree / text / geo / hashed instances)
       ▲
QUERY
  burrow.plan     PURE planner: (catalog stats + query) -> plan GADT
  burrow.exec     executor: interprets the plan; streaming; residual matcher; sort/spill; project
       ▲
AGGREGATION
  burrow.expr     the FULL native expression engine (the long tail, kept out of JS-bound query)
  burrow.agg      pipeline compiler + streaming executor; spilling; storage-touching stages
       ▲
DURABILITY & REACTIVE
  burrow.oplog    capped append + tail
  burrow.live     change streams (Fanout) + session/transaction manager
       ▲
ASSEMBLY
  burrow          command dispatch + the Backend.S impl + engine lifecycle (the public face)
  burrow.wire     (optional) OP_MSG server + cursor registry + RS facade + auth
```
(`*` = existing or pure/JS-safe and shared.)

### Why each boundary earns its place

| Module | Role | Purity | The optimization it owns | How it's tested |
|---|---|---|---|---|
| `bson_binary` | bytes ⇄ value, zero-copy reader | pure | read one field without decoding the doc | round-trip + fuzz vs libbson |
| `burrow.codec` | order-preserving keys | pure | encode into a reused buffer | `let%prop` vs `Bson.compare` |
| `burrow.lmdb` | raw FFI | unsafe | the only place raw pointers live | leak / finalizer tests |
| `burrow.store` | typed txn/dbi facade | IO | no allocation on get/put (slices) | temp-env unit tests |
| `burrow.record` | clustered docs | IO | sequential `_id` locality | unit + differential |
| `burrow.access` | indexes | IO | covered-query path, range cursors | unit + differential |
| `burrow.plan` | choose an access path | **pure** | the planner — OCaml's home turf | unit (assert the IR) + "plan = scan" prop |
| `burrow.exec` | run the plan | IO | pull-based streaming, bounded memory | temp store + differential |
| `burrow.expr` / `agg` | aggregation | mixed | spilling, index pushdown | differential vs mongod |
| `burrow.live` | change streams + txns | IO | lock-guards-snapshot, callbacks-outside-lock | concurrency stress |

The two payoffs you asked for fall straight out of this shape. The **planner and the codecs are pure
functions** — exactly what OCaml proves correct cheaply and runs fast (a GADT plan IR, exhaustive
matches, no IO) — and the **unsafe/IO surface is one module wide** (`burrow.lmdb` / `burrow.store`),
so "unbreakable" is an auditable claim. Low CPU and low memory come from the zero-copy reader, index
pushdown, covered queries, and a streaming-with-spill executor; parallel safety comes from immutable
snapshots plus a single writer, so reader domains scale with no locks on the hot path.

### The command + wire heads (assembly)

`burrow` exposes the Mongo command set (`find` / `insert` / `update` / `delete` / `findAndModify` /
`aggregate` / `createIndexes` / `collMod` / `hello` / …) as a dispatch table over the layers above —
the *same* table serves the embedded API and the wire server. `burrow.wire` adds only OP_MSG framing,
the cursor registry, the replica-set facade, and auth (§12); it carries no engine logic of its own.

## 6. Data model & on-disk format

- **BSON binary codec** (`bson_binary`, new, pure) — `Bson.t ↔` canonical BSON bytes per the BSON
  spec. This is the missing primitive: the wire protocol and the record store both want real BSON
  bytes, not extended-JSON strings (the libmongoc FFI's current ext-JSON bridge is a perf tax we do
  **not** repeat here). Fast, allocation-conscious, fuzz-tested against libbson output.
  *Evidenced (the spike, `mongo/burrow/binary/`): a `find_field` that walks to a named field — skipping
  the others by size, comparing names in place, allocating nothing for what it skips — extracts one
  field **10–34× faster** than decoding the whole document (10× for the last field of a 12-field doc,
  37 M ops/s for an early one vs ~1 M ops/s decoding the whole thing), and agrees with a full decode
  on every field. This is the predicate-eval / projection path: don't materialize what you don't read.*
- **Documents** stored as raw BSON bytes, fetched zero-copy from the mmap under a read txn, decoded
  to `Bson.t` only when the executor needs values (predicate eval, projection). Covered queries and
  count-from-index avoid decoding entirely.
- **`_id`** is the clustering key (minted as ObjectId or honored if supplied; the `id` codec lesson
  carries over).
- **Order-preserving ("memcomparable") key codec** — the crux primitive for indexes: encode a BSON
  value (or a tuple, per the index's field directions) into bytes whose **lexicographic order equals
  `Bson.compare`** (asc) or its inverse (desc), with a trailing `_id` for uniqueness/stability. The
  fiddly parts — unifying `Int`/`Int64`/`Float` into one sortable numeric encoding, type-bracket
  tags, strings, dates, ObjectId, null/min/max key, descending inversion, multikey expansion — are
  bounded and exactly specifiable, with `Bson.compare` as the oracle. **Property test:**
  `byte_compare(enc a, enc b) = sign(Bson.compare a b)` over generated BSON (`let%prop`).
  *Evidenced (the spike, `mongo/burrow/codec/`): 200k random scalar pairs — numbers (incl. NaN/±∞/±0/
  huge Int64), strings (incl. prefixes), dates, objectIds, bool, null, min/max — match `Bson.compare`
  exactly. The number encoding reduces to a sortable big-endian double (matching `Bson.compare`, which
  also compares numbers by float value); the leading tag byte (= `type_rank`) gives cross-type order
  for free. The test earned its keep immediately: it caught that `-0.0`/`+0.0` must be normalized (the
  encoding distinguished them; `Bson.compare` treats them equal). The scalar domain is done; compound
  keys (0x00-escaped concat) and descending (byte-invert) layer on top.*

## 7. Indexing — the full set

| Index type | Plan |
|---|---|
| single-field, **compound** | memcmp tuple key; prefix matching in the planner |
| **multikey** (arrays) | one index entry per array element; dedup _ids on read |
| **unique** / **sparse** | uniqueness enforced at write (key collision); sparse skips missing fields |
| **partial** (`partialFilterExpression`) | only index docs matching the predicate (reuse `matcher`) |
| **TTL** (`expireAfterSeconds`) | date index + a background reaper fiber (§9) |
| **text** (full-text) | a tokenizer + an inverted index (terms → _ids) sub-DB; `$text`/`$meta:textScore` |
| **2dsphere / 2d** (geo) | S2-cell / geohash covering keys; reuse `query/geo` for refinement |
| **hashed** | hash(field) key (sharding-shaped, but useful for even point-lookup distribution) |
| **wildcard** (`$**`) | index every (path,value) pair; for flexible-schema lookups |
| **collation** | locale-aware key encoding (ICU — a scope decision; default simple binary) |

The planner consumes `index_names`/specs from the catalog, reconciled at open from the model
declarations (the typed layer already drives `ensure_index`). `explain` reports candidate + winning
plans. Hints (`hint:`) force an index. **Covered queries** (projection ⊆ index keys) skip the record
store entirely.

## 8. The aggregation engine — full coverage

The pure `query/aggregate` + `query/expr` stay the **lean shared subset** for minimongo/JS. Burrow
ships a **superset native executor** so the server is faithful without bloating the browser bundle
(pure operators can be shared into `query/expr` where it helps; storage-touching and memory-heavy
stages are native-only).

- **Stages:** `$match $project $addFields/$set $unset $replaceRoot/$replaceWith $group $sort $limit
  $skip $count $unwind $lookup` (both forms, incl. `let`/sub-pipeline) `$graphLookup $facet $bucket
  $bucketAuto $sortByCount $sample $unionWith $out $merge $redact $geoNear $setWindowFields $densify
  $fill $documents $indexStats $collStats`. Atlas-only (`$search`, `$vectorSearch`) are out.
- **Expression operators — the long tail, faithfully:** arithmetic, comparison, boolean,
  conditional (`$cond $switch $ifNull`), string (incl. `$regexFind/$regexFindAll/$regexMatch`,
  `$replaceOne/$replaceAll`, `$trim`, `$split`, `$concat`), date (the full `$dateAdd/$dateDiff/
  $dateTrunc/$dateToParts/$dateFromParts/$dateToString/$dateFromString` family), array (`$map $filter
  $reduce $zip $slice $concatArrays $indexOfArray $sortArray $first $last`), set (`$setUnion/
  $setIntersection/$setDifference/$setIsSubset/$anyElementTrue/$allElementsTrue`), object
  (`$mergeObjects $objectToArray $arrayToObject $getField $setField`), type (`$type $convert $toX
  $isNumber`), trig/math, and `$let`. `$function`/`$accumulator` (server-side JS) are **out** (no JS
  engine — same line we drew for `$where`).
- **Accumulators:** `$sum $avg $min $max $first $last $push $addToSet $mergeObjects $stdDevPop
  $stdDevSamp $count`; **window accumulators** for `$setWindowFields`: `$rank $denseRank
  $documentNumber $shift $derivative $integral $expMovingAvg $locf $linearFill` + running windows.
- **Execution:** `$match`/`$sort` push down to indexes; `$group`/`$sort` **spill to a temp LMDB
  sub-DB** beyond a memory budget (faithful to mongod's `allowDiskUse`); `$lookup`/`$graphLookup`
  resolve foreign collections through the catalog (in-process, no socket); `$merge`/`$out` are
  transactional writes through the record store + indexes + oplog.

## 9. Special collections

- **Clustered** — the default (record store is clustered on `_id`); `clusteredIndex` option accepted.
- **Capped** — a sub-DB with a byte cap + insertion-order ring eviction (oldest overwritten);
  **tailable cursors** block-wait on new inserts via an Eio condition. (The oplog is a capped
  collection internally.)
- **TTL** — a TTL index drives a background reaper fiber that range-scans expired docs by date and
  deletes them (emitting oplog delete events so change streams + Pulse see expiries).
- **Views** — read-only, defined by a pipeline over a source collection; resolved by prepending the
  view's pipeline at query time. On-demand materialized views via `$merge`/`$out`.
- **Time-series** (`timeseries: {timeField, metaField, granularity|bucketMaxSpanSeconds}`) — faithful
  to Mongo's model: a hidden `system.buckets.<name>` record store of **buckets**, each grouping
  measurements for one `metaField` value over a time window into a compact, column-oriented layout;
  inserts route to the open bucket for the series and roll over by span/size; reads/aggregations go
  through an internal unpack stage (`$_internalUnpackBucket`) presenting the logical measurement view.
  Buckets compress well (delta/columnar) — a real space + scan win, and a flagship feature.
- **Large values & file storage.** The 16 MB BSON document limit is enforced faithfully on write
  (as in mongod), which pushes big payloads to one of two paths. Inline **`BinData`** (≤ 16 MB) is
  stored as raw bytes in the record and read **zero-copy** from the mmap — no chunking, no copy.
  Beyond 16 MB, **GridFS** (the `fs.files` + `fs.chunks` two-collection convention) works over plain
  CRUD, so wire clients get it for free. For `:embedded:` we do better than chunk-documents: an
  optional **native blob store** — a content-addressed sub-DB (`blake3(content) → bytes`, ref-counted,
  so identical files dedup) with **streaming** read/write as Eio flows (a multi-GB file never
  materializes in memory) and zero-copy download straight from the mmap. The standard GridFS view is
  projected on top, so a wire client still sees `fs.files`/`fs.chunks` while the embedded app gets
  dedup, streaming, and no per-chunk-document overhead — another place colocation beats a server.
  (The on-disk value size is never the constraint — LMDB stores large values; the 16 MB cap is purely
  the Mongo protocol contract we honor for compatibility.)

## 10. Change streams & the oplog

- **Oplog:** a capped `__oplog` sub-DB keyed by a monotonic `(seq)` (also our `clusterTime`/operation
  time), value = the change event (op, ns, documentKey, fullDocument and/or update description,
  optional pre/post images). Every committed write appends to it **inside the same LMDB txn** — so
  the data change and its oplog entry are atomic (no lost or phantom events on crash).
- **Delivery:** in-process observers (Pulse) tail the oplog via the `Fanout` discipline —
  **synchronous, in-process, ordered** — which is *faster and more exact* than mongod's async
  network stream. Resume tokens = oplog `seq`.
- **This closes the native-fence gap (gap 2) for free.** Because we own the oplog and its monotonic
  ordering, the write fence is exact: a method's `updated` fires once delivery has reached the
  write's `seq`. (Contrast the libmongoc backend, where the fence is best-effort under stream lag.)
- **Faithful semantics:** collection/db/deployment scope, `fullDocument: updateLookup`,
  pre/post-images (a configurable image sub-DB), `invalidate` on drop/rename, `$changeStream` as an
  aggregation stage (so wire clients get it through the normal `aggregate` + `getMore` tailing path).

## 11. Transactions & sessions

- **Logical sessions** (`lsid`) are tracked (drivers always send them); causal-consistency tokens
  (`afterClusterTime`/operationTime) map to oplog `seq`.
- **Multi-document ACID transactions** (`startTransaction`/`commitTransaction`/`abortTransaction`):
  a transaction maps to a single LMDB **write txn** held across the session's ops, giving atomicity +
  snapshot isolation + durability natively. **Honest constraint:** LMDB is single-writer, so
  concurrent write transactions are **serialized** (one at a time) rather than MVCC-optimistic like
  WiredTiger. For embedded/local single-node this is correct and usually fine; faithful *semantics*
  (all-or-nothing, snapshot reads) without faithful *write concurrency*. If concurrent write txns
  become a real need, an MVCC overlay (versioned values + conflict detection above LMDB) is the
  escalation — a deliberate, separate project, flagged not hidden.
- **Reads** are always MVCC snapshots (LMDB read txns) — fully concurrent, never blocked by writers.
- **Retryable writes** (`txnNumber`) are deduplicated via a per-session retry record.
- Write/read concern + read preference fields are **accepted** and trivially satisfied on a single
  node (the facade reports a primary).

## 12. Wire protocol & client compatibility (the locked requirement)

`:port:` runs an Eio TCP server speaking the MongoDB wire protocol so official clients connect:

- **Framing:** `OP_MSG` (the modern, only-really-needed opcode), `OP_COMPRESSED` (zlib/zstd/snappy —
  we already vendor zlib), and the legacy `OP_QUERY`/`OP_REPLY` *just* enough for the initial
  `isMaster`/`hello` handshake some drivers still open with.
- **Handshake / the single-node replica-set facade:** `hello` reports `isWritablePrimary: true`,
  `setName`, `hosts: [self]`, `me`, `maxWireVersion` (a modern version), `maxBsonObjectSize` (16 MiB),
  `maxMessageSizeBytes`, `maxWriteBatchSize`, `logicalSessionTimeoutMinutes`, `topologyVersion`,
  election/opTime fields. Presenting as a one-node replica set is the key trick: drivers then enable
  **change streams, retryable writes, causal consistency, and transactions** against us.
- **Cursors:** a per-connection cursor registry with 64-bit cursor ids; `find`/`aggregate` open,
  `getMore` advances (with `batchSize`/tailable/awaitData), `killCursors` frees. Server-side cursor
  timeouts.
- **Command surface (what drivers + Fennec actually issue):** `hello`/`isMaster`, `ping`,
  `buildInfo`, `getParameter`, `find`, `getMore`, `killCursors`, `insert`, `update`, `delete`,
  `findAndModify`, `aggregate`, `count`, `distinct`, `create`, `drop`, `collMod`, `renameCollection`,
  `createIndexes`, `dropIndexes`, `listIndexes`, `listCollections`, `listDatabases`, `dbStats`,
  `collStats`, `explain`, `validate`, `create/refreshSessions`, `endSessions`,
  `startTransaction`-via-OP_MSG-flags/`commitTransaction`/`abortTransaction`, `getLastError`
  (legacy), `serverStatus` (a faithful-enough subset). Unknown/admin-only commands return a clean
  `CommandNotFound` rather than lying.
- **Auth posture:** embedded/local default is **no auth** (it's your process / your file). For exposed
  `:port:`, **SCRAM-SHA-256** is the phase to add (drivers negotiate it via `saslStart`/
  `saslContinue`); x.509/LDAP/Kerberos are out.
- **Compatibility is *tested*, not asserted:** a conformance suite runs `mongosh` + the Node/Python/
  Go/Rust drivers (and our libmongoc path) against `:port:` and exercises CRUD, aggregation, indexes,
  change streams, transactions, and the handshake. This is the gate for "client-compatible."

## 13. Concurrency & multicore (Eio domains, airtight)

The model that makes LMDB + Eio + OCaml-5 multicore correct and fast:

- **One writer.** A dedicated **writer fiber** (optionally its own domain) owns the single LMDB write
  txn and consumes a write-command `Eio.Stream`. Each command applies its full effect — modifier →
  BSON bytes → record put + every index update + oplog append — in **one txn**, commits, then fires
  `Fanout` observers *outside* the txn. Atomicity, oplog ordering, and the data/event coupling all
  fall out of "one write txn per logical write."
- **Many readers, in parallel.** Reads open **short** LMDB read txns (MVCC snapshots). Query
  execution is CPU-bound over mmap'd pages (page faults are synchronous, not Eio-suspending), so a
  read txn opens, runs the plan to completion, and closes **without an Eio yield in between** — which
  is exactly what keeps `MDB_NOTLS` txn↔fiber affinity sound and avoids the long-lived-reader
  file-growth pitfall. Heavy reads (big aggregations, parallelizable scans) **fan out across an Eio
  domain pool**, each domain on its own read txn over the same consistent snapshot — multicore query
  execution is the payoff for OCaml 5 + Eio.
- **The airtight rules** (binding invariants we test): a write txn never spans a scheduler yield that
  could admit a second writer; a read txn never outlives the function that opened it; no OCaml value
  is touched while the runtime lock is dropped; handles are finalizer-safe; "map full" grows-and-
  retries; corruption on open fails loudly (never silently serves garbage).
- This is the part to **prove**, not hope: a stress harness with concurrent writer + reader domains
  asserting linearizable-per-key history and exact final state (we already do a version of this for
  minimongo's fanout in `test_mongo.ml`).

**Measured (the spike — 10-core Apple Silicon: 4 perf + 6 eff).** The model holds, repeatably:
- *Off-scheduler commit works.* An ~8 ms F_FULLFSYNC commit run via `Eio_unix.run_in_systhread` with
  the runtime lock released runs **concurrently** with other fibers — a co-running CPU worker finished
  in ~`max(writer, worker)`, not the sum; the domain stays live through the sync. Done inline on a
  fiber it serializes (~sum). So a slow durable commit never freezes the engine, and the single writer
  must (and does) commit through the systhread.
- *Reads scale across cores* on a shared, immutable MVCC snapshot with **zero contention**: full-scan
  throughput went **1.00× / 2.07× / 3.57× / 4.44×** at 1 / 2 / 4 / 8 domains — near-linear across the
  **4 performance cores** (3.57× on 4), then bandwidth/efficiency-core-bound. The ceiling is the
  hardware, not the design. (`MDB_NOTLS` so each domain owns its reader slot; the scan releases the
  runtime lock so it never blocks the stop-the-world GC; single writer + immutable read snapshots =
  no data races by construction.)

## 14. Faithful-coverage feature matrix

| Area | IN (faithful) | FACADE (single-node pretend) | OUT |
|---|---|---|---|
| BSON types & CRUD | all types, find/insert/update/replace/delete, findAndModify, bulk | write/read concern, read pref | — |
| Query operators | comparison/logical/element/eval/$regex/$mod/$expr/$jsonSchema/array/bitwise/geo | — | `$where`, `$text` needs text index, server JS |
| Update operators | field/array/bitwise, `$each/$slice/$sort/$position`, positional `$`/`$[]`/`$[id]`+arrayFilters, `$currentDate` | — | — |
| Aggregation | the full stage + expression + accumulator + window set (§8), spilling | — | `$search`/`$vectorSearch` (Atlas), `$function`/`$accumulator` (JS) |
| Indexes | single/compound/multikey/unique/sparse/partial/TTL/text/2dsphere/hashed/wildcard | — | collation via ICU (maybe), `$**` edge cases |
| Special collections | clustered, capped, TTL, **time-series**, views, GridFS-as-convention | — | — |
| Change streams | collection/db/deployment, resume, full/pre/post images, invalidate | replica-set topology in `hello` | sharded change streams |
| Transactions | multi-doc ACID, snapshot isolation, retryable writes | concurrent write txns serialized (not MVCC) | distributed/2PC |
| Sessions | logical sessions, causal consistency | — | — |
| Admin/commands | the driver + Fennec surface (§12), `explain` | `serverStatus`/`replSetGetStatus` subset | full diagnostics surface |
| Wire protocol | OP_MSG, OP_COMPRESSED, cursors, handshake | single-node RS facade | OP_QUERY beyond handshake |
| Auth | none (embedded) / SCRAM-SHA-256 (`:port:`) | — | x.509/LDAP/Kerberos |
| Topology | single node | RS facade for clients | replication, sharding, elections, oplog rollback |

"Faithful" = same results as mongod on our supported surface, proven by differential test. Topology
is explicitly a *facade*: we look like a one-node replica set so clients light up the features that
gate on it, without implementing real replication.

## 15. Testing & correctness

- **Differential vs real mongod** — extend `mongo/.../test_diff.ml` to a *third* backend: every op
  (CRUD, every operator, every pipeline stage, indexes, change streams, txns) runs through minimongo,
  Burrow, and a real mongod and must agree. This is the primary correctness oracle and it already
  exists in skeleton.
- **Property tests (`let%prop`)** — the memcomparable key codec (`byte_compare(enc a, enc b) =
  sign(Bson.compare a b)`), the BSON binary codec (round-trip + match libbson bytes), planner
  equivalence (index plan result = collection-scan result for the same query).
- **Crash-recovery tests** — kill mid-write (and mid-fsync-mode), reopen, assert no torn writes, no
  lost committed txns, oplog consistent with data.
- **Concurrency stress** — concurrent writer + reader domains; linearizable-per-key history; exact
  final state.
- **Driver-conformance suite** — `:port:` against `mongosh` + official drivers + our libmongoc (§12).
- **Portability** — the linked Burrow artifact depends only on system shared libs (extends the
  existing `portability/check.sh`).

## 16. Phasing

- **S1 — durable core, embedded-only.** Vendored LMDB + owned binding; `bson_binary`; catalog +
  record store (clustered `_id`); the pure engine over collection scans; insert/update/remove/find/
  count/distinct; `:embedded:` selection in `Runtime`/`Dynamic`. Differential green for non-indexed
  ops. *Self-contained on-disk MongoDB with no mongod, behind the existing Backend.S.*
- **S2 — indexes + planner.** Memcomparable key codec (property-tested), single/compound/multikey/
  unique/sparse, the rule-based planner (point/range/prefix/sort-via-index/covered), `$jsonSchema`
  install, `explain`. Differential green for indexed ops.
- **S3 — reactive + the full aggregation engine.** Oplog + in-process change streams + **exact
  fence**; wire into Pulse (realtime e2e on `:embedded:`); the full pipeline/expression/window engine
  with spilling.
- **S4 — special collections + durability hardening.** TTL/capped/views/clustered + **time-series**
  + **GridFS / native blob store**; fsync modes; concurrency stress + crash-recovery; multicore query
  parallelism; benchmarks vs mongod (colocation should win locally).
- **S5 — `:port:` + client compatibility.** OP_MSG server, cursor registry, the RS facade,
  sessions + transactions over the wire, the driver-conformance suite; optional SCRAM for exposed
  ports; remaining index types (text/geo/hashed/wildcard) and partial/collation.

S1–S3 is the "dramatic improvement" core — a fast, durable, in-process Mongo that closes the fence
gap and removes the mongod dependency. S4 adds the flagship surface (time-series + native file
storage). S5 is the client-compatibility commitment and is a larger, separable effort.
**Convergence** — lifting the now-pinned-down pure semantics into the shared `query` layer and
upgrading minimongo to it (§3) — runs as a deliberate cross-cutting follow-on, gated by the
differential harness, once the spec stabilizes.

## 17. Open decisions to lock first

1. **Substrate:** LMDB (recommended) vs. a pure-OCaml engine later — keep storage behind an interface
   so the second is possible without touching planner/oplog.
2. **Transaction concurrency:** serialized write txns (LMDB-native, simpler) vs. an MVCC overlay
   (faithful write concurrency, much more work). Start serialized; escalate only if needed.
3. **Aggregation split:** how much of the expanded expression set lives in the shared pure `query`
   lib (helps minimongo, costs JS size) vs. native-only in Burrow.
4. **Collation:** vendor ICU (faithful locale order, a real dep) vs. simple binary collation only.
5. **`:port:` scope & auth:** which driver versions are the conformance target; SCRAM now or later.
6. ~~**Name.**~~ **Decided: Burrow** — the flagship engine inside `fennec-mongo`, extractable later (§3).
7. **File storage:** ship the optional native content-addressed blob store (dedup + streaming) or
   plain GridFS-over-CRUD only at first.
8. **Operator-promotion policy:** the rule for graduating a pure operator from native `burrow.expr`
   into the JS-linked shared `query` core — the minimongo-convergence vs. bundle-size tradeoff (§3).
9. **Plan/stage IR shape:** lock the GADT for the plan and pipeline IRs early — it's the contract
   between the pure planner and the executor, and it shapes every later optimization.

The two parts to pressure-test *before* committing code are the **memcomparable key codec** (the
correctness foundation for all indexing) and the **LMDB↔Eio concurrency contract** (the thing that
must be airtight). Everything else composes from pieces we already have.
