# MONGO — the whole Mongo surface, top to bottom (deep-dive)

How Fennec deals with MongoDB, everywhere it does, and how the pieces connect. This is the
map for "we use mongo a lot" — the pure engine, the in-memory store, the vendored C driver,
the build-time static-linking dance, the reactive seam, and every consumer (Pulse, Accounts,
DDP, the dev/test CLI).

> Branch note: this doc lives on `mongo-deep-dive` (a worktree off `main`, kept separate from
> the `form-handlers` work). File paths below are repo-relative and identical on `main`.

---

## 0. The shape of it (one knob, two backends, one seam)

```
                MONGO_URL  (the single database knob)
                     │   mongo/driver/runtime.ml :: state ()
        ┌────────────┼─────────────────────────┐
     unset         :memory:                  <a uri>
        ▼            ▼                           ▼
   Missing       Memory                       Mongo {uri; db}
 (fail-clear)  Minimongo (mongo/mem)     native driver → libmongoc (mongo/driver + ffi)
        └──────── ONE Fennec_mongo_backend.S (CRUD + observe_changes + fence + indexes) ────────┘
                     │   selected at runtime by Fennec_mongo_dynamic.Dynamic (Mem | Native | Embedded | Missing)
                     ▼
        Reactive.Make(Dynamic)            (publish / subscribe / methods / observe-mux)
        Typed.Make / collection runtime   (codec validation, index reconcile, $jsonSchema gen)
        Fennec_pulse_app facade           (collection / publish / method_ / serve_ddp — auto-booted, no start)
                     │
        DDP over WebSocket  ── ejson wire codec + doc_hash resync ──►  browser Minimongo + merge_store
```

Three things make the whole stack cohere:

1. **`Bson.t` is the lingua franca.** Every layer speaks the same in-memory BSON value
   (`mongo/bson/bson.ml`). The query engine, Minimongo, the driver bridge, Pulse, Accounts, and
   the DDP wire all pass `Bson.t` — never a parallel document type.
2. **One `Backend.S` seam** (`mongo/backend/seam.ml`) abstracts storage to CRUD + a single
   `observe_changes` + a write `fence`. In-memory and native libmongoc are interchangeable behind it.
3. **The pure core compiles to JS unchanged.** `bson`, `json`, `query/*`, and `mem/*` depend only on
   Stdlib (+ `re` for `$regex`), so the *same* Minimongo + query engine runs server-side and in the
   browser bundle via js_of_ocaml. The native driver is the only native-only piece, and it's optional.

---

## 1. Pure value + codecs (`mongo/bson`, `mongo/json`, `mongo/bson_json`)

**`Bson.t`** (`mongo/bson/bson.ml`) — one variant over every BSON type (Null, Bool, Int, Int64,
Float, String, Document (ordered field list), Array, Object_id, Date, Timestamp, Binary, Regex,
Decimal128, Code/Code_with_scope, Symbol, Min/Max_key). Two comparison functions with deliberately
different semantics:

- `equal` — **cross-type numeric** (`Int 1 = Float 1.0 = Int64 1L`), order-sensitive on document
  fields, `NaN ≠ NaN`. Used for `$eq`/`$in`/dedup.
- `compare` — a **total order** following the BSON type-precedence table (MinKey < numbers <
  strings < documents < arrays < binary < objectId < bool < date < timestamp < regex < code <
  decimal128 < MaxKey), NaN sorts lowest. Used for sorting and `$min`/`$max`.
- `as_float : t -> float option` — the single "is this a number?" predicate (accepts Int/Int64/
  Float/Date), used everywhere range matching needs a numeric view.

Document order is preserved (it's a `list`, not a map) because projection/update/encode order is
observable. Pure Stdlib → JS-portable.

**`Json.t`** (`mongo/json/json.ml`) — a tiny, hardened pure JSON parser/serializer: bounded nesting
(≤1000, DoS guard), correct UTF-8 + surrogate-pair handling, non-finite → `null`, `%.17g` for
round-trippable doubles, integral-float cutoff at 1e15. Shared by the wire and the driver bridge.

**Two extended-JSON codecs — know the difference.** This trips people up:

| | `mongo/bson_json/bson_json.ml` | `fennec/ddp/ejson.ml` |
|---|---|---|
| Purpose | bridge to **libmongoc** (the C FFI boundary) | the **DDP wire** to browsers |
| Numbers | wrapped: `{$numberInt}`/`{$numberLong}`/`{$numberDouble}`/`{$numberDecimal}` | bare JS doubles |
| Int64 > 2^53 | preserved (string-wrapped) | **type identity lost** (→ float) — flagged |
| `$date` | `{$date:{$numberLong:"…"}}` | `{$date:<ms float>}` |
| Escapes | canonical Mongo forms unambiguous | 4 markers `$date/$binary/$type/$escape`; `{$escape}` wraps marker-shaped user objects |
| Native-only? | yes (uses `Json`) | no (rides to the browser) |

They are **separate by design** (driver needs lossless; browser has no Int64), but it means a
value that travels Mongo → `bson_json` → app → `ejson` → browser can lose Int64 *type* (value
survives within ±1e-15). Not hit in practice today (no Int64 in user-facing model fields).

---

## 2. Pure query/update engine (`mongo/query/*`)

The MongoDB semantics, in ~3k lines of dependency-light OCaml (Stdlib only, except `matcher`'s
`$regex` which uses `re`). All JS-portable. Verified faithful to Mongo; no bugs found.

- **`matcher.ml`** — selector matching. Two load-bearing rules: (1) range/comparison is **same-type
  only** (`{n:{$gt:"5"}}` never matches `n:10` — type bracketing via a scoped `bson_cmp`, distinct
  from `Bson.compare`); (2) a scalar predicate against an **array** field matches the array whole OR
  any element (`{tags:"a"}` matches `{tags:["a","b"]}`); array-ops (`$size`/`$all`/`$elemMatch`) bypass
  the fan-out. Dotted paths via `get_path`. `$regex` uses a bounded (4096) compiled-regex cache.
  Bitwise ops capped at bits 0–30 for native/JS determinism. Unknown operators never hide docs.
- **`modifier.ml`** — updates. `$set/$unset/$inc/$mul/$min/$max/$rename/$push($each/$position/$sort/
  $slice)/$addToSet/$pull/$pullAll/$pop/$bit/$setOnInsert`. Conservative numeric promotion (never
  downcasts Int64; non-numeric fields left untouched). Dotted paths auto-vivify intermediate docs.
  A non-operator doc is a full replacement (preserving `_id`).
- **`projection.ml`** — include/exclude (mode from first plain 0/1), dotted paths, `$slice`/
  `$elemMatch`, `_id` kept unless excluded. No positional `$` (needs the selector).
- **`sorter.ml`** — compiles a sort spec to a comparator (Schwartzian transform; missing < present;
  stable). Reuses `Bson.compare`.
- **`expr.ml` + `aggregate.ml`** — aggregation expressions + pipeline stages (`$match/$project/
  $addFields/$set/$unset/$sort/$limit/$skip/$count/$unwind/$group/$sortByCount/$sample/$replaceRoot/
  $lookup/$unionWith/$facet/$bucket`). `$lookup`/`$unionWith` resolve foreign collections through a
  caller-supplied `lookup : string -> Bson.t list` hook (the native backend lets mongod do it; the
  in-memory backend wires the hook across collections). `$sample` is deterministic head-n (no RNG).
- **`geo.ml`** — `$geoWithin/$geoIntersects/$near/$nearSphere`, pure computational geometry (ray
  casting, haversine with clamp), GeoJSON `[lng,lat]`, bounded parse cache. Filter only (no index).
- **`diff.ml`** — `diff_fields` (field-level changed/cleared, never `_id`) and `diff_ordered` (LCS so
  only genuinely-moved docs reposition). This is the shared engine behind BOTH observe paths.
- **`id.ml`** — `random_id` (17-char unmistakable alphabet) + `object_id` (24 hex) with an
  **injectable RNG** (deterministic in tests; `Math.random` in JS; splitmix in latency-comp).

---

## 3. In-memory engine (`mongo/mem`)

**`minimongo.ml`** — the in-memory collection. Storage = `Hashtbl id→doc` + reversed insert-order
list; mutations all follow `with_lock (commit + Fanout.enqueue) → Fanout.pump` (commit and enqueue
atomic under the lock, delivery outside it).

- **Unique indexes are enforced** via a per-index side-map (`key-tuple → id` Hashtbl) maintained
  live; conflict detection is O(1) per index, checked *before* commit → raises `Unique_violation`.
  Non-unique indexes are name-tracked (so reconcile's orphan-drop works) but **don't yet accelerate
  scans** — every query is O(n) (`minimongo.ml` notes this seam).
- **`observe_changes`** (field-level, unordered) maintains a **live window** for sort/skip/limit
  cursors: a doc entering displaces the boundary, one leaving promotes the next — a limited
  publication never leaks past its limit. Boundary short-circuit: a matching doc sorting strictly
  below a full unskipped window's tail can't enter → O(1) skip (hot-leaderboard optimization).
- **`observe`** (document-level, ordered) recomputes + `diff_ordered`s on each change (heavier).

**`fanout.ml`** — the change-fanout concurrency primitive (OCaml 5 multicore-safe). Discipline:
*locks guard snapshots/commits; subscriber callbacks NEVER run under a lock*; events enqueue
atomically with the producer's commit and are delivered in commit order by one drainer at a time.
`on_drained` is the **exact write-fence** (runs once everything enqueued-so-far is delivered) — this
is what makes a method's `updated` correct in-memory. Collapses to synchronous on a single domain /
in JS (uncontended mutex).

---

## 4. Native driver (the C libs)

Optional. When built, it's a self-contained static link of libmongoc; when it can't build, it
degrades and `:memory:` keeps working. Five sub-parts:

### 4a. Vendored source + the build (`mongo/vendor/`, `mongo/config/`)
- **`vendor/build.sh`** builds **mongo-c-driver 2.3.0** from a SHA256-pinned tarball
  (`mcd.tar.gz`, else downloaded) as **static archives** (`ENABLE_SHARED=OFF`, `ENABLE_STATIC=ON`,
  `ENABLE_PIC=ON`) with a **native TLS backend** (Secure Transport on macOS, OpenSSL on Linux) so
  the final binary **trusts the OS cert store** with zero setup — TLS to Atlas works on a clean
  host. SASL/snappy/zstd off, zlib bundled, SRV on, CSE/AWS-auth/tests/docs off. Built into a cache
  **outside the repo** keyed by `(version, OS, arch, libc, backend)` — survives `dune clean`,
  idempotent via a stamp file, partial-build cleanup. Prints the install prefix on stdout.
- **`config/discover.ml`** (dune-configurator) runs build.sh, reads `mongoc2-static` pkg-config, and
  performs the key trick: rewrite any dynamic `-lfoo` → the full `libfoo.a` path it can find (forces
  static inclusion on both ld64 and GNU ld/lld without `-Bstatic/-Bdynamic` juggling). Hunts extra
  lib dirs (OpenSSL's multiarch/`lib64`), fixes framework name casing (`CoreFoundation`/`Security`).
  **Degrade-safe:** any failure (no cmake/curl, unsupported OS, build error) → emits `-DHAVE_MONGOC=0`
  and no link libs; build survives the whole matrix.

### 4b. The C stubs (`mongo/ffi/mongoc_stubs.c`)
Guarded by `HAVE_MONGOC`. When 1, the real glue; when 0, every entry compiles to a stub that
`caml_failwith`s a clear "rebuild with cmake" message (and `available()` returns false). Three
design rules make it safe + Eio-friendly:
1. **Release the runtime lock around blocking I/O** — `caml_enter/leave_blocking_section` wraps every
   libmongoc call; the OCaml side runs each in an Eio systhread, so a blocking change-stream `next`
   never stalls the scheduler.
2. **Touch no OCaml value while the lock is released** — every input string is `strdup`'d to the C
   heap first, work goes into C buffers, then the lock is re-acquired to allocate the result.
3. **Handles are GC custom blocks with finalizers** — `pool` (a `mongoc_client_pool_t` + uri) and
   `change_stream` (a client checked out for the stream's lifetime) are released deterministically,
   even on exceptions. Data slots are NULL'd before the real pointer lands so a GC mid-alloc is safe.

The C boundary speaks **extended-JSON strings both ways** (`bson_init_from_json` /
`bson_as_canonical_extended_json` / `…relaxed…`). `find`/`aggregate` roll a hand-grown growable
buffer to assemble the result JSON array (libbson's `bson_string_t` was removed in 2.x), with OOM
guards. `mongo_write` is shared by insert/update/delete and owns/free's its strdup'd args on every
path (including the `caml_failwith` longjmp). A quiet log handler drops below WARNING.

### 4c. The OCaml FFI shim (`mongo/ffi/mongo_ffi.ml`)
Thin `external` declarations: `init`, `available`, `pool_new`, `ping`, `command`, `find`,
`aggregate`, `insert_one`, `update_one`, `delete_one`, `watch_open/next/close`. Raise `Failure` on
driver error. `available () = false` on the stub build.

### 4d. The safe OCaml layer (`mongo/driver/*`)
- **`internal.ml`** — `run f = Eio_unix.run_in_systhread f` (the one place that drops the runtime lock).
- **`client.ml`** — pooled handle; default uri `…?directConnection=true&maxPoolSize=256`
  (directConnection talks to one node, skipping RS discovery; pool 256 because an open change stream
  **holds a pooled connection for its whole lifetime**).
- **`collection.ml` / `database.ml`** — CRUD + index ops, bridging `Bson.t ↔ extended-JSON` per call.
- **`change_stream.ml`** — typed `operation` variant + `event {op; ns; document_key; full_document;
  resume_token; raw}`; `watch ~full_document ~max_await_ms ~resume_after`; `next` returns `None` on
  server timeout (no deadlock).
- **`live.ml`** — the live-query engine: **one change stream per collection**, fanned out to
  per-query views (canonical query key dedups identical queries), with the same windowed observe
  semantics as Minimongo (so the seam is uniform). A **stream budget** (default 200): once exceeded,
  collections fall back to **polling** (default 0.25 s) so the connection pool can't be exhausted.
  Reference-counted views/observers; orphan-stream detection; instrumentation counters. Daemons fork
  into the ambient Eio switch.
- **`runtime.ml`** — `MONGO_URL`/`:memory:`/`FENNEC_DB` → `Missing | Memory | Mongo{uri; db}`
  (default db `fennec`). `Missing` is a clear failure, never a hidden memory fallback.
- **`server.ml`** + **`mongod/mongod.ml`** — launch/adopt a mongod, wait for PRIMARY, init the
  replica set (`replSetInitiate` with `127.0.0.1:<port>`, never a hostname). `reuse` adopts an
  already-answering port; `ephemeral` uses a tmpfs dbpath wiped on stop (with an `at_exit` reap
  backstop). Replica set is required because change streams need an oplog.

### 4e. Portability guard (`mongo/portability/check.sh`)
A post-build assertion that the binary depends only on OS-provided shared libs (macOS `/usr/lib`,
`/System`; Linux libc/libm/libdl/pthread/resolv/…). Fails the build if a Homebrew/dev dylib leaked.
This is what turns "self-contained, no pre-install but mongod" from a hope into a tested invariant.

---

## 5. The Backend seam + runtime selection (`mongo/backend/`, `mongo/dynamic/`)

**`Backend.S`** — the storage contract: `insert/update/remove/find/find_one/count/aggregate/distinct`
over `Bson.t` + a shared `query = {selector; sort; skip; limit; fields}` + `observe_changes` +
`fence` + `ensure_index/drop_index/index_names`.

- **`Backend.Mini`** adapts Minimongo: `observe_changes` passes the *full* query (→ windowed live
  observe); `fence = Minimongo.on_drained` (**exact**).
- **`Fennec_mongo_dynamic`** is `Backend.S` over the native driver: writes go through driver
  collection/`command`, `observe_changes` uses `Live` (real change streams). Its `fence` is
  **best-effort (runs `k` immediately)** — mongod's change-stream delivery is async and v1 carries no
  resume-token plumbing, so a method's `updated` can briefly precede its deltas under lag (the
  resume-token fence is a noted seam). `aggregate` ignores the in-memory `lookup` hook (mongod does it).
- **`Fennec_mongo_dynamic.Dynamic`** is the runtime-selectable backend: `type collection = Mem of
  Minimongo.t | Native of Coll.t | Embedded of … | Missing of string`, one `Backend.S`, per-op dispatch.
  `from_env ~sw ~name` reads `Runtime.backend ()` → `Mem (Minimongo.create ())` for `:memory:`, an
  `Embedded` Burrow collection for `burrow://…`, a `Native` driver collection for `mongodb://…`, or
  `Missing` (ops fail clearly) when unset; the db comes from the URL. `Fennec.serve` installs an ambient
  switch (`set_switch`) and auto-boots the data layer + any mongosh wire endpoint (`boot`), so app and
  accounts code open collections by name via `Dynamic.collection ~name` — there is no `Pulse.start`.

---

## 6. The reactive + typed + facade layers (`fennec/pulse/...`)

- `Reactive.Make(Dynamic)` — publish/subscribe/methods + the observe multiplexer (one backend
  observe shared across identical `(collection, query)` subscriptions).
- `Typed.Make` / `collection/` — the typed layer over the Codec model. `reconcile` reconciles
  **indexes** (`reconcile_indexes` → `Backend.ensure_index` per declared `Index.t`, dropping only
  fennec-named orphans). `Filter`/`M`/`Sort`/`Proj`/`Index`/`Schema` are the typed handles.
- **`Schema`** (`collection/schema.ml`) renders the Codec `view` to a `$jsonSchema` document
  (bsonType widened for ints per EJSON reality; `opt` = absence from `required`, not a union;
  variants → `oneOf` with a tag enum; deliberately **no `additionalProperties`** so legacy docs stay
  writable). `Def.validator` exposes it.
- **`Fennec_pulse_app`** (`pulse/app/`) — the ambient facade an app actually uses: there is NO `start`
  (the data layer is auto-booted by `Fennec.serve`); `collection def` memoizes one `R.Collection` per
  name (over `Dynamic.collection ~name`) and reconciles its indexes once; `publish def` is **one call**
  that wires *both* the live DDP publication *and* the SSR seed; `method_` registers a typed handler;
  `serve_ddp` is the DDP paw. Example wiring is `examples/site/server.ml` (`seed`/`publish`/`method_` in
  `~on_start`).

---

## 7. Consumers (the "where we use it")

### 7a. Pulse realtime (the main consumer)
Collections defined by `[@@deriving collection ~name:"x"]` (e.g. `examples/site/web/store/task.ml`)
flow through the facade above. The browser runs the **same Minimongo** as a client cache, fed DDP
deltas via `merge_store` (per-field precedence merge), with optimistic stubs (sim writes) and the
write fence. So Mongo semantics are identical on both ends of the wire.

### 7b. Accounts — a *separate* Mongo-backed store (`fennec/accounts/accounts.ml`)
Accounts does **not** go through Pulse's typed collection. It has its own three-tier store:
a `Collection_store` abstraction (minimal `find_one/find/insert_one/update_one/delete_many` over
`Bson.t`) that wraps **either** `Minimongo` **or** `Fennec_mongo_driver.Collection` directly, with
12 collections (`accounts_users`, `_identities`, `_challenges`, `_passkeys`, `_orgs`,
`_org_memberships`, `_org_invites`, `_mfa_enrollments`, `_scim_*`, `_audit`). Its own `Codec`
submodule encodes each entity to `Bson.t`. `ensure_indexes` creates real Mongo indexes (unique
sparse on username/email; lookups on userId/stableKey) and is a no-op on Minimongo. `Store.unavailable`
stubs everything when `MONGO_URL` is unset. So Accounts is a second, parallel "way we deal with mongo."

### 7c. DDP wire (`fennec/ddp/`)
`ejson.ml` (the browser-facing extended-JSON, §1), `message.ml` (Added/Changed/Removed carry
`(string * Bson.t) list` field deltas + `cleared`), and `doc_hash.ml` (MD5-of-canonical-sorted-fields,
first 12 hex) which powers v2 delta-resync: the client sends `have:{coll:{id:hash}}` and the server
skips unchanged docs.

---

## 8. Dev/Test orchestration (the CLI side — `cli/`)

The single knob is **`MONGO_URL`**; the CLI owns who sets it:

- **`cli/dev/mongo_rs.ml`** — `ensure_dev ~root ~base_port`: if `MONGO_URL` is already set, do
  nothing; else if `mongod` is found, start a **managed single-node replica set `rs0`** at
  `base_port+80`, dbpath `_build/.fennec/mongo/dev-<port>`, and `export` MONGO_URL. If mongod is
  missing → warn, leave unset (features fail clearly). `Owned` (we spawned it) vs `Adopted` (a stable
  port already answering after e.g. a SIGKILL leak — initiate the RS via the driver without owning
  the process). The supervisor (`cli/dev/supervisor.ml:100,137,194`) calls it, tracks the pid, stops
  it on teardown. Replica set (not standalone) because Pulse's observe path needs change streams.
- **`cli/testcmd`** — defaults to explicit `:memory:` per suite (`instance.ml:44` →
  `Runtime.memory_url`) for fast deterministic tests; `--mongo` (`run.ml`) launches **one managed RS
  per suite** and overrides MONGO_URL in that suite's server env (`run.ml:301`). Reaped via `Reaper`.
- **`cli/dev/stublibs.ml`** — the bit that makes the **C lib reachable at runtime**: a
  directly-spawned bytecode dev/test server can't rely on `dune exec`/`opam env`, so `ensure ()` walks
  `_build/default` for dirs holding the libmongoc `dll*.so` stub and prepends them (plus the opam
  switch's `stublibs`) to `CAML_LD_LIBRARY_PATH`. Called before every spawn (`boot.ml:26`,
  `server_proc.ml:55`, `supervisor.ml:59`, `run.ml:318/429/502`). Without this, the dev server can't
  `dlopen` the mongo stub.

---

## 9. Findings — what was closed, what's an accepted tradeoff, what's left

Status as of branch `mongo-close-gaps` (off `main` @ `b69d76d`).

**CLOSED (committed, tested):**

1. ✅ **`$jsonSchema` validator is now INSTALLED.** `Schema.validator`/`Def.validator` produced the
   document but nothing applied it — DB-side structural validation was inert. Now `Backend.S` has
   `ensure_validator`; the typed layer installs `Def.validator` at attach (next to index reconcile).
   Native: `create`/`collMod` with `validationLevel:"moderate"`, so **mongod itself rejects
   shape-violating foreign writes**. Minimongo: a new `Query.Matcher.json_schema_matches` implements
   the structural subset `Schema` emits (bsonType/required/properties/items/enum/oneOf/length/pattern/
   bounds/uniqueItems), wired as the `$jsonSchema` operator and enforced on insert + moderate updates
   — the identical rule, for dev/test parity. Tests: `test_pulse.ml` (in-engine reject + moderate),
   `test_diff.ml` (native collMod rejects bad writes against a real mongod).
2. ✅ **Int64 is preserved across the DDP wire** (§1). `ejson` now encodes `Bson.Int64` as a
   `{$type:"Int64",$value:"<decimal>"}` string marker (type + full magnitude); the same codec
   compiles to the browser, so it's symmetric. Meteor wire byte-identity is unaffected.

**VERIFIED NON-ISSUES (no change — the original flags were wrong or immaterial):**

3. The `Bson_json` import in `accounts.ml` is **not dead** — it's used at lines 3779/3782/6484 for
   HTTP JSON responses/parsing.
4. Passkey `sign_count` (`Int32.to_int` → `Bson.int`) is lossless on 64-bit OCaml (int is 63-bit;
   `Int32.of_int` truncates back to the same bit pattern). The only exposure is a cosmetic negative
   number for counters ≥ 2^31 — i.e. ~2 billion authentications of one credential. Unreachable.
5. The identity `_id` `stableKey\000userId` NUL-join is collision-safe because both halves are
   generated ids (no NUL); changing the on-disk `_id` format would be a migration with no real payoff.

**ACCEPTED TRADEOFF (documented, deliberately not "fixed"):**

6. **Minimongo non-unique indexes don't accelerate scans (every query is O(n)).** This is correct for
   Minimongo's actual scale: it backs dev/test and the **browser client cache** (a user's subscribed
   subset) — never a large server-side dataset (the server uses the native driver against a real
   mongod with real indexes). A correct in-memory query planner would need multikey indexing,
   cross-type numeric key normalization (the matcher treats `Int 1` = `Float 1.0`), and type
   bracketing — disproportionate complexity for data that is small by construction. Non-unique indexes
   stay name-tracked (so reconcile's orphan-drop works); unique indexes are enforced. If a real
   Minimongo dataset ever grows, build the planner then, behind a parity test against the scan.

**OPEN (one real feature left):**

7. **Native `fence` is best-effort** (`fennec_pulse_mongo.ml`): runs `k` immediately, so under
   change-stream lag an optimistic client can briefly show pre-method state. In-memory `fence` is
   exact (`Fanout.on_drained`). The correct fix is clusterTime correlation: capture each write's
   `operationTime` from its reply, have `Live` track the max delivered change-event `clusterTime` per
   collection, and make `fence` block until delivered ≥ the last write's time (the write's own change
   event guarantees progress). It touches `client`/`collection`/`fennec_pulse_mongo`/`live`, is
   concurrency-sensitive, and needs a replica-set test (`test_observe` style) — a deliberate,
   separately-scoped change rather than a tail-end edit. Practical impact today is a sub-second
   flicker in rare lag windows (the same tradeoff Meteor shipped for years).

---

## 10. File index (where to look)

| Area | Path |
|---|---|
| BSON value + compare/equal | `mongo/bson/bson.ml` |
| JSON + the two extended-JSON codecs | `mongo/json/json.ml`, `mongo/bson_json/bson_json.ml`, `fennec/ddp/ejson.ml` |
| Query engine | `mongo/query/{matcher,modifier,projection,sorter,expr,aggregate,geo,diff,id}.ml` |
| In-memory + reactivity | `mongo/mem/{minimongo,fanout}.ml` |
| C driver: vendored build | `mongo/vendor/build.sh`, `mongo/config/discover.ml`, `mongo/portability/check.sh` |
| C driver: stubs + FFI | `mongo/ffi/{mongoc_stubs.c,mongo_ffi.ml}` |
| C driver: safe OCaml | `mongo/driver/{internal,client,collection,database,change_stream,live,runtime,server}.ml`, `mongo/mongod/mongod.ml` |
| Backend seam + selection | `mongo/backend/seam.ml`, `mongo/dynamic/{adapters,wire_server}.ml` |
| Typed/Codec/Schema/facade | `fennec/pulse/{typed.ml,collection/*,codec/*,app/fennec_pulse_app.ml}` |
| Accounts store | `fennec/accounts/accounts.ml` (`Collection_store`, `Codec`, `Store`) |
| DDP wire | `fennec/ddp/{ejson,message,doc_hash}.ml` |
| Dev/test CLI | `cli/dev/{mongo_rs,stublibs,supervisor}.ml`, `cli/testcmd/{run,instance,boot}.ml` |
