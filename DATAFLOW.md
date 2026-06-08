# DATAFLOW — the database / realtime / client story (Meteor's heart, on fennec)

Status: **spec & decision record** (implementation follows). This is the single plan for how
data flows from MongoDB (or in-memory Minimongo) through publications, over DDP, into a browser's
reactive cache, and onto the screen — and for how that work is sliced into packages and
orchestrated by the CLI. It is grounded in the working prototype in the adjacent `ocaml-light`
repo (the `mongo/`, `meteor/core`, `meteor/ddp`, `meteor/client` trees) and in fennec's current
architecture (Fur signals, the Eio HTTP/WS server, the `build`/`dev`/`test` CLI, `fennec_buildkit`).

Read alongside: `examples/CLI-INTEROP.md` (the dune↔CLI boundary this extends) and
`docs/TEST-CLI.md` (the `:memory:` default that keeps tests mongod-free).

---

## 0. What this is, in one paragraph

Meteor's value is **end-to-end reactive data**: a server `publish`es a query; a client `subscribe`s;
the server streams `added`/`changed`/`removed` deltas as the data changes; the client merges them
into a local cache; the UI re-renders automatically. Methods (RPC) ride the same socket. We already
have the hard parts proven in `ocaml-light`: a real MongoDB driver with live queries, a
transport-agnostic reactive core, a DDP codec validated live against Meteor 3.x, and a pure
client-side merge box. fennec already has the better foundation underneath (an Eio HTTP/WS server
with permessage-deflate, Fur signals, a native bundler, a CLI that supervises processes). This plan
re-homes the data line onto fennec: a **standalone `fennec-mongo` package** + a **reactive/DDP layer
in the `fennec` framework** + a **client data layer on Fur** + **CLI help for mongod**.

---

## 1. The shape, in one picture

```
                         ┌───────────────────────────────── fennec-mongo (one opam package, no npm) ──┐
   native only           │  driver/  ── libmongoc + libbson, statically linked (FFI, Eio systhread) │
   (real MongoDB)        │  live/    ── change streams → observe_changes (field-level deltas)        │
                         │  ┌─────────── pure trio (native + js_of_ocaml) ────────────────────────┐ │
   pure, links into ─────┼─▶│ bson  ·  query (matcher/modifier/projection/sorter/diff/id)  · mem  │ │
   the jsoo client       │  │  (same source on server (native) and client (jsoo) — one toolchain) │ │
                         │  └  (mem = Minimongo: in-memory Mongo + simulated observe_changes)──────┘ │
                         └────────────────────────────────▲──────────────────────────────────────────┘
                                                           │ Backend.S  (insert/update/remove/find/count/observe_changes)
   server (fennec framework) ──────────────────────────────┴──────────────────────────────────────────
     Reactive.Make(Unified)  ── Collection · publish · subscribe · methods   ( :memory: ⇄ real mongo by URI )
                 │ observe_changes drives →  DDP session  (extended, sub-tagged; stateless hot path)
                 │                                  │ rides
                 ▼                                  ▼
            Fennec.serve / Endpoint        Paw.Websocket.make "/websocket"  +  "/sockjs" shim
                                           (fennec/server: Ws RFC6455 + permessage-deflate — REUSED)
   ───────────────────────────────────────────────│ DDP frames (added/changed/removed/ready/result/updated)
   client (js_of_ocaml) ────────────────────────────▼──────────────────────────────────────────────────
     ws client → DDP codec → merge box (§5b: per-(coll,id) existsIn + per-field precedence)
                                 │ writes winning docs into per-collection Minimongo
                                 ▼  notifies
                            Fur signal per collection  →  find = computed query · subscribe = effect+loading
                                 ▼
                            Fur vdom re-render
```

---

## 2. Decisions on record (made; this section is the contract)

1. **Real driver = libmongoc + libbson, statically linked in full** (the `fennec_buildkit`
   pattern: a build rule emits `libfennec_mongoc.a`, the OCaml lib links it via `foreign_archives`
   + `c_library_flags`). It is a *fair* external dep — lean, fast to build, carries its weight.
   **Minimongo is the default backend for dev and test** (`:memory:`, zero deps, deterministic).
2. **mongod is system-installed**, never compiled by us (compiling mongo is far too slow). The CLI
   **helps install / locate / launch** a system mongod (fetch the official prebuilt binary; the
   `ocaml-light/comet` orchestration already does a version of this — we rebuild it better). Real
   mongo is opt-in; `:memory:` covers the common dev/test loop.
3. **Latency compensation (optimistic UI) is deferred.** v1 is **server-only** method dispatch.
   We leave explicit placeholders/comments where client method stubs + the `updated` rollback
   barrier will go, and **the DX story for it is an open design discussion** (§9) — many options.
4. **`fennec-mongo` is its own package** (compiled mongo-client C + bson, plus Minimongo). The
   **`fennec` framework package depends on it** and bolts the Meteor-style reactive API on top.

---

## 3. `fennec-mongo` — the standalone data package

A self-contained MongoDB story, useful outside fennec, published as one opam package. Mirrors
`ocaml-light/mongo/` (which already has zero Meteor/DDP dependencies).

### 3.1 Library layout (sub-libraries under the `fennec-mongo` package)

| Sub-lib | Modules | Target | Purpose |
|---|---|---|---|
| `fennec-mongo.bson` | `Bson` | native + **js** | the value type (closed variant: Null/Bool/Int/Int64/Float/String/Document/Array/ObjectId/Date/Binary/Regex/Decimal128/…); pure, dep-free |
| `fennec-mongo.query` | `Matcher` `Modifier` `Projection` `Sorter` `Diff` `Id` | native + **js** | the pure query engine: selector ops ($eq/$ne/$gt/$in/$or/$elemMatch/…), update ops ($set/$inc/$push/…), projection, stable sort, doc diffing + LCS for ordered observe, id generation |
| `fennec-mongo.mem` (**Minimongo**) | `Minimongo` | native + **js** | in-memory Mongo: find/insert/update/remove/count + `observe_changes`/`observe` (simulated events off mutations). Pure. **The default backend.** |
| `fennec-mongo.json` | `Bson_json` | native only | Bson ⇄ MongoDB extended-JSON (Yojson-backed), the C-boundary format |
| `fennec-mongo.driver` | `Client` `Database` `Collection` `Change_stream` `Live` `Server` | native only | the real driver: libmongoc FFI; CRUD/find/aggregate/count/distinct/indexes/command; change streams → field-level `observe_changes`; single-node replica-set fixture for tests |

The **pure trio** (`bson`/`query`/`mem`) is what compiles to JavaScript and what the *client* reuses
verbatim — the client's Minimongo is literally the same module as the server's.

### 3.2 The native driver — static linking (the buildkit pattern)

The driver talks to MongoDB through **libmongoc 2.x** over its wire protocol; the OCaml↔C boundary
passes BSON as extended-JSON (`bson_init_from_json` / `bson_as_relaxed_extended_json`). Blocking
calls are pushed to a systhread via `Eio_unix.run_in_systhread` (releasing the OCaml runtime lock in
the C stubs), so change streams never stall the Eio scheduler.

Packaging mirrors `buildkit/dune` exactly:

```
(rule (targets libfennec_mongoc.a libfennec_mongoc.h)
      (deps (source_tree native))
      (action (run bash %{dep:native/build.sh})))      ; statically links libmongoc + libbson
(rule (target link_flags.sexp) (action (with-stdout-to … (run bash %{dep:native/emit_flags.sh}))))
(library (name fennec_mongo_driver) (modes native)
 (foreign_stubs (language c) (names mongoc_stubs))
 (foreign_archives fennec_mongoc)
 (c_library_flags (:include link_flags.sexp)))
```

Result: the **driver is one statically-linked archive**, no runtime libmongoc dependency — same
self-contained story as esbuild/Lightning-CSS. **Decided:** libmongoc + libbson are **vendored and
compiled from pinned source** into the archive (exactly like the Go/Rust shims) — reproducible, no
fetch/trust step, builds fast enough to carry its weight.

### 3.3 Publish: opam only — **no npm package**

- **opam** package `fennec-mongo` (native): the full driver + pure trio. A real OCaml MongoDB
  library, usable by any OCaml project.
- **No npm package.** The prototype's npm bundle existed *only* because its Melange/React client
  consumed Minimongo as JS modules. fennec's client is **js_of_ocaml**, so a fennec app links the
  pure trio straight into its own `(modes js)` client bundle (§7) — no separate artifact, no second
  toolchain, no Melange. The trio is plain OCaml; jsoo compiles it as part of the client. A
  standalone JS Minimongo for *non-OCaml* consumers is out of scope; if ever wanted it's a jsoo
  `Js.export` bundle (one toolchain), decided then — never Melange.

**Measured guarantee — the dev hot loop stays free.** dune's dev profile uses jsoo *separate
compilation*: each library is compiled to JS once and cached (`bson.cma.js`, `minimongo.cma.js`).
Linking the trio into a `(modes js)` client costs a one-time **~0.46s** cold jsoo build; thereafter
editing an app module re-jsoo's only that module and links the cached lib JS (**~0.11s**),
*unchanged* by the trio's presence. So the data layer adds nothing to the per-edit cycle.

### 3.4 The live-query contract (`observe_changes`)

The one reactive primitive the whole stack is built on, identical for both backends:

```
observe_changes query
  ~added:(id → fields → unit)            (* doc entered the result set *)
  ~changed:(id → fields → cleared → unit)(* field-level delta: changed fields + unset names *)
  ~removed:(id → unit)                    (* doc left the result set *)
  () : { stop : unit -> unit }
```

- **Minimongo**: events emitted synchronously off each mutation.
- **Real driver**: a MongoDB **change stream** per collection (requires a replica set — hence the
  single-node RS fixture), fanned out in-process with per-query field-level dedup; collections over
  a stream budget fall back to interval polling + reconcile.

---

## 4. The `Backend.S` seam (fennec-mongo ⇄ framework)

The only contract between the data package and the reactive framework. Tiny and total
(`ocaml-light/meteor/core/backend.ml`):

```ocaml
type query = { selector : Bson.t; sort : Bson.t; skip : int; limit : int; fields : Bson.t }
module type S = sig
  type collection
  val insert  : collection -> Bson.t -> string
  val update  : collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
  val remove  : collection -> Bson.t -> int
  val find    : collection -> query -> Bson.t list
  val find_one: collection -> query -> Bson.t option
  val count   : collection -> Bson.t -> int
  val observe_changes : collection -> query
      -> added:(string -> Bson.t -> unit)
      -> changed:(string -> Bson.t -> string list -> unit)
      -> removed:(string -> unit) -> handle
end
```

The framework provides one `Unified` implementation whose `collection` is
`[ `Mem of Minimongo.t | `Native of Driver.Collection.t ]` and dispatches per op — the
sqlite-style `:memory:` ⇄ `mongodb://…` flip lives entirely here.

---

## 5. The `fennec` framework — reactive data layer (server)

Ports `ocaml-light/meteor/core` (transport-agnostic; depends only on the mongo pure trio + driver).

- **`Reactive.Make(Backend)`** produces the userland surface: `Collection` (create/insert/update/
  upsert/remove/find/find_one/count, with `id_generation` STRING|MONGO and per-collection/per-cursor
  `transform`, plus `allow`/`deny` + `insert_from_client`), `publish`, `subscribe`, `methods`,
  `ObjectID`, `EJSON`.
- **Publications** are driven by `observe_changes`: `publish name (fun () -> cursor)` registers a
  closure; `subscribe` runs each cursor's `observe_changes` into a per-collection merge box and emits
  the deltas. **§5b decision:** on the hot path the server is *stateless per session* — it forwards
  the shared observe delta **tagged with the sub id** (extended mode) rather than maintaining a
  per-session document copy + diff. The full per-session merge box exists only as the compat shim for
  stock Meteor clients (§6).
- **Methods**: a dispatch table `invocation -> Bson.t list -> Bson.t`. **v1 server-only** (no client
  stub, no `updated` barrier beyond the trivial "writes done" signal). Placeholder comments mark the
  latency-comp seam (§9).
- **Reactivity here is pure `observe_changes` callbacks** — there is no server-side Tracker. (Fur's
  signal graph lives on the *client*, §7.)
- **Userland API** sits beside today's `Fennec.serve` / `Endpoint`: an app declares collections,
  publishes, and methods; `serve` wires the DDP endpoint automatically in dev/prod.

---

## 6. DDP — the wire + server session (on fennec's existing server)

Ports `ocaml-light/meteor/ddp` (codec + session are pure; depend only on `bson`).

- **Codec** (`Message` + `Ejson`): the full message catalog (connect/connected/failed,
  ping/pong, sub/unsub/nosub/ready, added/changed/removed[/addedBefore/movedBefore], method/result/
  updated, error) ⇄ JSON, with EJSON's four escapes (`$date/$binary/$type/$escape`). Numeric error
  codes coerced (real Meteor sends `"error":404`).
- **Session**: a pure state machine — `connect`→`connected`; `sub`→ run the publication, stream
  **sub-tagged** `added/changed/removed`, then `ready`; `unsub`→ stop + `nosub`; `method`→ dispatch,
  `result` + `updated`; `ping`/`pong`; `close`→ stop all subs.
- **Transport — REUSE fennec's server, do not port comet's.** The session rides
  `Paw.Websocket.make "/websocket" (fun (ch : Fennec_core.Ws_channel.t) -> …session loop…)`, using
  fennec's existing RFC 6455 `Ws` + permessage-deflate. A ~30-line `/sockjs` framing shim
  (`o`/`h`/`a[]`/`c[]`) accepts the *unmodified stock Meteor browser client*; our own client uses raw
  `/websocket`. (`ocaml-light/comet` was the prototype's predecessor to fennec — its server is what
  `fennec/server` already replaces, better.)
- **Interop is a hard requirement and a test target**: our frames → real Meteor 3.x, an independent
  DDP client → our server, and real Meteor frames → our codec — all proven in the prototype; re-prove
  under `fennec test`.

---

## 7. The client data layer (browser, js_of_ocaml + Fur)

**Keep the pure logic; rewrite only the reactive binding.**

**Keep (pure, → JS):**
- the **merge box** (`ocaml-light/meteor/client`, the §5b design): per-`(collection,id)`
  `existsIn : Set<subId>` + per-field precedence lists `[{sub,value}]` + per-sub contributed-id sets;
  one Minimongo collection per name holds the winning document; sub-stop is O(that sub's docs).
- the **DDP message codec** and **Minimongo** — the *same* `fennec-mongo` pure trio modules compiled
  to JS (not a client copy).
- the **DDP message dispatch** (decode frame → `merge_box.added/changed/removed` → notify).

**Rewrite on Fur (replaces the reason-react `useSyncExternalStore` hooks + `mel.raw` socket):**

| prototype (React-shaped) | fennec (Fur) |
|---|---|
| `useSubscribe name params : bool` via `useSyncExternalStore` | `subscribe name params` → effect that refcounts the sub + a `loading : bool` **signal** |
| `useFind coll selector ?opts : array` via `useSyncExternalStore` + `useRef` cache | `find coll selector ?opts` → a Fur **computed** over the per-collection store **signal** |
| per-collection store version + `on_change` listener | a Fur `signal` bumped on every merge-box write; `find`/`subscribe` read it, so they recompute automatically |
| `ws_connect` via `[%mel.raw]` `WebSocket` | a small js_of_ocaml WebSocket client (raw `/websocket` to our server; `/sockjs` dialer for real Meteor) |
| reason-react / Melange | **dropped entirely** — Fur signals + vdom |

This is strictly cleaner than the React version: the merge box already centralizes state, so a single
Fur signal per collection is the whole reactivity bridge — no `useSyncExternalStore`, no hook rules.

**SSR path** mirrors today's Fur data-seeding: on the server, `subscribe` collects the publication's
docs into the inline seed; the client hydrates the merge box from the seed so the first `find` is
synchronous (no loading flash).

---

## 8. CLI orchestration (mongod lifecycle)

Extends the `CLI-INTEROP.md` contract: **dune builds; the CLI supervises external processes and
reacts to outputs.** mongod is exactly such an external process — supervised like the app server is
today.

- **`fennec test` → `:memory:` by default.** Minimongo, no mongod, deterministic, parallel-safe
  (unchanged from `docs/TEST-CLI.md`). Real-mongo tests are opt-in (a flag / env that points at a
  system mongod, with the single-node replica-set fixture for change streams).
- **`fennec dev`** connects to a local mongod when the app's connection string is `mongodb://…`,
  else runs `:memory:`. The CLI **helps** with mongod: detect a system install; if absent, fetch the
  **official prebuilt** binary (SHA-verified, cached — never compiled) and offer to launch a
  dev-scoped single-node replica set (change streams need an RS). This is a thin `fennec`-side helper
  (a `mongo` setup/launch concern), not a build step.
- **Never in the build graph.** mongod is runtime-only; `dune build` never touches it. The CLI is the
  reactor that starts/stops it for the dev/test lifecycle, same boundary as everything else.

**Decided: no new verb.** mongod detection + launch fold into `fennec dev` (and opt-in
`fennec test`); install help is a guided prompt. The lean `build`/`dev`/`test` surface is preserved.

---

## 9. Latency compensation (DEFERRED — placeholders only)

v1 ships **server-only** methods: the client `call`s, the server executes, the client gets `result`
+ `updated`. No optimistic UI. We mark the seam explicitly so the future work is obvious:

- where the **client method stub** would run (a registered simulation against the client's Minimongo,
  producing optimistic writes tagged to the in-flight method id),
- where the **`updated` barrier** would reconcile (on `updated`, drop the method's optimistic writes
  and let the authoritative server data stand — the rollback),
- and the **DX question** (the open discussion): how a userland method declares its client
  simulation cleanly. Options to weigh later — a shared `let%method` that compiles to both ends; an
  explicit `~stub` argument; a separate client-only registration; auto-replaying the same handler
  against Minimongo when `is_simulation`. **Decide the DX before building it.**

---

## 10. End-to-end dataflow (the exhaustive trace)

**Write → screen (reactive read path), real mongo:**
1. A client (or method) writes: `Collection.insert/update/remove` → `Unified` → `Driver.Collection.*`
   → libmongoc → MongoDB.
2. MongoDB's change stream fires → `Driver.Live` computes a field-level delta → the publication's
   `observe_changes` callback runs.
3. The DDP session emits `added/changed/removed` **tagged with the sub id** (extended mode) over the
   websocket (permessage-deflate).
4. The client ws loop decodes the frame → `merge_box.{added,changed,removed}` updates the
   per-`(collection,id)` precedence view and writes the winning doc into the collection's Minimongo.
5. The merge box bumps that collection's **Fur signal**.
6. Every `find` computed over that collection recomputes; the Fur vdom diffs and patches the DOM.

**`:memory:` path** is identical except steps 1–2 collapse: the Minimongo mutation emits the
`observe_changes` event synchronously — same publication → DDP → client path. Same code, different
backend (the §2.1 flip).

**Read-only (initial) path:** `subscribe` → publication runs its cursor → `observe_changes` initial
`added` batch → `ready`. The client's `subscribe` effect flips `loading` false on `ready`; `find`
shows seeded data immediately (SSR) or after the first batch.

**Method path:** client `call` → `method` frame → server dispatch → writes flow through the reactive
path above → `result` + `updated`. (v1: no client-side optimism; §9.)

---

## 11. Package & dependency DAG (target)

```
fennec-mongo  (one opam pkg; pure trio links into the jsoo client — no npm)
   bson ← query ← mem(minimongo)            [native + js]
   bson → json → driver(libmongoc, static)  [native only]
        ▲
        │  (Backend.S)
fennec  (framework pkg)
   reactive core (Collection/publish/subscribe/methods)  ── depends on fennec-mongo (pure trio + driver)
   ddp (codec + session)                                  ── depends on fennec-mongo.bson
   server transport: REUSES fennec/server (Ws + deflate)  ── no new transport package
   client: merge box + ddp client + Fur hooks             ── fennec-mongo pure trio (js) + fennec.fur
fennec-cli
   mongod install/launch helper (dev + opt-in test)       ── fetch prebuilt, supervise; never compiles
```

`fennec-mongo` has **no** dependency on `fennec` / `fennec.fur` / DDP — the arrow points one way, so
it stays a clean standalone package.

---

## 12. Implementation phasing

1. **`fennec-mongo` pure trio** (bson + query + minimongo) — port, native + JS (jsoo; links into the
   client, no npm), unit/property-tested (`let%test`/`let%prop`). ✓ **DONE** (commit `c5df89a`).
2. **`fennec-mongo.driver`** — libmongoc + change-stream `Live`; integration test behind a
   real-mongod gate. **Decision reaffirmed (2026-06): vendored libmongoc over a pure-OCaml wire
   driver** — battle-tested, full feature coverage; the cost (C vendoring + FFI) is accepted.
   ◑ **FFI de-risked** — proved a C stub linking libmongoc connects to a lifecycle-launched mongod
   and runs a command (`{ping:1}` → PASS). Concrete build facts (mongo-c-driver 2.x via Homebrew):
   pkg-config names `mongoc2`/`bson2` (+ `mongoc2-static`/`bson2-static`, with `libmongoc2.a`/
   `libbson2.a` present — the static-vendoring path); `foreign_stubs (language c)` + `-cclib` link
   flags work; dynamic linking needs the lib dir on the loader path (rpath/static fixes this).
   **This is a PORT, not a greenfield build** — the adjacent `ocaml-light/mongo/` already has the
   complete driver, and we reuse its design wholesale:
   - **Marshalling = extended JSON via libbson** (NOT a hand-written BSON binary codec — that idea is
     dropped). The FFI passes canonical/relaxed extended-JSON strings; `bson_init_from_json` /
     `bson_as_canonical_extended_json` do the byte-level work. Decimal128 is just `{$numberDecimal}`
     (a string), so libbson handles it — no decimal128 binary math in OCaml.
   - **Vendoring is solved**: port `vendor/mcd.tar.gz` (mongo-c-driver 2.3.x source) + `vendor/build.sh`
     (static archives, native TLS → OS cert store, SASL/zstd/snappy off, idempotent per-OS cache) +
     `config/discover.ml` (`Configurator.V1`: build, then `mongoc2-static`/`bson2-static` pkg-config
     → flags, rewriting `-lfoo` → full `.a` path for static inclusion; a portability guard asserts
     "only system libraries ship").
   Progress:
   - (a) ✓ **`bson_json` DONE** — `fennec-mongo.bson_json` (+ `fennec-mongo.json`, a self-contained
     pure JSON), `Bson.t ↔ canonical extended JSON`, round-trip tested, jsoo-compiles.
   - (b) ✓ **buildkit + FFI DONE** — `fennec/mongo/{vendor,config,ffi,portability}`: `vendor/build.sh`
     (builds mongo-c-driver 2.3.0 from the vendored tarball as static archives, native TLS, per-OS
     cache), `config/discover.ml` (`Configurator.V1` → static flags, full `.a` paths, degrades to
     `HAVE_MONGOC=0` where it can't build so the matrix stays green — verified the stub branch
     compiles), `ffi/mongoc_stubs.c` + `mongo_ffi.ml` (the raw binding, extended-JSON across the
     boundary, runtime-lock released per call, GC-managed pool/change-stream handles, `available`
     flag). Proven end to end: connect + ping + insert + find against a lifecycle-launched mongod,
     and the test binary depends on **OS libraries only** (otool/ldd guard `portability/check.sh`,
     run on `dune test`) — self-contained + statically linked, the downstream-binary guarantee.
   NEXT: (c) port the Eio layer (`client`/`collection`/`database`/`change_stream`) wrapping the raw
   FFI in Eio systhreads, exposed as a `Backend.S` impl (insert/update/remove/find/find_one/count +
   `observe_changes` via change streams) so `Reactive.Make` runs over real mongo unchanged.
   (d) differential correctness tests — every op through Minimongo AND real mongo (gated by the
   lifecycle manager), results asserted equal.
3. **Backend.S + Reactive core** in `fennec` — Collection/publish/subscribe/methods over the
   backend seam; runs on `:memory:` now (native backend slots in behind `Backend.S` later).
   ✓ **DONE** — `fennec/data` (`fennec.data`): `Backend` + `Reactive.Make` + `Mini`, 15 tests,
   compiles to JS.
4. **DDP codec + session** — pure, unit-tested. ✓ **DONE** — `fennec/ddp` (`fennec.ddp`): `Json`
   + `Ejson` + `Message` + `Session` (extended sub-tagged mode) + `Sockjs`, 9 tests, compiles to
   JS. (Re-proving captured real-Meteor frames rides along with the server wiring in Phase 5.)
5. **DDP on fennec's server** — `Paw.Websocket.make "/websocket"` + `/sockjs` shim. ✓ **DONE** —
   `fennec/realtime` (`fennec.realtime`): `Make(R)` bridges a reactive instance to a DDP session
   (delta-driven — publications feed the session sink from `run_publication`/`observe_changes`, not
   the merge box; methods route through `R.call`), with `serve`/`serve_sockjs`/`paw`. Proven end to
   end over a fake `Ws_channel` (connect/sub→tagged-added+ready, live delta push, method result,
   method-error code, sockjs framing) AND over a **real WebSocket**: `examples/site/e2e/realtime_e2e`
   is a self-contained `(test)` (CI-run via `dune runtest`) — a minimal RFC 6455 server (fennec's
   `Ws` codec) wires a DDP session, and a `Cdp` WebSocket client does a full connect/subscribe/method
   round-trip asserting the live server→client push (the method's insert → `observe_changes` →
   sub-tagged `added` → frame). The whole server stack across a socket, deterministic, no browser.
6. **Client** — ✓ **DONE**. `fennec/live` (`fennec.live`): the §5b `Merge_store` (precedence +
   refcount + progressive enrichment), `Subkey`, the Fur `Live.find` binding (a signal that
   recomputes as the store changes), `seed` for SSR hydration. `fennec/live/client`
   (`fennec.live.client`): the DDP WebSocket client, isomorphic via a virtual module (browser impl =
   a real Js_of_ocaml WebSocket; native stub for SSR), with `connect`/`subscribe`/`call`/`find`.
   Wired into `examples/site` as a live task list (`task_list.mlx` + the `/ddp` endpoint), and
   **proven in a real headless Chrome** (`test/browser`): subscribe renders the seeded docs, and
   addTask pushes a new doc back through the open subscription into the DOM — the whole loop,
   fennec-mongo → reactive → DDP → realtime server → jsoo client → merge store → Fur signal → DOM.
7. **CLI mongod helper** — detect/fetch/launch a dev mongod; `:memory:` stays the test default.
   ◑ lifecycle lib **DONE** — `fennec/mongo/mongod` (`fennec-mongo.mongod`, native/Unix): `find` +
   `install_hint` + `start`/`stop`/`with_ephemeral` (own data dir + free port, waits until it
   accepts connections, graceful SIGTERM→SIGKILL, ephemeral dir auto-removed). Proven against a real
   mongod 8.3.3 (launch→connect→stop→cleanup). Remaining: fold it into `fennec dev`/`fennec test`
   (no new verb) so a real mongod is one flag away.
8. **(Stretch) latency compensation** — only after the DX discussion (§9).

---

## 13. Decisions (resolved) + the one parked item

All resolved, applying the project's established principles:

- **libmongoc sourcing — vendor + compile from pinned source** into the static archive (buildkit
  pattern; reproducible, no fetch). (§3.2)
- **mongod CLI surface — no new verb.** Detection + launch fold into `fennec dev`; `fennec test`
  stays `:memory:` by default with opt-in real-mongo; install help is a guided prompt. (§8)
- **Change streams in dev — the CLI auto-stands-up a single-node replica set** when real-mongo is
  used (change streams require an RS); `:memory:` Minimongo is the default everywhere else and needs
  none.
- **No npm package — opam only.** The npm bundle was a Melange/React-era need; fennec's jsoo client
  links the pure trio directly into its own client bundle, with **zero added per-edit cost** (dune
  separate compilation caches each lib's JS — measured one-time ~0.46s, hot loop ~0.11s; §3.3). A
  standalone JS Minimongo is out of scope unless a non-OCaml consumer appears — then a jsoo
  `Js.export` bundle, never Melange.
- **Ordered publications (`addedBefore`/`movedBefore`) — defer.** Decode them in the codec (cheap,
  keeps Meteor interop) but don't implement merge-box ordering until a sorted-pub use case is real.
- **Userland API ergonomics — its own later DX pass.** Land the mechanics end-to-end first, then do
  a dedicated authoring-DX pass (the way the testing story earned `let%http` / `fennec test`), once
  the real shape can be felt.

**Still open by deliberate decision** — the **optimistic-UI / latency-compensation DX** (§9): how a
userland method declares its client simulation cleanly. To be designed *before* it's built; options
are enumerated in §9. This is the only unresolved decision, and it's parked on purpose.
