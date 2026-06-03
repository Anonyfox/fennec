# CLI ⇄ dune ⇄ framework ⇄ app: the interop contract

This document is the law for how the four moving parts connect. It exists to keep
the seams clean and prevent spaghetti. If a change would violate a rule here,
the rule wins — change the design, not the rule (or change this doc deliberately).

## The four actors

| Actor | Package / location | Owns |
| --- | --- | --- |
| **dune** | (the build tool) | The build graph. The **only** source-tree watcher. Builds everything — the native OCaml server, the js_of_ocaml client bundle (Fennec.Fur), and assets (CSS/JS) — because assets are dune rules that call the CLI. |
| **CLI** (`fennec`) | `cli/`, package `fennec-cli` | Operational lifecycle only: `fennec build` (one-shot asset build, invoked *by* dune rules) and `fennec dev` (orchestrates `dune build --watch` + supervises the server process). Distributed as a prebuilt binary. |
| **Framework** (`fennec`) | `fennec/`, package `fennec` | The runtime: HTTP core, Eio server, and the livereload **relay** (holds the browser sockets; the CLI drives it). Watches nothing itself. Shipped to opam. |
| **User app** | e.g. `examples/site/` | A **plain dune project**. Depends on the framework lib; uses dune rules that call the CLI for assets. Knows nothing about the CLI's existence at the code level. |

## The load-bearing principle

> **dune is the only thing that watches source and builds. Everything else reacts
> to build *outputs* — never to source files, never to another tool's internals.**

A single reactor — the CLI — watches the build *outputs* it cares about, with one
cross-platform native fs-watcher (the `notify` crate):
- the built **server exe** → restart the server on change;
- the served **bundles** (the web root) → ping the framework's dev control socket
  to hot-swap CSS / full-reload on change.

The framework watches nothing; it only relays the CLI's frontend signal to the
browser over the livereload socket. Nobody parses dune's stdout. Nobody runs a
second source watcher. This is what keeps the parts decoupled.

## Touchpoints (the entire interface surface)

1. **CLI → dune**: a standard `dune build --watch <target>` invocation. No custom protocol.
2. **dune → CLI**: asset rules call `%{bin:fennec} build …`. Outputs are ordinary dune targets.
3. **CLI → app**: process lifecycle (spawn / signal / wait) + dev config via env (`FENNEC_ENV`, and `FENNEC_LIVERELOAD` = a unix-socket path). On a frontend edit the CLI sends one line (`css`/`reload`) to that dev-only loopback socket; the app relays it to browsers. That one line is the whole protocol.
4. **app ↔ browser**: the framework's livereload websocket (`/_fennec/livereload`) + an injected client script. Framework's concern entirely.
5. **Shared state across all of them**: the `_build` output dir + the port. That's it.

## Asset pipeline (how CSS/JS/npm get built)

Assets are **dune rules that call the CLI**, so there is ONE build graph and ONE
watcher for everything. Example (an app's `dune`):

```lisp
(rule (targets app.css) (deps src/app.scss)
 (action (run %{bin:fennec} build -o . src/app.scss)))
(rule (targets app.js)  (deps src/app.js)
 (action (run %{bin:fennec} build -o . src/app.js)))
```

- esbuild does node-module resolution itself, so `node_modules/` is just an input
  dir to the JS rule. `npm install` stays the user's action (a `fennec install`
  convenience may come later). Nothing here changes the model.
- `dune build --watch` rebuilds these incrementally on edit — that is what drives
  frontend livereload.
- Sources live in `src/` so input and output names never collide.

## Livereload, derived (not bolted on)

Two cases, one client mechanism (a persistent websocket; see `fennec/core/dev.ml`):

- **Backend change** (server/shared OCaml): dune rebuilds the exe → the CLI
  restarts the server → the livereload socket drops → the client reconnects when
  the new server is up → reload. **No server-side signal needed**; the disconnect
  *is* the trigger. Robust by construction (tolerates the server being briefly down).
- **Frontend-only change** (CSS/JS): the exe is untouched, so the server does
  **not** restart and the socket stays open. The CLI sees the served bundle change
  and sends the server a frame over the dev control socket: `css` → stylesheet
  hot-swap (no reload), anything else → full reload.

**All watching lives in the CLI, evented, in one place.** A single native
fs-watcher (the `notify` crate, vendored in the CLI's static archive — FSEvents /
inotify / kqueue / Windows) covers, recursively, both the rebuilt exe and the
served web root beneath it; it reacts the instant dune finishes writing, with no
polling in the steady state. Two refinements keep it honest:

- it watches the *served* web-root file (assembled *last*), so it never signals
  before the bytes the browser will fetch are in place;
- dune rewrites the whole web root (fresh mtimes for unchanged files) on every
  build, so the CLI gates on a **content hash**, not mtime — a rewrite with
  identical bytes is ignored, and a CSS-only edit never full-reloads the JS.

The framework watches nothing — it's a pure relay: it holds the browser sockets
and forwards the CLI's frame. This is deliberate. The framework runs inside the
user's lean server, which must **not** link the CLI's native archive (see
decoupling); putting all watching in the CLI keeps the server dependency-free
*and* makes every hop evented. The only timed loops left are the two with no event
to wait on: the browser's reconnect retry (nothing fires when a server returns)
and the CLI's ~1 Hz crash check (a crashed process emits no fs event).

The heavy *source-tree* watching is always dune's optimized native watcher; the
CLI never touches source. We never hand-roll a watcher — the CLI uses a proven
library.

The livereload script is injected into HTML responses **in memory** (before the
last `</body>`); it never rewrites a user file on disk. All of it is gated on dev
mode, so a prod build ships none of it.

## Decoupling guarantees (the point of all this)

- **Delete the CLI** → `dune build --watch` + `dune exec ./server.exe` is still a
  working dune project: it builds and serves, and a manual restart reloads the
  browser via the reconnect loop. You lose the *automation* — auto-restart and CSS
  hot-swap — because all output-watching lived in the CLI, by design. ← decoupling proof.
- **Delete dev mode** → it's all behind `FENNEC_ENV`; the prod server contains no
  livereload code, no watcher, no injected script.
- **No file acrobatics** → the user's source is never rewritten on disk.

## Decisions on record

- **All build-output watching lives in the CLI; the framework is a relay.** Exactly
  one process watches (one native fs-watcher, evented — no polling in the steady
  state), and the server stays free of any native watch dependency. That is what
  lets "delete the CLI" leave a clean dune project, and it makes every hop of the
  dev loop evented. The framework exposes a dev-only loopback control socket
  (`FENNEC_LIVERELOAD`) the CLI pings on a frontend edit; the server forwards the
  frame to browsers. Chosen over an in-server watch (which would either add a
  native dep to every prod binary, or force a poll inside the server).
- **No dev proxy.** The app binds the real port in dev exactly as in prod
  (dev≈prod fidelity). OCaml's sub-second rebuild + the browser's reconnect poll
  make restart downtime invisible. We accept it over a proxy that would diverge
  from prod.
- **Diagnostics / error overlay: DEFERRED (explicitly).** Basic livereload needs
  zero dune RPC. A browser error overlay (showing a compile error instead of a
  stale page) needs dune's *diagnostics* stream. We will adopt **`dune rpc`** for
  that when its surface is stable enough — NOT by parsing dune output (that would
  violate the principle). Marked here so it isn't forgotten. **← TODO: diagnostics.**

## The web root: bundles + `public/`, one tree

Bundle outputs are static files. So there is **one web root** — the output dir of
`fennec build` — holding every bundle AND the staged `public/` tree, served at
their paths. The server doesn't know any bundle's name; it serves files.

- **dune is the bundle manifest.** Each bundle is its own `fennec build` rule with
  its own flags and `--out-name` subpath (`--out-name admin/admin.js` →
  `/admin/admin.js`). Whitelabel/subapps = more rules, no central list.
- **No `assemble`/`embed` subcommand** — two flags on `build`:
  - `--include FILE` copies a pre-built bundle into the web root (clash-checked).
  - `--public DIR` stages a static tree in afterward (clash-checked vs bundles).
  - `--embed FILE` bakes the assembled web root into an OCaml `path→bytes` module.
- **Assembly is one `(dir webroot)` rule** that `--include`s the bundle file
  targets + `--public`s the tree. (dune wipes a dir target on each rebuild, so the
  bundles stay separate file targets and livereload watches THOSE, not the wiped
  webroot copies — preserving CSS-hot-swap vs JS-reload.)
- **Clashes are hard errors**, naming the path: bundle↔bundle (dune duplicate
  target) and bundle↔public (`fennec build` errors). No escape hatch.

Dev serves `webroot/` from disk; prod serves the embedded map — one
`Fennec_server.Static` path either way. *Verified: the prod binary serves both a
bundle (`/app.js`) and a public file (`/robots.txt`) with the `webroot/` dir
physically deleted.*

### Two orthogonal dev/prod axes (don't conflate them)

- **`FENNEC_ENV` (runtime)** — read by the running binary. Selects disk vs
  embedded serving, livereload on/off, the injected dev script. One flag, the
  sole *runtime* mode switch. `fennec dev` sets `FENNEC_ENV=development`; unset
  defaults to dev.
- **`--profile dev|release` (build)** — selects whether the binary actually
  *carries* the embedded assets. The embed module is generated with the real
  bytes only under `release`; under `dev` it is an **empty stub**
  (`lookup _ = None`) with no dependency on the asset bytes.

Why the build axis matters — measured: the embed module is compiled *into*
`server.exe`. If it always carried the real bytes, **a CSS edit would relink the
executable** (the module depends on `app.css`), dragging the OCaml exe recompile
into every asset edit (~0.9s). The dev stub breaks that dependency, so an asset
edit rebuilds only the bundle + reassembles `webroot/` — no relink. A runtime flag
can't fix a build-time link dependency; hence two axes. They align in practice
(dev profile + dev env together) but are not the same lever.

### The dev server is bytecode; release is native

A third use of the build axis, for raw iteration speed. The server executable is
`(modes byte exe)`:

- **dev** builds and runs `server.bc` (**bytecode**) — no native code generation
  and no native link. Both the per-module recompile and (especially) the link are
  far cheaper, and a plain `.bc` runs standalone in the dev environment (no custom
  runtime, unlike `byte_complete`, which does a slow C link).
- **release** builds the native `server.exe`.

`fennec dev` watches/runs the `.bc`; nothing else changes (the supervisor spawns a
file, native or bytecode alike). It's the same decoupling as the embed stub: a
build-time choice the runtime never sees.

### Dev-cycle speed (measured, `DUNE_CACHE` off, real edits)

The target is **"instant" ≈ 100 ms** of *felt* latency. `dune build` pays a fixed
~0.1 s startup per invocation; `fennec dev` runs `dune build --watch` as a daemon
and pays it once, so felt latency ≈ (numbers below − the ~0.1 s floor).

| edit → rebuild | `dune build` | ≈ `fennec dev` (daemon) |
| --- | --- | --- |
| `.scss` → web root | ~0.08 s | **< 0.1 s** |
| page `.mlx` → web root (CSR bundle) | ~0.19 s | ~0.09 s |
| page `.mlx` → `server.bc` (SSR, bytecode) | ~0.21 s | ~0.12 s |
| (same, native `server.exe`, for contrast) | ~0.48 s | — |

A page edit rebuilds SSR + CSR in parallel, so the felt loop is ~0.1 s — near
instant. The remaining floor is the per-file preprocessing (the mlx reader + the Fur
ppx) plus the js_of_ocaml step, inherent to the toolchain, not our wiring.

The `public/` tree is served verbatim at its paths (`public/img/logo.svg` →
`/img/logo.svg`).

Both modes go through one `Static` path that is HTTP-airtight: correct MIME by
extension, strong **ETag** (content hash) + **304** on `If-None-Match` /
`If-Modified-Since`, **Range** requests (`206` / `416`), `Cache-Control`,
`Accept-Ranges`, HEAD.

**Compression and headers apply to EVERY response** (static and dynamic SSR),
via `Responder.finalize`:

- gzip/deflate per `Accept-Encoding` (q-values honoured), only for compressible
  types past a min size, with `Vary: Accept-Encoding`. In-process real zlib.
- ETag + conditional 304, `Date`, correct `Content-Length`, HEAD → empty body.

**Decisions on record:**
- **Compression: zlib gzip everywhere, on demand** (static + dynamic), per
  request. Self-sufficient — no fronting proxy assumed (consistent with no-proxy).
- **WebSocket permessage-deflate (RFC 7692)** is implemented (RSV1 + raw-deflate,
  no_context_takeover) — required for real Meteor/DDP client interop, and good in
  general.

## Current status (site, verified)

The core is an Elixir/Phoenix/Plug-inspired primitive: a **Paw** (`conn -> conn`)
that touches a connection — middleware, routes, static, the websocket, and the SSR
app are all paws. An **Endpoint** binds a (host pattern, port) to a paw pipeline; a
server runs many endpoints (dev: each on its own localhost port; prod: shared port,
selected by Host pattern). See `examples/site/` for the full surface.

- `fennec build` builds CSS+JS as dune targets via `%{bin:fennec}`. ✓
- Paw core: typed assigns, pipelines, routes, halting — colocated unit tests. ✓
- Multi-endpoint / multi-app: two apps (web + admin) on two endpoints/domains,
  per-app asset bundles, shared components by reference. ✓
- Universal router + `.App` paw: path → `.mlx` page map with an SSR layout
  (overridable for whitelabeling). ✓
- Isomorphic SSR + hydration: one `.mlx` → server render + client hydrate +
  interactivity (proven end-to-end in a real headless Chrome via the Eio CDP e2e). ✓
- Helmet-like `<Head>`: metadata set in the tree, child-wins, identical SSR + CSR,
  via a per-render context sink (no globals, works on any React/Preact). ✓
- Static `public/`: dev-from-disk and prod-embedded, with MIME/ETag/304/Range. ✓
- Compression: gzip + deflate negotiation on static and dynamic, zlib in-process;
  WS permessage-deflate. ✓
- Web root from STABLE per-bundle file targets + one `--include` assembly rule, so
  livereload distinguishes CSS hot-swap from JS reload (proven via a WS client). ✓
- Bytecode dev server (`server.bc`) / native release; near-instant dev loop
  (~0.1 s felt) — see "Dev-cycle speed" above. ✓
- Cross-target Unicode is safe by construction: native SSR and js_of_ocaml both emit
  UTF-8 directly, so non-ASCII string literals just work — no delimiter dance, no ppx. ✓
- Livereload end-to-end (the websocket is itself a paw) + `fennec dev`
  orchestration. ✓
- Full framework unit suite (core + paw + ws/gzip/deflate + endpoint/static/head +
  Fennec.Fur) + the curl SSR integration test + colocated mlx component tests. ✓

SSR and CSR are the SAME source now: each app is one real Dune library (`web_app`,
`admin_app`) linked natively into the server (SSR via `Fur_ssr.handler`) and compiled
to JS via `js_of_ocaml` for the client (the `client/` executables, `(modes js)`) — no
React lib, no `[@react.component]` ppx, no Melange, so no `copy_files` mirror and no
shared `/react.js` runtime. A per-app client bundle is just the jsoo output, served
verbatim. Reset build state with `dune clean` (a full clean); manually deleting a
`_build/` subtree desyncs dune's incremental DB.

Not yet (future iterations): the DDP/reactive data layer (no mongo yet),
diagnostics overlay (`dune rpc`).
