# CLI ⇄ dune ⇄ framework ⇄ app: the interop contract

This document is the law for how the four moving parts connect. It exists to keep
the seams clean and prevent spaghetti. If a change would violate a rule here,
the rule wins — change the design, not the rule (or change this doc deliberately).

## The four actors

| Actor | Package / location | Owns |
| --- | --- | --- |
| **dune** | (the build tool) | The build graph. The **only** source-tree watcher. Builds everything — OCaml server, Melange client, and assets (CSS/JS) — because assets are dune rules that call the CLI. |
| **CLI** (`fennec`) | `cli/`, package `fennec-cli` | Operational lifecycle only: `fennec build` (one-shot asset build, invoked *by* dune rules) and `fennec dev` (orchestrates `dune build --watch` + supervises the server process). Distributed as a prebuilt binary. |
| **Framework** (`fennec`) | `fennec/`, package `fennec` | The runtime: HTTP core, Eio server, livereload. Reacts to build **outputs**, never to source. Shipped to opam. |
| **User app** | e.g. `examples/helloworld/` | A **plain dune project**. Depends on the framework lib; uses dune rules that call the CLI for assets. Knows nothing about the CLI's existence at the code level. |

## The load-bearing principle

> **dune is the only thing that watches source and builds. Everything else reacts
> to build *outputs* — never to source files, never to another tool's internals.**

Every reactor watches an *output artifact* it cares about:
- The **CLI supervisor** watches the built **server exe** (one file) → restart on change.
- The **framework** (dev mode) watches built **assets** (`app.css`, `app.js`) → push livereload frame on change.

Nobody parses dune's stdout. Nobody runs a second source watcher. This is what
keeps the parts decoupled.

## Touchpoints (the entire interface surface)

1. **CLI → dune**: a standard `dune build --watch <target>` invocation. No custom protocol.
2. **dune → CLI**: asset rules call `%{bin:fennec} build …`. Outputs are ordinary dune targets.
3. **CLI → app**: process lifecycle only (spawn / signal / wait) + dev config via env (`FENNEC_ENV`). No custom protocol.
4. **app ↔ browser**: the framework's livereload websocket (`/_fennec/livereload`) + an injected client script. Framework's concern entirely.
5. **Shared state across all of them**: the `_build` output dir + the port. That's it.

## Asset pipeline (how CSS/JS/npm get built)

Assets are **dune rules that call the CLI**, so there is ONE build graph and ONE
watcher for everything. Example (`examples/helloworld/dune`):

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

- **Backend change** (server/shared OCaml): dune rebuilds the exe → CLI restarts
  the server → the livereload socket drops → the client reconnects when the new
  server is up → reload. **No server-side signal needed**; the disconnect *is*
  the trigger. Robust by construction (tolerates the server being briefly down).
- **Frontend-only change** (CSS/JS): the server does **not** restart, socket stays
  open. The framework polls its own built asset's mtime (~3 Hz, Stdlib only — no
  native filewatcher, no platform divergence) and pushes a frame: `css` →
  stylesheet hot-swap (no reload), anything else → full reload.

The heavy source-tree watching is dune's optimized native watcher; our part is a
trivial portable output-mtime poll. We never write a real filewatcher.

The livereload script is injected into HTML responses **in memory** (before the
last `</body>`); it never rewrites a user file on disk. All of it is gated on dev
mode, so a prod build ships none of it.

## Decoupling guarantees (the point of all this)

- **Delete the CLI** → `dune build --watch` + `dune exec ./server.exe` still gives
  working livereload (via the reconnect loop). You lose only auto-restart and CSS
  hot-swap. The project was always a plain dune project. ← decoupling proof.
- **Delete dev mode** → it's all behind `FENNEC_ENV`; the prod server contains no
  livereload code, no watcher, no injected script.
- **No file acrobatics** → the user's source is never rewritten on disk.

## Decisions on record

- **No dev proxy.** The app binds the real port in dev exactly as in prod
  (dev≈prod fidelity). OCaml's sub-second rebuild + the browser's reconnect poll
  make restart downtime invisible. We accept it over a proxy that would diverge
  from prod.
- **Diagnostics / error overlay: DEFERRED (explicitly).** Basic livereload needs
  zero dune RPC. A browser error overlay (showing a compile error instead of a
  stale page) needs dune's *diagnostics* stream. We will adopt **`dune rpc`** for
  that when its surface is stable enough — NOT by parsing dune output (that would
  violate the principle). Marked here so it isn't forgotten. **← TODO: diagnostics.**

## Current status (helloworld, verified)

- `fennec build` builds CSS+JS as dune targets via `%{bin:fennec}`. ✓
- Framework serves SSR HTML + assets over the Eio server; livereload script
  injected in dev. ✓
- Livereload end-to-end: SCSS edit → `dune --watch` → `app.css` → `css` frame
  (hot-swap); JS edit → `reload` frame; backend edit → exe rebuild → server
  restart. ✓
- `fennec dev` orchestrates `dune build --watch` + server supervision. ✓

Not yet (future iterations): the Melange/MLX isomorphic client (SSR + hydration),
the DDP/reactive data layer, npm convenience, diagnostics overlay.
