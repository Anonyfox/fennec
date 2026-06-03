# Fennec.Fur

An isomorphic OCaml UI runtime: the **same** `.mlx` components render on the server
(SSR → HTML string) and in the browser (js_of_ocaml → DOM, with true hydration).
Signals for reactivity, file-tree routing with typed paths, fast-render data, scoped
styles — no React, no Melange, no hand-written JavaScript.

> Status: this submodule is complete, tested, documented, and **the framework's only
> frontend story** — the React/Melange isomorphic stack it replaced has been removed.
> It is wired end-to-end into `examples/site` (the multi-app SSR + hydration example),
> whose `e2e/` drives a real headless Chrome over the DevTools Protocol — a hand-written
> WebSocket+CDP client on Eio, no npm, no Lwt, no chromedriver.

## Libraries

| `public_name` | role | target |
|---|---|---|
| `fennec.fur` | core: signals, vdom, SSR, head, data, router, reconciler (abstract `.mli`) | byte + native |
| `fennec.fur.html` | generated typed HTML elements (`<div>` ⇒ `Fur_html.div`) | byte + native |
| `fennec.fur.ppx` | mlx → Fur ppx: JSX, `<script setup>`, event/`each`/`key` sugar, scoped `[%%style]` | ppx |
| `fennec.fur.platform` | **virtual** platform interface (events, storage, history) | byte + native |
| `fennec.fur.platform_native` | inert stubs (SSR-safe by construction) | byte + native |
| `fennec.fur.platform_browser` | js_of_ocaml implementation | byte (jsoo) |
| `fennec.fur.client` | browser runtime: reconciler backend + head/data/router bindings + boot | byte (jsoo) |
| `fennec.fur.server` | Eio two-pass SSR render driver + mount dispatch | byte + native |
| `tools/` | `route_gen` (file-tree → typed routes) + `style_extract` (scoped scss) | exes |

The native-vs-browser split is a **dune virtual library** resolved at link time — no
runtime hook refs, no functors. An SSR binary links `fennec.fur.platform_native`; a
client links `fennec.fur.platform_browser`. Browser code therefore *cannot* reach the
server render, by construction.

## Authoring (a component / page)

`<script setup>` style — top-level bindings are the per-instance setup, `view` is the
reactive render; the ppx generates the component. No `open`, no `make`, no `fun () ->`:

```ocaml
let count = signal 0
let view =
  <div className="counter">
    <button onClick=(count -= 1)>"−"</button>
    <span className="count">(get count)</span>
    <button onClick=(count += 1)>"+"</button>
  </div>
```

- **State** — one primitive, `signal`: in setup it's local (per instance), in a shared
  module it's global. `get` subscribes, `set`/`update` notify; `watch` is a reactive
  side-effect. No providers, no prop-drilling.
- **Head** — `Head.title "…"` / `Head.meta ~name … `; deeper components override
  (key-deduped, last wins); isomorphic + rehydration-safe.
- **Data** — `Data.string "/api/x" ~fallback:"…" ()` is a reactive resource. SSR runs
  the fetch and embeds it (`window.__FUR_DATA__`); the client seeds with no flash;
  later/dynamic fetches hit the network. `~client_only` for browser-only data.
- **Routing** — file-tree (`index.mlx` / `[id].mlx` / `[...rest].mlx`) via `route_gen`,
  with a generated typed `Paths` (links are compile-checked). `p "/x/%d" n` for
  in-app, `ext "/other"` for outer reach.
- **Events / browser** — handlers stay `unit -> unit`; read the live event via
  `target_value ()` / `key ()` / `prevent_default ()`; `Browser.local_get/set/remove`
  for storage. All SSR-safe (inert on the server).

## Engineering properties

- **Typesafe by construction.** `signal` / `vnode` / `attr` / `Router.t` / `Data.t`
  are abstract behind `fur.mli`; the only sanctioned `Obj` is the documented total
  child/key coercion. The platform split is compile-time, not a runtime hook.
- **The reconciler is unit-tested.** `Fur.Reconcile (B : BACKEND)` runs against
  js_of_ocaml in the browser and an in-memory fake in tests, so the keyed diff +
  hydration are verified in milliseconds without a browser.
- **`.mli` is the recompile firewall.** Editing an implementation body never
  recompiles dependents; a component-edit warm rebuild is ≈ dune's own overhead.
- **Per-request isolation (TODO before concurrent server use).** A few pieces keep
  per-render state in module globals (`Head.sources`, `Data.seed`/`source`, a router's
  `current`). Fine on the client and the one-shot SSR binary; before fennec's
  concurrent Eio server uses Fur, give each request its own context (Eio fiber-local).
  No public API changes — only where the backing store lives. Tagged `IMPORTANT` in
  the source.

## Tests

```sh
dune exec fennec/fur/test/test.exe   # 63 checks: signals, matcher, head merge, SSR,
                                     # data, router, keyed reconcile (fake backend),
                                     # and a ppx-compiled component — pure, no browser.
```

Real-browser behaviour (hydration, events, data fetch/refetch, localStorage, SPA nav,
bundle isolation) is covered end-to-end by `examples/site/e2e` — a headless Chrome driven
over the DevTools Protocol from Eio (`sh examples/site/e2e/run.sh`).
