# fennec

An **isomorphic web framework for OCaml** — a native [Eio](https://github.com/ocaml-multicore/eio)
server and a signals-based UI runtime (`Fennec.Fur`) that renders on the server and hydrates
in the browser, from one source. **No npm, no React, no Melange, no Lwt.** One language, end
to end. Built for the desert: lean, fast, self-contained.

You write components in OCaml. They server-render to HTML for the first paint and become
interactive in the browser via [js_of_ocaml](https://ocsigen.org/js_of_ocaml/) — no client
framework runtime, no bundler magic, no `node_modules`.

## A taste — one isomorphic component

```ocaml
(* counter.mlx — server-rendered, then hydrated in the browser, same source *)
[%%style {scss| .count { font-weight: 700; min-width: 2ch } |scss}]   (* colocated, auto-scoped *)

let make ?(label = "count") () =
  let count = signal 0 in                       (* local reactive state, live after hydration *)
  fun () ->
    <span className="counter">
      <span className="clabel">(node (label ^ ":"))</span>
      <button className="cbtn dec" onClick=(count -= 1)>"−"</button>
      <span className="count">(get count)</span>
      <button className="cbtn inc" onClick=(count += 1)>"+"</button>
    </span>
```

The `[%%style]` block is extracted and scoped to this component (a `data-fur` hash) — no
external `.scss`, no className collisions. The same file is linked natively into the server
for SSR and compiled to JS for the client. That's the whole model.

## The monorepo

One root `dune-project`, several independently-publishable packages:

| Package | What it is | Status |
| --- | --- | --- |
| **`fennec`** | The runtime: HTTP core, Paw routing, Eio HTTP/WS server, and the `Fur` UI runtime | ✅ working |
| **`fennec-cli`** | The `fennec` binary — native JS/CSS bundlers + the dev loop, one self-contained binary | ✅ working |
| **`fennec-hunt`** | Pure-OCaml app testing — HTTP assertions, real-browser (CDP), and system/dev-loop checks; standalone or via `fennec test` | ✅ working |
| `fennec-ddp` · `fennec-mongo` | The Meteor-style reactive data layer (DDP over WebSocket, mongo/minimongo) | 🧭 roadmap |

Concurrency is **Eio-only**, by design.

## What's in the box

### `fennec` — the runtime

- **HTTP core** — request/response, MIME, HTTP-date and semantics, a WebSocket channel; RFC-correct, with colocated unit tests.
- **Paw** — an Elixir/Plug-style `conn -> conn` primitive: typed assigns, pipelines, routes, halting. Middleware, static, the websocket, and the SSR app are all paws.
- **Server** — a compact Eio HTTP + WebSocket server: static serving with strong ETag / 304 / Range / HEAD, gzip + deflate negotiation (in-process zlib), WebSocket permessage-deflate, multi-app routing by Host, and a dev livereload relay.
- **Fur** — the isomorphic UI runtime: signals, a vdom + reconciler, SSR, js_of_ocaml hydration, a typed router, a `<Head>` manager, and data resources with fast-render seeds. No React, no Melange, no preact runtime.

### `fennec-cli` — tooling

- **`fennec build`** — bundles JS (esbuild) and compiles/optimizes CSS + SCSS (Lightning CSS + grass) from a single statically-linked binary. The native engines (Go + Rust) are linked in at release time, so end users download a prebuilt binary — no Node, no toolchain.
- **`fennec dev`** — runs `dune build --watch` (dune is the sole source watcher) and supervises the server; a native fs-watcher reacts to build *outputs* to restart the backend or hot-swap CSS. A felt dev loop around **~0.1 s** (measured). Delete the CLI and a plain `dune build --watch` + `dune exec` still works — the decoupling is a contract, not an accident (see [`CLI-INTEROP.md`](./examples/CLI-INTEROP.md)).

### `fennec-hunt` — testing

- A pure-OCaml testing toolkit: typed **HTTP** assertions (a hand-written client on Eio, no cohttp), a real-**browser** driver over the Chrome DevTools Protocol (auto-waiting page DSL, self-explaining failures, no chromedriver/Selenium), and typed **system** checks for the dev loop (spawn/port/filesystem, deterministic, no orphans).
- Two ways in: use it **standalone** as a library you drive yourself ([`fennec/hunt/README.md`](./fennec/hunt/README.md)), or through the **optimized CLI integration** — `fennec test` (unit / http / browser / system), where a suite is just a `let%http` / `let%browser` / `let%system` block — no `main`, no wiring ([`docs/TEST-CLI.md`](./docs/TEST-CLI.md)).

## Design commitments

- **Eio-only** — direct-style, structured concurrency, leak-free teardown under one switch. No Lwt, no cohttp.
- **No npm / no React / no Melange** — the client is a self-contained js_of_ocaml bundle.
- **dev ≈ prod** — the app binds the real port in dev exactly as in prod; no dev proxy. A dev build is bytecode for speed, release is native, and prod servers embed their assets into a single binary.
- **Curated interfaces** — `.mli` firewalls on the load-bearing modules, colocated tests throughout.

## Status & roadmap

The server, routing, isomorphic SSR + hydration, multi-app endpoints, the asset pipeline,
the dev loop, and real-browser e2e are working and tested end to end (see
[`examples/site`](./examples/site), the living DX benchmark). **Not yet built:** the
Meteor-style reactive data layer — DDP over WebSocket and a mongo/minimongo client. That is
the next major chapter.

## Build

```sh
dune build            # build everything
dune runtest          # run the unit + integration suites
dune exec -- fennec --help
```

Building the CLI binary needs Go and Rust toolchains (only for the native bundlers); the
framework library itself needs neither. End users install a prebuilt `fennec` binary per
platform from GitHub Releases.

Deeper docs: [`examples/site/README.md`](./examples/site/README.md) ·
[`examples/CLI-INTEROP.md`](./examples/CLI-INTEROP.md) ·
[`fennec/fur/README.md`](./fennec/fur/README.md) ·
[`fennec/hunt/README.md`](./fennec/hunt/README.md) ·
[`docs/TEST-CLI.md`](./docs/TEST-CLI.md).

## License

MIT.
