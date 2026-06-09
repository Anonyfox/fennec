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

## Get started

```sh
git clone https://github.com/Anonyfox/fennec && cd fennec
dune build && export PATH="$PWD/_build/default/cli:$PATH"
cd examples/site && fennec dev        # → http://localhost:4000, hot-reloading
```

**New to OCaml?** You don't need to be an expert — the docs open with a 5-minute orientation for
JS/Rails/Phoenix devs, then a zero-to-running quickstart. **Full docs index:
[`docs/README.md`](./docs/README.md)** (start here → guide per building block → reference).

## The monorepo

One root `dune-project`, several independently-publishable packages:

| Package | What it is | Status |
| --- | --- | --- |
| **`fennec`** | The runtime: HTTP core, Paw middleware, Eio HTTP/WS server, automatic HTTPS, the `Fur` UI runtime, and the reactive **data + realtime** layer (DDP, live queries, SSR-with-live-data) | ✅ working |
| **`fennec-cli`** | The `fennec` binary — a native JS/CSS bundler plus the dev + test CLI, one self-contained binary | ✅ working |
| **`fennec-hunt`** | Pure-OCaml app testing — inline unit + property tests, HTTP assertions, real-browser (CDP), and system/dev-loop checks; standalone or via `fennec test` | ✅ working |
| **`fennec-mongo`** | BSON, a pure Mongo query/update/projection/sort/aggregate engine, in-memory minimongo with reactive observe, extended-JSON, and an optional native libmongoc driver (change streams) | ✅ working |

Concurrency is **Eio-only**, by design.

## What's in the box

### `fennec` — the runtime

- **HTTP core** — request/response, MIME, HTTP-date and semantics, a WebSocket channel; RFC-correct, with colocated unit tests.
- **Paw** — an Elixir/Plug-style `conn -> conn` primitive: typed assigns, pipelines, routes, halting. Middleware, static, the websocket, and the SSR app are all paws. The batteries (logger, security headers, CORS, rate-limit, sessions, CSRF, basic-auth, force-https, …) are **opt-in** — nothing imposed by default, composed in order as needs grow ([`docs/PAW.md`](./docs/PAW.md)).
- **Server** — a compact Eio HTTP + WebSocket server: static serving with strong ETag / 304 / Range / HEAD, gzip + deflate negotiation (in-process zlib), WebSocket permessage-deflate, multi-app routing by Host, and a dev livereload relay.
- **Automatic HTTPS, in-process** — `serve ~acme:(Acme.auto ())` obtains and auto-renews Let's Encrypt certificates (zero-downtime hot-reload) for **every domain and subdomain your endpoints declare**, one line — no nginx, no reverse proxy; pure-OCaml TLS, no Lwt. **Multi-tenant**: wildcard certs via a pluggable DNS provider (DNS-01) and on-demand issuance for runtime customer domains. Bring your own cert with `serve ~tls`; behind a proxy or PaaS it just serves plain HTTP on `$PORT`. Pluggable cert storage — file (default), memory, or a custom store (k8s Secret / S3 / Redis) ([`docs/HTTPS.md`](./docs/HTTPS.md)).
- **Fur** — the isomorphic UI runtime: signals, a vdom + reconciler, SSR, js_of_ocaml hydration, a typed router, a `<Head>` manager, and data resources with fast-render seeds. No React, no Melange, no preact runtime.
- **Reactive data + realtime** — Meteor-style sync: DDP publications/subscriptions over WebSocket, a Mongo/minimongo query + observe engine with change-stream-backed **live queries**, and **SSR-with-live-data** — the server renders live data into the first paint, the browser hydrates it flicker-free, then the subscription streams updates. In-memory by default; an optional native Mongo driver for production ([`fennec/mongo/README.md`](./fennec/mongo/README.md)).

### `fennec-cli` — tooling

- **`fennec build`** — bundles JS (esbuild) and compiles/optimizes CSS + SCSS (Lightning CSS + grass) from a single statically-linked binary. The native engines (Go + Rust) are linked in at release time, so end users download a prebuilt binary — no Node, no toolchain.
- **`fennec dev`** — runs `dune build --watch` (dune is the sole source watcher) and supervises the server; a native fs-watcher reacts to build *outputs* to restart the backend or hot-swap CSS. A felt dev loop around **~0.1 s** (measured). Delete the CLI and a plain `dune build --watch` + `dune exec` still works — the decoupling is a contract, not an accident (see [`docs/internal/CLI-INTEROP.md`](./docs/internal/CLI-INTEROP.md)).
- **`fennec test`** — runs and verifies the app in five cuts: `unit` (inline tests + doctests), `http`, `browser`, `system`, and `docs` (doc-coverage, warn by default). It orchestrates dune and runs each suite isolated and deterministic — authoring is a bare `let%http` / `let%browser` / `let%system` block, no `main`, no wiring ([`docs/TEST-CLI.md`](./docs/TEST-CLI.md)).

### `fennec-hunt` — testing

- A pure-OCaml testing toolkit: inline **unit** (`let%test`) and type-driven **property** tests (`let%prop`, a lean qcheck-core layer — generators and counterexample printers derived from argument types), typed **HTTP** assertions (a hand-written client on Eio, no cohttp), a real-**browser** driver over the Chrome DevTools Protocol (auto-waiting page DSL, self-explaining failures, no chromedriver/Selenium), and typed **system** checks for the dev loop (spawn/port/filesystem, deterministic, no orphans).
- Two ways in: use it **standalone** as a library you drive yourself ([`fennec/hunt/README.md`](./fennec/hunt/README.md)), or through the **optimized CLI integration** — `fennec test` (unit / http / browser / system), where a suite is just a `let%http` / `let%browser` / `let%system` block — no `main`, no wiring ([`docs/TEST-CLI.md`](./docs/TEST-CLI.md)).

## Design commitments

- **Eio-only** — direct-style, structured concurrency, leak-free teardown under one switch. No Lwt, no cohttp.
- **No npm / no React / no Melange** — the client is a self-contained js_of_ocaml bundle.
- **dev ≈ prod** — the app binds the real port in dev exactly as in prod; no dev proxy. A dev build is bytecode for speed, release is native, and prod servers embed their assets into a single binary.
- **Curated interfaces** — `.mli` firewalls on the load-bearing modules, colocated tests throughout.

## Status & roadmap

Working and tested end to end (see [`examples/site`](./examples/site), the living DX benchmark):
the server, routing, isomorphic SSR + hydration, multi-app endpoints, the asset pipeline, the dev
loop, real-browser e2e, **automatic multi-tenant HTTPS**, and the **reactive data + realtime layer**
(DDP, Mongo/minimongo, change-stream live queries, SSR-with-live-data). **Next:** a `fennec new`
project scaffold and a guided first-app tutorial, then prebuilt per-platform CLI binaries.

## Build from source (contributors)

```sh
dune build            # build everything
dune runtest          # run the unit + integration suites
dune exec -- fennec --help
```

Building the CLI binary needs Go and Rust toolchains (only for the native bundlers); the
framework library itself needs neither. (Prebuilt per-platform `fennec` binaries are planned, so
app authors won't need the toolchains — see the [quickstart](./docs/QUICKSTART.md).)

**Docs: [`docs/README.md`](./docs/README.md)** — a 5-minute orientation + zero-to-running quickstart,
a guide per building block (Fur, Paw, HTTPS, data/realtime, testing, the CLI), and the package reference.

## License

MIT.
