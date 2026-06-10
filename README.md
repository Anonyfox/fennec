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

The `fennec` CLI ships as a prebuilt, self-contained binary — no Node, Go, or Rust toolchain:

```sh
# Linux x64 — for macOS Apple Silicon use fennec-macos-arm64. Homebrew + more platforms are coming.
curl -L https://github.com/Anonyfox/fennec/releases/latest/download/fennec-linux-x64 -o fennec
chmod +x fennec && sudo mv fennec /usr/local/bin/fennec
```

Building apps needs OCaml 5.x + opam ([install](https://ocaml.org/install)). Clone the repo and run
the example:

```sh
git clone https://github.com/Anonyfox/fennec
cd fennec/examples/site && fennec dev        # → http://localhost:4000, hot-reloading
```

Edit `frontend/apps/web/index.mlx` and save — the page hot-reloads ([`examples/site`](./examples/site)
is the full tour). Prefer to build the CLI yourself (adds Go + Rust)? See
[Build from source](#build-from-source-contributors).

## AI-first loop

Fennec treats humans and coding agents as first-class users of the same CLI. Before editing, ask the
source-generated framework map what public path to use:

```sh
fennec discover "build an SSR page with a local counter"
```

During iteration, supported agents such as Codex and Claude Code can attach to the dev loop:

```sh
fennec dev --agent --attach
```

After that, compiler diagnostics, reload kind, affected surface, and inline-test results arrive in
the agent's next step automatically. Run plain `fennec` for the generated guide that explains the
human and agent workflow from the installed binary.

**New to OCaml?** You don't need to be an expert. The mental map: opam ≈ npm, dune ≈ your bundler,
`.mlx` ≈ JSX, `signal` ≈ `useState`, paws ≈ middleware, `.mli` ≈ a `.d.ts`, and there's no `null`.
The compiler + Merlin/LSP carry the rest.

## Packages

One root `dune-project`, several independently-publishable packages — each README covers its main
modules in terse, incremental sections; the precise API is inline in the `.mli`s.

| Package | What it is |
| --- | --- |
| **[`fennec`](./fennec/README.md)** | The runtime: HTTP core, Paw middleware, the Eio HTTP/WS server, automatic HTTPS, the `Fur` isomorphic UI, and **Pulse** — the reactive data + realtime layer. **PWA support out of the box**: one declaration generates the manifest + service worker, and the app keeps working offline — warm cache, optimistic writes, user-confirmed updates |
| **[`fennec-cli`](./cli/README.md)** | The `fennec` binary — a JS/CSS bundler plus the dev & test CLI, one self-contained binary |
| **[`fennec-hunt`](./hunt/README.md)** | Pure-OCaml app testing — unit + property + HTTP + real-browser (CDP) + system checks |
| **[`fennec-mongo`](./mongo/README.md)** | BSON + a pure Mongo query/update/aggregate engine, in-memory minimongo, extended-JSON, an optional native libmongoc driver |

Everything is **Eio-only**, by design.

## Design commitments

- **Eio-only** — direct-style, structured concurrency, leak-free teardown under one switch. No Lwt, no cohttp.
- **No npm / no React / no Melange** — the client is a self-contained js_of_ocaml bundle.
- **dev ≈ prod** — the app binds the real port in dev exactly as in prod; no dev proxy. A dev build is bytecode for speed, release is native, and prod servers embed their assets into a single binary.
- **Curated interfaces** — `.mli` firewalls on the load-bearing modules, colocated tests throughout, and doc-coverage gated by `fennec test docs`.

## Status & roadmap

Working and tested end to end (see [`examples/site`](./examples/site), the living DX benchmark):
the server, routing, isomorphic SSR + hydration, multi-app endpoints, the asset pipeline, the dev
loop, real-browser e2e, **automatic multi-tenant HTTPS**, and **Pulse** — the **reactive data + realtime layer**
(DDP, Mongo/minimongo, change-stream live queries, SSR-with-live-data). The CLI ships as a prebuilt,
self-contained binary (Linux x64, macOS Apple Silicon) on every release. **Next:** a `fennec new`
project scaffold and a guided first-app tutorial; more release platforms and package managers (Homebrew).

## Build from source (contributors)

```sh
dune build            # build everything
dune runtest          # run the unit + integration suites
dune exec -- fennec --help
```

Building the CLI binary needs Go and Rust toolchains (only for the native bundlers); the framework
library itself needs neither. Architecture / decision records live in
[`docs/internal/`](./docs/internal/).

## License

MIT.
