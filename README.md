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
  let count = signal 0 in                       (* SETUP — runs once per mount (local state) *)
  <span className="counter">                    (* RENDER — re-runs on `count`; the trailing body *)
    <span className="clabel">(label ^ ":")</span>
    <button className="cbtn dec" onClick=(count -= 1)>"−"</button>
    <span className="count">(!count)</span>
    <button className="cbtn inc" onClick=(count += 1)>"+"</button>
  </span>
```

A component is **one function returning JSX**: the `let … in` lines above are setup (run once
when it mounts — create signals, subscribe), and the trailing JSX is the render (re-runs when a
signal it reads changes). No manual render-thunk `fun () ->` — the same single-function feel as
React/Solid/Svelte. (The explicit `… () = … fun () -> <jsx>` form still compiles.)

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

## Start your own frontend app

A Fennec frontend uses the `.mlx` dialect (JSX-identical: bare text + `{expr}`). The whole `.mlx`
toolchain is **`fennec-cli` and nothing else** — no external `mlx` package. The `fennec` binary IS
the **build** preprocessor (`fennec mlx-pp`, which `dune` runs on every `.mlx`), with the mlx parser
vendored straight into it; `ocamlmerlin-fennec-mlx` is the **editor** reader (also self-contained — it
parses `.mlx` in-process). One install gets the lot:

```sh
opam install fennec-cli   # the whole .mlx toolchain: fennec (= fennec mlx-pp) + ocamlmerlin-fennec-mlx
```

(The prebuilt `fennec` download above ships `ocamlmerlin-fennec-mlx` alongside it — that's everything;
there is no stock `mlx` to install.)

**Scaffold a working app** — one command, then build and run, editor already lit up:

```sh
fennec new hello-fennec
cd hello-fennec
dune build      # compiles; the .mlx is preprocessed by `fennec mlx-pp`
fennec dev      # runs with livereload — prints the local URL to open
```

**Or wire it by hand.** A `.mlx` app is a plain dune project with **one** extra stanza in its
`dune-project` — paste this verbatim (it tells dune to run `fennec mlx-pp` on `.mlx` and use the
fennec merlin reader in the editor):

```lisp
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader fennec-mlx)
  (preprocess
   (run %{bin:fennec} mlx-pp %{input-file}))))
```

…and declare `fennec` + `fennec-cli` in the package's `(depends …)` — no `mlx`. That's the whole
setup; the dialect is the only fennec-specific thing in the build, and `%{bin:fennec}` resolves it
exactly like the asset bundler.

**Editor (VS Code / any LSP).** The OCaml Platform extension / `ocamllsp` picks up
`ocamlmerlin-fennec-mlx` from your opam switch automatically — no settings to change — so `.mlx`
files get hover, completion, and jump-to-definition. The reader works off `fennec-cli` alone; the
optional `opam install ocamlmerlin-mlx` only adds richer error-recovery while you type. **If `.mlx`
IntelliSense is dead, or a build fails with a cryptic preprocessor error, run `fennec doctor`:** it
checks every piece of the toolchain (the `fennec` binary, the editor reader, the `(dialect …)`
stanza) and prints the exact fix for anything missing.

## AI-first loop

Fennec treats humans and coding agents as first-class users of the same CLI. Before editing, ask the
source-generated framework map what public path to use:

```sh
fennec discover "build an SSR page with a local counter"
```

During application iteration, supported agents such as Codex and Claude Code can attach to the dev
loop:

```sh
fennec dev --agent --attach
```

After that, compiler diagnostics, reload kind, affected surface, and inline-test results arrive in
the agent's next step automatically. Run plain `fennec` for the generated guide that explains the
human and agent workflow from the installed binary.

That attach loop is for apps built with Fennec. Contributors editing this framework repository use
normal focused `dune build` / `dune runtest` checks.

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
- **Genuinely fast** — in an apples-to-apples shootout against Go, Rust, Node, and Elixir ([`benchmarks/`](./benchmarks)), the HTTP layer's request-processing throughput lands between Go and Rust, while doing more per response than any of them.
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
