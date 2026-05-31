# fennec

A Meteor-style fullstack reactive framework for OCaml — native on the server,
[Melange](https://melange.re) on the client. Built for the desert: lean, fast,
and self-contained.

This is a **monorepo**: one root `dune-project` declares several independent,
separately-publishable packages.

| Package | What it is | Status |
| --- | --- | --- |
| [`fennec-cli`](./cli) | The `fennec` binary — native JS bundler + CSS/SCSS engine, statically linked | ✅ working |
| `fennec-mongo` | BSON, query, minimongo, Mongo driver | planned |
| `fennec-ddp` | DDP protocol over WebSocket | planned |
| `fennec` | Core: data, web, client, UI | planned |

Concurrency is **Eio-only** — a deliberate, forward-looking choice.

## fennec-cli

A single self-contained binary that bundles JavaScript (esbuild) and
compiles/optimizes CSS and SCSS (Lightning CSS + grass). **No Node, no Go, no
Rust, no toolchain** at the consumer side — the native engines are statically
linked into the binary at release time. Download it and build.

```console
$ fennec build src/main.ts styles/app.scss styles/extra.css
  src/main.ts       -> dist/main.js    1.2 KB
  styles/app.scss   -> dist/app.css    412 B
  styles/extra.css  -> dist/extra.css  198 B
```

One invocation drives **both** engines at once: each input is routed to the
right tool by its file extension (`.js/.mjs/.cjs/.jsx/.ts/.tsx` → esbuild;
`.css` → Lightning CSS; `.scss/.sass` → grass → Lightning CSS). Production
optimizations (minify, tree-shake, dead-code elimination, CSS nesting/`calc`
reduction) are **on by default**.

### `fennec build` flags

| Flag | Default | Effect |
| --- | --- | --- |
| `-o, --outdir DIR` | `dist` | Output directory (created if missing). |
| `--no-minify` | off | Disable minification. |
| `--format FMT` | `esm` | JS output format: `esm`, `iife`, or `cjs`. |
| `--global-name NAME` | — | Global var for the bundle's exports (with `--format iife`). |
| `--external MODULE` | — | Leave an import unbundled. Repeatable. |
| `--sourcemap` | off | Inline source map (JS only). |
| `--banner TEXT` | — | Text prepended to each JS bundle. |

`fennec build --help` documents everything with examples.

A long-running `fennec dev` (livereload dev server holding a warm build context)
is the next subcommand.

### How the binary stays toolchain-free for users

The native complexity lives in **exactly one place — the release pipeline**:

1. [`buildkit`](./buildkit) is an internal library that statically links the
   esbuild shim (Go `c-archive`) and the CSS shim (Rust `staticlib`) into OCaml
   via C FFI. A dune rule compiles both archives for the host and emits the
   per-OS linker flags (`buildkit/native/emit_flags.sh`).
2. CI builds and tests this on every target OS.
3. On a `v*` tag, [`release.yml`](./.github/workflows/release.yml) builds the
   `fennec` binary per platform and uploads it to GitHub Releases.

So end users get a prebuilt binary; only the framework's own CI ever needs Go and
Rust.

## Building from source

Requires Go and Rust toolchains (only for building the binary itself):

```sh
dune build              # compiles the native archives + the CLI
dune runtest            # runs the buildkit test suite
dune exec -- fennec --help
```

## Releasing

```sh
git tag v0.0.1 && git push origin v0.0.1   # triggers the Release workflow
```

## License

MIT
