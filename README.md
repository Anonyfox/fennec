# fennec

A Meteor-style fullstack reactive framework for OCaml — native on the server,
[Melange](https://melange.re) on the client. Built for the desert: lean, fast,
and self-contained.

This is a **monorepo**: one root `dune-project` declares several independent,
separately-publishable packages.

| Package | What it is | Status |
| --- | --- | --- |
| [`fennec-buildkit`](./buildkit) | In-process JS bundler (esbuild) + CSS/SCSS engine (Lightning CSS + grass), statically linked | ✅ working |
| `fennec-mongo` | BSON, query, minimongo, Mongo driver | planned |
| `fennec-ddp` | DDP protocol over WebSocket | planned |
| `fennec` | Core: data, web, client, UI | planned |
| `fennec-cli` | The `fennec` binary + toolkit lib | planned |

Concurrency is **Eio-only** — a deliberate, forward-looking choice.

## fennec-buildkit

Drive a real frontend build pipeline from OCaml with no node, no subprocess, and
no second-language runtime. esbuild and the CSS engine are compiled to native
static libraries and linked straight into your binary, so bundling is a function
call:

```ocaml
(* production bundle *)
let js = Fennec_buildkit.Esbuild.build ~entry:"app.js" ~minify:true ()

(* warm context for millisecond incremental rebuilds (dev servers) *)
let ctx = Fennec_buildkit.Esbuild.create ~entry:"app.js" ()
let js  = Fennec_buildkit.Esbuild.rebuild ctx
let ()  = Fennec_buildkit.Esbuild.dispose ctx

(* SCSS -> minified CSS *)
let css = Fennec_buildkit.Css.scss ~minify:true ".btn { &:hover { color: red } }"
```

### How the native libraries are linked, seamlessly

The hard part of shipping native code is making it work on every OS with **zero
manual steps**. Buildkit does this entirely inside the normal `dune build`:

1. A single dune **rule** runs [`native/build.sh`](./buildkit/native/build.sh),
   which compiles the esbuild shim (`go build -buildmode=c-archive`) and the CSS
   shim (`cargo build --release`, `crate-type = ["staticlib"]`) **for the host
   platform**.
2. The same script detects the OS and emits `link_flags.sexp` — the exact system
   libraries this platform needs (macOS: `CoreFoundation`, `Security`, `resolv`,
   plus Rust's `native-static-libs`; Linux: `pthread`, `dl`, `resolv`, …),
   captured from `cargo rustc --print native-static-libs` so it is never guessed.
3. `dune` links the archives via `(foreign_archives …)` and the generated flags
   via `(c_library_flags (:include link_flags.sexp))`.

Source revisions are pinned (`go.sum`, `Cargo.lock`) for reproducible builds. The
only build-time requirement is a Go and a Rust toolchain, declared as opam
`depexts` (`conf-go`, `conf-rust`) so opam provisions them automatically.

### Public API

- `Esbuild.create / rebuild / dispose / build` — bundling with `format`
  (`iife`/`esm`/`cjs`), `global_name`, `external_`, `minify`, `sourcemap`,
  `banner`.
- `Css.transform` — optimize/minify modern CSS (nesting, `calc()`, dedupe).
- `Css.scss` — compile SCSS (vars, mixins, `@for`, functions) then optimize.

## Building

```sh
dune build       # compiles the native archives + the library
dune test        # runs the buildkit test suite
```

## License

MIT
