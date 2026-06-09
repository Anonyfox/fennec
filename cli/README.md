# fennec-cli — the `fennec` binary

A native JS/CSS bundler plus the dev + test CLI, in one self-contained binary. The native engines
(Go + Rust) link in at build time, so end users download a prebuilt binary — no Node, no toolchain.
It sits *beside* dune (which owns the build graph); delete the CLI and plain `dune build --watch` +
`dune exec` still works — the decoupling is a contract ([`../docs/internal/CLI-INTEROP.md`](../docs/internal/CLI-INTEROP.md)).

## `fennec build`

Bundles JS (esbuild) and compiles/optimizes CSS + SCSS (Lightning CSS + grass) from one binary; the
engine is chosen per file extension. In a fennec project, dune rules call it; it's also a standalone
bundler.

## `fennec dev`

Runs `dune build --watch` and supervises the server: a native fs-watcher reacts to build *outputs* —
restart the backend on a change, hot-swap CSS without a refresh. A felt loop around **~0.1 s**.
Discovers the server (the one executable that calls `Fennec.serve`), binds the real port (dev ≈ prod,
no proxy), and reaps the whole process group on teardown (no orphans, port reclaim).

## `fennec test`

Runs and verifies the app in five cuts, orchestrating dune and running each suite isolated +
deterministic. Authoring is a bare block — no `main`, no wiring:

| cut | authored as | what it does |
|---|---|---|
| `unit` | `let%test` / `let%prop` + doctests | inline unit + type-driven property tests |
| `http` | `let%http` | typed HTTP assertions against a booted instance |
| `browser` | `let%browser` | real headless-Chrome e2e (CDP — no Selenium) |
| `system` | `let%system` | dev-loop checks (spawn / port / fs), deterministic, no orphans |
| `docs` | — | doc-coverage gate (`--strict` to fail; `--promote` moves `.ml` docs to `.mli`) |

`fennec test all` runs them fast-to-slow. The testing *library* underneath is `fennec-hunt`
([`../hunt/README.md`](../hunt/README.md)) — usable standalone too.

---

Internal commands (not run by hand): `__esbuild-worker` (the warm worker `dev` spawns) and
`gen-doctests` (codegen for a dune rule). The dev-loop ↔ agent bridge is
[`../docs/internal/AGENT-FASTLANE.md`](../docs/internal/AGENT-FASTLANE.md).
