# fennec-cli — the `fennec` binary

A native JS/CSS bundler plus the dev + test CLI, in one self-contained binary. The native engines
(Go + Rust) link in at build time, so end users just download a prebuilt binary from
[GitHub Releases](https://github.com/Anonyfox/fennec/releases) (Homebrew + more platforms coming) —
no Node, no toolchain.
It sits *beside* dune (which owns the build graph); delete the CLI and plain `dune build --watch` +
`dune exec` still works — the decoupling is a contract ([`../docs/internal/CLI-INTEROP.md`](../docs/internal/CLI-INTEROP.md)).

Run `fennec` with no arguments to print the generated Markdown guide. It is terse enough for humans
and shaped so coding agents can learn the supported workflow directly from the installed binary.

## `fennec build`

Bundles JS (esbuild) and compiles/optimizes CSS + SCSS (Lightning CSS + grass) from one binary; the
engine is chosen per file extension. In a fennec project, dune rules call it; it's also a standalone
bundler.

## `fennec discover`

Answers task-shaped questions before code is written:

```sh
fennec discover "protect admin route with basic auth"
fennec discover "build an SSR page with a local counter"
fennec discover "upload a file from a multipart form"
fennec discover --browse Fennec.Paw
```

The answer comes from the framework snapshot generated from public interfaces, docs, examples, and
tests. It is not symbol lookup; it is the pre-edit orientation path for humans and agents that do not
yet know which Fennec API to use.

## `fennec dev`

Runs `dune build --watch` and supervises the server: a native fs-watcher reacts to build *outputs* —
restart the backend on a change, hot-swap CSS without a refresh. A felt loop around **~0.1 s**.
Discovers the server (the one executable that calls `Fennec.serve`), binds the real port (dev ≈ prod,
no proxy), and reaps the whole process group on teardown (no orphans, port reclaim).
When `MONGO_URL` is unset, dev auto-starts/adopts a local MongoDB replica set if `mongod` is
available; if not, the app still boots and database-backed features fail clearly when used.

For coding agents working on an app, `fennec dev --agent --attach` installs one guarded user-level
post-tool hook for supported harnesses. After application edits, the devserver verdict is injected
into the next model step, so agents do not need to remember ad-hoc `dune build` / `dune runtest`
probes. Recovery is `fennec agent status`; low-level hook commands are not the normal workflow.
Framework and monorepo work uses focused Dune checks instead.

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
