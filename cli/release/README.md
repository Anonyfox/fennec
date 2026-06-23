# `fennec release` — the production build, verified

`fennec release` turns a Fennec project into a deployable artifact in one step and refuses to ship a
broken one. Plain `dune build --profile release` already produces the same binary — the embed and
web-root rules are dune rules, gated on the release profile — so this command is **not** a second build
engine. It is the ergonomics, the verification, and the deploy contract around that build: the parts
that are easy to get wrong and fail *silently*.

```
fennec release            # discover → build native (release) → verify → strip → stage ./dist → contract
fennec release --check    # build + verify only (CI gate); stage nothing, write nothing
fennec release --docker   # also write a runtime ./Dockerfile
```

## What it guards

A plain release build lets two things fail without a word:

1. **A non-lean binary.** A production server must link none of the heavy e2e/CDP test machinery
   (`Fennec_hunt__Cdp`/`Chrome`/`Http_client`, `Yojson`). `Lean` scans the built binary's bytes for
   those module names and refuses to stage a leak. (The same invariant is enforced at `dune runtest` by
   `examples/site/check_lean.ml`; the two needle lists are kept in sync by hand so that guard stays
   zero-dependency.)
2. **An un-embedded web root.** Under `--profile release` an app's `assets.ml` should be the real embed
   (`fennec build --embed`); if the rule didn't run it silently falls back to the dev stub
   (`lookup _ = None`) and the binary 404s every asset. `Verify` reads the generated module and refuses
   the stub. An API/SSR-only server (no web root at all) is fine — there is simply nothing to embed.

And it surfaces the runtime **deploy contract**: the deploy-specific environment the binary needs (a
`MONGO_URL`, the port). There is **no mode flag** — the binary is production by default. Fennec keys
dev-vs-production off how the binary was *built*, not an env var: the dev loop runs the **bytecode**
`server.bc`, a release is the **native** `server.exe`, so `Sys.backend_type` decides
(`Paw.Dev_proto.is_dev`). A release just runs as production. `FENNEC_ENV=development|production` remains
an explicit override for either (e.g. running a native build locally in dev mode).

## The pieces (a pure pipeline, no Eio, no compiler, no app code)

```
Target.resolve  →  Build.run  →  gate (Lean + Verify)  →  Stage.run  →  Contract
```

| Module | Role |
|---|---|
| `Lean` | the prod-lean scan — pure Stdlib, scans a binary for forbidden dev/test module needles. |
| `Target` | resolve the deployable via `Fennec_dev.Discover` (the one `Fennec.serve`), pointed at the native `.exe` + its `assets.ml`/`webroot` under `_build/default`. |
| `Build` | the one-shot `dune build --profile release <target>` (dune's output streams through; pinned to `_build/default`). |
| `Verify` | classify the build's `assets.ml`: `Embedded n` / `Stub` / `No_webroot`. |
| `Stage` | copy the artifact out of `_build/`, chmod, and `strip` it into `./dist`. |
| `Contract` | the staged-artifact report, the run line, the env table, and the optional Dockerfile. |
| `Release` | the orchestrator the CLI subcommand calls (`Release.run : opts -> int`). |
| `Util` | the few filesystem primitives the phases share (`read_file` / `mkdir_p` / `count_files`). |
| `Cross` | `--target` cross-builds via Docker — platform parsing, the multi-stage build Dockerfile, the buildx command. |

## Wiring

- The CLI `release` subcommand (`cli/fennec.ml`) is a one-line call into `Release.run`.
- The build shares `_build/default` with `dev` / `fennec test` per the build ladder (release is a
  ship-time one-shot, not an interactive loop) — see `Fennec_dev.Build_dir.release_context_dir`.
- The artifact is a **single self-contained binary**: for a web app the JS/CSS/static web root is
  embedded, so nothing else needs to ship.

## Cross-compilation (`--target`)

`fennec release` with no target builds for the current host — on a Mac that's a macOS binary, which
won't run on a Linux server. `--target` cross-builds via Docker instead:

```
fennec release --target linux/amd64                       # -> ./dist/linux-amd64/<server>
fennec release --target linux/amd64,linux/arm64           # both arches
fennec release --target linux/arm64 --image myapp:1        # a deployable runtime image instead
fennec release --target linux/amd64 --build-image my/fennec-build:5.4   # a pre-baked toolchain image
```

**Why Docker, not a cross-compiler.** OCaml has no official native cross-compilation, and the C stubs
(burrow's LMDB, the mongo FFI) make a cross-toolchain impractical. So — like `cross-rs` for Rust — we
don't cross-compile; we build *natively inside a Docker image that is the target platform* (`buildx
--platform`) and extract the binary (`--output type=local`) or tag a runtime image. `Cross` generates a
multi-stage Dockerfile under `.fennec/release/`; the build runs in the container, the result lands on
the host.

**Reach.** Docker builds **Linux** targets (`amd64`, `arm64`). macOS and Windows have no buildable
containers, so build those on a native host (a Mac makes its own macOS binary with a plain `fennec
release`) or a CI matrix:

```yaml
# .github/workflows/release.yml — one job per OS, native runners
strategy:
  matrix:
    include:
      - { os: ubuntu-latest,  target: linux/amd64 }   # or `fennec release --target` from one host
      - { os: macos-latest }                           # host build → macOS arm64
      - { os: windows-latest }                         # host build → Windows
```

**Speed + the pre-baked image.** The first cross-build is slow: it installs the toolchain (OCaml + Go +
Rust) and compiles every dependency. buildx caches layers, so repeats are fast — but for CI or frequent
use, bake the toolchain once and pass it with `--build-image`:

```dockerfile
# fennec-build.Dockerfile — bake once (docker build -t my/fennec-build:5.4 -f fennec-build.Dockerfile .),
# publish, then: fennec release --target linux/amd64 --build-image my/fennec-build:5.4
FROM ocaml/opam:debian-12-ocaml-5.4
USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends pkg-config libssl-dev libmongoc-dev ca-certificates curl xz-utils \
 && rm -rf /var/lib/apt/lists/*
ARG TARGETARCH
RUN curl -fsSL "https://go.dev/dl/go1.24.0.linux-${TARGETARCH}.tar.gz" | tar -C /usr/local -xz
USER opam
RUN curl --proto '=https' --tlsv1.2 -fsSL https://sh.rustup.rs | sh -s -- -y --default-toolchain stable --profile minimal
ENV PATH="/usr/local/go/bin:/home/opam/.cargo/bin:${PATH}"
# (optionally pre-`opam install` your app's deps here so the per-build step is just `dune build`)
```

> **Downstream apps:** the in-container build runs `opam install . --deps-only`, so the `fennec` /
> `fennec-cli` packages must be reachable by opam — `opam pin` them (or publish to your opam repo)
> before cross-building an app outside this monorepo.
