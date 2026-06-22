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

And it surfaces the runtime **deploy contract** the binary hides — above all `FENNEC_ENV=production`,
the switch that makes the server serve its *embedded* assets instead of looking for a `webroot/` dir on
disk. Forgetting it is the classic single-binary-deploy footgun; the framework also warns about it at
runtime (see `Fennec.web_source`).

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

## Wiring

- The CLI `release` subcommand (`cli/fennec.ml`) is a one-line call into `Release.run`.
- The build shares `_build/default` with `dev` / `fennec test` per the build ladder (release is a
  ship-time one-shot, not an interactive loop) — see `Fennec_dev.Build_dir.release_context_dir`.
- The artifact is a **single self-contained binary**: for a web app the JS/CSS/static web root is
  embedded, so nothing else needs to ship.
