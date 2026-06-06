# `fennec test` — the testing command

One command for a Fennec app's tests, in three sharp cuts. It erases the boilerplate a
hunt-based suite needs today (hand-wired `(executable)` stanzas, a `run.sh`, the
"build-the-binary-not-`dune exec`" dance, server spawn + readiness + teardown) by owning the
orchestration — while keeping each suite **isolated and deterministic**.

## The cuts

The suite is a positional noun (Playwright `--project`, cargo `--lib/--test`, rails
`test:system` — the cross-ecosystem convention). Slow cuts are opt-in; the bare command is
the fast CI gate.

| Command | Runs | Needs | Mechanism |
|---|---|---|---|
| `fennec test` | **unit** (default) | nothing | `dune build @runtest` |
| `fennec test http` | Http suites | a booted app per suite | orchestrated |
| `fennec test browser` | Browser suites | app per suite **+ Chrome** | orchestrated |
| `fennec test all` | unit → http → browser | everything | orchestrated, fast-to-slow |

Bare `fennec test` runs on every push. `http`/`browser` are explicit and own their setup.

## Per-suite isolation (the core invariant)

Tests are **stateful** — they mutate the server's data, and (soon) their own database. So
each suite gets its **own exclusive instance**, never a shared one:

- Suite *i* → a **deterministic** port `base + i` (reproducible; `base` from `--port`, default
  a test range clear of dev's 4000 and macOS AirPlay's 7000 — default 8200).
- Its **own server process** — the app artifact spawned with `FENNEC_PORT=<port>`,
  `FENNEC_DEV_LIVERELOAD=0` (determinism), and (future) `FENNEC_DATABASE_URL=<isolated>`.
- Its **own state**. No shared port, no shared DB.

Because instances share nothing, suites run **in parallel, bounded by `-j`**, deterministically.
This is the whole reason the URL/port were made externally controllable
(`Fennec.serve` reads `FENNEC_PORT`; `Hunt.Http.hunt` takes `~url`): so the harness can hand
each suite a private instance.

## The typed env contract (`Test_proto`)

The seam between the CLI (which *sets* the env) and a suite (which *reads* it) is a typed
module — mirroring `Dev_proto` — so there's no stringly-typed drift:

| Env var | Set by CLI | Read by | Meaning |
|---|---|---|---|
| `FENNEC_TEST_URL` | per suite | `hunt` / `Run` | the suite's target instance (`http://localhost:<port>`) |
| `FENNEC_PORT` | per suite | `Fennec.serve` | the instance's port (already wired) |
| `FENNEC_DEV_LIVERELOAD` | `0` | the server | determinism |
| `FENNEC_DATABASE_URL` *(future)* | per suite | the app | isolated DB per instance |

**Typesystem offload for correctness by construction:** `hunt`'s `~url` becomes *optional*,
defaulting to `FENNEC_TEST_URL`. A suite therefore *cannot* hardcode a colliding port — it
always targets the harness-assigned instance. Resolution order: explicit `~url` → env
`FENNEC_TEST_URL` → (if `~spawn` given) a localhost default → otherwise a clear error
("run via `fennec test`, or pass `~url`/`~spawn`"). Same for `Run.main_cli ~base_url`.

## Discovery (convention + custom aliases)

Dune has no glob, so suites are named, but the wiring is one line per dir:

```
test/
  http/      *.ml — Http suites (each an executable; hunt-based)
  browser/   *.ml — Browser suites (each registers via Live.test; Run-driven)
  dune       — unit tests on @runtest (the default gate)
```

Each suite dir declares its executables on a **custom alias** (`@http` / `@browser`), *not*
`@runtest`, so a bare `dune test` never runs the slow cuts:

```lisp
; test/http/dune
(executables (names checkout_test users_test) (libraries fennec-hunt fennec))
(rule (alias http) (deps checkout_test.exe users_test.exe) (action (progn)))
```

`fennec test http` builds `@http` one-shot (fennec holds the workspace lock briefly), then
runs the built artifacts itself — so nothing nests dune at runtime.

## Lock-safe orchestration

The dune workspace-lock deadlock (a test that shells out to dune while another dune watches)
is dissolved by making **fennec the only dune-aware orchestrator**:

1. Discover the app server (`discover.ml`).
2. **One-shot `dune build`** the server + the cut's suites.
3. Per suite, in parallel (≤ `-j`): allocate a port, **spawn the server artifact directly**
   (`_build/default/.../server.bc`, never `dune exec`) with the `Test_proto` env, wait for
   readiness (poll the port), run the **suite artifact** with `FENNEC_TEST_URL` set, then kill
   the instance.
4. Aggregate exit code (non-zero if any suite failed).

Reuses `cli/dev/`: `discover`, `server_proc`, `port`, `pidfile`. A dev's already-running
`fennec dev` can be reused for the matching port (reuse-existing detection) instead of
re-booting.

## Flags (the minimal sharp six — nothing more)

```
fennec test [SUITE]
  --grep RE            run only suites/cases matching (passed through to the suite)
  -x, --max-failures N stop after N failures (default fail-fast; --no-fail-fast disables)
  --watch              re-run on change (dev loop)
  --reporter list,junit  human default; comma-list for CI (junit/json)
  -j, --workers N      parallel suites (default = CPUs; -j1 forces serial)
  --headed             browser cut only: show the browser
  --screenshots DIR    browser cut only: PNG on failure
```

Strict exit code. Explicitly rejected: cargo's `-- ` two-namespace split, jest's
watch-flag zoo, a reporter/coverage flag farm.

## Graceful edge cases (every one a clear message, never a crash or hang)

- **No suites found** → friendly notice (where it looked, how to add one), exit 0.
- **Build failure** → surface dune's errors, exit non-zero, run nothing.
- **Server boot failure** (a suite's instance won't come up) → name the suite + the reason
  (port busy, crash on start), fail that suite, keep the others isolated.
- **Port busy** → reclaim a leftover of ours (existing `port.ml` logic) or reassign.
- **Chrome missing** (browser cut) → clear message with the `CHROME=` / install hint; the cut
  fails cleanly, it does not crash. `unit`/`http` are unaffected.
- **A suite hangs** → per-suite wall-clock timeout → that suite fails, others continue.
- **Interrupted (Ctrl-C)** → structural teardown of every spawned instance (Eio switch / the
  pidfile reaper), no orphans, no held ports.

## Scope

`fennec test` orchestrates **app-targeting** suites (test against your app, isolated). A
fully self-contained suite that brings its own fixture server (`hunt ~spawn:[…]`) still
works — it just brings its own server instead of fennec booting one — and is the path for
testing-the-tester or third-party servers. The `fennec-hunt` package's *own* integration
tests (the probe/tls fixtures) live in its package, not under `fennec test`.

## Consumers

- **Downstream app**: write `test/http/*.ml` + `test/browser/*.ml`, run `fennec test`. No
  `run.sh`, no manual executable+spawn wiring, no lock dance.
- **Example app**: `run.exe` + `http_test.exe` + `run.sh` collapse into the convention dirs.
- **lib (`fennec-hunt`)**: pure unit tests via `dune runtest`; untouched by `fennec test`.
