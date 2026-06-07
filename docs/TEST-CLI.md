# `fennec test` — the testing command

One command for a Fennec app's tests, in four sharp cuts. It erases the boilerplate a
hunt-based suite needs today (hand-wired `(executable)` stanzas, a `run.sh`, the
"build-the-binary-not-`dune exec`" dance, server spawn + readiness + teardown) — and the
stringly-typed `e2e/*.sh` integration scripts — by owning the orchestration, while keeping each
suite **isolated and deterministic**.

## Quickstart

```sh
fennec test                         # the fast unit gate (inline let%test, next to your code)
fennec test new http checkout       # scaffold test/http/checkout_test.ml (+ dune + runner, once)
# …edit checkout_test.ml…
fennec test http                    # build + run it, isolated, against your app
fennec test http --grep checkout    # just that suite (a filter matching nothing FAILS, never green)
fennec test all                     # unit → http → browser → system
```

A suite is one block — no `main`, no env wiring, no dune edit:

```ocaml
(* test/http/checkout_test.ml *)
open Fennec_hunt.Http
let%http "checkout" = fun () ->
  check "home is 200" (fun () -> get "/" ~expect:[ status 200; is_html ])
```

Drop another `*_test.ml` in the same folder and it just runs. The cuts and the machinery below
explain *why* this stays isolated, deterministic, and lock-safe.

## The cuts

The suite is a positional noun (Playwright `--project`, cargo `--lib/--test`, rails
`test:system` — the cross-ecosystem convention). Slow cuts are opt-in; the bare command is
the fast CI gate.

| Command | Runs | Needs | Mechanism |
|---|---|---|---|
| `fennec test` | **unit** (default) | nothing | `dune build @runtest` |
| `fennec test http` | Http suites | a booted app per suite | orchestrated |
| `fennec test browser` | Browser suites | app per suite **+ Chrome** | orchestrated |
| `fennec test system` | System suites | the real `fennec dev` (suites spawn it) | orchestrated, serial |
| `fennec test all` | unit → http → browser → system | everything | orchestrated, fast-to-slow |

Bare `fennec test` runs on every push. `http`/`browser`/`system` are explicit and own their setup.

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

## Authoring — one model, zero ceremony

A suite is a `let%cut` block — same shape across cuts, differing only in the context the closure
receives (an Http client, a `page`, a `sandbox`). No `main`, no env wiring, no per-file dune:

```ocaml
(* test/http/checkout_test.ml *)            (* test/system/reclaim_test.ml *)
open Fennec_hunt.Http                        module S = Fennec_hunt.System
let%http "checkout" = fun () ->              let%system "port frees on kill" = fun sb ->
  check "200" (fun () ->                       let dev = S.dev sb in
    get "/" ~expect:[ status 200 ])            S.wait_ready dev ~port:4000 ();
                                               S.signal dev Sys.sigkill;
(* test/browser/cart_test.ml *)                S.wait_until (fun () -> not (S.port_open 4000))
open Fennec_hunt.Live
let%browser "cart" = fun page ->
  page |> goto "/products" |> click ".add" |> expect_text ".count" "1" |> ignore
```

`fennec test new <cut> <name>` scaffolds the first file in a cut (the one-time dune + runner + a
starter); after that, adding a suite is just dropping another `*_test.ml`. `let%system_manual`
registers an opt-in scenario (skipped unless `--manual`) — for a destructive case like
`fennec dev --clean`. The ppx strips every block to `()` in a production build (zero cost).

## Discovery (convention + a `-linkall` library)

Each cut dir is ONE library plus a one-line runner — written once (by `fennec test new`), never
edited as suites are added:

```
test/
  http/      *_test.ml — let%http blocks      (Fennec_hunt.Http)   + run.ml + dune
  browser/   *_test.ml — let%browser blocks   (Fennec_hunt.Live)   + run.ml + dune
  system/    *_test.ml — let%system blocks    (Fennec_hunt.System) + run.ml + dune
  dune       — inline let%test on @runtest (the default gate)
```

```lisp
; test/http/dune — written once; adding suites never touches it
(library (name http_suites) (modules (:standard \ run)) (libraries fennec-hunt)
 (library_flags (-linkall)) (preprocess (pps fennec-hunt.ppx)))
(executable (name run) (modules run) (libraries http_suites fennec-hunt))
```

`-linkall` forces every suite module to link, so each `let%cut` block registers as a module-init
side effect (the same mechanism as inline `let%test`); `run.ml` is the entry
(`let () = exit (Fennec_hunt.Http.run ())`). Dropping a `*_test.ml` needs **no dune edit**
(`:standard` includes it). These are plain executables (NOT on `@runtest`, since they need a booted
server / Chrome), so a bare `dune test` stays the fast unit gate, and `fennec test` runs the
built `run.exe` directly — nothing nests dune at runtime.

## Lock-safe orchestration

The dune workspace-lock deadlock (a test that shells out to dune while another dune watches)
is dissolved by making **fennec the only dune-aware orchestrator**:

1. Discover the app server (`discover.ml`), **`dune shutdown`** any orphaned dev watcher, and put
   the opam stublibs on `CAML_LD_LIBRARY_PATH` (so a directly-spawned bytecode server finds its C
   stubs — `opam env` doesn't set it).
2. **One-shot `dune build`** the server + its webroot + the cut's single runner
   (`test/<cut>/run.exe`), from the workspace root (then restore the cwd — `test all` runs this
   once per cut).
3. Per suite **file**, in parallel (≤ `-j`): make the port usable (**reclaim a leftover of *ours*,
   refuse to touch a *foreign* holder** — `port.ml`), **spawn the server artifact directly**
   (`_build/default/.../server.bc`, never `dune exec`) with the `Test_proto` env, wait for
   readiness, then run the runner as **`run.exe --only-file <suite>`** with `FENNEC_TEST_URL` set —
   so it executes just that file's tests against this dedicated instance — and tear the instance
   down (structural `Fun.protect`). Parallel output is buffered + flushed atomically; serial runs
   stream live. (System needs no booted server: its runner runs once, each scenario in its own
   sandbox, serially.)
4. Aggregate exit code (non-zero if any suite failed); a cross-suite roll-up footer.

Every spawned pid (server + suite) is registered so Ctrl-C / SIGTERM tears the whole fleet
down — no orphans, no held ports. Reuses `cli/dev/`'s `discover`, `port`, and the `Dev_proto`
env names; the instance lifecycle (`boot.ml`) is the test command's own. Each suite gets its
**own** instance — a running `fennec dev` is never reused, since isolation is the whole point.

## The System cut (different by design)

Http/browser suites test your app *through* a server fennec boots for them. **System** suites
test `fennec dev` *itself* — process hygiene, port reclaim, host routing, livereload, the
build-error panel — so they **spawn `fennec dev` themselves** and assert its observable side
effects. That inverts the orchestration:

- **No per-suite instance.** fennec builds the server + webroot + suite exes once, sets the
  harness env, and runs each suite; the suite owns every process it starts.
- **Serial, not parallel.** The suites drive dev on its *real* fixed ports (gateway `:4000`,
  endpoints `:400x`), so they can't share the machine concurrently — they run one at a time.
- **Typed system primitives, not shell.** Each suite is written against `Fennec_hunt.System`: a
  typed vocabulary for what the `.sh` scripts did — `spawn`/`run` (argv as a *list*: no shell, no
  quoting, no injection), condition waits (`wait_ready`, `wait_output`, `wait_until` — deadline-
  bounded, **never `sleep`-and-hope**), typed filesystem + `with_edit` (edit a real source, always
  reverted — even on failure), and a one-shot HTTP client (`request`/`header`, with a routing Host
  header). Every spawned process is put in its own session, so on teardown — pass, fail, exception,
  or timeout — the **whole process group** is reaped. No orphans, even one the tool under test
  leaked.

The harness env (set by the orchestrator, read by each suite):

| Env var | Meaning |
|---|---|
| `FENNEC_BIN` | the fennec under test (the orchestrating binary itself) |
| `FENNEC_APP_DIR` | the project to run `fennec dev` in (the invocation cwd) |
| `FENNEC_SERVER_BC` | the built server bytecode (for the leftover-reclaim scenario) |
| `FENNEC_ROOT` | the workspace root (for `_build` sentinels) |

**`@manual` tag.** A system suite whose source header carries `@manual` is *built* (so it can't
bitrot) but **skipped** by the automated run, with a clear note. The escape hatch for a suite that
can't run unattended: the heal suite runs `fennec dev --clean`, which wipes the shared `_build` and
would delete the other suites' exes mid-run — run it directly.

Bytecode stublibs: a suite may spawn the `.bc` server directly, which must `dlopen` its C stubs —
`opam env` does not set `CAML_LD_LIBRARY_PATH`. The cut reuses `fennec dev`'s own `ensure_stublibs`
(single source of truth) so suites inherit it and propagate it to anything they spawn.

## Flags (minimal and sharp — nothing more)

```
fennec test [SUITE]
  -g, --grep RE        run only tests whose NAME (the let%cut label) contains RE — every cut;
                       a filter that matches nothing FAILS (never a silent green)
  -x, --max-failures N stop after N suites fail (default fail-fast = stop at the first)
      --no-fail-fast   run every suite even after a failure
  -j, --jobs N         parallel suites (default = CPUs; -j1 forces serial)
      --port BASE      base port for the per-suite instance blocks (default 8200)
      --headed         browser cut only: show the browser window
      --screenshots DIR  browser cut only: write a PNG on failure into DIR
      --reporter R     browser cut only: reporter style (auto | plain | pretty)
```

Strict exit code (non-zero if any suite fails). Fail-fast is at the **suite** level (the
orchestrator's unit; it can't count another process's cases). A wedged suite is killed by a
per-suite wall-clock backstop — `FENNEC_TEST_TIMEOUT=<seconds>`, default 600 — and reported as
a failure while the others continue. Explicitly rejected: cargo's `-- ` two-namespace split,
jest's watch-flag zoo, a reporter/coverage flag farm. (`--watch` is deferred — the live loop is
`fennec dev`; re-run `fennec test` between edits.)

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

`fennec test` orchestrates **app-targeting** suites (test against your app, isolated) plus the
**system** suites (which target `fennec dev` itself). A fully self-contained suite that brings its
own fixture server (`hunt ~spawn:[…]`) still works — it just brings its own server instead of
fennec booting one — and is the path for testing-the-tester or third-party servers. The
`fennec-hunt` package's *own* integration tests (the probe/tls fixtures) live in its package, not
under `fennec test`.

## Consumers

- **Downstream app**: `fennec test new <cut> <name>` once, then drop `let%http` / `let%browser` /
  `let%system` blocks into `test/<cut>/`. No `main`, no `run.sh`, no `e2e/*.sh`, no per-file dune,
  no manual executable+spawn wiring, no lock dance — `fennec test` orchestrates it.
- **Example app** (done): `run.exe` + `http_test.exe` + `run.sh` collapsed into
  `examples/site/test/http/` (smoke + api) and `examples/site/test/browser/` (the full web
  suite); the six `e2e/*.sh` scripts collapsed into `examples/site/test/system/` (process,
  domains, livereload, errors, heal). CI runs `fennec test http` + `browser` + `system`.
- **lib (`fennec-hunt`)**: pure unit tests via `dune runtest`; untouched by `fennec test`. Its
  own TLS/probe fixtures stay manual exes under `examples/site/e2e/`.
