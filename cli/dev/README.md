# `cli/dev` — the `fennec dev` engine

This directory is the **development server**: the long-running process behind `fennec dev`. You edit a
file, and within milliseconds the right thing happens — the backend restarts, or the page hot-reloads,
or a compiler error is shown with a code frame, or the affected unit tests re-run. It serves **two
audiences at once**: a human watching a terminal, and a coding agent reading a machine journal.

If you are new here, read this file, then [`supervisor.ml`](supervisor.ml) (the heart), then follow
the data flow below into whichever module you care about.

## The big picture

One reactive loop. It waits for the next event from `dune build --watch`, decides what changed, acts
on it, and reports the outcome on both lanes. Every iteration is exception-wrapped, so nothing — a dead
child, a syscall failure, a parser surprise — can take the process down.

```
  you save a file
        │
        ▼
  dune build --watch ──▶ Dune_watch ───▶  Supervisor.handle  (the one decision point)
                        (typed events)         │
                                               ├─ backend artifact changed ──▶ Server_proc (restart the child)
                                               ├─ only web assets changed  ──▶ ping the server's reload socket
                                               ├─ build FAILED             ──▶ Diagnostics (parse) ─▶ problem panel
                                               └─ green settle             ──▶ Dev_tests (re-run affected tests)
                                               │
                                               ▼
                              ┌────────────────┴─────────────────┐
                              ▼                                   ▼
                         Ui  (humans)                     Agent_event  (agents)
                    quiet event log +                  events.jsonl  (edit→verdict)
                    one live problem panel             errors.jsonl  (runtime HTTP errors)

  meanwhile, the running child streams its output back:
  Server_proc.drain ─▶ classify each line ─▶ Http access ─▶ Ui.http  +  Agent_event.emit_error (on failures)
```

## The two output lanes

The same dev-loop facts are rendered twice, by design — humans and agents want different shapes, and
neither should have to parse the other's:

- **Humans → [`ui.ml`](ui.ml)** (over [`tty.ml`](tty.ml)): an append-only event log with one live,
  repainting "current problems" region. Build/reload/test lines, and live HTTP traffic (dim + status-
  coloured; failures promoted to a prominent red line).
- **Agents → [`agent_event.ml`](agent_event.ml)**: a small append-only JSON journal an agent waits on.
  Two **separate** streams on purpose:
  - `events.jsonl` — the **edit→verdict** contract (`mark` / `wait` / `hook` key off its ids).
  - `errors.jsonl` — **runtime HTTP errors** (5xx / error-funnel). Kept apart so a flood of async
    failures can never disturb the edit-verdict id sequence. Listed by `fennec agent errors`.

## File map

**Orchestration**
| File | Role |
|------|------|
| [`supervisor.ml`](supervisor.ml) | **The heart.** The reactive loop: one `run` function that wires up the children and reacts to each watch event. Start here. |
| [`discover.ml`](discover.ml) | Find *the* server in a dune project — the single executable that calls `Fennec.serve`. |
| [`doctor.ml`](doctor.ml) | `fennec doctor` — what a stuck beginner (or agent) runs when the dev loop won't start. |
| [`new_app.ml`](new_app.ml) | `fennec new NAME` — scaffold a minimal working app. |
| [`skill_doc.ml`](skill_doc.ml) | The agent/human guidance `fennec skill` prints. |

**The build / watch loop**
| File | Role |
|------|------|
| [`dune_watch.ml`](dune_watch.ml) | `dune build --watch` as a typed, bullet-proof event stream (settles, errors, exit). |
| [`server_proc.ml`](server_proc.ml) | The server child: spawn, drain its merged stdout+stderr, **classify** each line, reap, graceful stop. |
| [`assets.ml`](assets.ml) | Detect served-asset changes (CSS-only hot-swap vs full reload). |
| [`crash_limiter.ml`](crash_limiter.ml) | Sliding-window restart limiter, so a boot-crash loop can't spin. |
| [`port.ml`](port.ml) | Who holds a TCP port, and whether the holder is *our own* leftover (safe to reclaim). |
| [`pidfile.ml`](pidfile.ml) | Cross-run orphan reaping — two `fennec dev`s can't fight for the port. |
| [`stublibs.ml`](stublibs.ml) | Put the project's C-stub dirs on `CAML_LD_LIBRARY_PATH` so a bytecode child can `dlopen` them. |
| [`mongo_rs.ml`](mongo_rs.ml) | A managed dev MongoDB (replica set) when the app needs one. |

**Tests in the loop** (run only what an edit affects, warm)
| File | Role |
|------|------|
| [`dev_tests.ml`](dev_tests.ml) | Discover inline-test runners; after a green settle, re-run only the ones whose artifact changed. |
| [`test_worker.ml`](test_worker.ml) | Supervisor-side client for the resident **warm test worker**. |
| [`test_chain.ml`](test_chain.ml) | Derive a test's app-local Dynlink chain from `dune describe`. |
| [`worker/fennec_test_worker.ml`](worker/fennec_test_worker.ml) | The resident worker: forks + `Dynlink`s fresh objects per edit (~7 ms vs ~480 ms cold). |

**Output — humans**
| File | Role |
|------|------|
| [`ui.ml`](ui.ml) | The terminal UI: event log + live problem region + live HTTP lines. |
| [`tty.ml`](tty.ml) | Terminal capability detection + escape helpers. |
| [`diagnostics.ml`](diagnostics.ml) | Parse a failed build's text into a focused, framed diagnostic. |

**Output — agents**
| File | Role |
|------|------|
| [`agent_event.ml`](agent_event.ml) | The agent journal: `events.jsonl` + `errors.jsonl`, `mark`/`wait`/`hook`/`status`/`errors`. |
| [`agent_attach.ml`](agent_attach.ml) | Install the guarded post-tool hook into a coding agent's settings. |
| [`verdict.ml`](verdict.ml) | The `Verdict.t` sum type (one dev-loop outcome) + its agent-event encoding. |

**Shared**
| File | Role |
|------|------|
| [`affected.ml`](affected.ml) | Infer the affected surface (backend / web / component / route) from the changed files. |
| [`build_dir.ml`](build_dir.ml) | The one build-layout authority — which `_build/<profile>` dir the current command uses, and every path derived from it. |

## Build layout — one authority, cargo-style (`build_dir.ml`)

[`Build_dir`](build_dir.ml) is the single source of truth for *which build directory* a command works
in, so the same logic isn't scattered across the dev loop, the warm worker, and `fennec test`:

- **`dev` (default)** → the standard `_build/default`. `dune build`, `dune runtest`, **and `fennec
  test`** all use this. `Build_dir.profile ()` returns `"dev"` unless told otherwise.
- **`fastdev`** → an isolated `_build/fastdev` (an *absolute* `DUNE_BUILD_DIR` — dune rejects a relative
  build dir nested under `_build`). **Only `fennec dev`** opts in (it exports `FENNEC_DEV_PROFILE` via
  `Build_dir.dev_loop_profile`), so its byte build + warm worker never share — and never invalidate —
  the native `dev` build. Like cargo's `target/{debug,release}`.

This is what lets **`fennec test` run while `fennec dev` is running**: disjoint build dirs ⇒ disjoint
dune locks. `fennec test`'s `dune shutdown` targets the default RPC and can't see the dev loop's
(separate) one. The supervisor exports `DUNE_BUILD_DIR` once; everything else just calls
`Build_dir.context_dir` / `Build_dir.describe_prefix`.

> Downstream note: in this monorepo the example rebuilds all of fennec from source (it's a *local*
> dep), which is why a cold `fennec dev` is slow once per profile. A real downstream app depends on the
> *opam-installed* fennec packages (prebuilt) — dune never recompiles those per profile, so its cold
> build is just its own code + jsoo.

## The agent fastlane (how an agent uses this)

`fennec dev --agent` writes the journal; a coding agent installs a post-tool hook (`agent_attach.ml`)
that, after each edit, calls `fennec agent hook`. That blocks for the **next** dev verdict and returns
it as model context — so the agent never tails logs or guesses. Runtime HTTP errors that piled up since
the last tool are appended to the same feedback, and the full list is `fennec agent errors`. The
contract is the `events.jsonl` id sequence; keep it pure (see the two-stream note above).

## Invariants worth knowing before you change things

- **Totality.** The loop never crashes the process. Wrap new syscalls.
- **The classifier is pure.** `Server_proc.classify_line` is unit-tested in isolation; the *effect* of
  a line lives in the supervisor's `on_line`. Keep that split.
- **Two streams, never merged.** Runtime errors go to `errors.jsonl`, never `events.jsonl`.
- **Warm-worker profile match.** The worker's preloaded framework must be built in the *same* dune
  profile as the app `.cma`s the watch builds (`fastdev` by default), or `Dynlink` rejects on a CRC
  mismatch (→ safe cold fallback). See [`worker/fennec_test_worker.ml`](worker/fennec_test_worker.ml).
- **Tests are colocated.** Each module carries its own `let%test`; `dune runtest cli/dev` runs them.
