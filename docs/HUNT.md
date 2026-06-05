# fennec_hunt — the testing package

Rename `fennec_e2e` → `fennec_hunt`. The fennec hunts for bugs. Two layers:

- **Http tests** — hit a running server as a black box. Process lifecycle + typed HTTP
  client + assertions. No browser. For: "does my API return 401?", "does the dev server
  survive SIGKILL?", "does host routing work?"
- **Browser tests** — drive a real Chrome over CDP. Full-stack: JS execution, hydration,
  DOM, livereload. For: "does the counter increment?", "does the page hydrate correctly?"

One package, layered. You import what you need. The browser layer sits on top of the Http
layer (it reuses the server lifecycle). No cost if you don't call the browser functions —
yojson/base64 link but are tiny.

---

## Module layout

```
fennec_hunt/
  ├── server.ml(i)      — spawn a server process, wait for TCP-ready, teardown (Eio switch)
  │                        supports --port for parallel isolated instances
  │
  ├── http.ml(i)         — typed HTTP client: get/post/put/delete/head → response
  │                        response = { status: int; headers: (string*string) list; body: string }
  │                        no deps beyond Eio (raw TCP + hand-written HTTP/1.1 request)
  │
  ├── assert.ml(i)       — assertion DSL over http responses:
  │                        status_is, body_contains, header_eq, body_json_has, ...
  │                        + process assertions: port_free, port_held, process_alive, ...
  │
  ├── browser.ml(i)      — Chrome lifecycle: find binary, launch, connect, provision contexts
  │                        (absorbs the current chrome.ml)
  │
  ├── page.ml(i)         — the page DSL: goto, click, fill, expect_text, expect_js, ...
  │                        (absorbs the current driver.ml / live.ml / backend.ml)
  │
  ├── runner.ml(i)       — register tests, run with concurrency, report pass/fail
  │                        (absorbs the current reporter.ml + the run logic)
  │
  ├── failure.ml(i)      — test failure as data + renderer (stays as-is)
  │
  └── (private)
      ├── cdp.ml         — the CDP WebSocket wire client
      ├── cdp_backend.ml — the CDP implementation of the page backend
      └── conformance.ml — compile-time proof that cdp_backend satisfies the backend sig
```

## The rename

| Before (`fennec_e2e`) | After (`fennec_hunt`) | Change |
|---|---|---|
| `Fennec_e2e.Run` | `Fennec_hunt.Runner` | rename + split: server spawn moves to `Server` |
| `Fennec_e2e.Live` | `Fennec_hunt.Page` (or `Browser`) | rename; the page DSL |
| `Fennec_e2e.Reporter` | `Fennec_hunt.Runner` (merged) | or stays separate |
| `Fennec_e2e.Failure` | `Fennec_hunt.Failure` | stays |
| `Fennec_e2e.Backend` | `Fennec_hunt.Page_backend` (private or semi-public) | rename |
| `Fennec_e2e.Driver` | absorbed into `Page` | the functor is applied, not exposed |
| chrome.ml | `Browser` (public, so users can configure headless/binary) | promote |
| cdp.ml | stays private | — |
| cdp_backend.ml | stays private | — |
| — (new) | `Server` | spawn + lifecycle + readiness |
| — (new) | `Http` | typed HTTP test client |
| — (new) | `Assert` | assertion DSL |

## What moves where

**From `run.ml` → `Server`:**
- `wait_http_ready` (the TCP poll loop) → `Server.wait_ready`
- The `server_exe` spawn block (lines 35–44) → `Server.spawn`
- Port extraction from base_url → `Server.port_of_url`

**From `run.ml` → `Runner`:**
- `main` (the Eio loop + test execution) stays but calls `Server.spawn` instead of inline
- `main_cli` (the argv parser) stays
- Reporter integration stays

**New `Http`:**
- A minimal Eio-based HTTP/1.1 client: `Http.get ~port ~path` → `Http.response`
- `Http.response = { status: int; headers: (string * string) list; body: string }`
- Built on raw `Eio.Net.connect` + write/read — no external HTTP library
- This replaces `curl -s` in the shell scripts

**New `Assert`:**
- `Assert.status_is 200 resp` — raises `Failure` with a clear message on mismatch
- `Assert.body_contains "ok" resp`
- `Assert.header_eq "Content-Type" "application/json" resp`
- `Assert.port_free port` — via `Unix.connect` probe
- `Assert.process_alive pid` — via `Unix.kill pid 0`
- These replace `[ "$STATUS" = "401" ] || fail "..."` in the shell scripts

## Consumer migration

**Before (shell script):**
```sh
( cd examples/site && exec fennec dev ) >/tmp/log 2>&1 & DEV=$!
for i in ...; do grep -q "ready" /tmp/log && break; sleep 0.5; done
curl -s http://localhost:4001/api/health | grep -q '"ok":true' || fail "..."
kill -INT $DEV
```

**After (OCaml, Http test):**
```ocaml
let () = Hunt.Runner.http "health check" @@ fun server ->
  let r = Hunt.Http.get server "/api/health" in
  Hunt.Assert.status_is 200 r;
  Hunt.Assert.body_contains {|"ok":true|} r
```

**Before (shell script, process lifecycle):**
```sh
kill -9 $DEV
i=0; while port_held; do sleep 0.5; i=$((i+1)); [ $i -gt 20 ] && fail "port stuck"; done
```

**After (OCaml, Http test):**
```ocaml
Unix.kill (Hunt.Server.pid server) Sys.sigkill;
Hunt.Assert.port_free_within ~timeout:10.0 port
```

**Before (site.ml, Browser test — unchanged in spirit):**
```ocaml
open Fennec_hunt.Page  (* was: Fennec_e2e.Live *)
let () = test "counter hydrates" @@ fun page ->
  page |> goto "/" |> hydrated
  |> click "[data-testid=increment]"
  |> expect_text "[data-testid=count]" "1"
```

## The example's e2e directory after migration

```
examples/site/e2e/
  browser.ml     — Browser tests (the current site.ml, renamed for clarity)
  http.ml        — Http tests (the dev-server guards, rewritten from shell → OCaml)
  run.ml         — the runner: boots fennec dev, runs both layers
  dune           — depends on fennec-hunt + fennec
```

The shell scripts stay as `e2e/*.sh` for a while (secondary smoke check), then get removed
once the OCaml versions are proven stable.

## opam / dune changes

- Package: `fennec-e2e` → `fennec-hunt` (new opam file, deprecate the old)
- Library: `fennec_e2e` → `fennec_hunt`
- `check_lean.ml`: update the forbidden-module check from `Fennec_e2e` to `Fennec_hunt`
- All consumer `open Fennec_e2e.Live` → `open Fennec_hunt.Page`

## Dependencies

Unchanged: `eio`, `eio.unix`, `eio_main`, `unix`, `yojson`, `base64`. The Http client is
hand-written on raw Eio sockets (no cohttp dep). The Assert module is pure OCaml.

## Execution order

1. Rename the package + directory: `fennec/e2e/` → `fennec/hunt/`, library name → `fennec_hunt`
2. Update all consumers (`examples/site/e2e/`, `check_lean.ml`, opam files)
3. Verify: `dune build` + existing Browser tests still pass
4. Add `Server` module (extract from `run.ml`)
5. Add `Http` module (typed Eio HTTP client)
6. Add `Assert` module (assertion DSL)
7. Write the Http tests for the dev-server guards (replace shell scripts)
8. Verify: all Http tests + Browser tests pass together
9. Remove the shell scripts (once the OCaml versions are proven)
