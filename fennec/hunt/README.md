# fennec-hunt

Testing for OCaml web apps, in pure OCaml — two independent layers, one package. Import
whichever a test needs.

## Http tests — `Fennec_hunt.Http`

A typed HTTP/1.1 client and deterministic assertions against any URL. No browser. One
request → one response → immediate pass/fail.

```ocaml
open Fennec_hunt.Http

let () = hunt "my API" ~url:"http://localhost:4000" ~spawn:["./server"] @@ fun () ->

  check "health" (fun () ->
    get "/health" ~expect:[status 200; is_json; json_path_is "ok" "true"]);

  check "create user" (fun () ->
    post "/users" ~json:(`Assoc [("name", `String "alice")])
      ~expect:[status 201; json_is_uuid "id"; json_path_is "name" "alice"]);

  check "auth flow" (fun () ->
    post "/login" ~form:[("user", "admin"); ("pass", "secret")] ~expect:[status 200];
    get "/dashboard" ~expect:[status 200; body_contains "Welcome"])  (* cookies carry over *)
```

`hunt` runs a suite against a URL — with `~spawn` it starts a server and waits for it,
otherwise it tests an already-running one (so the same suite runs against local, CI,
staging, or prod). `check` is one test case with a fresh cookie jar. Requests take
`~headers` / `~host` / `~query` / `~body` / `~form` / `~json` and an `~expect` list of
assertions: status families, body (substring / exact / regex / emptiness), headers, JSON by
dotted path (value, type, UUID, datetime, array length), cookies, redirects, timing — plus
`expect (fun r -> …)` for anything custom. No retry, no polling; a failed assertion fails
immediately with expected / actual / elapsed / URL.

## Browser tests — `Fennec_hunt.Live` + `Run`

A hand-written WebSocket + Chrome DevTools Protocol client drives a headless Chromium
directly — **no Node, no chromedriver, no Selenium, no Lwt**. You install a Chromium-family
browser; everything else is one OCaml library.

```ocaml
open Fennec_hunt.Live

let () = test "adds an item to the cart" @@ fun page ->
  page
  |> goto "/products"
  |> click ".product:first-child .add-to-cart"
  |> expect_text ".cart .count" "1"
  |> expect_visible ".cart .line-item"
  |> ignore

let () = Fennec_hunt.Run.main_cli ~base_url:"http://localhost:8080" ()
```

A step blocks until its condition holds or it times out — you never write an explicit wait.
On timeout the pipe short-circuits and the runner prints a report that names the step, shows
the page's real state, and tells you how to re-run just that test.

- **Auto-waiting DSL** — `goto`, `click`, `fill`, `press`, `within`, `expect_*`, `read_*`,
  `eval`; each step waits for its own precondition.
- **Event-driven & deterministic** — one in-page MutationObserver/rAF round-trip per wait
  (no polling), navigation matched by loader id, evals pinned to the live context.
- **Failures that explain themselves** — a numbered pipeline trace, the captured
  `outerHTML` / selector probe / URL / console, a tuned hint, and a rerun command.
- **A reporter that travels** — colour + unicode + a live status line on a TTY; plain ASCII
  on a CI log. Auto-detected (`NO_COLOR`, `FORCE_COLOR`, `TERM`, `LANG`, `COLUMNS`), airtight
  under `--jobs`.
- **A fake backend** — the DSL/runner are written against an abstract `Backend.S`, so they
  are fully unit-tested with no browser and behave identically live.

Runner flags: `--grep`, `--bail`, `--jobs N`, `--retries N`, `--headed`, `--timeout S`,
`--browsers M`, `--reporter auto|plain|pretty`, `--color`/`--no-color`/`--ascii`. Point at
any Chromium-family browser with `CHROME=/path/to/chrome`.

## Operating the machinery

Both layers share one server-lifecycle model (`Target`, internal):

- **A target is a URL.** `~url:"http://localhost:4000"` is the identity — the host and port
  are parsed from it. The Http client connects to that host (DNS-resolved), so a target can
  be local *or remote* (`~url:"http://staging.internal:8080"`). **`https://` works too** — the
  connection is upgraded to TLS (via `tls-eio`) with an *accept-any-certificate* default,
  which is the right behaviour for testing your own server behind a self-signed localhost cert
  or a staging box (you're checking behaviour, not validating a public CA). A non-TLS server
  on an `https://` URL fails clearly at the handshake.
- **Spawn or attach.** With `~spawn:["./server"]` (Http) / a positional server path (Browser),
  the harness starts that command, waits for the URL to accept connections (event-driven, up
  to `~timeout`, default 30s), and **kills it on exit** — the process is tied to the Eio
  switch, so teardown is structural (no leak even on failure or exception). Without a spawn
  argument, the harness tests an already-running server (CI, staging) and just waits for it.
- **Readiness is the only wait.** Once the server answers, every request is one TCP call →
  one response → immediate verdict. No retry, no polling in the test path — runs are
  deterministic.

**Parallelism / isolation.** The target URL is the only thing tying a run to a server, so
two runs against two URLs are fully independent. Give each worktree or CI shard a different
port (`~url:"http://localhost:5000"`, `…:5001`, …) and they never collide. The Browser
runner additionally fans out tests across N browsers with `--jobs`/`--browsers`.

**Running the tests.** Build the test executable and run the **binary directly** — do *not*
run it via `dune exec` if the spawned command itself calls dune (e.g. `fennec dev` runs
`dune describe`): `dune exec` holds the workspace lock and the child deadlocks. Build once,
run the artifact:

```sh
dune build examples/site/e2e/http_test.exe
_build/default/examples/site/e2e/http_test.exe        # Http tests
_build/default/examples/site/e2e/run.exe --jobs 4     # Browser tests
```

A plain HTTP target (one that doesn't shell out to dune) runs fine under `dune exec`.

## Notes

- **Eio-only by design** (direct-style, structured concurrency, leak-free teardown under one
  switch) — hence OCaml 5+. Dependencies: `eio`, `yojson`, `base64`, `re`. No cohttp — the
  HTTP client is hand-written on raw sockets.
- **No dependency on the `fennec` web framework** it grew up beside; works against any web
  server. A separate package precisely so a production server never links the test machinery.

See the [API docs](https://anonyfox.github.io/fennec) (`Fennec_hunt.Http`, `…Live`, `…Run`,
`…Backend`, `…Driver`, `…Reporter`, `…Failure`). MIT licensed.
