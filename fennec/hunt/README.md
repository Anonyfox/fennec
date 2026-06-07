# fennec-hunt

Testing for OCaml web apps, in pure OCaml — three independent layers in one package, no
dependency on any framework. Import whichever a test needs.

- **Http tests** (`Fennec_hunt.Http`) — hit any URL, assert on the response. No browser.
- **Browser tests** (`Fennec_hunt.Live`) — drive a real headless Chromium over the DevTools
  Protocol. No Node, no chromedriver, no Selenium.
- **System tests** (`Fennec_hunt.System`) — a typed vocabulary for what shell scripts do
  (spawn processes, run commands, watch output, poke the filesystem, probe ports), on Eio:
  argv as a list (no shell, no injection), deadline-bounded condition waits (never `sleep`),
  and total process-group teardown (no orphans). Replaces hand-rolled `.sh` integration tests.

## Getting started

A test suite is a plain executable. Depend on the library and run the binary:

```lisp
; test/dune
(executable (name api_test) (libraries fennec-hunt))
```

```sh
dune build test/api_test.exe
./_build/default/test/api_test.exe          # exits non-zero if any check fails
```

Write the suite in `test/api_test.ml` (examples below). In CI it's a build step whose exit
code is the result — these suites drive a live server (and, for Browser tests, a browser),
so they aren't hermetic `dune test` targets. Run the **built binary**, not `dune exec`, when
the suite spawns a server that itself shells out to dune (the workspace lock would deadlock)
— see [How it works](#how-it-works).

## Http tests

A typed HTTP/1.1 client (hand-written on Eio sockets — no cohttp) with deterministic
assertions. One request → one response → immediate pass/fail.

```ocaml
open Fennec_hunt.Http

let () = hunt "my API" ~url:"http://localhost:4000" ~spawn:["./server"] @@ fun () ->

  check "health" (fun () ->
    get "/health" ~expect:[status 200; is_json; json_path_is "ok" "true"]);

  check "create user" (fun () ->
    post "/users" ~json:(`Assoc [("name", `String "alice")])
      ~expect:[status 201; json_is_uuid "id"; json_path_is "name" "alice"]);

  check "auth flow" (fun () ->                                   (* cookies carry over *)
    post "/login" ~form:[("user", "admin"); ("pass", "secret")] ~expect:[status 200];
    get "/dashboard" ~expect:[status 200; body_contains "Welcome"])
```

- **Requests** — `get`/`post`/`put`/`patch`/`delete`/`head`/`options`, with `~headers`,
  `~host` (virtual-host testing), `~query`, `~body` / `~form` / `~json` / `~multipart` (file
  uploads), `~follow:true` (chase redirects), and `~timeout` (per-request deadline).
- **Assertions** (`~expect` list) — status families; body (substring / exact / regex /
  emptiness); headers; JSON by dotted path (value, type, UUID, datetime, array length);
  cookies; `redirect_to`; `max_elapsed`; and `expect (fun r -> …)` for anything custom.
- **Cookie jar** — automatic and per-`check`: a `Set-Cookie` is replayed on later requests
  in the same check. No retry, no polling.
- **`eventually`** — for genuinely async expectations: `eventually (fun () -> get "/jobs/1"
  ~expect:[json_path_is "state" "done"])` re-runs until it passes or a deadline. Opt-in;
  the default request path stays one-shot. Not for masking flakiness.
- **Robust by default** — every request is bounded by a timeout, so a hung server fails the
  check (`request timed out after Ns`) instead of freezing the run.

A failed check is self-explaining — expected, actual, the request, and how long it took:

```text
  FAIL  create user (2ms)
     expected status 201, got 500
       body: {"error":"db connection refused"}
       request: POST /users
       elapsed: 2ms
```

## Browser tests

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

A step blocks until its condition holds or times out — you never write an explicit wait.
On timeout the pipe short-circuits and the runner prints which step failed, the page's real
state, and how to re-run just that test.

- **Auto-waiting DSL** — `goto`, `click`, `fill`, `press`, `within`, `expect_*`, `read_*`,
  `eval`; each step waits for its own precondition (event-driven, no polling).
- **Self-explaining failures** — a numbered trace, the captured `outerHTML` / selector probe
  / URL / console, a hint, and a rerun command. With `--screenshots DIR`, a PNG of the page
  at the moment of failure is written too.
- **A reporter that travels** — colour + live status line on a TTY, plain ASCII on a CI log
  (auto-detected). Fans out across browsers with `--jobs` / `--browsers`.
- **A fake backend** — the DSL is written against an abstract `Backend.S`, so it's
  unit-tested with no browser and behaves identically live.

## System tests

```ocaml
module S = Fennec_hunt.System

let () = S.main @@ fun () ->
  S.test "the dev server frees its port when killed" (fun sb ->
    let dev = S.spawn sb [ "fennec"; "dev" ] in   (* argv is a list — no shell, no quoting *)
    S.wait_ready dev ~port:4000 ();                (* condition wait, never sleep *)
    S.signal dev Sys.sigkill;
    S.wait_until (fun () -> not (S.port_open 4000));
    S.check "port freed" (not (S.port_open 4000)))
  (* on teardown the whole process group is reaped — no orphans, even ones the child leaked *)
```

For testing a tool that itself spawns processes (a dev server, a CLI). Each scenario runs in a
disposable sandbox; every process goes into its own session, so teardown kills the **whole tree**.

- **Typed, not stringly.** `spawn`/`run` take argv as a list (no shell, no quoting, no injection);
  results are typed (`status`, `output`, `ms`); paths are typed; HTTP via `request`/`header`.
- **Deterministic.** `wait_ready` / `wait_output` / `wait_until` / `wait_port` are all
  deadline-bounded and raise `Timeout` rather than hang — there is no `sleep`-and-hope.
- **Contained.** `with_edit` rewrites a real source file and always restores it; the sandbox dir
  and the whole process group are torn down on pass, fail, exception, or timeout.

## How it works

- **A target is a URL.** The host and port come from `~url`; the client connects to that host
  (DNS-resolved), so a target can be local or remote. `https://` works — TLS via `tls-eio`,
  accepting any certificate by default (the right call for a self-signed localhost or a
  staging box: you're checking behaviour, not validating a CA).
- **Spawn or attach.** With `~spawn` the harness starts the command, waits for the URL to
  answer, and kills it on exit (tied to the Eio switch — no leak, even on failure). Without
  it, it tests an already-running server. The wait for readiness is the only wait.
- **Parallel by URL.** Two runs against two ports are independent — give each CI shard or
  worktree its own port.
- **Run the built binary, not `dune exec`,** if the spawned server itself shells out to dune
  (`dune describe`, etc.) — `dune exec` holds the workspace lock and the child would deadlock.
  A suite that only hits an already-running URL has no such constraint.

## Notes

Eio-only (OCaml 5+). Dependencies: `eio`, `yojson`, `base64`, `re`, and `tls-eio` (https).
No cohttp, no Lwt. Browser tests need a Chromium-family browser on `PATH` (or set `CHROME`).
A separate package precisely so a production server never links the test machinery.

See the [API docs](https://anonyfox.github.io/fennec): `Fennec_hunt.Http`, `…Live`, `…Run`,
`…System`, `…Backend`, `…Driver`, `…Reporter`, `…Failure`. MIT licensed.
