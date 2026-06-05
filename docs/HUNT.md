# fennec_hunt — the testing package

The fennec hunts for bugs. One package, two independent layers — import what you need:

- **Http tests** (`Fennec_hunt.Http`) — a typed HTTP/1.1 client + deterministic assertions
  against any URL. No browser. Every request is one call → one response → immediate
  pass/fail. For "does my API return 401?", "does host routing work?", "is the JSON shape
  right?"
- **Browser tests** (`Fennec_hunt.Live` + `Run`) — a real Chrome over the DevTools Protocol.
  Full-stack: JS execution, hydration, DOM, client-side state. For "does the counter
  increment?", "does the page hydrate correctly?"

It is its own opam package (`fennec-hunt`), so a production server depending on `fennec`
never links it (or its yojson/base64/re deps).

---

## Http tests

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
    (* cookies from the login carry automatically to the next request *)
    get "/dashboard" ~expect:[status 200; body_contains "Welcome"])
```

- **`hunt label ~url ?spawn ?env ?timeout body`** — a test suite against `url`. If `~spawn`
  is given, starts that command and waits for the URL to respond; otherwise tests an
  already-running server (local, remote, CI, staging). `~url` is the identity — the port is
  derived from it, so parallel instances just pass different URLs.
- **`check label body`** — one test case. Fresh cookie jar, timed, pass/fail reported.
- **Requests** — `get`/`post`/`put`/`patch`/`delete`/`head`/`options` with `~headers`,
  `~host` (virtual-host shorthand), `~query`, `~body`/`~form`/`~json`, `~expect`.
- **Assertions** (for `~expect` lists) — status (`status`, `status_not`, `status_2xx`…),
  body (`body_contains`/`is`/`not_contains`/`empty`/`matches`…), headers (`header_is`/
  `contains`, `has_header`, `no_header`, `content_type`, `is_json`, `is_html`), JSON path
  (`json_path_is`/`contains`/`matches`, `json_has`, `json_length`, `json_is_string`/`number`/
  `bool`/`null`/`array`/`uuid`/`datetime`), cookies (`has_cookie`, `no_cookie`), `redirect_to`,
  `max_elapsed`, and the escape hatch `expect (fun r -> …)`.
- **Extractors** (from the last response) — `header`, `header_opt`, `response_body`,
  `response_status`, `elapsed_ms`, `json_field`, `json_path`, `json`, `cookie`, `cookie_opt`.
- **Helpers** — `basic_auth`, `bearer`, `json_content_type`.

The cookie jar is automatic and per-`check`: a `Set-Cookie` in one response is sent on the
next request in the same check, and reset between checks. Deterministic — same requests,
same cookies. No retry, no polling: a failing assertion fails immediately with a structured
message (expected, actual, elapsed time, URL).

## Browser tests

```ocaml
open Fennec_hunt.Live

let () = test "counter hydrates and increments" @@ fun page ->
  page
  |> goto "/" |> wait_for "window.__hydrated === true"
  |> click ".inc" |> expect_text ".count" "1"
  |> ignore

(* a separate runner executable boots the server + browser and runs every registered test *)
let () = Fennec_hunt.Run.main_cli ~base_url:"http://localhost:4001" ()
```

The page DSL is a `page -> page` pipe (browser tests are step-by-step: navigate, act, assert).
Conditions are auto-waited (event-driven, one CDP round-trip — no client-side polling), and a
failed step produces a structured `Failure.t` with the page state at the moment of failure.

## Module layout

```
fennec/hunt/
  http.ml(i)         — PUBLIC. Http tests: hunt/check/get/assertions/extractors.
  live.ml            — PUBLIC. Browser DSL = Driver.Make(Cdp_backend).
  driver.ml(i)       — PUBLIC. The page DSL + runner, as a functor over a backend.
  backend.ml(i)      — PUBLIC. The page-backend contract (for custom backends).
  run.ml(i)          — PUBLIC. The browser runner (main / main_cli).
  reporter.ml(i)     — PUBLIC. Capability-detecting terminal reporter (browser runs).
  failure.ml(i)      — PUBLIC. Test failure as data + renderer.
  http_client.ml     — private. The raw HTTP/1.1 client (one request per connection).
  cdp.ml             — private. The CDP WebSocket wire client.
  cdp_backend.ml     — private. The CDP implementation of Backend.S.
  chrome.ml          — private. Chrome discovery + launch + lifecycle.
  conformance.ml     — private. Compile-time proof Cdp_backend : Backend.S.
  fennec_hunt_util.ml — private. Tiny shared util (substring search).
```

## Dependencies

`eio`, `eio.unix`, `eio_main`, `unix`, `yojson` (JSON assertions), `base64` (basic-auth +
the WebSocket handshake key), `re` (regex assertions), and the `tls-eio` / `x509` /
`mirage-crypto-rng` / `domain-name` stack (TLS for `https://` targets). No npm, no Lwt, no
chromedriver, no Selenium, no cohttp — the HTTP client is hand-written on raw Eio sockets,
upgraded to TLS via tls-eio for https. The only runtime requirement for Browser tests is a
Chromium-family browser on the host.

## Design notes

- The two layers share no machinery. Http tests run synchronously inline (`hunt`/`check`
  print results directly); Browser tests register globally and run via `Run` with
  concurrency, grep, retries, and the capability-aware `Reporter`. The execution models
  differ because the testing models differ — Http is one-shot request/response, Browser is a
  stateful navigate-act-assert sequence.
- The naming differs per layer on purpose: Http assertions read as Hurl/Supertest-style
  predicates (`status 200`, `body_contains "ok"`) inside `~expect` lists; Browser assertions
  read as Playwright-style pipe steps (`expect_text ".count" "1"`). Each is idiomatic in its
  paradigm.
- `Backend`/`Driver` are public so the DSL can be driven against a custom backend (and so the
  in-memory fake in `fennec/hunt/test/` can unit-test the DSL without a browser).
