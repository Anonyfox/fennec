# fennec-hunt

Real-browser end-to-end testing for OCaml, in pure OCaml. A hand-written WebSocket +
Chrome DevTools Protocol client drives a headless Chromium directly — **no Node, no
chromedriver, no Selenium, no Lwt**. You install a Chromium-family browser; everything else
is one OCaml library.

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

## Why

- **Auto-waiting DSL** — `goto`, `click`, `fill`, `press`, `within`, `expect_*`, `read_*`,
  `eval`; each step waits for its own precondition.
- **Event-driven & deterministic** — one in-page MutationObserver/rAF round-trip per wait
  (no polling), navigation matched by loader id, evals pinned to the live context. No retry
  hacks.
- **Failures that explain themselves** — a numbered pipeline trace, the captured
  `outerHTML` / selector probe / URL / console, a tuned hint, and a copy-pasteable rerun
  command.
- **A reporter that travels** — colour + unicode + a live status line on a TTY; plain ASCII
  with no cursor control on a CI log or pipe. Auto-detected (`NO_COLOR`, `FORCE_COLOR`,
  `TERM`, `LANG`, `COLUMNS`), airtight under `--jobs`.
- **A fake backend** — the DSL and runner are written against an abstract `Backend.S`, so
  they are fully unit-tested with no browser and behave identically live.

## Run

```sh
fennec-hunt-suite --grep "cart" --jobs 4 --reporter pretty   # your runner exe
```

Flags: `--grep`, `--bail`, `--jobs N`, `--retries N`, `--headed`, `--timeout S`,
`--browsers M`, `--reporter auto|plain|pretty`, `--color`/`--no-color`/`--ascii`. Point at
any Chromium-family browser with `CHROME=/path/to/chrome`.

## Notes

- **Eio-only by design** (direct-style, structured concurrency, leak-free teardown under one
  switch) — hence OCaml 5+.
- Has **no dependency on the `fennec` web framework** it grew up beside; works against any
  web server. It is a separate package precisely so a production server never links the
  test/CDP machinery.

See the [API docs](https://anonyfox.github.io/fennec) (`Fennec_hunt.Live`, `…Run`,
`…Backend`, `…Driver`, `…Reporter`, `…Failure`). MIT licensed.
