# `fennec console` — a live REPL with the app + framework loaded

`fennec console` boots the whole app — the framework, your libraries, the data backend (the persistent
burrow in dev) — into an OCaml toplevel and drops you into a REPL, like `iex -S mix` or `rails console`.
You evaluate against the **live runtime**, with the app's own modules in scope:

```
fennec» Pulse.find Task.collection (Sift.Filter.empty)
fennec» Accounts.create_user (Accounts.current ()) ~username:"alice" ~email:"a@x.com" ()
fennec» Web_app.Routes.mount
```

Two ways in:

- **`fennec console`** — a standalone REPL (no HTTP server).
- **`fennec dev --console`** — the dev loop *and* a REPL pinned to the bottom of the feed; HTTP, build
  results, and errors stream above while you evaluate below (the `iex -S mix phx.server` shape).

## The load-bearing trick

The REPL runs *inside* a server process that is linked with `-linkall`, so `Toploop.execute_phrase`
resolves the process's **own linked modules against their live runtime symbols** — no second process, no
reloaded copy. That's what makes `Pulse.find` hit the live burrow exactly like the app does.

## The pieces (one socket protocol, swappable frontends)

```
  terminal driver  ◀── unix socket, Console_protocol ──▶  in-server engine (the toplevel)
  (cli/console)                                            (fennec/console/engine)
        │                                                          ▲
        └── future: a web / editor frontend = another sink over the SAME protocol
```

| Library | Path | Role |
|---|---|---|
| `fennec.console.protocol` | `fennec/console/protocol` | the wire seam — request/reply variant types + a length-prefixed Marshal codec. Stdlib-only; the types *are* the contract. |
| `fennec.console.engine` | `fennec/console/engine` | **dev-only, bytecode-only.** Hosts the toplevel in the server: evaluates in the live Eio context, streams output, completes from the typing env, cancels a hung eval. Registers via `Fennec.set_console_hook`, so fennec's core never names the compiler. |
| `fennec_console` | `cli/console` | the terminal frontend — raw-mode line editor (history, emacs keys, tab completion, Ctrl-C/D), a Unix-socket client, and a pluggable render `sink` (print-above-prompt). Drives both modes. |
| `fennec_tty` | `cli/tty` | terminal capability + escape helpers, shared by the driver and the dev UI. |

## Wiring

- `Fennec.serve` is factored into a shared `with_runtime` (boot the data layer once) used by both the
  HTTP server and `Fennec.console_run` (boot + REPL, no HTTP).
- The console runs a dedicated `console` byte target — `-linkall`, links the engine, `let () =
  Fennec.console_run ()`. It calls `console_run`, **not** `serve`, so server discovery ignores it and
  `server.bc` / `server.exe` are untouched. The example and `fennec new` scaffold both ship the target.
- **Prod stays byte-identical**: the engine (and the compiler it pulls) never reach the native
  `server.exe`; the prod-lean guard enforces it.

## Caveats / next

- The console is a **sibling process** sharing the backend on disk (rails-console parity). It does not
  share the running server's *in-memory* state — that would need the dev server itself to embed the
  engine, which `Discover`'s "exactly one `Fennec.serve`" invariant currently precludes.
- Under `dev --console`, the console is built and spawned once at startup; app-code edits do not hot-
  reload into it (restart the console for fresh code).
- Polish not yet wired: `[@@ocaml.toplevel_printer]` custom printers (values render structurally today)
  and ppx-on-typed-phrases (so `[%q …]`/fur work at the prompt). Calling compiled functions and plain
  OCaml works fully.
