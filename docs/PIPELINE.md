# The Fennec pipeline model

A single concept, three pages of consequences.

## The primitive

A **paw** is a function:

```ocaml
type Paw.t = Conn.t -> Conn.t
```

It receives a connection (the current request + any in-flight response) and returns an
updated connection. That's it. Middleware, a route, static file serving, a websocket
upgrade, an SSR app mount — they are all paws.

## Answering and declining

A conn starts **unanswered** (no response set). A paw may:

- **Decline** — return the conn unchanged (or with metadata added). The next paw runs.
- **Answer** — set a response (`Conn.text`, `Conn.html`, `Conn.json`, `Conn.redirect`,
  etc.). The conn is now "answered."

`Paw.seq [a; b; c]` composes paws left-to-right. Each runs only while the conn is
unanswered — so the **first paw to set a response wins** and subsequent paws are skipped.

This is the short-circuit: precedence is declaration order. Put specific routes before
generic ones; put a catch-all last.

## Post-processing (paws after the answer)

The always-phase pipeline short-circuits on the first answer. But the **matched phase**
(`pipe_matched` / `use_matched`) runs unconditionally on every already-answered conn —
every paw gets a chance to inspect or modify the response via `Conn.before_send`.

This means:
- A logging paw can see which response was chosen.
- An auth paw can override the response (return 401 instead).
- A header-stamping paw can add headers to every response.

```
  ➜  http://localhost:4001

  Request flow:

  Host_router (select endpoint by Host header)
     │
     ▼
  ┌─ always-phase pipeline ──────────────────────────────┐
  │  Logger → CORS → Security headers → Static → Routes  │
  │  (first to answer wins; unanswered = 404)             │
  └───────────────────────────────────────────────────────┘
     │
     │  Did any paw answer?
     │  NO  → error funnel (No_route → 404 by default)
     │  YES ↓
     │
  ┌─ matched-phase pipeline ──────────────────────────────┐
  │  Auth → Rate limit → Response logging                  │
  │  (every paw runs, can override or post-process)        │
  └────────────────────────────────────────────────────────┘
     │
     ▼
  Response (or error funnel if exception / timeout)
```

## Why two phases matter

Without the matched phase, auth middleware runs on **every** request — including those
that don't match any route. An unmatched request gets a 404 from the catch-all, but the
auth paw already ran and may have turned it into a 401 or required a login header that
doesn't belong on a "not found" response.

With the matched phase, auth only fires when a route actually matched — so a true 404
stays a 404. This eliminates a common, subtle bug class in every flat-pipeline framework.

For simple apps (no `pipe_matched` calls), the matched phase is empty and the pipeline
behaves as a single flat list — zero DX cost for the common case.

## Multiple apps on one endpoint

`Endpoint.app ~at:"/seller" seller_render` is a paw that answers GET/HEAD requests under
`/seller/` when the render function matches, and declines otherwise. You can mount multiple
apps at different subpaths:

```ocaml
let marketplace =
  Endpoint.make ~name:"marketplace" ()
  |> Endpoint.pipe [ logger; security; static ]
  |> Endpoint.app ~at:"/seller" Seller_app.Routes.render
  |> Endpoint.app ~at:"/buyer"  Buyer_app.Routes.render
  |> Endpoint.app ~at:"/"       Public_app.Routes.render
```

First match wins (a `/seller/dashboard` request is answered by the seller app; it never
reaches the buyer or public app). The `app` mount is **not terminal** — paws after it
still run for post-processing.

A catch-all or 404 page is a convention (a `rest__` file-route in the Fur router), not a
framework requirement. Without one, an unanswered conn falls to the error funnel, which
renders `No_route` via the `~on_error` handler (default: 404 text).

## Composing reusable pipelines

A paw list is just an `Paw.t list`. Build one, name it, reuse it:

```ocaml
let common = [ Paw.Logger.make (); Paw.Security_headers.make (); powered_by; static ]

let web   = Endpoint.make ~name:"web" ()   |> Endpoint.pipe common |> Endpoint.app ...
let admin = Endpoint.make ~name:"admin" () |> Endpoint.pipe common |> Endpoint.app ...
```

`Endpoint.pipe common` appends the list. `Endpoint.use (Paw.get "/health" handler)` appends
one. `Endpoint.pipe_matched [auth]` appends to the matched phase. That's the full API.

## The error funnel

All request-scoped errors flow through one function:

```ocaml
type request_error =
  | Handler_exception of exn * Http.request
  | Handler_timeout of Http.request
  | No_route of Http.request

Fennec.serve ~on_error:(fun err -> match err with ...) [web; admin]
```

The default renders plain text (500 / 503 / 404). Override `~on_error` to render JSON,
branded error pages, or log to a structured sink. One function, one place.

## Summary

| Concept | API |
|---|---|
| A paw | `Conn.t -> Conn.t` |
| Compose | `Paw.seq [a; b; c]` (first answer wins) |
| Always-phase | `Endpoint.pipe` / `Endpoint.use` / verb shortcuts |
| Matched-phase | `Endpoint.pipe_matched` / `Endpoint.use_matched` |
| Mount an app | `Endpoint.app ~at:"/path" render` (not terminal) |
| Error funnel | `Fennec.serve ~on_error` |
| Test a pipeline | `Paw.run handler request` (pure, no server needed) |
