# fennec-paw

A small, complete **HTTP toolkit for OCaml on [Eio](https://github.com/ocaml-multicore/eio)** — no cohttp, no Lwt. Routing, a middleware pipeline, WebSockets, in-process TLS with automatic HTTPS, and a virtual-host server, all behind one curated `Paw.*` namespace. It depends on **no other fennec library**, so it stands on its own; the rest of the framework builds on top of it.

## One primitive

A **paw** is a `Conn.t -> Conn.t`: given a connection it either *answers* the request or *declines* (passes it through). Routes, middleware, static serving, the WebSocket upgrade — every piece is a paw. Compose them with `Paw.seq`; the first to answer wins.

It's pipes all the way down. Start at `endpoint`, pipe middleware and routes onto it one per line, and `serve` the result on its own line — the only nesting is the handler, the one `fun c -> …` that genuinely *is* nested:

```ocaml
let app =
  Paw.endpoint ()
  |> Paw.use (Paw.Logger.make ())
  |> Paw.get "/" (fun c -> c |> Paw.html "<h1>hello</h1>")
  |> Paw.get "/users/:id" (fun c ->
         c |> Paw.text (Option.value (Paw.param c "id") ~default:"?"))

let () = Paw.serve [ app ]
```

Everything is one `Paw.` away. The endpoint verbs (`endpoint`/`use`/`use_matched`/`get`/`post`/`form`/`app`) take the endpoint last, so they chain with `|>`. Inside a handler, **reads** pull a value out (`Paw.param c "id"` — checks path, then query, then body) and **writes** thread the conn through, conn-last, so you *pipe* the response, Elixir/Plug style:

```ocaml
let create c =
  let title = Option.value (Paw.body_param c "title") ~default:"" in
  c |> Paw.set_status 201 |> Paw.set_cookie "sid" sid |> Paw.json (encode title)
```

A complete runnable hello is in [`examples/paw_hello/`](../examples/paw_hello/hello.ml), and a simplest→enterprise tour is in [`examples/paw_cookbook/`](../examples/paw_cookbook/). For pure unit tests you don't even need a socket — `Paw.run app request` drives a pipeline to a response in memory.

Two escape hatches, both visible: a **reusable, mountable route** is `Paw.Route.get "/" handler` (a plain paw — mount it with `|> Paw.use`); and for **full control** (your own Eio env, a prebuilt router) drop from `serve` to `Host_router.build` + `Server.run`. Nothing is hidden.

## Find your way around

The whole public API is the single `Paw` module (`paw.mli` is its table of contents). The source is organized by concern, one folder per layer:

| Folder | What lives there |
| --- | --- |
| `http/` | the HTTP vocabulary — request/response, headers, cookies, multipart, MIME, dates, caching semantics (pure types, no I/O) — plus the zero-copy C request-head parser (`Http_parse`, the one hot spot hand-written in C) |
| `conn/` | `Conn` — the per-request carrier — `Assigns`, its typed request-scoped state, and `Sse` (Server-Sent Events) |
| `pipeline/` | the `Paw` algebra (`seq`/`run`) + the route verbs (`get`/`post`/… with `:name` / `*rest` captures) |
| `routing/` | `Endpoint` (a named app) and Host-header routing (`Host_router`, `Host_pattern`) |
| `middleware/` | the battery — see below |
| `ws/` | WebSockets (RFC 6455): frame codec, the upgrade paw, the channel |
| `server/` | the Eio HTTP/1.1 acceptor, response finalization, gzip/deflate, dev port planning |
| `tls/` | in-process TLS termination, SNI, and automatic HTTPS via ACME / Let's Encrypt |
| `dev/` | live-reload and the dev control wire |

## The middleware battery

Each is a `make` returning a `Paw.t`, so it drops into any `seq` or `Endpoint`:

- **Sessions & security** — `Session` · `Csrf` · `Cors` · `Security_headers` · `Force_https`
- **Authentication** — `Basic_auth` · `Bearer_auth` · `Webhook` (verify inbound HMAC signatures — GitHub/Slack/generic)
- **Traffic shaping & limits** — `Rate_limit` · `Body_limit`
- **Observability** — `Logger` · `Request_id` · `Metrics` · `Response_time` (`Server-Timing` header)
- **Request hygiene** — `Method_override` · `Normalize_path` (trailing-slash 308) · `Trusted_proxy` (real client IP/scheme from `X-Forwarded-*`) · `Accepts` (406 content negotiation) · `Ip_filter` (allow/deny by IP or IPv4 CIDR)
- **Response shaping & assets** — `Static` · `Cache_control` · `Set_header` · `Status_pages` (bodies for empty error responses)
- **Operations** — `Health` (a liveness probe)

### Built-in behaviours (no wiring)

The server already does the HTTP-correct thing on a miss: a path that exists for other methods returns **`405` with an `Allow` header** (not a blanket 404), and an **`OPTIONS`** probe with no explicit handler is auto-answered **`204` + `Allow`**. An explicit `OPTIONS` route or a `Cors` paw still wins.

Building a JSON API? Pass the ready-made renderer — `Paw.serve ~on_error:Paw.json_errors apps` — and those framework errors come back as `{"error":…,"status":…}` instead of plain text.

## What a handler answers with

All lifted to top-level `Paw.*` (conn-last, so they pipe): `Paw.text` · `Paw.html` · `Paw.json` (you bring the encoder — paw ships no JSON/template engine; that's your data model's job) · `Paw.redirect` · `Paw.send_file` (with `?download` for attachments) · `Paw.download` (in-memory bytes as a saved file — a generated CSV/PDF) · `Paw.sse` (Server-Sent Events: `c |> Paw.sse (fun ~push -> push ~event:"tick" data)`) · `Conn.send_chunked` (arbitrary streaming) · `Conn.upgrade` (WebSocket).

## Batteries that usually need wiring

- **Automatic HTTPS** — `Paw.Acme` obtains and renews Let's Encrypt certificates (HTTP-01, optional DNS-01 for wildcards, on-demand issuance), backed by a pluggable `Cert_store`.
- **TLS termination** — `Paw.Tls_termination` loads a cert chain + key and selects per host via SNI; pass it to `Server.run ~tls`.
- **Virtual hosts** — one `Host_router` table serves many apps on one port, selected by the `Host` header (exact, `*.`wildcard, or catch-all).

## Performance

Fast by design, measured honestly. In a same-machine, apples-to-apples shootout ([`benchmarks/`](../benchmarks)) against idiomatic Go (`net/http`), Rust (`actix-web`), Node (Fastify), and Elixir (Plug), **paw's request-processing throughput lands between Go and Rust — roughly 3× Go and Node, behind only Rust — while doing more per response than any of them** (a content ETag, conditional-request handling, and compression negotiation). The single hand-crafted C hot path — the zero-copy request-head parser — keeps per-header cost off the table; everything above it is idiomatic effect-based OCaml on Eio, sitting right on the runtime's IO floor with near-zero framework overhead.

## Dependencies

Standard OCaml libraries only: `eio` · `tls` / `tls-eio` · `x509` · `digestif` · `base64` · `zarith` · `ptime` · `mirage-crypto-rng` · `zlib`. No database, no framework, no JSON library — the ACME client hand-rolls the handful of JSON reads it needs, so a production server stays lean.
