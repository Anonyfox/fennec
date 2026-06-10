# fennec — the runtime

The HTTP server, middleware, isomorphic UI, and the Pulse reactive data layer. One package, several libs; the
precise API lives inline in each module's `.mli`. `Fennec` (the `app` lib) is the facade you call.
(The standalone [`fennec-hunt`](../hunt/README.md) and [`fennec-mongo`](../mongo/README.md) packages
live at the repo root, not under here.)

## HTTP core — `fennec.core`

Request/response, `Http.meth`, MIME, HTTP-date, RFC semantics (ETag / Range / conditional), cookies,
multipart, and a WebSocket channel. RFC-correct, colocated unit tests.

## Paw — the middleware primitive — `fennec.paw`

A **paw** is `Conn.t -> Conn.t`: it *declines* (passes through) or *answers* (short-circuits the
rest). Routes, static serving, the websocket upgrade, and the SSR app are all paws. Compose with
`Paw.seq`; order is precedence. **The default is nothing** — add capability as needs grow:

| when you want… | paw |
|---|---|
| request logs · trace id · metrics | `Logger` · `Request_id` · `Metrics` |
| security headers · CORS · rate-limit | `Security_headers` · `Cors` · `Rate_limit` |
| sessions · CSRF · basic-auth | `Session` · `Csrf` · `Basic_auth` |
| HTTP→HTTPS behind a proxy | `Force_https` |

Write your own in a line: `let mine : Paw.t = fun c -> Conn.before_send c (…)`.

## Server — `fennec.server`

A compact Eio HTTP/1.1 + WebSocket server: static serving (strong ETag / 304 / Range / HEAD), gzip +
deflate, WebSocket permessage-deflate, multi-app routing by Host, graceful shutdown.
`Fennec.serve [endpoints]` is the single entry point.

## HTTPS — in-process, opt in incrementally

No nginx, no reverse proxy; pure-OCaml TLS (`tls` / `x509` / `mirage-crypto`), no Lwt.

| `Fennec.serve …` | you get |
|---|---|
| `[eps]` | plain HTTP — `:80` in prod; `$PORT` / `FENNEC_PORT` behind a proxy or PaaS |
| `~tls:(Tls.of_files ~cert ~key)` | your own certificate — `:443` + a `:80`→https redirect |
| `~acme:(Acme.auto ())` | automatic Let's Encrypt for every declared domain — renewed, zero-downtime hot-reload |
| `~acme:(Acme.auto ~dns_provider …)` | + **wildcard** certs via DNS-01 (`*.tenant.app`) |
| `~acme:(Acme.auto ~on_demand …)` | + per-customer domains issued **on first connect** |

Dev-safe (ACME no-ops in dev — never hits Let's Encrypt). Pluggable cert store: `file` (default) ·
`memory` · a custom value (k8s Secret / S3 / Redis), with a multi-instance lease. Self-contained
binaries link `libgmp` statically across the mac / linux (incl. musl) matrix.

## Fur — the isomorphic UI — `fennec.fur`

Components written in OCaml that server-render to HTML and hydrate in the browser, from one source.
Signals (`signal` / `get` / `set`), a vdom + reconciler, a typed file-tree router, a `<Head>` manager,
and data resources with fast-render seeds. No React / Melange / preact runtime — the client is a
js_of_ocaml bundle.

## Pulse — reactive data, end to end — `fennec.pulse` (+ `.mongo` · `.server` · `.live`, over `fennec.ddp`)

**Pulse** is the live heartbeat of your data — the data-side counterpart to Fur. Meteor-style sync:
DDP publications/subscriptions over WebSocket, a Mongo/minimongo query + observe engine with
change-stream-backed **live queries**, and **SSR-with-live-data** (the server renders live data into
the first paint, the browser hydrates flicker-free, then the subscription streams deltas). Every
change is a **Beat**; Pulse keeps every client in **Rhythm**. In-memory by default; an optional native
Mongo driver (`fennec-mongo`) for production.

Aggregation runs in-memory too, with cross-collection **`$lookup` / `$unionWith`** — the same joins on
the server and (over its subscribed subset) on the client. The client **reconnects and resyncs** after
a dropped socket; methods report success/failure via `Pulse.call_result`; publications are
parameterized (`subscribe ~params`). At scale, every subscription to the same query rides **one shared
backend observe** (refcounted), so a broadcast feed costs one selector eval per change, not one per
viewer. And the whole vertical is **multicore-safe** — the server runs one Eio domain per core, and
the data layer holds one structural discipline throughout (locks guard snapshots/commits; events
deliver outside all locks, in commit order), proven by multi-domain stress tests.

**Writes go through methods — the one blessed path** (no allow/deny, by decree). A method is one
typed value shared by server and client (`Method.define name ~args ~result` — the codec IS the
validation; drift is a compile error), with **opt-in optimistic UI** (`?stub`): the stub's writes win
instantly via the client cache's precedence band, the server's `updated` (behind a write fence)
reveals truth, and seeded id streams make the optimistic row and the real row one row. Methods are
serial per connection and re-send on reconnect (at-least-once). The guide:
[`../docs/internal/METHODS.md`](../docs/internal/METHODS.md).

**Coming from Meteor, your daily words don't change — only the namespace.** `Pulse.publish` /
`subscribe` / `method` / `call`, `find`, `insert` / `update` / `remove`, and `Ddp` / `Mongo` /
`Minimongo` all stay literal: Pulse rides on those honest, Meteor-compatible substrates, so the
brand lives in the namespace, not the verbs you type.

---

The precise API + every parameter is inline in the `.mli`s. Architecture / decision records:
[`../docs/internal/`](../docs/internal/) (`DATAFLOW`, `CLI-INTEROP`, …).
