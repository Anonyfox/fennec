# fennec-paw

A small, complete **HTTP toolkit for OCaml on [Eio](https://github.com/ocaml-multicore/eio)** — no cohttp, no Lwt. Routing, a middleware pipeline, WebSockets, in-process TLS with automatic HTTPS, and a virtual-host server, all behind one curated `Paw.*` namespace. It depends on **no other fennec library**, so it stands on its own; the rest of the framework builds on top of it.

## One primitive

A **paw** is a `Conn.t -> Conn.t`: given a connection it either *answers* the request or *declines* (passes it through). Routes, middleware, static serving, the WebSocket upgrade — every piece is a paw. Compose them with `Paw.seq`; the first to answer wins.

```ocaml
let app =
  Paw.endpoint
    [ Paw.Logger.make ();
      Paw.get "/" (fun c -> c |> Paw.html "<h1>hello</h1>");
      Paw.get "/users/:id" (fun c ->
          c |> Paw.text (Option.value (Paw.param c "id") ~default:"?")) ]

let () = Paw.serve [ app ]
```

The happy path is one `Paw.` away. `serve` builds the host router and owns the Eio loop; `endpoint` bundles middleware + routes. **Reads** pull a value out (`Paw.param c "id"` — checks path, then query, then body); **writes** thread the connection through, conn-last, so a handler reads once then *pipes* the response, Elixir/Plug style:

```ocaml
let create c =
  let title = Option.value (Paw.body_param c "title") ~default:"" in
  c |> Paw.set_status 201 |> Paw.set_cookie "sid" sid |> Paw.json (encode title)
```

A complete runnable hello is in [`examples/paw_hello/`](../examples/paw_hello/hello.ml), and a simplest→enterprise tour is in [`examples/paw_cookbook/`](../examples/paw_cookbook/). For pure unit tests you don't even need a socket — `Paw.run app request` drives a pipeline to a response in memory.

Need more control — virtual hosts, the two-phase pipeline (middleware that runs only on matched routes), your own Eio env? Drop from `serve`/`endpoint` to the `Endpoint` builder (`Endpoint.make () |> use … |> get …`) + `Host_router.build` + `Server.run`. Nothing is hidden; the shortcuts are just the common case.

## Find your way around

The whole public API is the single `Paw` module (`paw.mli` is its table of contents). The source is organized by concern, one folder per layer:

| Folder | What lives there |
| --- | --- |
| `http/` | the HTTP vocabulary — request/response, headers, cookies, multipart, MIME, dates, caching semantics (pure types, no I/O) |
| `conn/` | `Conn` — the per-request carrier — and `Assigns`, its typed request-scoped state |
| `pipeline/` | the `Paw` algebra (`seq`/`run`) + the route verbs (`get`/`post`/… with `:name` / `*rest` captures) |
| `routing/` | `Endpoint` (a named app) and Host-header routing (`Host_router`, `Host_pattern`) |
| `middleware/` | the battery — see below |
| `ws/` | WebSockets (RFC 6455): frame codec, the upgrade paw, the channel |
| `server/` | the Eio HTTP/1.1 acceptor, response finalization, gzip/deflate, dev port planning |
| `tls/` | in-process TLS termination, SNI, and automatic HTTPS via ACME / Let's Encrypt |
| `dev/` | live-reload and the dev control wire |

## The middleware battery

Each is a `make` returning a `Paw.t`, so it drops into any `seq` or `Endpoint`:

`Session` · `Csrf` · `Cors` · `Static` · `Logger` · `Rate_limit` · `Basic_auth` · `Force_https` · `Security_headers` · `Request_id` · `Metrics` · `Method_override`

## Batteries that usually need wiring

- **Automatic HTTPS** — `Paw.Acme` obtains and renews Let's Encrypt certificates (HTTP-01, optional DNS-01 for wildcards, on-demand issuance), backed by a pluggable `Cert_store`.
- **TLS termination** — `Paw.Tls_termination` loads a cert chain + key and selects per host via SNI; pass it to `Server.run ~tls`.
- **Virtual hosts** — one `Host_router` table serves many apps on one port, selected by the `Host` header (exact, `*.`wildcard, or catch-all).

## Dependencies

Standard OCaml libraries only: `eio` · `tls` / `tls-eio` · `x509` · `digestif` · `base64` · `zarith` · `ptime` · `mirage-crypto-rng` · `zlib` · `yojson`. No database, no framework.
