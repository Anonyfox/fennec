# Paw — the opt-in middleware ladder

A **paw** is fennec's request-pipeline primitive: a `Conn.t -> Conn.t` step (Elixir/Plug-style), with
typed assigns, halting, and route matching. Routing, the websocket, static serving, and the SSR app
are all paws. *(The conceptual model — answering/declining, the two-phase pipeline — is
[`PIPELINE.md`](./PIPELINE.md); the canonical API is the `.mli` (`Fennec.Paw`, `Fennec.Conn`). This
page is the practical battery ladder.)*

**The default is nothing.** An endpoint with just your routes imposes no middleware:

```ocaml
Endpoint.make ~name:"api" ~hosts:[ "*" ] ()
|> Endpoint.get "/health" (fun c -> Conn.json c {|{"ok":true}|})
|> Endpoint.app fallback
```

You add capability by piping batteries — each one is `Module.make … : Paw.t`, composed with
`Endpoint.pipe`, in order:

```ocaml
Endpoint.make ~name:"web" ~hosts:[ "*" ] ()
|> Endpoint.pipe [ Paw.Logger.make (); Paw.Security_headers.make (); Fennec.static ~name:"webroot" ~assets ]
|> Endpoint.get "/health" handler
|> Endpoint.app ssr
```

## The ladder — add as needs grow

| When you want… | Paw | `make` |
|---|---|---|
| request logs | `Paw.Logger` | `?sink ()` |
| a request id / trace header | `Paw.Request_id` | `?header ()` |
| latency/status metrics → your sink | `Paw.Metrics` | `(meth ~path ~status ~duration_ms) ` |
| baseline security response headers | `Paw.Security_headers` | `?extra ()` |
| serve a static / embedded web root | `Paw.Static` (`Fennec.static`) | `?cache_control source` |
| `PUT`/`DELETE` from HTML forms | `Paw.Method_override` | `?field ?header ()` |
| cross-origin API access | `Paw.Cors` | `?origins ?methods ?headers ?credentials …` |
| throttle abusive clients | `Paw.Rate_limit` | `?key ?capacity ?per_second …` |
| HTTP basic auth on an endpoint | `Paw.Basic_auth` | `~username ~password ?realm ()` |
| signed cookie sessions | `Paw.Session` | `~secret …` |
| CSRF protection for forms | `Paw.Csrf` | `~secret …` |
| a WebSocket route | `Paw.Websocket` | `path handler` |
| redirect HTTP→HTTPS **behind a proxy** | `Paw.Force_https` | `?status ?hsts ()` |

Everything is opt-in and order-sensitive (logger first, auth before the protected routes, etc.).
Halting (`Conn.halt`) short-circuits the rest — a `Basic_auth` 401 never reaches your handler.

## Write your own

A paw is just a function — trivial to write and unit-test:

```ocaml
let powered_by : Paw.t =
 fun c -> Conn.before_send c (fun r -> { r with Http.headers = ("X-Powered-By", "fennec") :: r.Http.headers })
```

## HTTPS is *not* a paw

TLS termination and ACME live at the server level — `Fennec.serve ~tls` / `~acme` — not in the paw
pipeline. The one HTTPS-adjacent paw is **`Force_https`**, which you add only when something *else*
terminates TLS (a proxy / PaaS) and you want to redirect plain HTTP and emit HSTS. See
[`docs/HTTPS.md`](./HTTPS.md) for the termination / automatic-certificate story.
