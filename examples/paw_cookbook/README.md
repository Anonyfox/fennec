# paw cookbook

Standalone [`fennec-paw`](../../paw/README.md) services, simplest → enterprise. Each file is a complete program that links `fennec-paw` and the Eio entry point and **nothing else** — run any with `dune exec examples/paw_cookbook/<name>.exe`.

Every one is a small delta on the same shape: `Paw.serve [ endpoints ]`.

| File | Shows |
| --- | --- |
| [`../paw_hello/hello.ml`](../paw_hello/hello.ml) | the floor — one endpoint, two routes |
| `json_api.ml` | a few JSON routes + logging + a request id |
| `crud.ml` | REST verbs (GET/POST/PUT/DELETE) + method override |
| `middleware.ml` | the battery + the always-vs-matched pipeline phases |
| `sessions.ml` | signed sessions + CSRF + static files + cookies |
| `websocket.ml` | a WebSocket endpoint beside HTTP routes |
| `virtual_hosts.ml` | many apps on one port, routed by Host; per-host Basic auth |
| `tls_byo_cert.ml` | in-process TLS from your own certificate + force-HTTPS |
| `https_acme.ml` | automatic HTTPS via Let's Encrypt — `serve ~acme` |

Read top to bottom for a guided tour: start at the hello, add middleware, then state, then real-time, then multi-tenant routing, then TLS, then automatic certificates. The handler bodies are stubs — the routing, middleware, and serve wiring are the point.

Testing HTTPS locally: run any server with `FENNEC_DEV_TLS=1` and `serve` terminates TLS on the dev port with a throwaway self-signed `localhost` cert — so Secure cookies, HSTS, and force-HTTPS behave exactly as in production, no cert files to manage (the browser warns on the self-signed cert; click through).
