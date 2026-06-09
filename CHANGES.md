# Changelog

The fennec packages are versioned together from this monorepo.

## Unreleased — 0.1.0 (first public release)

### `fennec` — the runtime
- Eio HTTP/1.1 + WebSocket server: static serving (strong ETag / 304 / Range / HEAD), gzip + deflate,
  WebSocket permessage-deflate, multi-app routing by Host, graceful shutdown.
- Paw middleware (`Conn.t -> Conn.t`): routes + opt-in batteries (logger, security headers, CORS,
  rate-limit, sessions, CSRF, basic-auth, force-https).
- Fur — isomorphic UI: signals, a vdom + reconciler, SSR, js_of_ocaml hydration, a typed file-tree
  router, a `<Head>` manager, data resources with fast-render seeds.
- Automatic HTTPS, in-process: TLS termination with your own cert (`~tls`); automatic, multi-tenant
  Let's Encrypt (`~acme` — HTTP-01, DNS-01 wildcards, on-demand per-customer domains) with
  zero-downtime renewal and a pluggable cert store (file / memory / external).
- **Pulse** — reactive data, end to end: DDP publish/subscribe over WebSocket, change-stream-backed
  live queries (each change a *Beat*), SSR-with-live-data. The daily API stays Meteor-compatible.

### `fennec-mongo`
- Pure BSON + a MongoDB query / update / projection / sort / aggregation engine + in-memory
  Minimongo with a reactive observe engine — the same source compiles native and to JavaScript.
- Optional, degrade-safe native libmongoc driver (change streams) + a managed `mongod` lifecycle.

### `fennec-hunt`
- Pure-OCaml app testing: inline unit (`let%test`) + type-driven property (`let%prop`) tests, typed
  HTTP assertions, a real headless-Chrome driver (CDP), and system/dev-loop checks. No Node/Selenium.

### `fennec-cli` (distributed as a prebuilt binary — GitHub Releases / Homebrew)
- `fennec build` (esbuild + Lightning CSS / grass), `fennec dev` (~0.1 s livereload loop),
  `fennec test` (unit / http / browser / system / docs), `fennec docs` (coverage + doctests).
