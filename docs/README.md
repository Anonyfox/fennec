# Fennec documentation

Start at the top and go as deep as you need.

## Start here

1. **[OCAML-FOR-WEB-DEVS.md](./OCAML-FOR-WEB-DEVS.md)** — 5-minute orientation if you're coming from
   JS/TS, Rails, or Phoenix. You don't need to know OCaml first.
2. **[QUICKSTART.md](./QUICKSTART.md)** — install, run the example, edit a page, test, ship.
3. **[The example app](../examples/site/README.md)** — the living reference: two isomorphic apps,
   file-tree routing, global state, isomorphic + live data, and full tests.

## Guides — one per building block

| Block | Guide |
|---|---|
| UI runtime (signals, SSR, hydration, router) | [`../fennec/fur/README.md`](../fennec/fur/README.md) |
| The pipeline / `conn -> conn` model | [`PIPELINE.md`](./PIPELINE.md) |
| Middleware batteries (opt-in ladder) | [`PAW.md`](./PAW.md) |
| HTTPS — TLS termination, automatic + multi-tenant ACME | [`HTTPS.md`](./HTTPS.md) |
| Reactive data + realtime (DDP, Mongo/minimongo, live queries, SSR-with-live-data) | [`../fennec/mongo/README.md`](../fennec/mongo/README.md) + the realtime tasks in the [example](../examples/site/README.md) |
| Testing (unit / property / HTTP / browser / system) | [`../fennec/hunt/README.md`](../fennec/hunt/README.md) |
| The `fennec test` command | [`TEST-CLI.md`](./TEST-CLI.md) |
| Dev loop ↔ plain dune (the decoupling contract) | [`internal/CLI-INTEROP.md`](./internal/CLI-INTEROP.md) |

## Reference

Each package's `.mli` interfaces are the curated API surface (the load-bearing modules have firewall
`.mli`s with doc comments — `fennec test docs` enforces coverage). The top-level
[README](../README.md) maps the packages.

## Internal notes

Design rationale, competitive research, and work-tracking live in [`internal/`](./internal/) — useful
for contributors, not needed to build an app.
