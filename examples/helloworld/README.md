# helloworld — the fennec isomorphic example

The leanest end-to-end fennec app, and the project's living DX benchmark. One
component (`shared/app.mlx`) is **server-rendered** and then **hydrated** on the
client — same source, two targets, no duplication.

## What it demonstrates

- **Isomorphic component**: `shared/app.mlx` compiles natively for SSR
  (`shared_native/`, via server-reason-react) and to JS via Melange
  (`shared_js/` + `client/`). The server renders it to HTML; the client mounts
  the same component over that HTML and makes it interactive.
- **Asset pipeline via dune + the CLI**: `dune` rules call `%{bin:fennec} build`
  to produce three bundles next to the server exe — `react.js` (the preact
  runtime exposed as `window.React`), `app.js` (the Melange client, bundled with
  react externalized to that runtime), and `app.css` (SCSS → minified).
- **Livereload** in dev (see [../CLI-INTEROP.md](../CLI-INTEROP.md)).

## Layout

```
shared/app.mlx        one isomorphic component (the source of truth)
shared_native/        SSR compile of shared/*.mlx  (server-reason-react)
shared_js/            Melange compile of shared/*.mlx
client/client.mlx     hydration entry (reads inline props, mounts <App>)
server.ml             SSR + serves the bundles; livereload in dev
src/app.scss          stylesheet
test/hydration.test.mjs  jsdom integration test (SSR + hydrate + interact)
```

## Run it

```sh
npm install                 # one-time: preact (client runtime) + jsdom (test)
dune build examples/helloworld/
dune exec examples/helloworld/server.exe     # http://localhost:8200

# or, with livereload:
fennec dev _build/default/examples/helloworld/server.exe
```

> The `npm install` here is the **only** npm step: it provides preact for the
> client runtime bundle. The fennec framework itself needs no npm. esbuild (in
> the `fennec` binary) resolves `node_modules`, so npm packages "just work" as
> bundle inputs — see the require-shim wiring in `dune`.

## Test it

The isomorphic loop is verified automatically: boot the built server, then drive
the real `react.js` + `app.js` bundles through jsdom — asserting SSR markup
exists before JS, the runtime installs, the component hydrates, and a button
click updates state.

```sh
dune build examples/helloworld/   # build the bundles first
npm test                          # node test/hydration.test.mjs
```
