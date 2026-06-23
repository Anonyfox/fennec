# `_client/` — generated build plumbing. Do not edit.

**This is not your code.** It exists so the build can compile the *same* `frontend/` twice — once for
the **OCaml server** (server-side rendering) and once for the **browser** (js_of_ocaml) — and stage the
resulting bundles into the served web root. That dual compilation is what makes a Fennec app isomorphic.

Everything here is driven by globs over `frontend/`, so **adding a page, component, handler, or whole
app needs zero changes in here** (or anywhere). dune:

- generates the per-app / per-handler js_of_ocaml bundle stanzas (`apps/gen`, `handlers/gen`),
- compiles the `-data-client` / `-conn-client` *mirrors* of your `frontend/` — these strip every
  server-only fetcher body and `Conn` usage, so secrets and server code never reach the browser bundle,
- and stages the result into `served/_apps|_handlers/<name>/main.{js,css}` for the web root.

You only ever touch `frontend/`. Leave this folder alone — and `_build/`, `.fennec/`, and `dist/` too.
The leading underscore is the signal: generated, transient, not source.
