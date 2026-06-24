# `_client/` — generated build plumbing. Do not edit.

**This is not your code.** It exists so the build can compile the *same* `frontend/` twice — once for
the **OCaml server** (server-side rendering) and once for the **browser** (js_of_ocaml) — and stage the
resulting bundles into the served web root. That dual compilation is what makes a Fennec app isomorphic.

Everything here is driven by globs over `frontend/`, so **adding (or nesting into subfolders) a page,
component, handler, or whole app needs zero changes in here** (or anywhere). dune:

- compiles the `-data-client` / `-conn-client` *mirrors* of your `frontend/` (`components/`,
  `documents/`, `handlers/mirror/`) — these strip every server-only fetcher body and `Conn` usage, so
  secrets and server code never reach the browser bundle. Each `<cat>/gen` runs `route_gen --mirror`,
  which walks the authored tree and emits a `copy_files#` per subfolder (flattening the nested tree to
  match the server lib's `(include_subdirs unqualified)`); `<cat>/run` is the mirror library +
  `dynamic_include`. Nest a file under `frontend/<cat>/` and its mirror appears automatically.
- generates the per-app / per-handler js_of_ocaml bundle stanzas (`apps/gen`, `handlers/gen`),
- and stages the result into `served/_apps|_handlers/<name>/main.{js,css}` for the web root.

You only ever touch `frontend/`. Leave this folder alone — and `_build/`, `.fennec/`, and `dist/` too.
The leading underscore is the signal: generated, transient, not source.
