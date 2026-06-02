# iso-lab — isomorphic OCaml UI runtime (PoC)

A self-contained experiment: a Vue-SFC-flavoured, signals-based UI runtime that
renders the SAME `.mlx` components on the server (SSR → string) and in the browser
(js_of_ocaml → DOM, with true hydration). No React/melange dependency.

## Layout

- `iso/` — platform-agnostic core: signals (`signal`/`get`/`set`/`update`), the
  `vnode` type, SSR (`to_html`/`document`), and `Iso.Head` (data-driven, reactive
  head management).
- `html/` — generated typed HTML element functions (per-element labeled attrs +
  value types; unknown tags/attrs are compile errors).
- `ppx/` — the mlx→runtime ppx: lowers JSX elements to `Html.*`, capitalized tags
  to component instances (`Iso.comp`), coerces non-vnode children to text.
- `app/` — the demo: `Store` (global state), `Counter` (local state), `Stats` +
  `Todo_list` (global), `App` (composition + head defaults), `Document` (server
  -only HTML shell template).
- `bin/` — `ssr.exe` (native SSR + page assembly), `client.bc.js` (jsoo CSR:
  `iso_dom` reconciler + `iso_head` head reconciler), `style_extract.exe`.
- `e2e.mjs` — jsdom end-to-end test (SSR → hydrate → interact → assert).

## State model

One primitive: `signal`. A signal created in a component's setup is LOCAL
(per-instance); a signal in a shared module is GLOBAL. `get` subscribes, `set`
notifies. No providers, no prop-drilling. Components are persistent instances
(`make () : unit -> vnode`: setup runs once, returns the render), each with its
own reactive effect — so updates are fine-grained and instances survive parent
re-renders.

## Head model

`Head.use (fun () -> [...])` in setup registers reactive metadata. Deeper/later
components override shallower ones (key-deduped, last wins). Isomorphic by design:
SSR marks tags `data-ih="<key>"`; the client reconciles `document.head` by the same
key, so the first pass is a no-op (rehydration-safe) and updates are reactive.

## Data model (isomorphic fetch / fast-render)

Data is a **reactive resource** — a signal of `Loading | Ready | Failed` — never
awaited mid-render, so the Eio↔Promise gap never reaches component code (each side
just resolves into a `set`). One keyed table (`window.__ISO_DATA__` ⇄ the server's
data context) serves SSR-embed and client-seed:

```ocaml
let g = Data.resource ~key:"/api/greeting" ~fallback:"…" ~decode:Fun.id ()
(* render: *) (Data.value g)   (* fallback until ready, then the value, reactively *)
```

- **Server** actually runs the fetch (Eio, in-process — the path doubles as the
  route key, so no HTTP-to-self / relative-path problem), awaits all fetches between
  render passes, then serializes the context into the page.
- **Client** seeds from `__ISO_DATA__`: a resource whose key is present resolves
  synchronously (no fetch, no loading flash, rehydration-safe), then the seed is
  consumed so `Data.refetch` and later keys hit the network for real.
- `~client_only:true` — browser-only data: SSR renders the fallback and embeds
  nothing; the client fetches after hydration.
- `on_mount f` — Vue `onMounted` equivalent: a browser-only side effect, no-op on
  the server.

The one platform split is `Iso.Data.source` (a hook): the server forks an Eio
fiber; the client does a real `fetch`. Components never see which is linked.

## Authoring & layout (the DX)

```
app/
  app.mlx          # layout shell: nav + (outlet ())
  document.mlx     # server-only HTML shell (slots: Doc.head/outlet/scripts)
  *.mlx            # reusable components (counter, greeting, …)
  store.ml         # global state
routes/            # the file tree IS the route table (Next.js convention)
  index.mlx        #  /
  products/
    index.mlx      #  /products
    [id].mlx       #  /products/:id   (param in the filename)
  [...rest].mlx    #  catch-all → not-found
```

A component/page is `<script setup>` style — top-level bindings are the per-instance
setup, `view` is the reactive render; the ppx generates `make`. No `open`, no `make`,
no `fun () ->`:

```ocaml
let count = signal 0                       (* setup *)
let view =                                  (* render *)
  <div class="counter">
    <button onClick=(fun () -> count -= 1)>"−"</button>
    <span>(get count)</span>
    <button onClick=(fun () -> count += 1)>"+"</button>
  </div>
```

(Define `make` explicitly to opt out — the full-power escape hatch for typed props /
custom args / the server-only `document.mlx`.)

A `route_gen` step scans `routes/` and emits one `Routes_gen` module: the router, the
page modules, and a **typed `Paths`** — one builder per route, so links are
compile-checked:

```ocaml
Paths.products_id ~id:"7"   (* ✓ /shop/products/7 *)
Paths.nope ()               (* ✗ compile error: Unbound value Paths.nope *)
Paths.products_id ~slug:"7" (* ✗ compile error: no such label ~slug *)
```

`Iso.p "/products/%d" 7` (ambient, runtime-checked) is the lighter alternative;
`Iso.ext "/admin"` links outside the app. The client entry is one line
(`Iso_csr.start`); the server entry is the mount table + data source + document.

## Router (universal, location-transparent)

Reuses fennec's pure `Matcher` (`:named` params, `*` tail) verbatim; the rest is a
tiny reactive layer on the signals runtime — no React `Shell`/context needed.

- **Location-transparent mounting.** An app is mounted at a BASE (`/shop`) and
  declares routes RELATIVE to it (`/products/:id`). The server strips the base; the
  client gets it injected. The same app works at any base. The mount table
  (`base -> app`) lives server-side; longest prefix wins.
- **Reactive.** `current` is a path signal; `Router.outlet` re-renders the matched
  page on navigation (keyed by relative path → param change remounts with fresh
  params). Pages mount/unmount cleanly via the effect scope (`on_cleanup`), so
  per-page `Head.use` titles never accumulate.
- **SPA navigation.** Client-side: pushState + popstate + global click interception
  scoped to the base (out-of-scope/external links fall through to the browser).
- **Typed path building (Phoenix `~p` flavour, no ppx).** OCaml format strings are
  already typed, so:
  ```ocaml
  App_router.p   "/products/%d" 42   (* in-app: typed hole, base auto-prefixed, dev-checked vs routes *)
  App_router.ext "/admin"            (* outer reach: raw url, no base, no check *)
  ```
  `%d` rejects a string at compile time; a path matching no route fails fast; `ext`
  is the explicit escape hatch for linking to other apps / external sites.

Deep-link SSR + hydration, reverse routing, reactive params, head cleanup, store
persistence across nav, popstate, and `on_mount` for nav-mounted components are all
covered by `e2e.mjs` (the page is SSR'd for `/shop/products/7`).

## Run

```sh
dune build iso-lab/bin/client.bc.js iso-lab/bin/ssr.exe
dune exec iso-lab/bin/ssr.exe -- \
  _build/default/iso-lab/bin/client.bc.js \
  _build/default/iso-lab/bin/styles \
  iso-lab/index.html \
  /shop/products/7          # request path (which app + route to SSR)
# E2E (needs jsdom; e.g. from a dir where it resolves):
node iso-lab/e2e.mjs
```
