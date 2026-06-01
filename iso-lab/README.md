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

## Run

```sh
dune build iso-lab/bin/client.bc.js iso-lab/bin/ssr.exe
dune exec iso-lab/bin/ssr.exe -- \
  _build/default/iso-lab/bin/client.bc.js \
  _build/default/iso-lab/bin/styles \
  iso-lab/index.html
# E2E (needs jsdom; e.g. from a dir where it resolves):
node iso-lab/e2e.mjs
```
