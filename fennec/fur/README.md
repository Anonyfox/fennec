# fennec.fur

**The isomorphic component layer** — signals, JSX (`.mlx`), SSR + hydration, and the reactive
head/data/router trio, in pure OCaml. One component source renders to HTML on the server (Eio) and
hydrates in the browser (js_of_ocaml); the reconciler is written against an abstract backend, so the
engine is unit-tested with no DOM at all.

## A component

```text
let make ?(label = "count") () =
  let count = signal 0 in
  <span className="counter">
    <button onClick={count -= 1}>−</button>
    <span>{label}: {!count}</span>
    <button onClick={count += 1}>+</button>
  </span>
```

That is a complete `.mlx` component: `let`-bindings are setup (run once), the trailing JSX is the
render thunk (re-runs reactively), and the ppx wires the component shape. Signals: `signal v` create ·
`get s` / `!s` read (subscribes) · `set s v` · `update s f` · `s += n` / `s -= n`. Writes are
synchronous (a `get` after `set` sees the new value); re-renders flush in a microtask batch (`batch`,
`flush_sync`, `memo`, `watch`, `on_mount` complete the set). `onClick={expr}` evaluates as a thunk.

## The `.mlx` dialect (JSX with OCaml inside)

Bare text, `{expr}` interpolation, and standard JSX rules — the pre-pass + ppx keep it
HTML/JSX-identical with **zero external toolchain** (the mlx parser is vendored into the `fennec`
binary; `fennec mlx-pp` runs in-process, and a merlin reader keeps the editor exact). The three
desugars beyond stock mlx:

1. **auto-`node`/`frag`** — a `{expr}` child becomes a vnode automatically; a `vnode list` child
   (an `{each …}`) gets an implicit fragment. Idempotent: already-a-vnode passes through.
2. **`!s`** — sugar for `get s`, gated to `.mlx` files only.
3. **bare text is text** — `{expr}` is the *sole* value escape (a literal `(` is prose). The only
   surprises are HTML/JSX's own: whitespace collapses, and a literal `{` needs quoting.

Errors report **column-exact** positions in the original `.mlx` (a `Posmap` remaps locations through
the pre-pass).

## What renders where

- **SSR** (`fennec.fur.server`): `Fur.to_html` walks the vnode tree to a string; a two-pass driver
  seeds `Data` resources so the first paint carries real data. `Fur.raw` injects verbatim markup —
  **server-only**: the client renders it inert (hydration adopts the SSR output as-is; a fresh
  client render shows the source as text — there is deliberately no markup-injection op in the
  browser backend).
- **Hydration** (`fennec.fur.client`): adopts the SSR DOM node-by-node with tag validation — a
  drifted node is rebuilt, never silently patched wrong.
- **Reconciliation** (in `fennec.fur`, the core): keyed diffs move only displaced nodes (LIS
  minimal-move); component re-renders patch in place and track root-shape swaps.
- **Head / Data / Router**: data-driven reactive `<head>` (deepest-wins merge), SSR-seeded typed
  resources (`Data.model` over a Sift codec, `Data.local` co-located fetchers — the server fn is
  stripped from the bundle, a leak is a compile error), and a base-aware isomorphic router.

A `Data` resource is `Loading` until its JSON arrives, then `Ready`; readers serve the `fallback`
until then. A failed fetch keeps the fallback (degrade, never crash) — surfacing fetch errors is a
deliberate future extension of the `set_source` contract.

## Package map

| Lib | Role |
|---|---|
| `fennec.fur` | the engine — signals, vnodes, Head/Data/Matcher/Router, the `Reconcile` functor, SSR `to_html`; `fur.mli` is the contract |
| `fennec.fur.ppx` | the component shape, the three JSX desugars, `[%%style]` extraction hooks, `-fennec-drop-tests` / `-data-client` strip flags |
| `fennec.fur.html` | generated typed HTML elements (per-element labeled attrs) |
| `fennec.fur.platform` (+ `_native`, `_browser`) | the ONE platform split, as a dune **virtual library** — no `if is_browser` in the engine |
| `fennec.fur.server` | the Eio SSR driver + mount dispatch + email rendering |
| `fennec.fur.client` | the jsoo runtime — DOM backend, hydration boot, head sync, client router |
| `fennec.fur.form` | typed client forms bound to a Sift codec (per-field reactive validation, HTML5 attrs emitted from the codec) |
| `fennec.fur.handler` | server handlers componentized (the form-post story) |
| `prepass/` | the `.mlx` pre-pass + vendored mlx parser (see `prepass/vendor/VENDOR.md`) + the merlin reader |
| `tools/` | `route_gen` (app wiring + client mirrors), `style_extract`, `stage` — build-time executables |

The whole package builds under the standard warning set (no `-w -a` anywhere), and the engine's 90
inline tests (signals, matcher, head merge, SSR, data, router, reconcile + hydration over a fake
backend, keyed minimal-move counts) live at the bottom of `fur.ml` — colocated per the repo's
testing ladder; ppx transform tests are golden `.mlx` files under `ppx/test/`.

## Design points

- **React-class by choice**: component-coarse reactivity over a vdom. Solid/Svelte fine-grained is a
  known ceiling, not chased — the bundle floor and the reconciler stay small.
- **Zero platform branching**: the engine is pure; native/browser differences live behind the
  virtual `Platform` module, resolved at link time.
- **The bundle pays only for what it uses**: unused Form/Data/Sift are DCE'd from app bundles
  (verified: an app using none of them ships none of them); inline tests are stripped from client
  mirrors by the ppx flag.
