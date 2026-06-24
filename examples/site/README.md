# site — the full-scope Fennec example

Two apps on two endpoints, fully isomorphic, on **Fennec.Fur** — signals + SSR +
js_of_ocaml hydration. **No Melange, no React, no npm, no hand-written JS.** The same
`.mlx` renders on the server (HTML) and in the browser (DOM, true hydration).

```
examples/site/
  server.ml            endpoints + paw pipeline + serve. SSR = Fur_ssr.handler per app,
                       with ~source (in-process data) + ~styles (inlined component CSS).
  public/              ONE shared static tree, served at / (favicon, robots.txt)
  frontend/            the authored userland — every file is a REAL Dune module (LSP works).
                       100% yours; five plain-named categories. See frontend/README.md.
    apps/<name>/       a self-contained app library (web_app, admin_app):
      main.mlx           config: base + which document shell
      layout.mlx         the app shell (nav + (outlet ()))
      index.mlx          /            (file name = route)
      about.mlx          /about
      products/index.mlx /products    (folders nest)
      products/id_.mlx   /products/:id  (dynamic param — see "Routing")
      rest__.mlx         catch-all / not_found (web app)
      main.css           CSS ORDERING MANIFEST: @layer + @import (web) — or main.scss + @use (admin)
      styles/            CSS escape hatch — drop .css/.scss (a theme, a vendor sheet), @import it
      scripts/           JS escape hatch — drop .js/.ts (a vendor lib); esbuild → loaded before jsoo
    components/        shared single-file components — markup + colocated [%%style] + inline
                       `let%test` (render via Fur.to_html) ALL in one .mlx, impossible to miss.
      marketing/hero.mlx   GROUP INTO SUBFOLDERS FREELY — no dune anywhere under here. The whole
      forms/name_form.mlx  tree folds into one lib, FLAT namespace (<Hero/> wherever hero.mlx sits).
    store/             global state (Store.todos …) — shared signals; nests the same way
    documents/         server-only document shells (default, admin_shell); nests the same way
    handlers/          standalone HANDLERS, one .mlx each, mounted in server.ml — an SPA handler
                       (load/view, own jsoo bundle) or a server-rendered FORM handler (view/submit).
      demo/greet.mlx     also nests freely (Site_handlers.Greet wherever greet.mlx sits)
      account/me.mlx
  _client/             GENERATED — build plumbing, do NOT edit (leading _ = generated, like _build/).
                       Holds the dual-compile mirrors (components, documents, handlers/mirror), the
                       Site_styles extractor (styles/), + the per-app/-handler jsoo bundles, all
                       auto-regenerated. See _client/README.md. frontend/ is now 100% yours.
  test/                the ONE test dir (component unit tests live inline in their .mlx, not here):
    dune                 the site_test_runner backend — scopes inline tests to examples/site/
    http/                Http suites (fennec-hunt);            `fennec test http`
    browser/             Browser suites (headless Chrome/CDP); `fennec test browser`
    system/              System suites — drive real `fennec dev`; `fennec test system`
    lean/                prod-lean guard: server.exe links none of the test machinery
  dune                 build graph: per-app CSS, webroot assembly, prod embed, server exe
```

## How it fits together
- **`frontend/apps/<name>/`** is its own Dune library (`web_app`, `admin_app`). The
  pages/layout/main are real modules, so editor tooling (Merlin/LSP) works and editing
  a page recompiles just that module. `route_gen --glue` reads the file tree and emits
  the wiring — `routes.ml` (router + mount) and `paths.ml` (typed path builders) — without
  ever inlining your source.
- **Escape hatches for non-inline assets.** When you need a vendor sheet/lib or a swappable theme
  rather than inline `[%%style]`: `apps/<name>/main.css` is an ordering manifest — `@layer theme, app;`
  then `@import url("styles/theme.css") layer(theme)` — so you control the cascade and **white-label by
  swapping one `@import`**; Lightning CSS bundles it at build time (or use `main.scss` + `@use`).
  Symmetric for JS: a `apps/<name>/scripts/main.{ts,js}` entry is esbuild-bundled (TS too) and loaded in
  `<head>` before the jsoo app bundle, so its `window` globals are FFI-reachable at hydration.
- **Group into subfolders freely — zero config.** Every authored category (`components`,
  `handlers`, `store`, `documents`) uses `(include_subdirs unqualified)`: drop a file into any
  subfolder, **no dune anywhere under it**, and it folds into the one library with a FLAT module
  namespace — a file's name is its module wherever it sits (`<Hero/>` whether `hero.mlx` is at the
  root or in `marketing/`). The only rule: no two files share a basename across subfolders (dune
  says so plainly if you slip). The `-data-client`/`-conn-client` browser mirror of the nested tree
  regenerates itself (`route_gen --mirror`) into `_client/` — you never touch it.
- **Strict per-app bundle isolation.** Separate app libs mean the web client bundle never
  links the admin app's code (asserted by the browser suite). Each app's client boot is generated
  (`route_gen --boots`), so there are no hand-written entry files.
- **`server.ml`** is pipelines + endpoints only — no HTML strings. Each endpoint mounts one
  app: `Endpoint.app (Fur_ssr.handler ~styles:Site_styles.css ~source:api_source ~mounts:…)`.
  `~source` is an in-process `path -> string option` the SSR driver runs between render
  passes (fast-render); the same values are also served over `/api/*` for the client to fetch.
- **CSS — single-file components.** Each component colocates its styles in a `[%%style]`
  block in the `.mlx`. The ppx stamps a `data-fur` scope hash on that component's elements;
  `style_extract` pulls every block out, scopes its selectors to the same hash, and emits
  `Site_styles.css`, which the server **inlines** into the document `<style>`. Global/base
  rules + design tokens (as CSS custom properties, so components use `var(--brand)`) live in
  the app's ONE external `main.scss` → `fennec build` (grass + Lightning CSS). Admin recolors
  the shared components just by pointing `--brand` at its accent.

## Feature tour (what each demo proves)
| where | feature |
|---|---|
| `<Counter/>` | LOCAL signal state (per-instance), `+=`/`-=`, live after hydration |
| `store/` + `<Todo_list/>` + `<Stats/>` | GLOBAL state — shared signals, reactive across components |
| `<Todo_list/>` | controlled input, `onInput`/`onKeydown` + ambient `target_value`/`key`, `each` with stable `key` |
| `<Greeting/>` | isomorphic data: `Data.string` + fast-render seed (no flash) + `refetch` (real XHR) |
| `<Browser_box/>` | `~client_only` data + `on_mount` (browser-only lifecycle) |
| `<Visits/>` | `Browser.local_get/set` (localStorage), SSR-safe |
| `layout.mlx` | typed links: `p "/about"` (in-app) and `ext "https://…"` (outward) |
| `products/id_.mlx` + `Paths.products_id` | dynamic param + generated typed path builders |
| `rest__.mlx` | catch-all / `not_found` with the unmatched path auto-bound |

## Routing (file-tree, all real modules)
| file | route | notes |
|---|---|---|
| `index.mlx` | `/` | |
| `about.mlx` | `/about` | literal segment = filename |
| `products/index.mlx` | `/products` | folders nest |
| `products/id_.mlx` | `/products/:id` | trailing `_` = **dynamic param** `id` |
| `rest__.mlx` | catch-all | trailing `__` = swallow the rest → `not_found` |

Param names must be valid OCaml module names, so the marker is a **trailing underscore**
(`id_` → `:id`) rather than `[id]`. Inside the page the param is **auto-bound and typed**:
`id_.mlx` gets a `let id : string` injected from the filename — just use `id`. For outgoing
links, `paths.ml` gives compile-checked builders: `Paths.products_id ~id:"7"`.

## Build / run / test
```sh
# dev: from this directory, just run. `fennec dev` discovers the server (the one
# executable that calls Fennec.serve), builds + watches it, supervises restarts, livereloads.
cd examples/site && fennec dev           # gateway on :4000 (host-routed); each scoped app gets the next port
#   fennec dev --dry-run                 # show the discovered server/target, don't run
#   fennec dev --port 9000               # isolated instance on a different port block (parallel worktrees)
#   fennec dev --target … SERVER_EXE     # explicit override (e.g. multi-server repos)

# component unit tests are INLINE in each component .mlx (let%test, render via Fur.to_html) —
# the unit gate runs them (scoped to the app by examples/site/test/dune's site_test_runner backend)
(cd examples/site && fennec test)

# the app's tests — `fennec test` builds once and orchestrates, lock-safe. Run from the app dir.
#   fennec test            # the fast unit gate (delegates to dune runtest)
#   fennec test http       # Http suites in test/http/ (dedicated isolated instance per suite)
#   fennec test browser    # Browser suites in test/browser/ (needs Chrome; CHROME=/path to override)
#   fennec test system     # System suites in test/system/ (drive the real `fennec dev`)
#   fennec test docs       # doc-coverage (warn-only; --strict gates, --promote moves .ml docs to .mli)
#   fennec test all        # unit → http → browser → system → docs (fast-to-slow)
(cd examples/site && fennec test all)

# the System cut replaces the old e2e/*.sh checks with typed, deterministic tests (condition
# waits, never sleep-and-hope; each spawns `fennec dev` and the whole process group is reaped on
# teardown — no orphans). It guards everything the scripts did:
#   - process hygiene  — SIGKILL frees the port, restart binds, a second start reaps the first,
#                        a clean shutdown leaves nothing (test/system/process_test.ml);
#   - port reclaim     — a leftover of OUR server is reclaimed; a foreign holder is named, never
#                        killed (process_test.ml);
#   - host routing     — the dev gateway (:4000) routes by Host exactly as prod, and `--port N`
#                        shifts the whole block (domains_test.ml);
#   - livereload       — an edit reaches BOTH the SSR and the rebuilt no-cache client bundle, and
#                        a revert leaves no stale build (livereload_test.ml);
#   - the error panel  — a build error shows the right count + message; a revert-to-identical fix
#                        clears it (errors_test.ml).
(cd examples/site && fennec test system)

# `fennec dev --clean` is destructive (it wipes the shared _build), so its test is tagged @manual
# and skipped by `fennec test system`. Run it directly when you want to validate --clean:
dune build examples/site/test/system/heal_test.exe
FENNEC_BIN=$(command -v fennec) FENNEC_APP_DIR=$PWD/examples/site FENNEC_ROOT=$PWD \
  _build/default/examples/site/test/system/heal_test.exe

# prod: native server with the web root embedded in the binary
dune build --profile release examples/site/server.exe
```
