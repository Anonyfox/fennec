# site — the full-scope Fennec example

Two apps on two endpoints, with the whole isomorphic surface in **one** place.

```
examples/site/
  server.ml          the operational surface: pipelines, endpoints, serve
  public/            ONE shared static tree (served at /), arbitrary layout
  frontend/          the ENTIRE isomorphic surface — dual-compiled in one go
    apps/
      default/       a web app:  main.mlx + main.scss + pages (home, about)
      admin/         a separate app:  main.mlx + main.scss + dashboard
    components/      shared, nested .mlx + colocated .scss (counter, nav, _theme)
    templates/       SSR document shells as real .mlx (default, admin)
  frontend_build/    generated CSR build machinery (two tiny dunes; ignore — see its README)
  dune               executable + the one `fennec assemble` rule
  test/site.test.mjs isomorphic test (SSR + hydrate, both apps)
```

Three top-level concerns, each with one obvious home:

- **`public/`** — dumb shared bytes, served at `/`. Not compiled, not per-app.
- **`frontend/`** — every `.mlx`/styling thing. The whole tree is compiled twice:
  natively for SSR (`server-reason-react`) and to JS for the client (`melange` +
  `reason-react`). `(include_subdirs qualified)` maps the folders to module paths:
  `apps/default/main.mlx → Frontend.Apps.Default.Main`,
  `components/counter.mlx → Frontend.Components.Counter`,
  `templates/default.mlx → Frontend.Templates.Default`.
- **`server.ml`** — pipelines + endpoints only. No HTML strings, no asset wiring.

## Conventions

- **An app** is a folder under `frontend/apps/` with two entry points:
  `main.mlx` (→ the JS bundle) and `main.scss` (→ the CSS bundle). `main.mlx` is
  *isomorphic*: it builds the route table (SSR reads it) **and** calls
  `Router.hydrate` (a no-op on the server, a real mount on the client) — one file,
  two compiles.
- **`components/`** — shared `.mlx`, arbitrarily nested, with **colocated** styles
  (`counter.mlx` + `counter.scss`). An app's `main.scss` pulls in what it uses via
  `@use` (resolved relative to the file); shared tokens live in `_theme.scss`.
- **`templates/`** — the SSR document shell as a **real `.mlx` component** (never
  shipped to the client). The router renders it with the page's `<Head>` tags
  (spliced as elements), the rendered body (injected raw for hydration), and the
  app's asset URLs. Whitelabel by giving an app a different template.
- **Predictable URLs** — a built blob's URL mirrors its source path minus
  `frontend/`: `frontend/apps/admin/main.scss` → `/_apps/admin/main.css`. The
  shared runtime is `/react.js`. Everything lands in **one** web root
  (`public/` ∪ generated), so the static paw is shared by all endpoints.

`server.ml` is the whole story:

```ocaml
let common =
  [ Plug.logger (); Plug.security_headers; powered_by;
    Fennec.static ~name:"webroot" ~assets:Assets.lookup ]   (* one shared web root *)

let web =
  Endpoint.make ~host:"localhost" ~port:80 ~dev_port:8200 ()
  |> Endpoint.pipe common
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.app
       (Router.render ~name:"default" ~template:Frontend.Templates.Default.make
          Frontend.Apps.Default.Main.router)

let () = Fennec.serve ~webroots:[ "webroot" ] [ web; admin ]
```

`~name:"default"` fixes the app's asset URLs (`/_apps/default/main.{js,css}`), so
the template hardcodes no paths.

## How the build fits together (and why so few dune files)

You edit two dune files: `frontend/dune` (the native SSR lib + shared config) and
the top-level `dune` (the server + one assemble rule). The melange CSR mirror lives
under `frontend_build/` (generated machinery) — the same sources via `copy_files`,
one `(subdir …)` line per app folder (the *only* place adding a brand-new app
touches dune). `fennec assemble`
then **discovers** the apps and, per app, esbuild-bundles its emitted JS and
compiles its `main.scss`, builds the shared `/react.js` once, and merges `public/`
— all into one web root, with no per-app dune wiring.

## Run it

```sh
npm install --prefix examples/site            # first time (preact + jsdom)
dune build examples/site/server.exe @examples/site/all
_build/default/examples/site/server.exe
# web   -> http://localhost:8200
# admin -> http://localhost:8201

npm test --prefix examples/site               # isomorphic test (SSR + hydrate)

# prod: embed the web root into the binary; endpoints select by Host on port 80
dune build --profile release examples/site/server.exe @examples/site/all
FENNEC_ENV=production _build/default/examples/site/server.exe
```

## Non-ASCII strings

OCaml string literals are byte strings; Melange doesn't round-trip raw non-ASCII
bytes as JS Unicode. For any string with non-ASCII characters (em dash, emoji),
use the `{js|…|js}` delimiter — Melange emits a proper JS string and native OCaml
treats it as the same UTF-8 bytes, so SSR and CSR agree:

```ocaml
<Head title={js|Home — Fennec Site|js} />
```
