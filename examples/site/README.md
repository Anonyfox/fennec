# site — the full-scope Fennec example

Two apps on two endpoints, proving the whole framework surface in one project:

- **Paw core** — every concern is a `Paw.t` (`conn -> conn`): the logger, security
  headers, a custom `X-Powered-By` plug, the API route, static serving, the SSR
  app, and (in dev) the livereload websocket.
- **Multiple endpoints** — `web` (`localhost`) and `admin` (`admin.localhost`).
  In **dev** each is reachable on its own port (`:8200`, `:8201`) — no `/etc/hosts`
  or proxy. In **prod** they share port 80 and are selected by **Host pattern**.
- **Universal router + `.App` paw** — a `path -> .mlx page` map mounted on an
  endpoint, with a default SSR layout that's overridable per app (whitelabeling:
  the admin app gets `class="admin"` and its own bundle).
- **Isomorphic** — one `.mlx` component tree is server-rendered, then hydrated by
  the per-app client bundle. The route table (`ui/web_router.ml`) is shared and
  dual-compiled, so server and client agree on `path -> page`.
- **Helmet-like `<Head>`** — pages set their own `<title>`/meta in the tree;
  child-wins precedence, identical on SSR and CSR.
- **Per-app bundles** — `web` and `admin` each assemble their own web root
  (`react.js` + `app.js` + `app.css` + `public/`), while sharing components
  (e.g. `Counter`) by reference.

## Layout

```
server.ml            the operational surface: pipelines, endpoints, serve
ui/                  SHARED app UI (dual-compiled native SSR + melange CSR)
  page_*.mlx         pages (a page is [params -> element])
  *_router.ml        route tables (path -> page), layout-agnostic
  counter.mlx nav.mlx  shared components
ui_native/ ui_js/    the two dune libs that compile ui/ for each target
web_client/  admin_client/   per-app melange entry (main.mlx -> hydrate)
public_web/ public_admin/    per-app static trees
app.scss             shared stylesheet
dune                 per-app bundle assembly + prod embed modules
test/site.test.mjs   isomorphic integration test (SSR + hydrate, both apps)
```

`server.ml` is the whole story:

```ocaml
let common = [ Plug.logger (); Plug.security_headers; powered_by ]

let web =
  Endpoint.make ~host:"localhost" ~port:80 ~dev_port:8200 ()
  |> Endpoint.pipe common
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.plug (Fennec.static ~name:"webroot_web" ~assets:Web_assets.lookup)
  |> Endpoint.app (Router.render web_router)

let () = Fennec.serve ~webroots:[ "webroot_web"; "webroot_admin" ] [ web; admin ]
```

## Run it

```sh
# dev: build everything (server + per-app bundles), then run
dune build examples/site/server.exe @examples/site/all
_build/default/examples/site/server.exe
# web  -> http://localhost:8200
# admin -> http://localhost:8201

# isomorphic test (boots the server, drives the real bundles through jsdom)
npm install --prefix examples/site   # first time (jsdom + preact)
npm test --prefix examples/site

# prod: embed assets into the binary, select endpoints by Host on port 80
dune build --profile release examples/site/server.exe @examples/site/all
FENNEC_ENV=production _build/default/examples/site/server.exe
```

## Note on non-ASCII strings

OCaml string literals are byte strings; Melange does not round-trip raw non-ASCII
bytes as JS Unicode. For any string containing non-ASCII characters (e.g. an em
dash or emoji) use the `{js|…|js}` delimiter — Melange emits it as a proper JS
string and native OCaml treats it as the same UTF-8 bytes, so SSR and CSR agree:

```ocaml
<Head title={js|Home — Fennec Site|js} />
```
