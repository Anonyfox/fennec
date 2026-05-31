(* The helloworld server — the leanest fullstack fennec app, and our DX taste
   check. A PLAIN dune executable: `dune exec` runs it, `fennec dev` runs it with
   livereload. Nothing here knows about the CLI.

   It server-renders the isomorphic <App> (the SAME app.mlx the client hydrates),
   wraps it in a full HTML document via Fennec_ui.Page, and serves the WEB ROOT —
   one tree holding every bundle (react.js, app.js, app.css) AND the public/ files
   (robots.txt, /img/logo.svg, …) at their paths. The server doesn't know any
   bundle's name; it just serves files. Web root = disk (dev) or embedded (prod). *)

module H = Fennec_core.Http
module App = Fennec_core.App
module Dev = Fennec_core.Dev
module Page = Fennec_ui.Page
module Srv = Fennec_server.Server
module LR = Fennec_server.Livereload
module Static = Fennec_server.Static

let port = 8200
let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* The web root. Dev: the assembled webroot/ dir next to the exe (built by dune,
   live). Prod: the embedded map baked into the binary. One Static path either
   way; the server is identical in both modes. *)
let webroot_dir = Filename.concat (Filename.dirname Sys.executable_name) "webroot"

let web_source =
  if is_dev then Static.Dir webroot_dir else Static.Embedded Webroot_assets.lookup

(* the one prop the page renders with and the client hydrates from *)
let name = "world"
let props_json = Printf.sprintf {|{"name":%S}|} name

let render_page _req =
  (* SSR the SAME component the client mounts *)
  let body_html = ReactDOM.renderToString (App_native.App.make ~name ()) in
  let doc =
    Page.document ~title:"fennec — hello world"
      ~description:"A server-rendered, hydrated fennec app." ~css_href:"/app.css"
      ~scripts:[ "/react.js"; "/app.js" ]
      ~props_json ~dev:is_dev ~body_html ()
  in
  H.html doc

let () =
  Eio_main.run @@ fun env ->
  let lr = LR.create () in
  let app =
    App.create ()
    (* the whole web root (bundles + public) served at its paths, with MIME +
       ETag/304 + Range, before pages/404 *)
    |> App.use_fallthrough (Static.handler web_source)
    |> App.pages [ App.page Routes.nil render_page ]
    |> App.not_found (fun _ -> H.text ~status:404 "not found")
  in
  let on_ws (req : H.request) (ws : Srv.ws) =
    if is_dev && req.H.path = Dev.endpoint then ws.Srv.on_close <- LR.register lr ws.Srv.send
  in
  Eio.Switch.run @@ fun sw ->
  if is_dev then begin
    let clock = Eio.Stdenv.clock env in
    (* Livereload watches the INDIVIDUAL bundle file targets (stable), NOT the
       webroot/ copies — dune wipes a dir target on every rebuild, so a CSS-only
       edit would otherwise bump app.js's mtime and force a full reload. Watching
       the source-side outputs preserves CSS-hot-swap vs JS-reload. *)
    let bundle name = Filename.concat (Filename.dirname Sys.executable_name) name in
    Eio.Fiber.fork ~sw (fun () -> LR.watch lr ~clock ~kind:LR.Css (bundle "app.css"));
    Eio.Fiber.fork ~sw (fun () -> LR.watch lr ~clock ~kind:LR.Reload (bundle "app.js"))
  end;
  Printf.eprintf "[fennec] helloworld on http://localhost:%d%s\n%!" port
    (if is_dev then " (dev: livereload on)" else "");
  Srv.run ~env ~on_ws ~port app
