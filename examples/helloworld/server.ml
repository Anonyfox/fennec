(* The helloworld server — the leanest fullstack fennec app, and our DX taste
   check. A PLAIN dune executable: `dune exec` runs it, `fennec dev` runs it with
   livereload. Nothing here knows about the CLI.

   It server-renders the isomorphic <App> (the SAME app.mlx the client hydrates),
   wraps it in a full HTML document via Fennec_ui.Page (which inlines the props
   the client reads), and serves the three built bundles (react.js, app.js,
   app.css) that dune produced next to this exe. In dev, the framework injects the
   livereload script and watches the built assets. *)

module H = Fennec_core.Http
module App = Fennec_core.App
module Dev = Fennec_core.Dev
module Page = Fennec_ui.Page
module Srv = Fennec_server.Server
module LR = Fennec_server.Livereload
module Static = Fennec_server.Static

let port = 8200
let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* static public/ assets: read from disk in dev (live edits), serve the embedded
   map in prod (single self-contained binary). The dev disk path is resolved from
   the exe location up to the source tree. *)
let public_source =
  if is_dev then
    let rec up n p = if n = 0 then p else up (n - 1) (Filename.dirname p) in
    (* .../_build/default/examples/helloworld/server.exe -> repo root (strip the
       filename + 4 dirs), then the source public/ dir *)
    let root = up 5 Sys.executable_name in
    Static.Dir (Filename.concat root "examples/helloworld/public")
  else Static.Embedded Public_assets.lookup

(* built assets sit next to the exe under _build *)
let asset name = Filename.concat (Filename.dirname Sys.executable_name) name
let read path = try In_channel.with_open_bin path In_channel.input_all with _ -> ""

let react_path = asset "react.js"
let js_path = asset "app.js"
let css_path = asset "app.css"
let react_js = ref (read react_path)
let app_js = ref (read js_path)
let css = ref (read css_path)

(* the one prop the page renders with and the client hydrates from *)
let name = "world"
let props_json = Printf.sprintf {|{"name":%S}|} name

let render_page _req =
  (* SSR the SAME component the client mounts *)
  let body_html = ReactDOM.renderToString (App_native.App.make ~name ()) in
  let doc =
    Page.document ~title:"fennec — hello world"
      ~description:"A server-rendered, hydrated fennec app."
      ~css_href:"/app.css"
      ~scripts:[ "/react.js"; "/app.js" ]
      ~props_json ~dev:is_dev ~body_html ()
  in
  H.html doc

let () =
  Eio_main.run @@ fun env ->
  let lr = LR.create () in
  let serve ct contents _ =
    H.respond ~content_type:ct ~headers:[ ("cache-control", "no-cache") ] !contents
  in
  let app =
    App.create ()
    |> App.get "/react.js" (serve "application/javascript; charset=utf-8" react_js)
    |> App.get "/app.js" (serve "application/javascript; charset=utf-8" app_js)
    |> App.get "/app.css" (serve "text/css; charset=utf-8" css)
    (* static public/ files (robots.txt, /img/logo.svg, …) served verbatim at
       their paths, with MIME + ETag/304 + Range, before pages/404 *)
    |> App.use_fallthrough (Static.handler public_source)
    |> App.pages [ App.page Routes.nil render_page ]
    |> App.not_found (fun _ -> H.text ~status:404 "not found")
  in
  let on_ws (req : H.request) (ws : Srv.ws) =
    if is_dev && req.H.path = Dev.endpoint then ws.Srv.on_close <- LR.register lr ws.Srv.send
  in
  Eio.Switch.run @@ fun sw ->
  if is_dev then begin
    let clock = Eio.Stdenv.clock env in
    (* watch built assets (outputs, not source); CSS hot-swaps, JS triggers reload *)
    Eio.Fiber.fork ~sw (fun () ->
        LR.watch lr ~clock ~kind:LR.Css ~on_change:(fun () -> css := read css_path) css_path);
    Eio.Fiber.fork ~sw (fun () ->
        LR.watch lr ~clock ~kind:LR.Reload ~on_change:(fun () -> app_js := read js_path) js_path)
  end;
  Printf.eprintf "[fennec] helloworld on http://localhost:%d%s\n%!" port
    (if is_dev then " (dev: livereload on)" else "");
  Srv.run ~env ~on_ws ~port app
