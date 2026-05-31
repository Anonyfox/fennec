(* The helloworld server — the leanest possible fennec app, and our DX taste
   check. It is a PLAIN dune executable: `dune exec` runs it, `fennec dev` runs it
   with livereload. Nothing here knows about the CLI.

   Assets (app.css from SCSS, app.js bundled) are produced by dune rules that call
   the `fennec` binary (see dune), so by the time this exe runs they exist under
   _build as plain files. We read them at startup and serve from memory. In dev,
   the framework injects the livereload script and watches the built assets. *)

module H = Fennec_core.Http
module App = Fennec_core.App
module Dev = Fennec_core.Dev
module Srv = Fennec_server.Server
module LR = Fennec_server.Livereload

let port = 8200

(* dev mode unless FENNEC_ENV=production. The CLI sets nothing special; running
   under `fennec dev` vs `dune exec` is the same binary — dev is the default for
   local runs, and a prod build would set FENNEC_ENV=production. *)
let is_dev = try Sys.getenv "FENNEC_ENV" <> "production" with Not_found -> true

(* Built assets live next to the exe under _build. Derive paths from the exe
   location so it works from any cwd. *)
let asset name =
  let dir = Filename.dirname Sys.executable_name in
  Filename.concat dir name

let read path = try In_channel.with_open_bin path In_channel.input_all with _ -> ""

let css_path = asset "app.css"
let js_path = asset "app.js"

let css = ref (read css_path)
let js = ref (read js_path)

let page _req =
  let body =
    Printf.sprintf
      {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>fennec — hello world</title>
<link rel="stylesheet" href="/app.css"/>
</head>
<body>
<main>
<h1>Hello from fennec 🦊</h1>
<p class="muted">Edit <code>page.scss</code> or <code>app.js</code> and watch it live-reload.</p>
<button id="ping">ping</button>
</main>
<script src="/app.js" defer></script>
</body>
</html>|}
  in
  (* in dev, inject the livereload client script (in-memory only) *)
  H.html (if is_dev then Dev.inject_html body else body)

let () =
  Eio_main.run @@ fun env ->
  let lr = LR.create () in
  let serve_css _ =
    H.respond ~content_type:"text/css; charset=utf-8" ~headers:[ ("cache-control", "no-cache") ]
      !css
  in
  let serve_js _ =
    H.respond ~content_type:"application/javascript; charset=utf-8"
      ~headers:[ ("cache-control", "no-cache") ]
      !js
  in
  let app =
    App.create ()
    |> App.get "/app.css" serve_css
    |> App.get "/app.js" serve_js
    |> App.pages [ App.page Routes.nil page ]
    |> App.not_found (fun _ -> H.text ~status:404 "not found")
  in
  (* the livereload websocket: register the browser, hold it open until close *)
  let on_ws (req : H.request) (ws : Srv.ws) =
    if is_dev && req.H.path = Dev.endpoint then begin
      let unregister = LR.register lr ws.Srv.send in
      ws.Srv.on_close <- unregister
    end
  in
  Eio.Switch.run @@ fun sw ->
  if is_dev then begin
    let clock = Eio.Stdenv.clock env in
    (* watch built CSS: on change, reread + hot-swap (no reload) *)
    Eio.Fiber.fork ~sw (fun () ->
        LR.watch lr ~clock ~kind:LR.Css ~on_change:(fun () -> css := read css_path) css_path);
    (* watch built JS: on change, reread + full reload *)
    Eio.Fiber.fork ~sw (fun () ->
        LR.watch lr ~clock ~kind:LR.Reload ~on_change:(fun () -> js := read js_path) js_path)
  end;
  Printf.eprintf "[fennec] helloworld on http://localhost:%d%s\n%!" port
    (if is_dev then " (dev: livereload on)" else "");
  Srv.run ~env ~on_ws ~port app
