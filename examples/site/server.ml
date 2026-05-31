(* The site server — TWO endpoints, each its own app on its own domain (dev: its
   own localhost port). The whole operational surface: a shared paw pipeline
   (logger, security headers, a custom plug), an API route, static serving, and
   the universal router via [Endpoint.app]. In prod endpoints are selected by Host
   pattern; in dev each gets its own port (no /etc/hosts). [Fennec.serve] owns
   Eio + dev livereload. *)

module Endpoint = Fennec.Endpoint
module Plug = Fennec.Plug
module Conn = Fennec.Conn
module Router = Fennec_router.Router

(* per-app SSR document shells (whitelabel via a body class + a bundle name) *)
let layout ~theme ~head_html ~body_html =
  Printf.sprintf
    {|<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"/><meta name="viewport" content="width=device-width, initial-scale=1"/>%s<link rel="stylesheet" href="/app.css"/></head><body class="%s"><div id="root">%s</div><script src="/react.js" defer></script><script src="/app.js" defer></script></body></html>|}
    head_html theme body_html

let web_router = Site_ui_native.Web_router.routes (Router.make ~layout:(layout ~theme:"web") ())
let admin_router = Site_ui_native.Admin_router.routes (Router.make ~layout:(layout ~theme:"admin") ())

(* a custom plug — trivial to write and unit-test: stamp every response *)
let powered_by : Fennec.Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      { r with Fennec.Http.headers = ("X-Powered-By", "fennec") :: r.Fennec.Http.headers })

let common = [ Plug.logger (); Plug.security_headers; powered_by ]

let web =
  Endpoint.make ~host:"localhost" ~port:80 ~dev_port:8200 ()
  |> Endpoint.pipe common
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.plug (Fennec.static ~name:"webroot_web" ~assets:Web_assets.lookup)
  |> Endpoint.app (Router.render web_router)

let admin =
  Endpoint.make ~host:"admin.localhost" ~port:80 ~dev_port:8201 ()
  |> Endpoint.pipe common
  |> Endpoint.plug (Fennec.static ~name:"webroot_admin" ~assets:Admin_assets.lookup)
  |> Endpoint.app (Router.render admin_router)

let () = Fennec.serve ~webroots:[ "webroot_web"; "webroot_admin" ] [ web; admin ]
