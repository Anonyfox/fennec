(* The site server — TWO endpoints, each its own app on its own domain (dev: its
   own localhost port). The whole operational surface: a shared paw pipeline
   (logger, security headers, a custom plug, ONE shared static web root), an API
   route, and the universal router via [Endpoint.app]. Each app mounts by name —
   the name fixes its predictable asset URLs (/_apps/<name>/main.{js,css}) — with
   its own SSR template (an .mlx component) for whitelabeling. In prod endpoints
   are selected by Host pattern; in dev each gets its own port. *)

module Endpoint = Fennec.Endpoint
module Plug = Fennec.Plug
module Conn = Fennec.Conn
module Router = Fennec_router.Router

(* a custom plug — trivial to write and unit-test: stamp every response *)
let powered_by : Fennec.Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      { r with Fennec.Http.headers = ("X-Powered-By", "fennec") :: r.Fennec.Http.headers })

(* shared pipeline: logging, security headers, the custom plug, and ONE static web
   root (public/ + every app's bundle, assembled together) served to all apps. *)
let common =
  [ Plug.logger (); Plug.security_headers; powered_by;
    Fennec.static ~name:"webroot" ~assets:Assets.lookup ]

let web =
  Endpoint.make ~host:"localhost" ~port:80 ~dev_port:8200 ()
  |> Endpoint.pipe common
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.app
       (Router.render ~name:"default" ~template:Frontend.Templates.Default.make
          Frontend.Apps.Default.Main.router)

let admin =
  Endpoint.make ~host:"admin.localhost" ~port:80 ~dev_port:8201 ()
  |> Endpoint.pipe common
  |> Endpoint.app
       (Router.render ~name:"admin" ~template:Frontend.Templates.Admin.make
          Frontend.Apps.Admin.Main.router)

let () = Fennec.serve ~webroots:[ "webroot" ] [ web; admin ]
