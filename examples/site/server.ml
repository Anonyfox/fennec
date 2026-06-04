(* The site server — TWO endpoints, each its own app on its own domain (dev: its own
   localhost port). The whole operational surface: a shared paw pipeline (logger,
   security headers, a custom paw, ONE shared static web root), an API route, and the
   app's SSR render via [Endpoint.app]. Each endpoint mounts just its own app, whose
   document shell references its predictable asset URLs (/_apps/<name>/main.{js,css}).
   In prod endpoints are selected by Host pattern; in dev each gets its own port.

   The render is [Fur_ssr.handler] — synchronous (no Eio): exactly the (path -> html
   option) shape [Endpoint.app] consumes. It is given ~styles (the inlined component
   [%%style], from Site_styles) and ~source (the in-process data fetcher below), so the
   web app gets server-rendered data + fast-render seeds. The SAME frontend lib is
   compiled to JS via js_of_ocaml for the client (./client). *)

module Endpoint = Fennec.Endpoint
module Paw = Fennec.Paw
module Conn = Fennec.Conn

(* The app's data: ONE place defines each value, used by BOTH the SSR source (in-process,
   for fast-render seeds) and the HTTP route (what the client fetches on refetch / for
   client-only data) — so server render and client agree byte-for-byte. *)
let greeting = "Hello from the server 👋"
let browser_only = "fetched live in the browser 🌐"

let api_source = function
  | "/api/greeting" -> Some greeting
  | "/api/browser-only" -> Some browser_only
  | _ -> None

(* a small on-disk fixture so /api/download can stream a real file (send_file) *)
let download_path =
  let p = Filename.temp_file "fennec_download" ".txt" in
  let oc = open_out p in
  output_string oc "hello from send_file";
  close_out oc;
  p

(* a custom paw — trivial to write and unit-test: stamp every response *)
let powered_by : Fennec.Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      { r with Fennec.Http.headers = ("X-Powered-By", "fennec") :: r.Fennec.Http.headers })

(* shared pipeline: logging, security headers, the custom paw, and ONE static web
   root (public/ + every app's bundle, assembled together) served to all apps. *)
let common =
  [ Paw.Logger.make (); Paw.Security_headers.make (); powered_by;
    Fennec.static ~name:"webroot" ~assets:Assets.lookup ]

let web =
  Endpoint.make ~host:"localhost" ~port:80 ~dev_port:8200 ()
  |> Endpoint.pipe common
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.get "/api/greeting" (fun c -> Conn.text c greeting)
  |> Endpoint.get "/api/browser-only" (fun c -> Conn.text c browser_only)
  (* streaming: a chunked (SSE-style) body and a streamed file download *)
  |> Endpoint.get "/api/stream"
       (fun c -> Conn.send_chunked c (fun emit -> emit "chunk-1"; emit "chunk-2"; emit "chunk-3"))
  |> Endpoint.get "/api/download" (fun c -> Conn.send_file c ~path:download_path ())
  |> Endpoint.app
       (Fur_ssr.handler ~styles:Site_styles.css ~source:api_source
          ~mounts:[ Web_app.Routes.mount ])

let admin =
  Endpoint.make ~host:"admin.localhost" ~port:80 ~dev_port:8201 ()
  |> Endpoint.pipe common
  |> Endpoint.app (Fur_ssr.handler ~styles:Site_styles.css ~mounts:[ Admin_app.Routes.mount ])

(* serve the assembled web root. Livereload is fully handled by the CLI in dev: it
   watches the served bundles and pings the server's dev control socket, which relays
   a CSS hot-swap or full reload to the browser. The server watches nothing. *)
let () = Fennec.serve [ web; admin ]



