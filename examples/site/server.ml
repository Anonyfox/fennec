(* The site server — TWO endpoints, each its own app on its own domain (dev: its own
   localhost port). The whole operational surface: a shared paw pipeline (logger,
   security headers, a custom paw, ONE shared static web root), API routes, the app's
   SSR render via [Endpoint.app], and — on the admin endpoint — basic auth in the
   MATCHED phase (so it only fires when a route matched, never on a 404).
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

(* the realtime backend: a published "tasks" collection + an addTask method, served as DDP over a
   websocket at /ddp. The browser (Task_list) subscribes and renders it live; addTask inserts and the
   new doc is pushed back through the open subscription — server→client push, no refetch.

   The whole data surface is the ambient [Fennec_pulse_app] facade ([Pulse] below): it wraps the
   Reactive/server/Typed functors over the runtime-selectable Dynamic backend (real MongoDB for a
   real global Mongo URL — `fennec dev` auto-starts one when mongod is available; `fennec test
   --mongo` supplies one per suite — or the in-memory engine for MONGO_URL=:memory:), so the app
   threads no functors and no backend instances. *)
module Pulse = Fennec_pulse_app

let realtime_ddp = Pulse.serve_ddp ~path:"/ddp" ()

(* runs once in the server's Eio context (serve ~on_start): start the facade, seed, publish, method.
   [Pulse.start] consumes the global Mongo state, so there is no app config branch here. Writes
   validate against Task.collection (an invalid value cannot reach the database); [Pulse.publish] is
   ONE call that wires both the live DDP publication AND the flicker-free SSR seed. *)
let setup_realtime ~sw =
  Pulse.start ~sw ~db:"fennec_example" ();
  Pulse.seed Task.collection
    [ { Task.id = ""; title = "Buy milk"; body = "" }; { Task.id = ""; title = "Walk the dog"; body = "" } ];
  Pulse.publish Task.collection;
  (* the TYPED method over the TYPED collection: handler and stub share the declarations, so a
     renamed field/method is a compile error in every file; a malformed call is a 400 before this
     handler runs, and an invalid document raises before it writes *)
  Pulse.method_ Site_methods.add_task (fun _inv title -> Pulse.insert Task.collection { Task.id = ""; title; body = "" })

(* shared pipeline: logging, security headers, the custom paw, and ONE static web
   root (public/ + every app's bundle, assembled together) served to all apps. *)
(* the web app as an installable PWA: generated manifest + service worker (precise precache of the
   app's own bundle assets; content-addressed cache version → atomic swap per deploy) *)
let web_pwa =
  Pwa.v "Fennec Site" ~theme_color:"#0f172a"
    ~icons:[ Pwa.icon ~sizes:"512x512" "/icon-512.png" ]

let common =
  [ Paw.Logger.make (); Paw.Security_headers.make (); powered_by;
    Fennec.static ~name:"webroot" ~assets:Assets.lookup ]

let web =
  Endpoint.make ~name:"web" ~hosts:[ "*" ] () (* the default app: catches every host not claimed below *)
  |> Endpoint.pipe
       (Pwa.paw web_pwa ~assets:Assets.lookup ~precache:[ "/_apps/web/main.js"; "/_apps/web/main.css" ]
       :: realtime_ddp :: common)
  |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true,"app":"web"}|})
  |> Endpoint.get "/api/greeting" (fun c -> Conn.text c greeting)
  |> Endpoint.get "/api/browser-only" (fun c -> Conn.text c browser_only)
  (* streaming: a chunked (SSE-style) body and a streamed file download *)
  |> Endpoint.get "/api/stream"
       (fun c -> Conn.send_chunked c (fun emit -> emit "chunk-1"; emit "chunk-2"; emit "chunk-3"))
  |> Endpoint.get "/api/download" (fun c -> Conn.send_file c ~path:download_path ())
  |> Endpoint.app
       (Fur_ssr.handler ~styles:Site_styles.css ~head_extra:(Pwa.head_html web_pwa)
          ~source:api_source ~mounts:[ Web_app.Routes.mount ])

let admin =
  Endpoint.make ~name:"admin" ~hosts:[ "admin.localhost" ] () (* scoped by host; more specific, so it wins *)
  |> Endpoint.pipe common
  |> Endpoint.app (Fur_ssr.handler ~styles:Site_styles.css ~mounts:[ Admin_app.Routes.mount ])
  |> Endpoint.pipe_matched [ Paw.Basic_auth.make ~username:"admin" ~password:"admin" ~realm:"Admin" () ]

(* serve the assembled web root. Livereload is fully handled by the CLI in dev: it
   watches the served bundles and pings the server's dev control socket, which relays
   a CSS hot-swap or full reload to the browser. The server watches nothing. *)
let () = Fennec.serve ~on_start:(fun ~sw ~sleep:_ -> setup_realtime ~sw) [ web; admin ]
