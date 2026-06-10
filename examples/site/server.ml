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
   websocket at /ddp by fennec.pulse.server over the in-memory reactive engine. The browser (Task_list)
   subscribes and renders it live; addTask inserts and the new doc is pushed back through the open
   subscription — server→client push, no refetch.

   The backend is the runtime-selectable Dynamic one: real MongoDB when MONGO_URL is set (the CLI's
   `--mongo` flag launches a managed mongod and sets it), else the in-memory engine. The driver is a
   hard dependency — the dev/test server still runs as fast bytecode: dune builds libmongoc + the C
   stub ONCE, and the dev/test harness puts that stub dir on CAML_LD_LIBRARY_PATH (so the bytecode
   dlopens it), so per-edit only OCaml recompiles — no native, no relink. *)
module D = Fennec_pulse_mongo.Dynamic
module RData = Fennec_pulse.Reactive.Make (D)
module RT = Fennec_pulse_server.Make (RData)

let realtime_ddp = RT.paw ~path:"/ddp" ()

(* runs once in the server's Eio context (serve ~on_start): pick the backend, seed, publish, method.
   [Dynamic.from_env] is the whole backend choice — real mongo when the CLI's --mongo flag exported
   MONGO_URL, else the in-memory engine — no config branch here. *)
let setup_realtime ~sw =
  let backend = D.from_env ~sw ~db:"fennec_example" ~name:"tasks" () in
  let tasks = RData.Collection.create ~name:"tasks" backend in
  List.iter
    (fun t -> ignore (RData.Collection.insert tasks (Bson.doc [ ("title", Bson.str t) ])))
    [ "Buy milk"; "Walk the dog" ];
  RData.publish "tasks" (fun _params -> RData.Cursor (RData.cursor tasks ()));
  (* SSR: hand the same docs to the SSR reactive so the first server-rendered paint already includes
     the tasks; the browser hydrates them flicker-free, then the live subscription re-confirms. *)
  Ddp_client.publish ~name:"tasks" (fun _ -> [ ("tasks", RData.Collection.fetch (RData.Collection.find tasks ())) ]);
  (* the TYPED method: the handler attaches to the SHARED declaration (Site_methods.add_task — the
     same value the browser calls through), so name/args/result can never drift; a malformed call is
     a 400 before this handler runs (the codec is the validation) *)
  RData.handle Site_methods.add_task (fun _inv title ->
      RData.Collection.insert tasks (Bson.doc [ ("title", Bson.str title) ]))

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



