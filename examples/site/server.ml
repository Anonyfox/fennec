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

(* runs once in the server's Eio context (serve ~on_start): seed, publish, method. The data layer is
   already booted by Fennec.serve (ambient switch + any mongosh endpoint, all from MONGO_URL), so there
   is no app config branch here. Writes validate against Task.collection (an invalid value cannot reach
   the database); [Pulse.publish] is ONE call that wires both the live DDP publication AND the SSR seed. *)
(* dev mailbox: the pure shape conversion from a sent message to an _fennec_outbox document — what was
   sent (Mail.t) → what the inbox renders (Outbox.t), stamped with a fixed-width UTC time so it sorts. *)
let outbox_doc (m : Fennec.Mail.t) : Outbox.t =
  let stamp =
    let tm = Unix.gmtime (Unix.gettimeofday ()) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  { Outbox.id = "";
    sender = Fennec.Mail.Address.to_header m.from;
    recipients = Fennec.Mail.Address.header_list (m.to_ @ m.cc);
    subject = m.subject;
    text = Option.value m.text ~default:"";
    html = Option.value m.html ~default:"";
    received = stamp }

let setup_realtime () =
  Pulse.seed Task.collection
    [ { Task.id = ""; title = "Buy milk"; body = "" }; { Task.id = ""; title = "Walk the dog"; body = "" } ];
  Pulse.publish Task.collection;
  (* the TYPED method over the TYPED collection: handler and stub share the declarations, so a
     renamed field/method is a compile error in every file; a malformed call is a 400 before this
     handler runs, and an invalid document raises before it writes *)
  Pulse.method_ Site_methods.add_task (fun _inv title -> Pulse.insert Task.collection { Task.id = ""; title; body = "" });
  (* --- dev mailbox (dev only): publish the captured-mail collection the inbox SPA subscribes to, and tee
     every send into it. In production this block is skipped — nothing is captured, published, or retained.
     This is the whole server-side assembly: the framework ships the capture seam ([Mail.set_dev_capture])
     and the reactive collection; the app picks the document shape and the route. --- *)
  if Fennec.is_dev then begin
    Pulse.publish Outbox.collection;
    Fennec.Mail.set_dev_capture (fun m -> ignore (Pulse.insert Outbox.collection (outbox_doc m)))
  end

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

let hello_secret = "hello-handler-demo-secret-key-1234"

(* The app's component styles, declared for BOTH surfaces in one place: the web endpoints below pass
   [~styles:Site_styles.css] (the document <style>), and this installs the SAME rules — var(--brand)
   already resolved at build — for email, so [Fur_email.to_email component] inlines them with nothing
   per render. Two halves of one styling story; the [%%style] blocks are the single source for both. *)
let () = Fur_email.install Site_styles.inline

let web =
  Paw.endpoint ~name:"web" ~hosts:[ "*" ] () (* the default app: catches every host not claimed below *)
  |> Paw.use (Pwa.paw web_pwa ~assets:Assets.lookup ~precache:[ "/_apps/web/main.js"; "/_apps/web/main.css" ])
  |> Paw.use realtime_ddp
  |> Paw.pipe common
  |> Paw.get "/api/health" (fun c -> c |> Paw.json {|{"ok":true,"app":"web"}|})
  |> Paw.get "/api/greeting" (fun c -> c |> Paw.text greeting)
  |> Paw.get "/api/browser-only" (fun c -> c |> Paw.text browser_only)
  (* streaming: a chunked (SSE-style) body and a streamed file download *)
  |> Paw.get "/api/stream"
       (fun c -> Conn.send_chunked c (fun emit -> emit "chunk-1"; emit "chunk-2"; emit "chunk-3"))
  |> Paw.get "/api/download" (fun c -> c |> Paw.send_file ~path:download_path)
  (* the middle layer: a server-rendered FORM HANDLER at /hello (typed form input + validation + CSRF +
     flash, no client JS — see frontend/handlers/hello.mlx). Session + CSRF paws back its flash + token;
     [Paw.form] registers GET+POST to the one ppx-generated [serve] (it dispatches by method). *)
  |> Paw.use (Paw.Session.make ~secret:hello_secret ())
  |> Paw.use (Paw.Csrf.make ~secret:hello_secret ())
  |> Paw.form "/hello" Site_handlers.Hello.serve
  (* standalone HANDLERS — mounted MANUALLY at any path, central-router style. The same handler
     (Site_handlers.Greet.serve, a hydrated SPA with its own bundle) is wired at two paths to show
     reuse: a query-string route and a path-param route. Adding a handler = drop a .mlx + one line here. *)
  |> Paw.get "/greet" Site_handlers.Greet.serve
  |> Paw.get "/hi/:name" Site_handlers.Greet.serve
  (* the dev mailbox — mounted ONLY in dev (Fennec.dev_only); in production these routes do not exist.
     "/dev/send-test-mail" is a demo trigger: hit it and the email shows up at /dev/mailbox live. *)
  |> Fennec.dev_only (fun e ->
         e
         |> Paw.get "/dev/mailbox" Site_handlers.Dev_mailbox.serve
         |> Paw.get "/dev/send-test-mail" (fun c ->
                (* the body is a [%%style] component, rendered for email by Fur_email — authored exactly
                   like a web component, minus the JS. No stylesheet, no tokens: the ambient styles are
                   auto-installed by Site_styles and var(--brand) was resolved at build time. *)
                let html = Fur_email.to_email (Email_welcome.make ~name:"Ada" ~url:"https://fennec.dev/confirm" () ()) in
                let open Fennec.Mail in
                ignore
                  (send
                     (make ~from:(Address.v ~name:"Fennec" "no-reply@fennec.dev")
                        ~to_:[ Address.v ~name:"You" "you@example.com" ] ~subject:"Welcome to Fennec"
                        ~text:"Welcome aboard — confirm your email to get started." ~html ()));
                c |> Paw.text "sent — open /dev/mailbox"))
  |> Paw.app
       (Fur_ssr.handler ~styles:Site_styles.css ~head_extra:(Pwa.head_html web_pwa)
          ~source:api_source ~mounts:[ Web_app.Routes.mount ])

let admin =
  Paw.endpoint ~name:"admin" ~hosts:[ "admin.localhost" ] () (* scoped by host; more specific, so it wins *)
  |> Paw.pipe common
  |> Paw.app (Fur_ssr.handler ~styles:Site_styles.css ~mounts:[ Admin_app.Routes.mount ])
  |> Paw.use_matched (Paw.Basic_auth.make ~username:"admin" ~password:"admin" ~realm:"Admin" ())

(* serve the assembled web root. Livereload is fully handled by the CLI in dev: it
   watches the served bundles and pings the server's dev control socket, which relays
   a CSS hot-swap or full reload to the browser. The server watches nothing. *)
let () = Fennec.serve ~on_start:(fun ~sw:_ ~sleep:_ ~net:_ -> setup_realtime ()) [ web; admin ]
