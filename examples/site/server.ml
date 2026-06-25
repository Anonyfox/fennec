(* The site server — TWO endpoints, each its own app on its own domain (dev: its own
   localhost port). The whole operational surface: a shared paw pipeline (logger,
   security headers, a custom paw, ONE shared static web root), API routes, the app's
   SSR render via [Endpoint.app], and — on the admin endpoint — basic auth in the
   MATCHED phase (so it only fires when a route matched, never on a 404).
   In prod endpoints are selected by Host pattern; in dev each gets its own port.

   The render is [Fur_ssr.handler] — synchronous (no Eio): exactly the (path -> html
   option) shape [Endpoint.app] consumes. It is given ~styles (the inlined component
   [%%style], from Site_styles) and ~source (the in-process data fetcher below), so the
   main app gets server-rendered data + fast-render seeds. The SAME web lib is
   compiled to JS via js_of_ocaml for the client (./client). *)

(* Paw is the HTTP foundation, re-exported by fennec — used directly, no Fennec.* proxy. *)
module Conn = Paw.Conn (* the conn verbs: before_send, body_param, redirect, send_chunked *)
module Accounts = Fennec.Accounts

(* The app's data. [/api/greeting] is GONE from here — it is now CO-LOCATED in the component
   (web/components/greeting.mlx, via [Data.local]): the value, the SSR seed source AND the refetch
   route all live in that one file, auto-registered by the framework. No api_source arm, no Paw.get here.

   The remaining keys are the OTHER demo shapes: [browser-only] (client-only data, no SSR seed) and the
   typed [site-info] (still wired the classic way to show the additive [Data.model] path coexisting with
   the new co-located [Data.local]/[Data.model_local] one). *)
let browser_only = "fetched live in the browser 🌐"

(* a TYPED isomorphic payload: ONE value, encoded with Site_info.codec for BOTH the SSR seed (api_source
   below) and the /api/site-info HTTP route — so the client's [Data.model Site_info.codec …] decodes the
   exact same bytes the server produced. *)
let site_info : Site_info.t = { name = "Fennec"; tagline = "OCaml, end to end."; stars = 1280 }
let site_info_json = Fennec.Sift.encode_json Site_info.codec site_info

let api_source = function
  | "/api/browser-only" -> Some browser_only
  | "/api/site-info" -> Some site_info_json   (* the typed resource's SSR seed (relaxed JSON) *)
  | _ -> None

(* a small on-disk fixture so /api/download can stream a real file (send_file) *)
let download_path =
  let p = Filename.temp_file "fennec_download" ".txt" in
  let oc = open_out p in
  output_string oc "hello from send_file";
  close_out oc;
  p

(* a custom paw — trivial to write and unit-test: stamp every response *)
let powered_by : Paw.t =
 fun c ->
  Conn.before_send c (fun r ->
      { r with Paw.Http.headers = ("X-Powered-By", "fennec") :: r.Paw.Http.headers })

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

(* a demo user so /me (and the reactive user_badge) work end-to-end the moment the example boots with a
   backend present. Idempotent: on a re-run [create_user] just fails on the duplicate, which we ignore.
   Needs auth ON (~accounts below) and a backend — with MONGO_URL unset and no mongod this no-ops and
   /me simply stays at its /login redirect. NOTE: a demo credential; a real app never ships a fixed one. *)
let seed_demo_user () =
  ignore
    (Accounts.create_user (Accounts.current ()) ~username:"alice" ~email:"alice@example.com"
       ~password:"correct-horse" ())

let setup_realtime () =
  Pulse.seed Task.collection
    [ { Task.id = ""; title = "Buy milk"; body = "" }; { Task.id = ""; title = "Walk the dog"; body = "" } ];
  Pulse.publish Task.collection;
  seed_demo_user ();
  (* the addTask method is now declared in web/methods/add_task.mlx — decl + stub + handler in ONE
     dual-compiled file, registered for free at boot (no Pulse.method_ wiring here anymore). *)
  (* --- the non-web core in action (collections/ + workflows/): [Tickets.open_ticket] is a [@workflow]
     that writes the ticket AND its first audit event in ONE transaction; [close] is a guarded
     transition with an @after effect; [auto_close_stale] is a @cron job. Referencing Tickets here links
     the workflow module so its reaction + schedule register at boot. Seed a couple by just CALLING the
     workflow (only when empty — it reads like a normal function), and publish both collections live. --- *)
  if Pulse.all Ticket.collection = [] then begin
    ignore (Tickets.open_ticket "Printer on fire");
    ignore (Tickets.open_ticket "Coffee machine down")
  end;
  Pulse.publish Ticket.collection;
  Pulse.publish Ticket_event.collection;
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
(* the main app as an installable PWA: generated manifest + service worker (precise precache of the
   app's own bundle assets; content-addressed cache version → atomic swap per deploy) *)
let main_pwa =
  Pwa.v "Fennec Site" ~theme_color:"#0f172a"
    ~icons:[ Pwa.icon ~sizes:"512x512" "/icon-512.png" ]

let common =
  [ Paw.Logger.make (); Paw.Security_headers.make (); powered_by;
    Fennec.static ~name:"webroot" ~assets:Assets.lookup ]

let hello_secret = "hello-handler-demo-secret-key-1234"

(* ── live auth (the one step that makes /me + the reactive user_badge go end-to-end) ──────────────
   Three tiny same-origin routes over the framework-native Accounts session. The GET renders a plain
   login form (no bundle); the POST verifies the password and ATTACHES the signed login cookie to the
   response conn — which a Form `submit` could not do (it returns an outcome, not a conn), so login is
   a raw handler. Logout expires the cookie. CSRF is enforced by the Csrf paw already in the pipeline
   (the form embeds [_csrf_token]); the Session paw backs it. Sign in as alice / correct-horse. *)
let login_page ?(error = false) c =
  let token = Paw.Csrf.token c in
  let err = if error then {|<p style="color:#b91c1c">Wrong username or password.</p>|} else "" in
  c
  |> Paw.html
       (Printf.sprintf
          {|<!doctype html><html lang="en"><head><meta charset="utf-8"><title>Sign in</title></head>
<body style="font:16px system-ui;max-width:22rem;margin:3rem auto">
<h1>Sign in</h1>%s
<form method="post" action="/login">
<input type="hidden" name="_csrf_token" value="%s">
<p><label>Username<br><input name="username" autofocus></label></p>
<p><label>Password<br><input name="password" type="password"></label></p>
<button type="submit">Sign in</button>
</form>
<p style="color:#666">Demo user: <code>alice</code> / <code>correct-horse</code></p>
</body></html>|}
          err token)

let do_login c =
  let username = Option.value (Conn.body_param c "username") ~default:"" in
  let password = Option.value (Conn.body_param c "password") ~default:"" in
  match Accounts.login_with_password (Accounts.current ()) (Accounts.By_username username) ~password with
  | Ok (_user, token) -> Conn.redirect (Accounts.set_login_cookie (Accounts.current ()) c token) "/me"
  | Error _ -> login_page ~error:true c

let auth_routes e =
  e
  |> Paw.get "/login" (fun c -> login_page c)
  |> Paw.post "/login" do_login
  |> Paw.get "/logout" (fun c -> Conn.redirect (Accounts.logout (Accounts.current ()) c) "/")

(* The app's component styles, declared for BOTH surfaces in one place: the app endpoints below pass
   [~styles:Site_styles.css] (the document <style>), and this installs the SAME rules — var(--brand)
   already resolved at build — for email, so [Fur_email.to_email component] inlines them with nothing
   per render. Two halves of one styling story; the [%%style] blocks are the single source for both. *)
let () = Fur_email.install Site_styles.inline

let main =
  Paw.endpoint ~name:"main" ~hosts:[ "*" ] () (* the default app: catches every host not claimed below *)
  |> Paw.use (Pwa.paw main_pwa ~assets:Assets.lookup ~precache:[ "/_apps/main/main.js"; "/_apps/main/main.css" ])
  |> Paw.use realtime_ddp
  |> Paw.pipe common
  |> Paw.get "/api/health" (fun c -> c |> Paw.json {|{"ok":true,"app":"main"}|})
  (* NOTE: no [/api/greeting] route here — it is auto-mounted by the framework from the co-located
     [Data.local] declaration in greeting.mlx (the SSR seed + refetch route both come from that one file). *)
  |> Paw.get "/api/browser-only" (fun c -> c |> Paw.text browser_only)
  (* the typed resource's refetch endpoint: the SAME bytes the SSR seed carries — Site_info.codec encodes
     once, both surfaces serve it, the client's Data.model decodes it back to the typed Site_info.t *)
  |> Paw.get "/api/site-info" (fun c -> c |> Paw.json site_info_json)
  (* streaming: a chunked (SSE-style) body and a streamed file download *)
  |> Paw.get "/api/stream"
       (fun c -> Conn.send_chunked c (fun emit -> emit "chunk-1"; emit "chunk-2"; emit "chunk-3"))
  |> Paw.get "/api/download" (fun c -> c |> Paw.send_file ~path:download_path)
  (* the middle layer: a server-rendered FORM HANDLER at /hello (typed form input + validation + CSRF +
     flash, no client JS — see web/handlers/hello.mlx). Session + CSRF paws back its flash + token;
     [Paw.form] registers GET+POST to the one ppx-generated [serve] (it dispatches by method). *)
  |> Paw.use (Paw.Session.make ~secret:hello_secret ())
  |> Paw.use (Paw.Csrf.make ~secret:hello_secret ())
  |> Paw.form "/hello" Site_handlers.Hello.serve
  (* live auth: /login (GET form + POST verify) and /logout, over the native Accounts session *)
  |> auth_routes
  (* standalone HANDLERS — mounted MANUALLY at any path, central-router style. The same handler
     (Site_handlers.Greet.serve, a hydrated SPA with its own bundle) is wired at two paths to show
     reuse: a query-string route and a path-param route. Adding a handler = drop a .mlx + one line here. *)
  |> Paw.get "/greet" Site_handlers.Greet.serve
  |> Paw.get "/hi/:name" Site_handlers.Greet.serve
  (* Mode B — a PERSONALIZED, authenticated server handler: [load] reads the userId from the conn,
     renders the user's dashboard server-side, and seeds that payload so the client hydrates already
     personalized (no snap). Anonymous ⇒ it redirects to /login. See web/handlers/me.mlx. *)
  |> Paw.get "/me" Site_handlers.Me.serve
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
       (Fur_ssr.handler ~styles:Site_styles.css ~head_extra:(Pwa.head_html main_pwa)
          ~source:api_source ~mounts:[ Main_app.Routes.mount ])

let admin =
  Paw.endpoint ~name:"admin" ~hosts:[ "admin.localhost" ] () (* scoped by host; more specific, so it wins *)
  |> Paw.pipe common
  |> Paw.app (Fur_ssr.handler ~styles:Site_styles.css ~mounts:[ Admin_app.Routes.mount ])
  |> Paw.use_matched (Paw.Basic_auth.make ~username:"admin" ~password:"admin" ~realm:"Admin" ())

(* serve the assembled web root. Livereload is fully handled by the CLI in dev: it
   watches the served bundles and pings the server's dev control socket, which relays
   a CSS hot-swap or full reload to the browser. The server watches nothing. *)
(* ~accounts turns the framework-native identity ON (password login + sessions + the always-on
   __currentUser publication that drives the reactive user_badge) so /me and the badge go live. With
   no backend (MONGO_URL unset, no mongod) identity is simply None and /me redirects to /login. *)
let () =
  Fennec.serve ~accounts:Accounts.defaults
    ~on_start:(fun ~sw:_ ~sleep:_ ~net:_ -> setup_realtime ())
    [ main; admin ]
