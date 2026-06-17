(* Fennec — the userland facade over the Paw/Endpoint/Server core. It owns the
   operational plumbing that's identical in every app: the Eio entry point, the
   dev/prod web-root flip (disk vs embedded), dev livereload (the websocket paw +
   a dev control socket the CLI pings), and starting the server. Userland writes
   endpoints as paw pipelines and hands them to [serve].

   Re-exports the pieces an app needs (Endpoint, Conn, Http, …) so a userland file
   opens one module. The prebuilt batteries live under {!Paw} as submodules
   ([Paw.Logger], [Paw.Session], [Paw.Csrf], …), each a [make] returning a paw. *)

module Conn = Paw.Conn
module Endpoint = Paw.Endpoint
module Tls = Paw.Tls_termination (* in-process HTTPS termination: load a cert+key, pass to serve ~tls *)
module Cert_store = Paw.Cert_store (* pluggable ACME cert storage: file (default) / memory / custom *)
module Acme = Paw.Acme (* automatic HTTPS (Let's Encrypt): serve ~acme:(Acme.auto ~email ()) *)
module Livereload = Paw.Livereload
module Http = Paw.Http
module Cookie = Paw.Cookie
module Dev_proto = Paw.Dev_proto (* the CLI<->server dev wire (env names, stderr line formats) *)
module Accounts = Fennec_accounts.Accounts
module Mail = Fennec_mail (* outbound email: one MAIL_URL knob (unset ⇒ logged to stdout in dev) *)

(* The presentation layer — Fur. ONE namespace ([open Fennec.Fur]) for everything that turns data
   into what a client sees: components & signals (live SPA, isomorphic core), standalone [Page]s
   (an isomorphic view + a server conn block + the page's own jsoo bundle), and server-rendered HTML
   via [Handler]. Typed HTTP input is [Form]/[Action]; JSON APIs are hand-built with [Respond].
   Includes the isomorphic Fur core, so [h]/[text]/[signal]/[document] are all here too. *)
module Sift = Sift (* the shape language — for hand-written codecs + the resource/form signatures *)

module Fur = struct
  include Fur (* core: h, text, frag, node, attr, class_, on, document, to_html, signal, get, set, … *)

  (* Standalone HANDLERS (frontend/handlers/*.mlx — view + load + own bundle) are authored as files and
     wired by the fur ppx + route_gen, not via a facade module: the generated server `serve` references
     the {!Fennec_fur_handler.Handler} runtime + {!Conn} directly. So nothing for them here. *)
  module Handler = Fennec_web.Handler (* render a component to a static HTML response + redirect/flash/csrf *)
  module Form = Fennec_web.Form (* typed form/query INPUT over the Sift model *)
  module Action = Fennec_web.Action (* typed path/query scalars + JSON-body decode *)
  module Respond = Fennec_web.Respond (* JSON output building blocks (hand-built APIs) *)
end

(* The whole fennec-paw library, under one name: the pipeline primitive + algebra, the flat endpoint
   builder ([Paw.endpoint () |> Paw.use … |> Paw.get …]), the conn-pipe response verbs, and every
   prebuilt battery ([Paw.Logger], [Paw.Session], …) — each [make] returns a plain [Paw.t]. *)
module Paw = Paw

type request_error = Paw.Server.request_error =
  | Handler_exception of exn * Http.request
  | Handler_timeout of Http.request
  | No_route of Http.request
  | Method_not_allowed of Http.request * Http.meth list

let is_dev = try Sys.getenv Dev_proto.env_mode <> "production" with Not_found -> true

(* Apply an endpoint transform only in dev (see [is_dev]); in production the endpoint passes through
   untouched, so any routes the transform would add simply do not exist. The clean dev-only mount. *)
let dev_only f e = if is_dev then f e else e

(* Structured-concurrency helpers for handlers. A handler runs inside an Eio fiber, so it
   can fan out concurrent work (parallel DB queries / HTTP calls): the sub-fibers overlap
   their waits, and if the request's deadline fires or the client goes away, the whole tree
   is cancelled together. No threads, no manual cancellation tokens. *)

(* run thunks concurrently, returning their results in order *)
let parallel (thunks : (unit -> 'a) list) : 'a list =
  let out = Array.make (List.length thunks) None in
  Eio.Fiber.all (List.mapi (fun i t () -> out.(i) <- Some (t ())) thunks);
  Array.to_list out |> List.map Option.get

(* run two thunks (of different types) concurrently *)
let both (f : unit -> 'a) (g : unit -> 'b) : 'a * 'b =
  let a = ref None and b = ref None in
  Eio.Fiber.both (fun () -> a := Some (f ())) (fun () -> b := Some (g ()));
  (Option.get !a, Option.get !b)

(* A web root for an app: dev reads the assembled webroot/ dir next to the exe
   (the per-app dune assembly), prod serves the embedded map. [name] disambiguates
   per-app dev webroots ("webroot_web", "webroot_admin"). *)
let web_source ~name ~assets : Paw.Static.source =
  if is_dev then Paw.Static.Dir (Filename.concat (Filename.dirname Sys.executable_name) name)
  else Paw.Static.Embedded (name, assets)

(* the static-serving paw for an app's web root. In DEV every asset is served [no-cache]
   (the browser still caches, but always revalidates via the strong ETag, so a 304 when
   unchanged is ~free) — otherwise a reloaded page would hydrate with a STALE cached bundle and
   an edit would never show. In PROD the source is embedded and immutable, so Static's
   content-aware default applies (HTML revalidates, other assets get a max-age). *)
let static ~name ~assets : Paw.t =
  let cache_control = if is_dev then Some "no-cache" else None in
  Paw.Static.make ?cache_control (web_source ~name ~assets)

(* The dev control socket. The CLI owns ALL filesystem watching (it's the one
   process that links the native fs-event watcher); the framework watches nothing.
   When a served asset's content changes, the CLI connects to this unix-domain
   socket — its path is handed in via [FENNEC_LIVERELOAD] — and sends one line:
   "css" (stylesheet hot-swap) or anything else (full reload). We relay that to
   every connected browser. Loopback by nature (a unix socket), dev-only, and
   absent in prod. [reuse_addr] clears a stale socket from a prior run; Eio removes
   the path again when the switch finishes. *)
let dev_control ~sw ~net (lr : Livereload.t) : unit =
  match Sys.getenv_opt Dev_proto.env_livereload with
  | None | Some "" -> ()
  | Some path -> (
    match try Some (Eio.Net.listen ~sw ~backlog:8 ~reuse_addr:true net (`Unix path)) with _ -> None with
    | None -> () (* couldn't bind (e.g. path too long): CSS hot-swap is lost, but
                    backend reload via the reconnect loop still works *)
    | Some socket ->
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Net.run_server socket
            (fun flow _addr ->
              let r = Eio.Buf_read.of_flow flow ~max_size:64 in
              match try Some (String.trim (Eio.Buf_read.line r)) with _ -> None with
              | Some "css" -> Livereload.broadcast lr "css"
              | Some _ -> Livereload.broadcast lr "reload"
              | None -> ())
            ~on_error:(fun _ -> ())))

(* Serve a list of endpoints, blocking. In dev, a livereload paw is prepended to
   every endpoint and a dev control socket is opened for the CLI to ping on a
   frontend edit (the framework itself watches nothing). Owns Eio + the lifecycle. *)
(* exactly one place starts the server. Many modules may LINK fennec, but a second [serve]
   call (a stray entrypoint in a library, a copy-pasted main) is a bug — fail loudly rather
   than half-start a second server. This is the runtime counterpart to the CLI's discovery,
   which finds the single [serve] site. *)
let started = Atomic.make false

let serve ?(timeout = 30.0) ?(max_conns = 10_000) ?tls ?acme ?on_error ?on_start
    (endpoints : Endpoint.t list) : unit =
  if not (Atomic.compare_and_set started false true) then
    failwith "Fennec.serve: a server is already running in this process — start the server in exactly one place";
  Eio_main.run @@ fun env ->
  let lr = Livereload.create () in
  (* Livereload is a dev convenience; it reloads the page on a frontend edit. For an e2e or
     any controlled run it is pure nondeterminism (spontaneous navigations), so it can be
     turned off while still serving the dev (on-disk) web root: set FENNEC_DEV_LIVERELOAD=0. *)
  let livereload_on =
    is_dev && (match Sys.getenv_opt Dev_proto.env_dev_livereload with Some ("0" | "off" | "false" | "no") -> false | _ -> true)
  in
  let endpoints =
    List.map (fun e -> Endpoint.prepend (Accounts.native_paw ()) e) endpoints
  in
  let endpoints =
    if livereload_on then List.map (fun e -> Endpoint.prepend (Livereload.paw lr) e) endpoints
    else endpoints
  in
  Eio.Switch.run @@ fun sw ->
  if livereload_on then dev_control ~sw ~net:(Eio.Stdenv.net env) lr;
  (* Boot the data layer inside the long-lived switch, BEFORE any endpoint is served: install the ambient
     Eio switch (so app + accounts collections open by name — no [sw] threading) and, when MONGO_URL is a
     burrow:// URL with an authority, front the embedded engine over the MongoDB wire protocol so `mongosh`
     connects (zero-config in dev). MONGO_URL alone decides the backend; there is no app-level "start". *)
  Fennec_mongo_dynamic.boot ~sw ~net:(Eio.Stdenv.net env) ();
  (* outbound email transport from MAIL_URL (smtp:// / smtps:// / unset ⇒ dev log), booted with the
     switch's net so SMTP submission runs on the server's Eio loop *)
  Fennec_mail.boot ~sw ~net:(Eio.Stdenv.net env) ();
  (* eager accounts: build the (memoized) store now — inside the switch, after the ambient switch is
     installed — so the engine opens + indexes are ensured at boot, not on the first authenticated
     request. Opt-in is preserved: no MONGO_URL ⇒ the no-op store, and a request with no session cookie
     resolves to [user_id = None] without touching the database. *)
  Accounts.boot ();
  (* never outlive the dev supervisor: if [fennec dev] dies (even by SIGKILL, which it can't
     clean up after) we'd otherwise keep the port and make the next `fennec dev` fail to bind.
     The supervisor (our direct parent) passes its pid as FENNEC_DEV_PARENT; we exit the moment
     getppid() stops matching it. When the supervisor dies we are reparented, so getppid changes
     to WHATEVER reaper takes us (init, a subreaper, a shell) — this is robust regardless of the
     reparent target, AND immune to pid recycling (getppid is our real current parent; a recycled
     pid elsewhere is not it). Independent of livereload, so an e2e run self-exits too. Cheap: a
     0.25s poll on a background fiber. *)
  (match Option.bind (Sys.getenv_opt Dev_proto.env_dev_parent) int_of_string_opt with
  | Some parent ->
    Eio.Fiber.fork ~sw (fun () ->
        let clock = Eio.Stdenv.clock env in
        let rec watch () =
          Eio.Time.sleep clock 0.25;
          if Unix.getppid () = parent then watch ()
          else (Printf.eprintf "%s dev supervisor gone — exiting\n%!" Dev_proto.chatter_prefix; exit 0)
        in
        watch ())
  | None -> ());
  (* app startup hook — runs once in the server's Eio context (the long-lived switch + a clock-backed
     sleep), after the switch is live and BEFORE any connection is served. This is where an app
     creates resources that need the runtime — e.g. a real-mongo backend's collections and their
     observe loops, which fork into [sw] and live for the server's lifetime. *)
  (* TLS source the server reads per connection. ~acme runs the ACME lifecycle here (before the
     server binds): it forks the :80 HTTP-01 listener and blocks on the initial issue (or loads a
     cached cert), so :443 has a cert before its first connection; the renewal loop then hot-reloads
     [cert_ref]. ~tls (BYO cert) is a constant source. The certifiable domains are the router's
     concrete (Exact) hosts unless overridden — wildcards/catch-all are reported by Acme.run. *)
  let challenges : (string, string) Hashtbl.t = Hashtbl.create 8 in
  (* guards the shared challenge table + the issuer's cert tables against concurrent access from the
     :80 front and worker-domain on-demand issuance (the per-connection source itself is lock-free). *)
  let tls_lock = Mutex.create () in
  (* ACME issues REAL certificates, so it runs only in production (FENNEC_ENV=production); FENNEC_ACME=1/0
     force on/off. In dev, ~acme no-ops (plain HTTP) — a dev build never touches Let's Encrypt. ~tls (a
     BYO cert) always applies, dev or prod. *)
  let acme_active =
    match (acme, Sys.getenv_opt "FENNEC_ACME") with
    | _, Some ("0" | "off" | "false" | "no") -> false
    | Some _, Some ("1" | "on" | "true" | "yes") -> true
    | Some _, _ -> not is_dev (* ~acme given, FENNEC_ACME unset/unrecognized → production only *)
    | None, _ -> false
  in
  let tls_source, on_demand =
    if acme_active then (
      let cfg = Option.get acme in
      (* certifiable domains from the router: concrete hosts always (HTTP-01); wildcards too (a
         [Suffix] ".app.com" → "*.app.com") when a DNS provider is configured (DNS-01) *)
      let derived =
        List.concat_map
          (fun e ->
            List.filter_map
              (fun h ->
                match Paw.Host_pattern.of_string h with
                | Ok (Paw.Host_pattern.Exact d) -> Some d
                | Ok (Paw.Host_pattern.Suffix s) when Paw.Acme.dns_enabled cfg -> Some ("*" ^ s)
                | _ -> None)
              (Endpoint.hosts e))
          endpoints
        |> List.sort_uniq compare
      in
      let domains = match Paw.Acme.domains_override cfg with Some d -> d | None -> derived in
      let r = Paw.Acme.run ~sw ~clock:(Eio.Stdenv.clock env) ~net:(Eio.Stdenv.net env) ~lock:tls_lock ~domains ~challenges cfg in
      (Some r.Paw.Acme.source, r.Paw.Acme.on_demand))
    else match tls with Some t -> (Some (fun () -> Some t), None) | None -> (None, None)
  in
  (* opt-in zero-config local HTTPS: in dev with no BYO/ACME cert, FENNEC_DEV_TLS=1 terminates TLS on
     the dev port with a throwaway self-signed cert for localhost (+ the endpoints' Exact hosts), so a
     dev can exercise HTTPS-only behaviour (Secure cookies, HSTS, redirects) locally with no cert to
     manage. Built once, presented as a constant source. Never engaged in production. *)
  let tls_source =
    match tls_source with
    | Some _ -> tls_source
    | None when is_dev && (match Sys.getenv_opt "FENNEC_DEV_TLS" with Some ("1" | "on" | "true" | "yes") -> true | _ -> false) ->
      let hosts =
        "localhost"
        :: List.concat_map (fun e -> List.filter_map (fun h -> match Paw.Host_pattern.of_string h with Ok (Paw.Host_pattern.Exact d) -> Some d | _ -> None) (Endpoint.hosts e)) endpoints
        |> List.sort_uniq compare
      in
      let cfg = Paw.Tls_termination.self_signed ~hosts () in
      Some (fun () -> Some cfg)
    | None -> None
  in
  (* in TLS-mode production the app is on :443; a :80 front does the HTTP→HTTPS redirect (+ serves
     the ACME challenge from the shared table). Dev keeps a single plain/forced port — no :80. *)
  if Option.is_some tls_source && not is_dev then Paw.Acme.serve_http_front ~sw ~net:(Eio.Stdenv.net env) ~lock:tls_lock ~challenges;
  (match on_start with Some f -> f ~sw ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env)) ~net:(Eio.Stdenv.net env) | None -> ());
  (* announce only AFTER the server actually binds (Server.run calls [on_listen] post-listen) with
     the (endpoint name, url) pairs it allocated — a failed bind never prints a misleading "ready"
     line first. The dev supervisor owns the terminal: report named URLs for its banner, else stay quiet. *)
  let announce (named : (string * string) list) =
    match (if is_dev then Sys.getenv_opt Dev_proto.env_dev_ui else None) with
    | Some ("1" | "on" | "true") -> Printf.eprintf "%s\n%!" (Dev_proto.urls_line named)
    | _ -> Printf.eprintf "%s serving %d endpoint(s)%s\n%!" Dev_proto.chatter_prefix (List.length named) (if livereload_on then " (dev: livereload on)" else "")
  in
  (* fail at boot on an ambiguous route table — a (method, exact path) declared twice — rather than
     silently shadowing the second handler at runtime *)
  (match List.concat_map Endpoint.conflicts endpoints with [] -> () | cs -> List.iter (fun c -> Printf.eprintf "fennec: %s\n%!" c) cs; exit 1);
  (* the routing table is the single source of truth for which domains we answer. Build it from the
     endpoints (name + host patterns); an invalid config (clashing domains, two catch-alls, a bad
     pattern, …) fails loudly here rather than mis-routing at runtime. *)
  match Paw.Host_router.build (List.map (fun e -> (Endpoint.name e, Endpoint.hosts e, e)) endpoints) with
  | Error errs ->
    Printf.eprintf "fennec: invalid endpoint configuration —\n%s\n%!" (Paw.Host_router.describe_errors errs);
    exit 1
  | Ok router -> (
    match Paw.Server.run ~timeout ~max_conns ?tls:tls_source ?on_demand ?on_error ~dev:is_dev ~on_listen:announce ~env router with
    | Ok () -> ()
    | Error (`Port_in_use port) ->
      Printf.eprintf "%s\n%!" (Dev_proto.port_busy_line port);
      exit Dev_proto.port_in_use_exit
    | Error (`Bad_plan msg) ->
      Printf.eprintf "fennec: %s\n%!" msg;
      exit 1)
