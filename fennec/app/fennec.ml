(* Fennec — the userland facade over the Paw/Endpoint/Server core. It owns the
   operational plumbing that's identical in every app: the Eio entry point, the
   dev/prod web-root flip (disk vs embedded), dev livereload (the websocket paw +
   a dev control socket the CLI pings), and starting the server. Userland writes
   endpoints as paw pipelines and hands them to [serve].

   Paw is the HTTP foundation Fennec builds on, and Fennec's dune (re_export)s it: an app
   names [Paw.Conn]/[Paw.Endpoint]/[Paw.Http]/[Paw.Logger]/… DIRECTLY (open Paw), with no
   pass-through proxy. Here we just [open Paw] for our own plumbing below (Conn/Endpoint/Http/
   Static/Livereload/Dev_proto/Server/Acme are all Paw submodules). *)

open Paw

module Tls = Paw.Tls_termination (* in-process HTTPS termination: load a cert+key, pass to serve ~tls *)
module Cert_store = Paw.Cert_store (* pluggable ACME cert storage: file (default) / memory / custom *)
module Acme = Paw.Acme (* automatic HTTPS (Let's Encrypt): serve ~acme:(Acme.auto ~email ()) *)
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

(* ── CO-LOCATED data: auto-mount the refetch routes ────────────────────────────────────────────
   A component that declares [Fur.Data.local key ~fallback (Server_only.fn fetch)] registers [fetch] in
   the [Fur.Data] process registry. The SSR driver already drains that registry for the fast-render SEED
   (Fur_ssr.handler chains it). The OTHER half of the old three-place split is the HTTP refetch route —
   and THIS is where it gets mounted, with zero server.ml wiring: [serve] prepends ONE paw per endpoint
   that, on a GET, looks up the request path in the registry and answers with the SAME bytes the seed
   carries (text/plain for a string [local], application/json for a typed [model_local]); any other path
   it DECLINES (falls through to the app's own routes / SSR). Prepended (not appended) so a data path is
   matched BEFORE an SSR app's catch-all would render it as a page.

   The lookup is DYNAMIC (per request) by design: a co-located resource registers when its component's
   SETUP first runs — i.e. on the FIRST SSR render of any page that mounts it. In the real lifecycle the
   page is server-rendered (registering the source) strictly before the client bundle can refetch, so the
   route is always live by the time a refetch arrives. A direct API hit before ANY page render of that
   component sees a 404 until the first render registers it — the one ordering caveat, not a concern for
   the page→hydrate→refetch flow. Mounted on EVERY endpoint, exactly as a Pulse publication is
   server-wide. With no co-located resources the registry is empty and this paw always declines (no-op). *)
let data_route_paw : Paw.t =
 fun c ->
  match Conn.meth c with
  | Http.GET -> (
    match Fur.Data.local_source (Conn.path c) with
    | Some src ->
      let body = Fur.Data.src_produce src in
      if Fur.Data.src_is_json src then Paw.json body c else Paw.text body c
    | None -> c (* decline: not a co-located data path *))
  | _ -> c

(* A web root for an app: dev reads the assembled webroot/ dir next to the exe
   (the per-app dune assembly), prod serves the embedded map. [name] disambiguates
   per-app dev webroots ("webroot_web", "webroot_admin"). *)

(* Guard the #1 production footgun (once, loudly): a `fennec release` binary embeds its web root, but
   serving the embedded copy requires FENNEC_ENV=production. Launched WITHOUT it, [is_dev] is true and
   we fall back to reading a webroot/ dir on disk beside the binary — which a single-binary deploy did
   not ship. The result is a silent 404 for every asset. So when the dev-mode disk dir is absent, say
   exactly that and exactly how to fix it. (In a real `fennec dev` session the dir exists, so this never
   fires there.) *)
let warned_missing_webroot = ref false

let web_source ~name ~assets : Paw.Static.source =
  if is_dev then begin
    let dir = Filename.concat (Filename.dirname Sys.executable_name) name in
    if (not (Sys.file_exists dir)) && not !warned_missing_webroot then begin
      warned_missing_webroot := true;
      Printf.eprintf
        "fennec: serving static %S from %s, but that directory does not exist.\n\
        \  If this is a `fennec release` binary, set FENNEC_ENV=production — a release embeds its assets,\n\
        \  and dev mode looks for them on disk instead. (Otherwise assemble the web root beside the binary.)\n%!"
        name dir
    end;
    Paw.Static.Dir dir
  end
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

(* ── the dev-only console seam ────────────────────────────────────────────────────────────────────
   The console eval engine ({!Fennec_console_engine}) links [compiler-libs.toplevel], so it must stay
   out of the prod binary. It registers itself HERE — a hook the dev byte build's engine sets at load
   time and the prod native build simply never has — so [serve] starts the REPL without [fennec]'s core
   ever referencing the compiler. Same shape as {!Mail.set_dev_capture}. The engine reads its unix-socket
   path from the environment ([FENNEC_CONSOLE_SOCK]); with none set it stays idle, so a server not
   launched by the fennec tooling never opens a console. *)
type console_start =
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  sleep:(float -> unit) ->
  endpoints:Endpoint.t list ->
  unit

let console_hook : console_start option ref = ref None
let set_console_hook f = console_hook := Some f

(* [FENNEC_CONSOLE=1] turns [serve] into a pure console: boot the runtime, run the eval engine, block —
   no HTTP listener. The user's [server.ml] is unchanged; [fennec console] sets the flag. *)
let console_mode () =
  match Sys.getenv_opt Dev_proto.env_console with Some ("1" | "on" | "true" | "yes") -> true | _ -> false

(* Boot the full data-layer runtime and hand [env]/[sw] to [f]: own Eio, install the ambient switch
   (mongo/burrow), bind the mail + accounts-HTTPS transports, build the accounts store, warm the SSR
   data registry, and arm the dev-supervisor self-exit watch. Shared VERBATIM by the HTTP server ([serve])
   and the console (no HTTP) so the REPL boots byte-for-byte like the running app — same backend, same
   accounts, same ambient switch. The boot order is load-bearing: the ambient switch is installed before
   accounts so collections open by name; see the per-step notes that lived in [serve]. *)
let with_runtime ?accounts (f : env:_ -> sw:Eio.Switch.t -> unit) : unit =
  (* apply the declarative Accounts config (if any) BEFORE [Accounts.boot] forces the store below *)
  Accounts.start ?config:accounts ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  (* install the ambient Eio switch (app + accounts collections open by name — no [sw] threading) and,
     for a burrow:// URL with an authority, front the embedded engine over the MongoDB wire protocol *)
  Fennec_mongo_dynamic.boot ~sw ~net ();
  (* outbound email transport from MAIL_URL (unset ⇒ dev log), on the server's Eio loop *)
  Fennec_mail.boot ~sw ~net ();
  (* the ambient outbound-HTTPS transport the Accounts SSO presets use for token-exchange / JWKS calls *)
  Accounts.set_http_transport (Accounts.Http_transport.default ~net ());
  (* eager accounts: build the (memoized) store now so indexes are ensured at boot, not first request *)
  Accounts.boot ();
  (* warm the co-located data registry so the auto-mounted refetch routes are deterministic from boot *)
  Fur_ssr.warm_data ();
  (* never outlive the dev supervisor: it passes its pid as FENNEC_DEV_PARENT and we self-exit the moment
     getppid() stops matching it — robust to any reparent target, immune to pid recycling. A 0.25s poll. *)
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
  f ~env ~sw

(* start the dev-only eval engine if it registered itself (the byte build) and we are in dev — so
   `fennec console` can attach to this live process. A prod build (no engine linked) is a silent no-op,
   and even in dev the engine self-gates on [FENNEC_CONSOLE_SOCK], so this never opens a socket
   unless the fennec tooling asked for one. *)
let start_console_engine ~env ~sw ~endpoints =
  if is_dev then
    match !console_hook with
    | Some start ->
      start ~sw ~net:(Eio.Stdenv.net env) ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env)) ~endpoints
    | None -> ()

(* A pure console — boot the runtime, run the eval engine, and block; NO HTTP, NO endpoints. This is the
   entry point of a dedicated console build (a byte target that links {!Fennec_console_engine} and whose
   whole [main] is [let () = Fennec.console_run ()]). Because it does NOT call {!serve}, the CLI's server
   discovery ignores it, so adding a console build never disturbs the app's single real server. It boots
   the SAME data layer as [serve] (same backend — the persistent burrow in dev — same accounts), so the
   REPL behaves like the app; with the burrow shared on disk it sees the running server's data too. *)
let console_run ?accounts () =
  with_runtime ?accounts @@ fun ~env ~sw ->
  match !console_hook with
  | Some start ->
    start ~sw ~net:(Eio.Stdenv.net env) ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env)) ~endpoints:[];
    Eio.Fiber.await_cancel ()
  | None ->
    Printf.eprintf "fennec: this console build is missing the engine — link fennec.console.engine\n%!";
    exit 1

let serve ?(timeout = 30.0) ?(max_conns = 10_000) ?tls ?acme ?accounts ?on_error ?on_start
    (endpoints : Endpoint.t list) : unit =
  if not (Atomic.compare_and_set started false true) then
    failwith "Fennec.serve: a server is already running in this process — start the server in exactly one place";
  if console_mode () then
    (* a pure console (no HTTP): boot the runtime, run the eval engine, and block on the live switch. *)
    with_runtime ?accounts @@ fun ~env ~sw ->
    (match !console_hook with
     | Some _ -> start_console_engine ~env ~sw ~endpoints; Eio.Fiber.await_cancel ()
     | None ->
       Printf.eprintf "fennec: %s set but the console engine is not linked (a dev-only build feature)\n%!"
         Dev_proto.env_console;
       exit 1)
  else
  with_runtime ?accounts @@ fun ~env ~sw ->
  let net = Eio.Stdenv.net env in
  (* the engine sees the user's declared endpoints (so `Console.routes ()` can list them); start it
     before the route-wiring transforms below so it captures the app as authored, not the paw-wrapped form *)
  start_console_engine ~env ~sw ~endpoints;
  let lr = Livereload.create () in
  (* Livereload is a dev convenience; turn it off (FENNEC_DEV_LIVERELOAD=0) for a deterministic e2e. *)
  let livereload_on =
    is_dev && (match Sys.getenv_opt Dev_proto.env_dev_livereload with Some ("0" | "off" | "false" | "no") -> false | _ -> true)
  in
  let endpoints =
    List.map (fun e -> Endpoint.prepend (Accounts.native_paw ()) e) endpoints
  in
  (* auto-mount the co-located data refetch routes (Fur.Data.local / model_local) on every endpoint *)
  let endpoints =
    List.map (fun e -> Endpoint.prepend data_route_paw e) endpoints
  in
  let endpoints =
    if livereload_on then List.map (fun e -> Endpoint.prepend (Livereload.paw lr) e) endpoints
    else endpoints
  in
  if livereload_on then dev_control ~sw ~net lr;
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
