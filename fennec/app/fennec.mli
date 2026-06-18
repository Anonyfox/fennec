(** Fennec — the userland facade.

    Everything a server application needs in one module: request/response types ({!Http},
    {!Cookie}), the connection carrier ({!Conn}), the paw primitive + batteries ({!Paw}),
    named endpoints ({!Endpoint}), concurrency helpers ({!parallel}, {!both}), asset serving
    ({!static}, {!web_source}), and the single entry point ({!serve}).

    The framework's internals (dev-mode wiring, the CLI↔server protocol, livereload relay)
    are NOT exported here — they are implementation details of {!serve}.

    {[ (* an app: endpoints (Host-routed), a paw pipeline, the SSR app, one entry point *)
       let web =
         Endpoint.make ~name:"web" ~hosts:[ "*" ] ()
         |> Endpoint.pipe [ Paw.Logger.make (); Paw.Security_headers.make () ]
         |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
         |> Endpoint.app (Fur_ssr.handler ~styles:Site_styles.css ~mounts:[ Web_app.Routes.mount ])

       let () = Fennec.serve [ web ]                 (* plain HTTP *)
       (* ~tls / ~acme for HTTPS; ~on_start:(fun ~sw ~sleep:_ -> …) for boot (e.g. Pulse setup) *) ]} *)

(** {1 Core types} *)

(** The request-scoped connection carrier (see {!Paw.Conn}). *)
module Conn = Paw.Conn

(** HTTP types: request, response, status codes, methods (see {!Paw.Http}). *)
module Http = Paw.Http

(** Cookie parsing and serialization (see {!Paw.Cookie}). *)
module Cookie = Paw.Cookie

(** Accounts: the framework-native identity/session substrate. Password/email/OAuth/OIDC/SAML/
    passkey/MFA/org/SCIM batteries, passwordless route helpers, passkey JSON ceremonies, OIDC
    ID-token verification, Mongo-shaped persistence, and Meteor-shaped auth words live here.
    Fennec installs Accounts everywhere: HTTP/SSR requests get {!Accounts.user_id}, live/DDP
    sessions inherit that user id, and the built-in Accounts methods are registered automatically.
    With no login cookie, identity is simply [None]. *)
module Accounts = Fennec_accounts.Accounts

(** Outbound email — one [MAIL_URL] knob picks the transport ([smtp://] STARTTLS, [smtps://] implicit TLS,
    or — unset — logged to stdout in dev). Compose a {!Mail.t} and {!Mail.send} it; {!Fennec.serve} boots
    the ambient transport. {!Mail.set_dev_capture} tees every send into a live collection — the seam the
    dev mailbox (a userland realtime SPA over that collection) is built on. *)
module Mail = Fennec_mail

(** {1 Endpoints — host routing, apps, and route-local middleware} *)

(** Named apps routed by the request's Host header (see {!Paw.Endpoint}).

    Use endpoints to define one app per host/catch-all, mount SSR apps, add route handlers, and
    attach matched-route middleware such as auth or rate limiting without turning unrelated 404s
    into 401/403 responses. *)
module Endpoint = Paw.Endpoint

(** {1 Fur — the presentation layer}

    One namespace ([open Fennec.Fur]) for everything that turns data into what a client sees:
    components & signals (live SPA, isomorphic core), and — for server-rendered HTML — the {!Fur.Handler}
    response side ([html]/[redirect]/[flash]) and the {!Fur.Form}/{!Fur.Action} typed-input side. A
    server "handler" reads typed inputs, runs Pulse/Accounts logic, and answers with a component
    rendered to static HTML (no JS). JSON APIs are hand-built with {!Fur.Respond}. Includes the
    isomorphic Fur core, so [h]/[text]/[signal]/[to_html] are here too. *)
module Sift = Sift

module Fur : sig
  (* the [struct include] idiom keeps [vnode]/[attr]/[signal] EQUAL to the core Fur types (a bare
     [module type of Fur] would re-abstract them and break interop with Handler/Form) *)
  include module type of struct include Fur end

  (* Standalone HANDLERS (frontend/handlers/*.mlx — view + load + own bundle) are authored as files and
     wired by the fur ppx + route_gen; the generated server [serve] uses {!Fennec_fur_handler.Handler}
     + {!Conn} directly, so there is no handler facade module here. *)
  module Handler : module type of Fennec_web.Handler
  module Form : module type of Fennec_web.Form
  module Action : module type of Fennec_web.Action
  module Respond : module type of Fennec_web.Respond
end

(** {1 Paw — the HTTP toolkit}

    The whole {{!Paw}fennec-paw} library, re-exported: the pipeline primitive and its algebra, the
    flat endpoint builder ([Paw.endpoint () |> Paw.use … |> Paw.get …]), the conn-pipe response verbs
    ([c |> Paw.set_status … |> Paw.json …]), and the middleware battery ([Paw.Logger], [Paw.Session],
    [Paw.Csrf], …). See the [fennec-paw] docs for the full surface. *)
module Paw = Paw

(** {1 Helpers} *)

(** [true] when [FENNEC_ENV] is absent or not ["production"]. *)
val is_dev : bool

(** [dev_only f e] applies the endpoint transform [f] only when {!is_dev}; in production [e] is returned
    untouched, so any routes [f] would add simply do not exist (no bundle served, no handler reachable).
    The clean dev-only mount for tooling, e.g. [... |> dev_only (Endpoint.get "/dev/mailbox" Mailbox.serve)]. *)
val dev_only : (Endpoint.t -> Endpoint.t) -> Endpoint.t -> Endpoint.t

(** A web root source: dev reads the assembled webroot dir next to the exe; prod serves the
    embedded asset map. *)
val web_source :
  name:string -> assets:(string -> string option) -> Paw.Static.source

(** A static-serving paw for an app's web root. In dev, assets are served [no-cache] (the
    browser revalidates via ETag); in prod, content-aware caching applies. *)
val static : name:string -> assets:(string -> string option) -> Paw.t

(** Run thunks concurrently (Eio fibers), returning results in order. *)
val parallel : (unit -> 'a) list -> 'a list

(** Run two thunks (of different types) concurrently. *)
val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b

(** {1 Error handling} *)

(** Request-scoped errors that flow through the unified error funnel. *)
type request_error = Paw.Server.request_error =
  | Handler_exception of exn * Http.request  (** a handler or middleware raised *)
  | Handler_timeout of Http.request  (** the per-request deadline expired *)
  | No_route of Http.request  (** no endpoint matched the Host header (and no ["*"] default) *)
  | Method_not_allowed of Http.request * Http.meth list
      (** the path exists but not for this method; carries the methods it does serve. The server
          renders [405] with an [Allow] header (and auto-answers [OPTIONS]) — override only the body. *)

(** {1 HTTPS} *)

(** In-process TLS termination — load a certificate + key and pass it to {!serve} as [~tls] to serve
    HTTPS directly, no reverse proxy. *)
module Tls : sig
  (** A loaded server TLS configuration. *)
  type t = Paw.Tls_termination.t

  (** [of_files ~cert ~key] loads a PEM certificate chain + private key from the given file paths.
      @raise Failure on a malformed certificate, key, or configuration. *)
  val of_files : cert:string -> key:string -> t

  (** [of_pem ~cert ~key] is {!of_files} from in-memory PEM strings. *)
  val of_pem : cert:string -> key:string -> t
end

(** Pluggable storage for ACME account keys + issued certificates. The default ({!Acme.auto} with no
    [~store]) is a file store; an ephemeral or multi-replica deployment provides its own (a k8s
    Secret / S3 / Redis / DB) by building a {!t}. See {!Paw.Cert_store}. *)
module Cert_store : sig
  (** A cert store — a record of operations (so an external backend is just a value, no functor). *)
  type t = Paw.Cert_store.t = {
    get : string -> string option;
    put : string -> string -> unit;
    delete : string -> unit;
    with_lease : string -> (unit -> unit) -> bool;  (** multi-instance dedup: only the holder runs the thunk *)
  }

  (** [memory ()] — in-process; dev / test / ephemeral (lost on restart). *)
  val memory : unit -> t

  (** [file ~dir] — the default: atomic, [0600] files under [dir]; survives restarts. *)
  val file : dir:string -> t
end

(** Automatic HTTPS via ACME (Let's Encrypt): HTTP-01 for the host router's concrete domains, a
    {!Cert_store}-backed cert, and zero-downtime renewal. Pass {!Acme.auto} to {!serve} as [~acme].
    Wildcards (DNS-01) and a dynamic catch-all (on-demand TLS) are out of scope. See
    {!Paw.Acme}. *)
module Acme : sig
  (** ACME configuration. *)
  type config = Paw.Acme.config

  (** A DNS provider for DNS-01 / wildcard certs — implement over your provider (Cloudflare /
      Route 53 / …). [upsert_txt] sets the TXT record [name] to [value]; [remove_txt] deletes it. *)
  type dns_provider = Paw.Acme.dns_provider = { upsert_txt : name:string -> value:string -> unit; remove_txt : name:string -> unit }

  (** [auto ?email ?store ?staging ?domains ?directory ?dns_provider ()] — automatic certificates.
      Never raises; a missing email leaves HTTPS off. Env overrides code: [FENNEC_ACME_EMAIL],
      [FENNEC_ACME_STAGING], [FENNEC_ACME_DIR] (store). ACME runs in production only unless
      [FENNEC_ACME=1] forces it; in dev it no-ops to plain HTTP. [store] defaults to a file store;
      [domains] overrides the router-derived set; [staging] uses Let's Encrypt staging. [dns_provider]
      enables DNS-01 so wildcard hosts (e.g. ["*.app.com"]) are certified. [on_demand] enables
      on-demand issuance: an HTTPS connection for an SNI host the callback approves gets its cert
      issued on first connect and cached — for runtime-added per-tenant / customer domains. *)
  val auto :
    ?email:string ->
    ?store:Cert_store.t ->
    ?staging:bool ->
    ?domains:string list ->
    ?directory:string ->
    ?dns_provider:dns_provider ->
    ?on_demand:(string -> bool) ->
    unit ->
    config
end

(** {1 Entry point} *)

(** Start the server with the given endpoints, blocking. In dev mode, livereload is
    automatically wired (unless [FENNEC_DEV_LIVERELOAD=0]). In prod, one port is bound and
    endpoints are selected by Host header. An invalid endpoint configuration (clashing
    domains, two catch-alls, a bad host pattern) fails loudly at boot with a clear message.

    [~on_error] receives every request-scoped error and returns a response. The default
    renders plain text (500 / 503 / 404). Override to render JSON, branded error pages, or
    log to a structured sink — one function, one place.

    [~on_start] runs once in the server's Eio context — after the runtime is live and before any
    connection is served — receiving the server's long-lived switch and a clock-backed [sleep]. It
    is where an app creates resources that need the runtime, e.g. a real-mongo backend's collections
    and their observe loops (which fork into [sw]); the in-memory backend needs nothing here.

    [~tls] terminates HTTPS in-process with a BYO certificate (no reverse proxy) — see {!Tls}.
    [~acme] instead obtains + auto-renews Let's Encrypt certificates for the concrete domains — see
    {!Acme}. (Give one or the other; [~acme] takes precedence.)

    Accounts is native: Fennec prepends the identity paw to every endpoint and live/DDP wiring
    exposes [user_id] plus built-in Accounts methods without manual paws or method registration.
    With no login, identity remains anonymous ([None]) and ["currentUser"] returns the anonymous
    session payload.

    [~accounts] is the one declarative Accounts config (start from {!Accounts.defaults} and override a
    field at a time): it parameterizes the native instance and auto-wires the routes + DDP methods it
    implies ({!Accounts.start} / {!Accounts.Wiring}). {b Omitting it leaves Accounts at today's
    defaults} — hard-wired, every optional feature off, all methods on, anonymous identity [None].

    This is the single place that starts the server — a second call is a runtime error.
    The CLI's discovery ({!Discover}) finds this call site automatically. *)
val serve :
  ?timeout:float ->
  ?max_conns:int ->
  ?tls:Tls.t ->
  ?acme:Acme.config ->
  ?accounts:Accounts.config ->
  ?on_error:(request_error -> Http.response) ->
  ?on_start:
     (sw:Eio.Switch.t -> sleep:(float -> unit) -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t -> unit) ->
  Endpoint.t list ->
  unit
