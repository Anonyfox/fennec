(** Fennec — the userland facade.

    Everything a server application needs in one module: request/response types ({!Http},
    {!Cookie}), the connection carrier ({!Conn}), the paw primitive + batteries ({!Paw}),
    named endpoints ({!Endpoint}), concurrency helpers ({!parallel}, {!both}), asset serving
    ({!static}, {!web_source}), and the single entry point ({!serve}).

    The framework's internals (dev-mode wiring, the CLI↔server protocol, livereload relay)
    are NOT exported here — they are implementation details of {!serve}. *)

(** {1 Core types} *)

(** The request-scoped connection carrier (see {!Fennec_paw.Conn}). *)
module Conn = Fennec_paw.Conn

(** HTTP types: request, response, status codes, methods (see {!Fennec_core.Http}). *)
module Http = Fennec_core.Http

(** Cookie parsing and serialization (see {!Fennec_core.Cookie}). *)
module Cookie = Fennec_core.Cookie

(** {1 Endpoints — named apps routed by host} *)

(** Named apps routed by the request's Host header (see {!Fennec_server.Endpoint}). *)
module Endpoint = Fennec_server.Endpoint

(** {1 Paw — the pipeline primitive + prebuilt batteries}

    A paw is [Conn.t -> Conn.t]. Compose with {!Paw.seq}; the first to answer wins.
    The batteries (logger, session, CSRF, …) are submodules, each a [make] returning a paw. *)
module Paw : sig
  type t = Conn.t -> Conn.t

  val seq : t list -> t
  val pass : t
  val run_conn : t -> Http.request -> Conn.t
  val run : t -> Http.request -> Http.response
  val on : Http.meth -> string -> t -> t
  val get : string -> t -> t
  val post : string -> t -> t
  val put : string -> t -> t
  val delete : string -> t -> t
  val patch : string -> t -> t
  val fallthrough : (Http.request -> Http.response option) -> t

  module Logger : sig
    val make : ?sink:(string -> unit) -> unit -> t
  end

  module Security_headers : sig
    val make : ?extra:(string * string) list -> unit -> t
  end

  module Request_id : sig
    val make : ?header:string -> unit -> t
  end

  module Method_override : sig
    val make : ?field:string -> ?header:string -> unit -> t
  end

  module Basic_auth : sig
    val make : username:string -> password:string -> ?realm:string -> unit -> t
  end

  module Force_https : sig
    val make : ?status:int -> ?hsts:int -> unit -> t
  end

  module Cors : sig
    (** Which origins to allow: [Any], or an explicit allowlist (reflected when matched). *)
    type origin = Fennec_server.Cors.origin = Any | These of string list

    val make :
      ?origins:origin ->
      ?methods:string list ->
      ?headers:string list ->
      ?expose:string list ->
      ?credentials:bool ->
      ?max_age:int ->
      unit ->
      t
  end

  module Rate_limit : sig
    val make :
      ?key:(Conn.t -> string) -> ?capacity:int -> ?per_second:float -> ?now:(unit -> float) -> unit -> t
  end

  module Metrics : sig
    val make : (meth:string -> path:string -> status:int -> duration_ms:float -> unit) -> t
  end

  module Websocket : sig
    val make : string -> (Fennec_core.Ws_channel.t -> unit) -> t
  end

  module Static : sig
    type source = Fennec_server.Static.source =
      | Dir of string
      | Embedded of string * (string -> string option)

    val make : ?cache_control:string -> source -> t
  end

  module Session : sig
    type store = Fennec_server.Session.store

    val make :
      secret:string ->
      ?cookie:string ->
      ?path:string ->
      ?lifetime:float ->
      ?same_site:Cookie.same_site ->
      ?http_only:bool ->
      ?secure:bool ->
      ?store:store ->
      unit ->
      t
  end

  module Csrf : sig
    val make :
      secret:string ->
      ?field:string ->
      ?header:string ->
      ?safe:string list ->
      unit ->
      t
  end
end

(** {1 Helpers} *)

(** [true] when [FENNEC_ENV] is absent or not ["production"]. *)
val is_dev : bool

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
type request_error = Fennec_server.Server.request_error =
  | Handler_exception of exn * Http.request  (** a handler or middleware raised *)
  | Handler_timeout of Http.request  (** the per-request deadline expired *)
  | No_route of Http.request  (** no endpoint matched the Host header (and no ["*"] default) *)

(** {1 HTTPS} *)

(** In-process TLS termination — load a certificate + key and pass it to {!serve} as [~tls] to serve
    HTTPS directly, no reverse proxy. *)
module Tls : sig
  (** A loaded server TLS configuration. *)
  type t = Fennec_server.Tls_termination.t

  (** [of_files ~cert ~key] loads a PEM certificate chain + private key from the given file paths.
      @raise Failure on a malformed certificate, key, or configuration. *)
  val of_files : cert:string -> key:string -> t

  (** [of_pem ~cert ~key] is {!of_files} from in-memory PEM strings. *)
  val of_pem : cert:string -> key:string -> t
end

(** Pluggable storage for ACME account keys + issued certificates. The default ({!Acme.auto} with no
    [~store]) is a file store; an ephemeral or multi-replica deployment provides its own (a k8s
    Secret / S3 / Redis / DB) by building a {!t}. See {!Fennec_server.Cert_store}. *)
module Cert_store : sig
  (** A cert store — a record of operations (so an external backend is just a value, no functor). *)
  type t = Fennec_server.Cert_store.t = {
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
    {!Fennec_server.Acme}. *)
module Acme : sig
  (** ACME configuration. *)
  type config = Fennec_server.Acme.config

  (** [auto ~email ?store ?staging ?domains ?directory ()] — automatic certificates. [store] defaults
      to a file store under [$FENNEC_ACME_DIR] / [$XDG_STATE_HOME/fennec/acme]; [domains] overrides
      the router-derived (Exact) set; [staging] uses Let's Encrypt staging (rate-limit-free testing). *)
  val auto : email:string -> ?store:Cert_store.t -> ?staging:bool -> ?domains:string list -> ?directory:string -> unit -> config
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

    This is the single place that starts the server — a second call is a runtime error.
    The CLI's discovery ({!Discover}) finds this call site automatically. *)
val serve :
  ?timeout:float ->
  ?max_conns:int ->
  ?tls:Tls.t ->
  ?acme:Acme.config ->
  ?on_error:(request_error -> Http.response) ->
  ?on_start:(sw:Eio.Switch.t -> sleep:(float -> unit) -> unit) ->
  Endpoint.t list ->
  unit
