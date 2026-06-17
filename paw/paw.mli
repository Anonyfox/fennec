(** Paw — a small, complete HTTP toolkit for OCaml on Eio. No cohttp, no Lwt.

    {2 One primitive}

    A {b paw} is a [Conn.t -> Conn.t]: given a connection it either {e answers} the request or
    {e declines} (passing it through untouched). Routes, middleware, static serving, the websocket
    upgrade — every piece is a paw. Compose them with {!seq}; the first to answer wins and the rest
    are skipped. {!run} drives a pipeline in memory (handy for tests); {!Server.run} drives it over a
    real Eio socket.

    {[
      let app =
        Paw.endpoint ()
        |> Paw.use (Paw.Logger.make ())
        |> Paw.get "/" (fun c -> c |> Paw.html "<h1>hello</h1>")
        |> Paw.get "/users/:id" (fun c ->
               c |> Paw.text (Option.value (Paw.param c "id") ~default:"?"))

      let () = Paw.serve [ app ]
    ]}

    {2 How it's organized}

    This module is the {e entire} public surface — you only ever open [Paw]. The implementation is
    split by concern into folders so it stays easy to find: [http/] (the vocabulary), [conn/] (the
    carrier), [pipeline/] (the algebra + routing), [routing/] (virtual hosts), [middleware/] (the
    battery), [ws/] (websockets), [server/] (the Eio runtime), [tls/] (HTTPS/ACME), [dev/]. It
    depends on no other fennec library. *)

(** {1 The pipeline primitive}

    A paw is [type t = Conn.t -> Conn.t]; compose with {!seq} (the first to answer wins). {!run}
    drives a pipeline to a response in memory (for tests); {!serve} / {!Server.run} drive it over a
    socket. The verbs that build apps ({!get}/{!use}/…) are below; the raw route-as-paw forms are
    under {!Route}. *)
type t = Conn.t -> Conn.t

(** Compose paws left-to-right; the first to answer wins, the rest are skipped. *)
val seq : t list -> t

(** The identity paw — declines everything (the unit of {!seq}). *)
val pass : t

(** Run a pipeline to a response — an unanswered conn becomes a 404. Handy for pure tests. *)
val run : t -> Http.request -> Http.response

(** Run a pipeline over a request, returning the final conn (e.g. to inspect a websocket upgrade). *)
val run_conn : t -> Http.request -> Conn.t

(** A paw from a [request -> response option] (e.g. static files): answers on [Some], else declines. *)
val fallthrough : (Http.request -> Http.response option) -> t

(** {1 Route-as-paw primitives}

    Build a single self-contained route — a paw that answers one method+path and declines otherwise —
    for mounting (via {!use}) or composing. An app reaches for the {!Endpoint} verbs ({!get}/{!post}/…);
    these are for reusable, mountable routes (what the accounts paws emit). *)
module Route : sig
  (** A route on an explicit method. The pattern may contain [:name] / trailing [*rest] captures. *)
  val on : Http.meth -> string -> t -> t

  (** A GET route. *)
  val get : string -> t -> t

  (** A POST route. *)
  val post : string -> t -> t

  (** A PUT route. *)
  val put : string -> t -> t

  (** A DELETE route. *)
  val delete : string -> t -> t

  (** A PATCH route. *)
  val patch : string -> t -> t
end

(** {1 Serve — the one-call entry point}

    [serve endpoints] builds a {!Host_router} from [endpoints] and runs it over Eio, owning the event
    loop (the [app.listen] of Paw).

    HTTPS is per-domain and mostly transparent: [?tls] terminates TLS in-process from a loaded
    certificate ({!Tls_termination} — one SAN cert, or several certs SNI-selected per domain via
    {!Tls_termination.of_file_pairs}); [?acme] obtains and renews Let's Encrypt certificates
    automatically for the domains the router answers (on-demand for new tenants). With {e either}, in
    production the app serves HTTPS on :443 and a :80 front 301-redirects HTTP→HTTPS (and answers the
    ACME HTTP-01 challenge) — so a BYO cert gets the same auto-upgrade as ACME. The listen port comes
    from [$FENNEC_PORT] (else 4000 in dev, [$PORT], or 443/80 when terminating TLS).

    A clashing route table or a busy port prints a message and exits. For finer control — your own Eio
    env, a prebuilt router — use {!Host_router.build} + {!Server.run}. *)
val serve :
  ?tls:Tls_termination.t ->
  ?acme:Acme.config ->
  ?on_error:(Server.request_error -> Http.response) ->
  ?on_listen:((string * string) list -> unit) ->
  Endpoint.t list ->
  unit

(** {1 The connection} *)

(** The per-request carrier: the parsed request, the response being built, path params, and typed
    request-scoped state. Every paw reads and writes through a {!Conn.t}. *)
module Conn = Conn

(** Typed, request-scoped key/value state threaded along a pipeline (e.g. the request id from
    {!Request_id}, the signed-in user). *)
module Assigns = Assigns

(** {1 Handler shortcuts}

    The verbs reached for in nearly every paw, lifted to [Paw.] from {!Conn}. {b Reads} pull a value
    out (conn-first); {b writes} thread the connection through (conn-{e last}), so a handler reads
    once, then pipes the response — Elixir/Plug style:

    {[
      let create c =
        let title = Option.value (Paw.param c "title") ~default:"" in
        c |> Paw.set_status 201 |> Paw.set_cookie "sid" sid |> Paw.json (encode title)
    ]}

    Reach for {!Conn} directly for the long tail (files, streaming, raw header lists, …). *)

(** Reads — point-free aliases of {!Conn}; go-to-definition and stack traces land there. [param]
    checks path, then query, then form body. *)
include module type of struct
  let param = Conn.param
  let query = Conn.query
  let cookie = Conn.cookie
  let header = Conn.req_header
  let body_param = Conn.body_param
end

(** Set the response status. *)
val set_status : int -> Conn.t -> Conn.t

(** Add a response header. *)
val set_header : string -> string -> Conn.t -> Conn.t

(** Set a response cookie ([http_only] + [SameSite=Lax] by default); see {!Conn.set_cookie}. *)
val set_cookie :
  ?path:string ->
  ?domain:string ->
  ?max_age:int ->
  ?expires:float ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:Cookie.same_site ->
  string ->
  string ->
  Conn.t ->
  Conn.t

(** Expire a response cookie. *)
val delete_cookie : ?path:string -> ?domain:string -> string -> Conn.t -> Conn.t

(** Answer with a ready-made {!Http.response}. *)
val respond : Http.response -> Conn.t -> Conn.t

(** Answer with an HTML body. *)
val html : ?status:int -> ?headers:(string * string) list -> string -> Conn.t -> Conn.t

(** Answer with a JSON body (you supply the already-encoded string). *)
val json : ?status:int -> ?headers:(string * string) list -> string -> Conn.t -> Conn.t

(** Answer with a plain-text body. *)
val text : ?status:int -> ?headers:(string * string) list -> string -> Conn.t -> Conn.t

(** Answer with a redirect to a URL (302 by default). *)
val redirect : ?status:int -> string -> Conn.t -> Conn.t

(** Stream a file from disk as the response. *)
val send_file : ?content_type:string -> path:string -> Conn.t -> Conn.t

(** {1 The endpoint — flat-pipe app assembly}

    Start with {!endpoint} and pipe middleware and routes onto it, one per line; hand the result(s) to
    {!serve}. These are {!Endpoint} verbs lifted to [Paw.] — the endpoint is the last argument, so they
    chain with [|>]; the only nesting is the handler, the one [fun c -> …] that genuinely is nested.

    {[
      let app =
        Paw.endpoint ~name:"api" ~hosts:[ "api.example.com" ]
        |> Paw.use (Paw.Logger.make ())
        |> Paw.use_matched (Paw.Rate_limit.make ())          (* only on a matched route, not 404s *)
        |> Paw.get "/health" (fun c -> c |> Paw.json {|{"ok":true}|})
        |> Paw.post "/users" create

      let () = Paw.serve [ app ]
    ]} *)

(** An empty endpoint to pipe onto. [~name] labels it (host routing + logs); [~hosts] is the Host
    patterns it answers ([["*"]] catch-all by default). *)
val endpoint : ?name:string -> ?hosts:string list -> unit -> Endpoint.t

(** Add always-run middleware (runs on every request, including 404s). *)
val use : t -> Endpoint.t -> Endpoint.t

(** Add matched-only middleware — runs solely when a route matches, so misses still 404 (the place
    for auth and rate limiting). *)
val use_matched : t -> Endpoint.t -> Endpoint.t

(** Add an always-run paw at the {e front} of the pipeline. *)
val prepend : t -> Endpoint.t -> Endpoint.t

(** Add several always-run paws at once. *)
val pipe : t list -> Endpoint.t -> Endpoint.t

(** Add several matched-only paws at once. *)
val pipe_matched : t list -> Endpoint.t -> Endpoint.t

(** Route a GET matching [pattern] to a handler ([:name] captures a segment, a trailing [*rest] the
    tail — read them back with {!param}). *)
val get : string -> t -> Endpoint.t -> Endpoint.t

(** Route a POST. *)
val post : string -> t -> Endpoint.t -> Endpoint.t

(** Route a PUT. *)
val put : string -> t -> Endpoint.t -> Endpoint.t

(** Route a DELETE. *)
val delete : string -> t -> Endpoint.t -> Endpoint.t

(** Route a PATCH. *)
val patch : string -> t -> Endpoint.t -> Endpoint.t

(** Register GET+POST for [pattern] to one handler (a form: render on GET, accept on POST). *)
val form : string -> t -> Endpoint.t -> Endpoint.t

(** Mount a [path -> string option] sub-app (an SSR renderer or embedded asset map) under [?at]
    (default ["/"]); it answers when it returns [Some]. *)
val app : ?at:string -> (string -> string option) -> Endpoint.t -> Endpoint.t

(** {1 HTTP vocabulary}

    Pure types and codecs — no I/O — so they are equally usable off the server. *)

(** Requests, responses, methods, and status codes. *)
module Http = Http

(** A case-insensitive header collection. *)
module Headers = Headers

(** [Cookie] / [Set-Cookie] parsing and serialization. *)
module Cookie = Cookie

(** Filename → content-type lookup. *)
module Mime = Mime

(** [multipart/form-data] parsing (file uploads). *)
module Multipart = Multipart

(** RFC 1123 / asctime HTTP date formatting and parsing. *)
module Http_date = Http_date

(** Caching, conditional-request, and range semantics (ETag, [If-*], 304/206/416). *)
module Http_semantics = Http_semantics

(** {1 The Eio runtime} *)

(** The HTTP/1.1 + WebSocket acceptor. {!Server.run} serves a {!Host_router} over Eio with
    keep-alive, response streaming, per-request deadlines, and optional in-process TLS. *)
module Server = Server

(** Response finalization — content-encoding negotiation, strong ETag + 304, [Date],
    [Content-Length], HEAD. Applied by {!Server}; exposed for custom drivers. *)
module Responder = Responder

(** gzip / deflate (real zlib), for [Content-Encoding] and websocket permessage-deflate. *)
module Gzip = Gzip

(** Deterministic dev port allocation from a base port (gateway + one port per endpoint). *)
module Port_plan = Port_plan

(** {1 Routing & virtual hosts} *)

(** A named app: a route table plus a two-phase paw pipeline (an always-run phase and a
    matched-only phase, so middleware like auth fires only on real routes, not on 404s). *)
module Endpoint = Endpoint

(** A validated Host-header → endpoint table; reports all routing conflicts up front. *)
module Host_router = Host_router

(** A single Host pattern: exact hostname, [*.]wildcard suffix, or catch-all. *)
module Host_pattern = Host_pattern

(** {1 Middleware battery}

    Prebuilt paws — the lego blocks. Each module's [make] returns a plain {!t}, so they drop into any
    {!seq} or {!Endpoint}. They are allocation-lean by construction (header paws build their
    before-send hook once at [make]; guards decline in O(1)), so mounting a stack costs almost nothing
    per request.

    On a miss the server is already HTTP-correct without any paw: a path that exists for other methods
    returns [405] with an [Allow] header (not a blanket 404), and an [OPTIONS] probe with no explicit
    handler is auto-answered [204] + [Allow] (an explicit [OPTIONS] route or {!Cors} still wins). *)

(** {2 Sessions & security} *)

(** Signed-cookie (or server-side) sessions, HttpOnly + SameSite by default. *)
module Session = Session

(** CSRF protection: a per-session signed, BREACH-masked token. *)
module Csrf = Csrf

(** Cross-Origin Resource Sharing, including preflight. *)
module Cors = Cors

(** Conservative security headers (CSP, HSTS, [X-Content-Type-Options], …). *)
module Security_headers = Security_headers

(** Redirect plain HTTP to HTTPS (honouring [X-Forwarded-Proto]). *)
module Force_https = Force_https

(** {2 Authentication} *)

(** HTTP Basic authentication. *)
module Basic_auth = Basic_auth

(** HTTP Bearer-token authentication — [401] + [WWW-Authenticate: Bearer] when the token is missing
    or rejected by your [verify]. *)
module Bearer_auth = Bearer_auth

(** {2 Traffic shaping & limits} *)

(** Token-bucket rate limiting, keyed by any function of the connection. *)
module Rate_limit = Rate_limit

(** Reject a request whose body exceeds a byte cap with [413 Payload Too Large]. *)
module Body_limit = Body_limit

(** {2 Observability} *)

(** One-line-per-response request logging (coloured on a TTY). *)
module Logger = Logger

(** A correlation id per request (from [X-Request-Id] or freshly minted), stored in {!Assigns}. *)
module Request_id = Request_id

(** Per-request timing handed to a reporter callback. *)
module Metrics = Metrics

(** Stamp the request's processing time onto the response as a [Server-Timing] header (and optionally
    [X-Response-Time]) — the client-visible companion to {!Logger} / {!Metrics}. *)
module Response_time = Response_time

(** {2 Request hygiene} — normalize the request before it reaches a route *)

(** Let an HTML form spoof PUT/PATCH/DELETE via a [_method] field or override header. *)
module Method_override = Method_override

(** Canonicalize the path by stripping trailing slashes, with a [308] (method/body-preserving)
    redirect — so URLs stay canonical and routes only declare the slash-free form. *)
module Normalize_path = Normalize_path

(** Behind a reverse proxy / load balancer, recover the real client IP and scheme from
    [X-Forwarded-For] / [X-Forwarded-Proto] — but only when the connecting peer is trusted, so they
    cannot be forged. Sets {!Conn.remote_ip} / {!Conn.scheme} for every downstream paw. *)
module Trusted_proxy = Trusted_proxy

(** Content negotiation: [406 Not Acceptable] unless the request's [Accept] header accepts one of the
    media types the endpoint serves (Plug's [:accepts]). *)
module Accepts = Accepts

(** Allow or deny a request by client IP — exact IPs or IPv4 CIDR. Pairs with {!Trusted_proxy} to
    filter the real client behind a proxy; for admin/internal endpoints and sender-pinned webhooks. *)
module Ip_filter = Ip_filter

(** {2 Response shaping & assets} *)

(** Static file serving from disk or an embedded map — path-traversal-safe, with ETag/304/range. *)
module Static = Static

(** Set a [Cache-Control] directive on answered responses (unless the handler set its own). *)
module Cache_control = Cache_control

(** Add fixed response headers — each only if absent, so a handler's own value wins. *)
module Set_header = Set_header

(** Give error responses a body when the handler left it empty (a bare [set_status 4xx/5xx]) — default
    ["<code> <reason>"] text, or branded HTML/JSON pages. *)
module Status_pages = Status_pages

(** {2 Operations} *)

(** A liveness / readiness probe endpoint (GET/HEAD [/healthz] by default). *)
module Health = Health

(** {1 WebSockets} (RFC 6455) *)

(** The frame reader/writer (fragmentation, ping/pong, close, permessage-deflate). *)
module Ws = Ws

(** The upgrade paw: answer a path by handing the caller a {!Ws_channel.t}. *)
module Websocket = Websocket

(** The bidirectional channel a websocket handler reads and writes. *)
module Ws_channel = Ws_channel

(** {1 TLS & automatic HTTPS} *)

(** In-process TLS termination from a cert chain + key, with SNI selection for multi-tenant hosts. *)
module Tls_termination = Tls_termination

(** Extract the SNI hostname from a TLS ClientHello (for on-demand certificate selection). *)
module Sni = Sni

(** A pluggable storage seam for the ACME account key + issued certificates (file, memory, or your
    own backend). *)
module Cert_store = Cert_store

(** Automatic HTTPS via ACME / Let's Encrypt: HTTP-01 per domain, a renewal loop, optional DNS-01
    for wildcards, and on-demand issuance. *)
module Acme = Acme

(** A minimal outbound HTTPS client (used by {!Acme}). *)
module Https_client = Https_client

(** The low-level ACME protocol client behind {!Acme}. Exposed for the pebble end-to-end wire test;
    most consumers want {!Acme}, the managed on-demand certificate source. *)
module Acme_client = Acme_client

(** {1 Dev} *)

(** A live-reload script injector for HTML responses (dev only). *)
module Livereload = Livereload

(** Dev-server niceties shared with tooling: stable feature flags. *)
module Dev = Dev

(** The CLI⇄server dev control wire — env-var names and the stderr line formats the dev runner
    parses. *)
module Dev_proto = Dev_proto
