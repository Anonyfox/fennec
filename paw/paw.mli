(** Paw — a small, complete HTTP toolkit for OCaml on Eio. No cohttp, no Lwt.

    {2 One primitive}

    A {b paw} is a [Conn.t -> Conn.t]: given a connection it either {e answers} the request or
    {e declines} (passing it through untouched). Routes, middleware, static serving, the websocket
    upgrade — every piece is a paw. Compose them with {!seq}; the first to answer wins and the rest
    are skipped. {!run} drives a pipeline in memory (handy for tests); {!Server.run} drives it over a
    real Eio socket.

    {[
      let app =
        Paw.seq
          [ Paw.Logger.make ();
            Paw.get "/" (fun c -> Paw.Conn.html c "<h1>hello</h1>");
            Paw.get "/users/:id" (fun c ->
              Paw.Conn.text c (Option.value (Paw.Conn.path_param c "id") ~default:"?")) ]

      (* in a test, with no socket: *)
      let resp = Paw.run app (Paw.Http.make_request ~meth:Paw.Http.GET ~path:"/users/42" ())
    ]}

    {2 How it's organized}

    This module is the {e entire} public surface — you only ever open [Paw]. The implementation is
    split by concern into folders so it stays easy to find: [http/] (the vocabulary), [conn/] (the
    carrier), [pipeline/] (the algebra + routing), [routing/] (virtual hosts), [middleware/] (the
    battery), [ws/] (websockets), [server/] (the Eio runtime), [tls/] (HTTPS/ACME), [dev/]. It
    depends on no other fennec library. *)

(** {1 The pipeline}

    The primitive [type t = Conn.t -> Conn.t], its algebra ({!seq}/{!run}/…) and the route verbs
    ({!get}/{!post}/…, with [:name] / [*rest] path captures). *)
include module type of Pipeline

(** {1 Serve — the one-call entry point}

    [serve endpoints] builds a {!Host_router} from [endpoints] and runs it over Eio, owning the event
    loop (the [app.listen] of Paw). [?tls] terminates TLS in-process from an already-loaded
    certificate ({!Tls_termination}); [?acme] obtains and renews certificates automatically via
    Let's Encrypt — serving the HTTP-01 challenge and an HTTP→HTTPS front on :80 — for the domains the
    router answers. The listen port comes from [$FENNEC_PORT] (else 4000 in dev, [$PORT], or 443/80
    when terminating TLS). A clashing route table or a busy port prints a message and exits. For finer
    control — your own Eio env, a prebuilt router — use {!Host_router.build} + {!Server.run}. *)
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

    The verbs reached for in nearly every paw, lifted to [Paw.] from {!Conn} — point-free, so they
    {e are} the same functions: go-to-definition, hover docs, and stack traces all land on {!Conn}.
    Reach for {!Conn} directly for the long tail (headers, files, streaming, status, …).

    - response: {!html} {!json} {!text} {!redirect} {!respond} {!send_file}
    - request: {!param} (a path or query value) {!query} {!cookie} {!header} {!body_param} *)
include module type of struct
  let html = Conn.html
  let json = Conn.json
  let text = Conn.text
  let redirect = Conn.redirect
  let respond = Conn.respond
  let send_file = Conn.send_file
  let param = Conn.param
  let query = Conn.query
  let cookie = Conn.cookie
  let header = Conn.req_header
  let body_param = Conn.body_param
end

(** [endpoint paws] builds an {!Endpoint} from a flat list of paws (middleware and routes, in
    declaration order) — the quick path for one app on one host ([?hosts] defaults to the catch-all
    ["*"]). For the two-phase builder, with middleware that runs {e only} on a matched route (auth,
    rate limiting), use the {!Endpoint} combinators directly. *)
val endpoint : ?name:string -> ?hosts:string list -> t list -> Endpoint.t

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

    Each module's [make] returns a plain {!t}, so they drop into any {!seq} or {!Endpoint}. *)

(** Signed-cookie (or server-side) sessions, HttpOnly + SameSite by default. *)
module Session = Session

(** CSRF protection: a per-session signed, BREACH-masked token. *)
module Csrf = Csrf

(** Cross-Origin Resource Sharing, including preflight. *)
module Cors = Cors

(** Static file serving from disk or an embedded map — path-traversal-safe, with ETag/304/range. *)
module Static = Static

(** One-line-per-response request logging (coloured on a TTY). *)
module Logger = Logger

(** Token-bucket rate limiting, keyed by any function of the connection. *)
module Rate_limit = Rate_limit

(** HTTP Basic authentication. *)
module Basic_auth = Basic_auth

(** Redirect plain HTTP to HTTPS (honouring [X-Forwarded-Proto]). *)
module Force_https = Force_https

(** Conservative security headers (CSP, HSTS, [X-Content-Type-Options], …). *)
module Security_headers = Security_headers

(** A correlation id per request (from [X-Request-Id] or freshly minted), stored in {!Assigns}. *)
module Request_id = Request_id

(** Per-request timing handed to a reporter callback. *)
module Metrics = Metrics

(** Let an HTML form spoof PUT/PATCH/DELETE via a [_method] field or override header. *)
module Method_override = Method_override

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
