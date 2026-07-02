(* The native I/O shell — the Eio HTTP/1.1 + WebSocket server. This interface is the whole public
   face: {!run} (the blocking multi-port, multi-domain acceptor over a compiled {!Host_router}
   table), the {!request_error} funnel with its two ready-made renderers, and {!read_request} (the
   request reader over the hand-crafted C head parser — exposed for the benchmark/tooling). All
   writing, WebSocket serving, TLS termination plumbing, and dispatch internals are private. *)

(** The live websocket channel a paw's upgrade callback receives — the server provides [send]
    (serialized, safe from any fiber); the handler sets [on_text] / [on_close]. *)
type ws = Ws_channel.t = {
  send : string -> unit;
  mutable on_text : string -> unit;
  mutable on_close : unit -> unit;
}

(** One parsed request head + body, as read off the wire (header keys in original case; lookups via
    {!Headers} are case-insensitive). *)
type parsed = {
  meth : string;
  target : string;
  version : string;  (** "HTTP/1.0" | "HTTP/1.1" | … *)
  headers : (string * string) list;
  body : string;
}

(** The outcome of reading one request off a connection. *)
type request_result =
  | Req of parsed
  | Conn_eof  (** clean end of stream — no (more) requests *)
  | Bad_request of string  (** malformed — answer 400 and close *)
  | Too_large of string  (** body/headers over a limit — answer 413 and close *)

val read_request :
  continue:(unit -> unit) -> timeout:Eio.Time.Timeout.t -> Eio.Buf_read.t -> int array -> request_result
(** Read one request: the head via the zero-allocation C parser ({!Http_parse} — pass a scratch array
    from [Http_parse.make_out ()], reused across requests), then the body (Content-Length or chunked,
    smuggling-guarded). [continue] fires just before the body is read (Expect: 100-continue);
    [timeout] bounds ONLY the blocking buffer-grow — a fully-buffered/pipelined head arms no timer.
    Exposed for the {e paw_bench} parser benchmark and head-parsing tooling; {!run} is the server. *)

(** What went wrong with a request the app did not answer — the [~on_error] funnel. A custom renderer
    matches on this; {!default_on_error} (text) and {!json_on_error} (API JSON) are ready-made. *)
type request_error =
  | Handler_exception of exn * Http.request  (** a paw raised — render a 500 *)
  | Handler_timeout of Http.request  (** the opt-in [request_timeout] elapsed — render a 503 *)
  | No_route of Http.request  (** no endpoint answered and the path is declared for no method — 404 *)
  | Method_not_allowed of Http.request * Http.meth list
      (** the path exists for OTHER methods (they're listed) — render a 405; keep the [Allow] header *)

val default_on_error : request_error -> Http.response
(** Plain-text renderings (500 / 503 / 404 / 405 + [Allow]); handler exceptions also log one stderr line. *)

val json_on_error : request_error -> Http.response
(** The same, as [{"error", "status"}] JSON bodies — a drop-in [~on_error] for an API
    ([Paw.serve ~on_error:Paw.Server.json_on_error apps]). The 405 keeps its [Allow] header. *)

val run :
  ?timeout:float ->
  ?request_timeout:float ->
  ?max_conns:int ->
  ?parallelism:int ->
  ?dev:bool ->
  ?tls:(unit -> Tls.Config.server option) ->
  ?on_demand:(string -> unit) ->
  ?on_error:(request_error -> Http.response) ->
  ?on_access:(Access.t -> unit) ->
  ?on_listen:((string * string) list -> unit) ->
  env:Eio_unix.Stdenv.base ->
  Endpoint.t Host_router.t ->
  (unit, [ `Bad_plan of string | `Port_in_use of int ]) result
(** Run a {!Host_router} table, blocking until SIGINT/SIGTERM (graceful: stop accepting, drain
    in-flight). Prod: ONE routed port, worker domains per core. Dev: the routed gateway + one forced
    convenience port per endpoint. Route tables compile ONCE here, never per request. Method-aware
    misses (405 + [Allow], auto-OPTIONS) are computed only after every endpoint declines — off the
    hot path.

    @param timeout          per-request idle/head read bound, seconds (default 30) — arms only when
                            the read actually blocks.
    @param request_timeout  per-request handler deadline, seconds; [<= 0] (default) = OFF — a
                            per-request Eio timer is costly, so the deadline is opt-in.
    @param max_conns        concurrent-connection cap (default 10_000).
    @param parallelism      worker domains; auto (1 in dev, all cores in prod) or FENNEC_PARALLELISM.
    @param dev              dev mode; default from the build/env ({!Dev_proto.is_dev}).
    @param tls              a per-connection source of the live TLS config (ACME renewal swaps certs
                            with no restart); [None]/absent = plain HTTP.
    @param on_demand        ensure a host's certificate before its TLS handshake (SNI is peeked).
    @param on_error         render a {!request_error}; default {!default_on_error}.
    @param on_access        called once per finished request with its {!Access.t} (final status,
                            µs duration, post-gzip bytes). ZERO-COST when absent: with no sink, the
                            event is never built.
    @param on_listen        called post-bind with the (endpoint name, url) pairs for the banner. *)
