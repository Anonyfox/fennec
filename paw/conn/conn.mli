(** The connection a request flows through — the single value every paw touches.

    Inspired by Plug's conn, with two deliberate departures OCaml lets us make where the BEAM
    couldn't: TYPED assigns (no untyped map, no casts, no need for Plug's separate [private]
    store), and a MUTABLE-backed value with the same [conn -> conn] pipe API (a setter
    mutates in place and returns the same conn, so N paws don't allocate N records — safe
    under Eio: one conn per request, one fiber, never shared).

    The type is abstract: build the response only through the functions here. Building it
    (status/headers/cookies) does NOT answer — the pipeline keeps running; only an
    {e answerer} (a body, redirect, stream, halt, or upgrade) short-circuits the rest.

    A handler is a paw [Conn.t -> Conn.t]: read the request, optionally build the response,
    then answer. Builders return the conn so a later answerer short-circuits the pipeline:

    {[
      let show_user c =
        match path_param c "id" with
        | None -> text ~status:400 c "missing id"
        | Some id ->
          let c = set_header c "x-cache" "miss" in
          json c (User.to_json (User.find id))
    ]}

    Server-side only — conns never cross to the client. *)

(** The mutable request/response carrier that flows through a paw pipeline. One conn per
    request, one fiber — never shared. Build the response through the setters and answerers
    below; the server reads {!resp}, {!stream}, or {!upgrade_handler} after the pipeline runs. *)
type t

(** A streamed response body the server writes without buffering. *)
type stream =
  | File of string * string                          (** path, content-type *)
  | Chunked of string * ((string -> unit) -> unit)   (** content-type, producer fed an [emit] *)

(** {1 Construction & server-facing consumption} *)

(** A fresh conn for a request (the server calls this; tests may too). *)
val make : Http.request -> t

(** The request. *)
val req : t -> Http.request

(** The buffered response the conn answered with, if any (the server reads this). *)
val resp : t -> Http.response option

(** The status + headers with an empty body — for running before_send over a streamed or
    headers-only response. *)
val resp_skeleton : t -> Http.response

(** The pending websocket-upgrade setup, if a paw requested one. *)
val upgrade_handler : t -> (Ws_channel.t -> unit) option

(** The pending streamed response, if any. *)
val stream : t -> stream option

(** Apply all registered before_send hooks to a response (the server calls this once). *)
val apply_before_send : t -> Http.response -> Http.response

(** Has the conn answered? (a response, a halt, an upgrade, or a stream). The runner stops
    feeding paws once answered. *)
val answered : t -> bool

(** {1 Request readers} *)

(** The URL path (percent-decoded, without the query string). *)
val path : t -> string

(** The effective method (a method-override paw may have replaced it). *)
val meth : t -> Http.meth

(** The [Host] header value (without port). Used for host-based routing. *)
val host : t -> string

(** ["http"] or ["https"] — from the transport, or the value a trusted-proxy paw forwarded. *)
val scheme : t -> string

(** The client IP — from the transport (may be a proxy address), or the real client a trusted-proxy
    paw extracted from [X-Forwarded-For]. *)
val remote_ip : t -> string option

(** The HTTP version string (e.g. ["HTTP/1.1"] or ["HTTP/2"]). *)
val version : t -> string

(** When this request started, epoch seconds — stamped once at {!make}. This is the single request
    timer the logger, the metrics callback, and the response-time header all read, instead of each
    grabbing its own clock at entry. *)
val started_at : t -> float

(** Microseconds elapsed since the request started (its duration so far), computed from the one
    {!started_at} stamp. *)
val elapsed_us : t -> int

(** [set_error c msg] records a short error message for the access log — set by the server's error
    funnel when a handler raises or times out. Purely observational (not an answerer). *)
val set_error : t -> string -> t

(** The recorded access-log error message, if any (see {!set_error}). *)
val error : t -> string option

(** [request_access_log c sink] installs a per-request access-log [sink] that the server invokes with
    the FINAL (post-finalize) {!Access} event — so the line carries the real post-gzip body size even
    though the logger paw runs before finalize. The {!Logger} paw uses this; setting it twice keeps the
    latest. Purely observational (not an answerer). *)
val request_access_log : t -> (Access.t -> unit) -> t

(** The installed per-request access-log sink, if any (the server reads this after finalize). *)
val access_sink : t -> (Access.t -> unit) option

(** A request header, case-insensitive (the first value if repeated). *)
val req_header : t -> string -> string option

(** All values of a (repeatable) request header, in order. *)
val req_headers : t -> string -> string list

(** Query params (parsed + percent-decoded lazily on first read, cached). *)
val query_params : t -> (string * string) list

(** A single query parameter value by name (case-sensitive). *)
val query : t -> string -> string option

(** Request cookies (parsed lazily). These read cookies sent by the browser on this request.
    To write a response cookie, use {!set_cookie}; to expire one, use {!delete_cookie};
    to persist request-to-request state, prefer {!Session.make}. *)
val cookies : t -> (string * string) list

(** A single request cookie value by name. This does not inspect pending response
    [Set-Cookie] headers. *)
val cookie : t -> string -> string option

(** Form body fields ([application/x-www-form-urlencoded] or [multipart/form-data], parsed
    lazily by content type). *)
val body_params : t -> (string * string) list

(** A single form field value by name. *)
val body_param : t -> string -> string option

(** Uploaded file parts from a [multipart/form-data] request body. Use this for incoming file
    upload handlers; HTTP tests can send matching bodies with {!Fennec_hunt.Http.file} and
    [~multipart]. *)
val files : t -> Multipart.part list

(** An uploaded file part by form field name. *)
val file : t -> string -> Multipart.part option

(** Path params captured by a [:name]/[*splat] route. *)
val path_params : t -> (string * string) list

(** A named segment captured by a [:name] or [*splat] route pattern. *)
val path_param : t -> string -> string option

(** A value by name, checked in order: path param, query string, then form body. *)
val param : t -> string -> string option

(** {1 Typed assigns} — request-scoped, type-safe key/value storage (see {!Assigns}). *)

(** Store a typed value under a key for downstream paws to retrieve. *)
val assign : t -> 'a Assigns.key -> 'a -> t

(** Retrieve a typed assign value; [None] if the key was never set. *)
val get : t -> 'a Assigns.key -> 'a option

(** Get or [Invalid_argument] — for a key an upstream paw guarantees. *)
val get_exn : t -> 'a Assigns.key -> 'a

(** {1 Response builders} — mutate the response WITHOUT answering; the pipeline continues. *)

(** Set the status. With no prior response this answers with that status and an empty body;
    after an answering paw it just overrides the code. *)
val set_status : int -> t -> t

(** Add a response header (accumulates; survives a later answering paw). *)
val set_header : t -> string -> string -> t

(** Set a response cookie (adds [Set-Cookie], does not answer). Use for small browser
    preferences, one-off flags, and remember-me style response cookies. Defaults:
    [path="/"], [http_only=true], [same_site=Lax]; [SameSite=None] implies [Secure].
    For request-to-request application state, prefer {!Session.make}. *)
val set_cookie :
  t ->
  ?path:string ->
  ?domain:string ->
  ?max_age:int ->
  ?expires:float ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:Cookie.same_site ->
  string ->
  string ->
  t

(** Expire a response cookie now by emitting a matching [Set-Cookie] deletion. The [path] and
    [domain] should match the cookie that was originally set. *)
val delete_cookie : t -> ?path:string -> ?domain:string -> string -> t

(** Set the effective method (used by a method-override paw). *)
val override_method : t -> Http.meth -> t

(** Set the effective client IP (used by a trusted-proxy paw, from [X-Forwarded-For]). Thereafter
    {!remote_ip} returns this value. *)
val override_remote_ip : t -> string -> t

(** Set the effective scheme (used by a trusted-proxy paw, from [X-Forwarded-Proto]). Thereafter
    {!scheme} returns this value. *)
val override_scheme : t -> string -> t

(** Set the captured path params (used by a :param/route). *)
val set_path_params : t -> (string * string) list -> t

(** Register a hook run on the final response just before sending (FIFO). The way a paw
    touches the RESPONSE (compression, security headers, logging) without answering. *)
val before_send : t -> (Http.response -> Http.response) -> t

(** {1 Answerers} — set a response and short-circuit the rest of the pipeline. *)

(** Answer with a full {!Http.response} (pre-set headers are preserved; the
    answer's content-type wins). *)
val respond : t -> Http.response -> t

(** Answer with a [text/plain] body. [status] defaults to 200. *)
val text : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with a [text/html; charset=utf-8] body. [status] defaults to 200. *)
val html : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with an [application/json] body. [status] defaults to 200. *)
val json : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with a Location header + a 3xx status (302 by default). *)
val redirect : ?status:int -> t -> string -> t

(** Stream a file from disk (content type defaults to the path's MIME type). [download:name] serves it
    as an attachment (the browser saves it as [name] rather than rendering it). *)
val send_file : t -> ?content_type:string -> ?download:string -> path:string -> unit -> t

(** Answer with in-memory bytes as a downloadable attachment — a generated CSV / PDF / export. The
    [Content-Disposition] filename is sanitized (no header injection) with a unicode-safe [filename*];
    [content_type] defaults to [filename]'s MIME type. *)
val download : t -> ?content_type:string -> filename:string -> string -> t

(** Stream a chunked (Transfer-Encoding: chunked) body: [produce emit] is run by the server,
    calling [emit] per chunk. Use content-type ["text/event-stream"] for SSE. *)
val send_chunked : t -> ?content_type:string -> ((string -> unit) -> unit) -> t

(** Answer by upgrading to a websocket; [setup] receives the live channel. *)
val upgrade : t -> (Ws_channel.t -> unit) -> t

(** Explicitly halt with no response (rare; the server turns this into a 404). *)
val halt : t -> t
