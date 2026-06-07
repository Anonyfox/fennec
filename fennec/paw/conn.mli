(** The connection a request flows through — the single value every paw touches.

    Inspired by Plug's conn, with two deliberate departures OCaml lets us make where the BEAM
    couldn't: TYPED assigns (no untyped map, no casts, no need for Plug's separate [private]
    store), and a MUTABLE-backed value with the same [conn -> conn] pipe API (a setter
    mutates in place and returns the same conn, so N paws don't allocate N records — safe
    under Eio: one conn per request, one fiber, never shared).

    The type is abstract: build the response only through the functions here. Building it
    (status/headers/cookies) does NOT answer — the pipeline keeps running; only an
    {e answerer} (a body, redirect, stream, halt, or upgrade) short-circuits the rest.

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
val make : Fennec_core.Http.request -> t

(** The request. *)
val req : t -> Fennec_core.Http.request

(** The buffered response the conn answered with, if any (the server reads this). *)
val resp : t -> Fennec_core.Http.response option

(** The status + headers with an empty body — for running before_send over a streamed or
    headers-only response. *)
val resp_skeleton : t -> Fennec_core.Http.response

(** The pending websocket-upgrade setup, if a paw requested one. *)
val upgrade_handler : t -> (Fennec_core.Ws_channel.t -> unit) option

(** The pending streamed response, if any. *)
val stream : t -> stream option

(** Apply all registered before_send hooks to a response (the server calls this once). *)
val apply_before_send : t -> Fennec_core.Http.response -> Fennec_core.Http.response

(** Has the conn answered? (a response, a halt, an upgrade, or a stream). The runner stops
    feeding paws once answered. *)
val answered : t -> bool

(** {1 Request readers} *)

(** The URL path (percent-decoded, without the query string). *)
val path : t -> string

(** The effective method (a method-override paw may have replaced it). *)
val meth : t -> Fennec_core.Http.meth

(** The [Host] header value (without port). Used for host-based routing. *)
val host : t -> string

(** ["http"] or ["https"] derived from the transport. *)
val scheme : t -> string

(** The client IP, as reported by the transport layer (may be a proxy address, not the browser). *)
val remote_ip : t -> string option

(** The HTTP version string (e.g. ["HTTP/1.1"] or ["HTTP/2"]). *)
val version : t -> string

(** A request header, case-insensitive (the first value if repeated). *)
val req_header : t -> string -> string option

(** All values of a (repeatable) request header, in order. *)
val req_headers : t -> string -> string list

(** Query params (parsed + percent-decoded lazily on first read, cached). *)
val query_params : t -> (string * string) list

(** A single query parameter value by name (case-sensitive). *)
val query : t -> string -> string option

(** Request cookies (parsed lazily). *)
val cookies : t -> (string * string) list

(** A single cookie value by name. *)
val cookie : t -> string -> string option

(** Form body fields ([application/x-www-form-urlencoded] or [multipart/form-data], parsed
    lazily by content type). *)
val body_params : t -> (string * string) list

(** A single form field value by name. *)
val body_param : t -> string -> string option

(** Uploaded file parts (multipart). *)
val files : t -> Fennec_core.Multipart.part list

(** An uploaded file part by form field name. *)
val file : t -> string -> Fennec_core.Multipart.part option

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

(** Set a response cookie (does not answer). Defaults: [path="/"], [http_only=true],
    [same_site=Lax]; [SameSite=None] implies [Secure]. *)
val set_cookie :
  t ->
  ?path:string ->
  ?domain:string ->
  ?max_age:int ->
  ?expires:float ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:Fennec_core.Cookie.same_site ->
  string ->
  string ->
  t

(** Expire a cookie now. *)
val delete_cookie : t -> ?path:string -> ?domain:string -> string -> t

(** Set the effective method (used by a method-override paw). *)
val override_method : t -> Fennec_core.Http.meth -> t

(** Set the captured path params (used by a :param/route). *)
val set_path_params : t -> (string * string) list -> t

(** Register a hook run on the final response just before sending (FIFO). The way a paw
    touches the RESPONSE (compression, security headers, logging) without answering. *)
val before_send : t -> (Fennec_core.Http.response -> Fennec_core.Http.response) -> t

(** {1 Answerers} — set a response and short-circuit the rest of the pipeline. *)

(** Answer with a full {!Fennec_core.Http.response} (pre-set headers are preserved; the
    answer's content-type wins). *)
val respond : t -> Fennec_core.Http.response -> t

(** Answer with a [text/plain] body. [status] defaults to 200. *)
val text : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with a [text/html; charset=utf-8] body. [status] defaults to 200. *)
val html : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with an [application/json] body. [status] defaults to 200. *)
val json : ?status:int -> ?headers:(string * string) list -> t -> string -> t

(** Answer with a Location header + a 3xx status (302 by default). *)
val redirect : ?status:int -> t -> string -> t

(** Stream a file from disk (content type defaults to the path's MIME type). *)
val send_file : t -> ?content_type:string -> path:string -> unit -> t

(** Stream a chunked (Transfer-Encoding: chunked) body: [produce emit] is run by the server,
    calling [emit] per chunk. Use content-type ["text/event-stream"] for SSE. *)
val send_chunked : t -> ?content_type:string -> ((string -> unit) -> unit) -> t

(** Answer by upgrading to a websocket; [setup] receives the live channel. *)
val upgrade : t -> (Fennec_core.Ws_channel.t -> unit) -> t

(** Explicitly halt with no response (rare; the server turns this into a 404). *)
val halt : t -> t
