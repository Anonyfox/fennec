(* The portable HTTP vocabulary — request/response as pure data (Stdlib only, no Eio), shared by the
   native server and any client/tooling. Constructors + the URL-codec helpers; semantics (caching,
   negotiation) live in {!Http_semantics}, header lookup in {!Headers}. *)

type meth = GET | POST | PUT | DELETE | PATCH | HEAD | OPTIONS | Other of string

val meth_of_string : string -> meth
(** Case-sensitive per RFC 9110 (["get"] is [Other "get"]). *)

val string_of_meth : meth -> string

val reason_phrase : int -> string
(** The standard reason phrase; an unknown code yields [""] (a legal empty phrase), never a wrong one. *)

type request = {
  meth : meth;
  path : string;  (** path only, no query string (raw, not percent-decoded) *)
  query_string : string;  (** the raw query, parsed lazily by the conn on demand *)
  headers : (string * string) list;
  body : string;
  host : string;  (** normalized Host without a port ([""] = absent) *)
  scheme : string;  (** "http" | "https" *)
  remote_ip : string option;  (** the peer's IP, when the transport knows it *)
  version : string;  (** "HTTP/1.1" etc. *)
}

val make_request :
  ?query_string:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?host:string ->
  ?scheme:string ->
  ?remote_ip:string option ->
  ?version:string ->
  meth:meth ->
  path:string ->
  unit ->
  request
(** Build a request; the connection metadata defaults sensibly so tests (and a client) need only the
    essentials. *)

type response = { status : int; headers : (string * string) list; body : string }

val respond : ?status:int -> ?headers:(string * string) list -> ?content_type:string -> string -> response
(** A response from a body ([content-type] prepended; default 200, [text/plain]). *)

val text : ?status:int -> ?headers:(string * string) list -> string -> response
val html : ?status:int -> ?headers:(string * string) list -> string -> response
val json : ?status:int -> ?headers:(string * string) list -> string -> response

val percent_decode : string -> string
(** Decode [%XX] escapes and ['+'] as space (the form-urlencoded convention). Allocation-free when
    there is nothing to decode; malformed escapes pass through literally. *)

val percent_encode : string -> string
(** Escape everything but the RFC 3986 unreserved set ([A-Za-z0-9-_.~]). *)

val parse_query : string -> (string * string) list
(** Parse ["a=1&b=two+words"] into percent-decoded pairs (a bare key yields [("k", "")]). *)

val split_target : string -> string * string
(** Split ["/path?a=1"] into [(path, raw_query)]. *)
