(** HTTP cookies (RFC 6265): pure parsing of the request [Cookie] header and
    serialization of [Set-Cookie] response headers. Values are verbatim — any
    encoding is the caller's choice. *)

(** The [SameSite] attribute. [None_] (wire value ["None"]) implies [Secure]. *)
type same_site = Strict | Lax | None_

(** Serialize to the wire value: [Strict] → ["Strict"], [Lax] → ["Lax"], [None_] → ["None"]. *)
val same_site_to_string : same_site -> string

(** Parse a request [Cookie] header value into [name=value] pairs (quotes stripped,
    whitespace trimmed). *)
val parse_header : string -> (string * string) list

(** Serialize one [Set-Cookie] value. [path] defaults to ["/"], [http_only] to [true],
    [same_site] to [Lax]; [SameSite=None] forces [Secure]. [max_age] is in seconds;
    [expires] is epoch seconds. *)
val to_set_cookie :
  name:string ->
  value:string ->
  ?path:string ->
  ?domain:string ->
  ?max_age:int ->
  ?expires:float ->
  ?secure:bool ->
  ?http_only:bool ->
  ?same_site:same_site ->
  unit ->
  string
