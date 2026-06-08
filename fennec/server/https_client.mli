(** A minimal outbound HTTPS client for the ACME flow (the hunt HTTP client is test-only — the
    prod-lean guard forbids it in a server binary). One request per connection (Connection: close)
    over the same tls-eio + x509 + ca-certs the server already links. *)

(** An HTTP response: status code, headers (as received, order-preserving), and body. *)
type response = { status : int; headers : (string * string) list; body : string }

(** [header_get headers name] — the first header named [name] (case-insensitive). *)
val header_get : (string * string) list -> string -> string option

(** [request ~net ?authenticator ~meth ?headers ?body url] performs one HTTPS request. [url] must be
    [https://host[:port]/path]. [authenticator] defaults to the OS trust store (via ca-certs); a test
    against a local ACME server (pebble) passes its own. @raise Failure on a malformed URL,
    resolution failure, or TLS error. *)
val request :
  net:_ Eio.Net.t ->
  ?authenticator:X509.Authenticator.t ->
  meth:string ->
  ?headers:(string * string) list ->
  ?body:string ->
  string ->
  response
