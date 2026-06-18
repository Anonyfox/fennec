(** The ambient outbound-HTTPS transport the provider presets use for token-exchange / profile /
    JWKS / OIDC-discovery calls.

    This is the seam that lets [Accounts.OAuth.github ~client_id ~client_secret ()] be a true
    one-liner: the preset's default exchange runs inside a request handler (which only sees a
    [Conn.t], with no Eio [net]), so the network handle is captured once at boot — exactly like
    {!Fennec_mail.boot} captures it for SMTP — and read back here when an exchange fires.

    The default transport is {!Paw.Https_client} (the same prod-safe tls-eio + x509 + ca-certs stack
    the server already links; the OS trust store authenticates the peer, never downgraded). Until
    {!set_transport} installs it, {!transport} is fail-closed: every call returns an [Error] rather
    than attempting an unauthenticated connection. *)

(** An outbound HTTP response. Reuses {!Paw.Https_client.response}: status, response headers
    (case-insensitive lookup via {!Paw.Https_client.header_get}), and the body. *)
type response = Paw.Https_client.response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

(** A minimal outbound-HTTP transport: a method, an absolute URL, request headers, and a body, to a
    response or a transport-level error string. Tests pass a stub; production uses {!default}. *)
type transport = meth:string -> url:string -> headers:(string * string) list -> body:string -> (response, string) result

(** Build the production transport over {!Paw.Https_client} bound to an Eio [net]. [authenticator]
    overrides the default OS-trust-store authenticator (for a local test server with its own CA);
    production omits it so the peer is verified against the system trust store. *)
val default : net:_ Eio.Net.t -> ?authenticator:X509.Authenticator.t -> unit -> transport

(** Install the process-ambient transport. {!Fennec.serve} calls this at boot with
    [default ~net ()]; an app or test may install a stub. The last call wins. *)
val set_transport : transport -> unit

(** The process-ambient transport, or a fail-closed transport (every call [Error]s) when none has
    been installed. Reading it never raises, so a preset exchange can call it unconditionally and
    map a missing-transport error to a normal login failure. *)
val transport : unit -> transport

(** A response header by name (case-insensitive); re-exported from {!Paw.Https_client.header_get}. *)
val header_get : (string * string) list -> string -> string option

(** [form_encode pairs] builds an [application/x-www-form-urlencoded] body. *)
val form_encode : (string * string) list -> string
