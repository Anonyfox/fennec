(** A minimal, correct SMTP submission client over Eio + tls-eio — the same outbound TLS stack the rest
    of the server uses ({!Fennec_server.Https_client}), no Lwt and no new C deps.

    It does exactly what transactional submission needs: greeting, [EHLO], opportunistic or implicit TLS,
    ESMTP [AUTH PLAIN]/[AUTH LOGIN], multi-recipient envelope, and a dot-stuffed [DATA] body. It is the one
    real protocol module of {!Fennec_mail}; the message bytes come from {!Mail_mime}. *)

type security =
  | Plaintext  (** no TLS — local dev catchers (Mailpit/MailHog) and the test server *)
  | Starttls  (** [smtp://] — upgrade via STARTTLS when the server advertises it, else stay plaintext *)
  | Implicit_tls  (** [smtps://] — TLS wraps the socket before the greeting (port 465) *)

type config = {
  host : string;
  port : int;
  security : security;
  user : string option;  (** ESMTP AUTH username (creds present ⇒ AUTH is attempted) *)
  pass : string option;
  ehlo_domain : string;  (** the name announced in EHLO (typically the sender's domain) *)
}

(** [send ~sw ~net config ~envelope_from ~recipients ~data] submits one already-serialized message
    ([data], from {!Mail_mime.serialize}) to every address in [recipients] (the bare To+Cc+Bcc emails)
    with [envelope_from] as the return path. Returns [Error msg] on any connection / TLS / protocol /
    rejection failure — it never raises. TLS peers are verified against the OS trust store (ca-certs). *)
val send :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  config ->
  envelope_from:string ->
  recipients:string list ->
  data:string ->
  (unit, string) result
