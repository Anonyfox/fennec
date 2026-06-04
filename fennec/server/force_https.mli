(** Force HTTPS — redirects plain-http requests to https, honouring an upstream
    [X-Forwarded-Proto] from a TLS-terminating proxy. Already-https requests pass through. *)

(** Build the force-https paw. [status] (default 308 — permanent and method/body preserving,
    so a redirected POST is not silently turned into a GET) is the redirect status. [hsts],
    if given, is a [Strict-Transport-Security] max-age (seconds) emitted on already-secure
    responses (with [includeSubDomains]) so the browser upgrades future requests itself. *)
val make : ?status:int -> ?hsts:int -> unit -> Fennec_paw.Paw.t
