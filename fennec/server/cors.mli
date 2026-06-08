(** CORS — Cross-Origin Resource Sharing. A paw that answers preflight ([OPTIONS] +
    [Access-Control-Request-Method]) with [204] + the negotiated headers, and stamps actual responses
    with [Access-Control-Allow-Origin] (and, when configured, credentials / exposed headers). A
    request without an [Origin] header is not a CORS request and passes through untouched. *)

(** Which origins to allow: {!Any}, or an explicit allowlist (reflected back when matched; a
    non-matching origin gets no CORS headers, so the browser blocks it). *)
type origin = Any | These of string list

(** [make ?origins ?methods ?headers ?expose ?credentials ?max_age ()] builds the CORS paw.
    [origins] defaults to {!Any}; [credentials] forces the concrete origin to be reflected (since
    ["*"] is illegal with credentials) and adds [Access-Control-Allow-Credentials]; [methods] /
    [headers] populate the preflight allow-lists; [expose] sets [Access-Control-Expose-Headers];
    [max_age] (seconds) caches the preflight. *)
val make :
  ?origins:origin ->
  ?methods:string list ->
  ?headers:string list ->
  ?expose:string list ->
  ?credentials:bool ->
  ?max_age:int ->
  unit ->
  Fennec_paw.Paw.t
