(** HTTP Bearer-token auth (RFC 6750) — answers 401 with a [WWW-Authenticate: Bearer] challenge
    unless the request carries an [Authorization: Bearer <token>] that [verify] accepts. The standard
    API-auth guard (paw's {!Basic_auth} is for browser prompts; this is for tokens / API keys).

    For protected routes attach it with {!Endpoint.use_matched}/{!Endpoint.pipe_matched} (guards real
    matches, keeps unrelated paths a 404); use the always phase to require a token on every request.

    {[ endpoint |> Endpoint.pipe_matched [ Bearer_auth.make ~verify:(fun t -> Db.valid_token t) () ] ]} *)

(** Build the bearer-auth paw. [verify token] decides validity — do the lookup there (it may do IO),
    and compare secrets in constant time to avoid a timing oracle. [realm] (default ["Restricted"])
    is reported in the challenge. *)
val make : verify:(string -> bool) -> ?realm:string -> unit -> Pipeline.t
