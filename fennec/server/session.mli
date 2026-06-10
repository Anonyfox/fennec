(** Sessions (Dream-grade). A small [string -> string] map per request for login state,
    flash messages, CSRF-related state, preferences, and other data that must survive across
    requests.

    Two stores: the default {b signed-cookie} store (stateless — the signed data rides in
    the cookie, so it scales horizontally for free) and an optional {b server-side} store
    ([?store]) where the cookie holds only a signed id. Either way a session has a
    [lifetime]: expired sessions load empty and a past-half-life session is auto-refreshed.
    Add {!make} early, read/write with {!get}/{!set} downstream. Constant-time verify.

    Use sessions for signed cookie-backed request-to-request state such as "remember the
    logged-in user". Use {!Fennec_paw.Conn.set_cookie} / {!Fennec_paw.Conn.delete_cookie} for a
    single unsessioned response cookie. *)

(** Sign a payload: ["<b64 payload>.<b64 hmac>"] (also usable as a generic signed token). *)
val sign : secret:string -> string -> string

(** Verify a signed token (constant-time), returning the payload if the signature holds. *)
val verify : secret:string -> string -> string option

(** A server-side session store (keyed by session id). Provide your own (Redis, SQL, …) or
    use {!memory_store}. *)
type store = {
  load : string -> (string * string) list option;
  save : string -> (string * string) list -> unit;
  delete : string -> unit;
}

(** A process-local, Mutex-guarded (domain-safe) in-memory store with a [ttl] (default 1 day)
    after which an idle session is evicted. *)
val memory_store : ?ttl:float -> unit -> store

(** Whether {!make} ran upstream on this conn (so {!get}/{!set} are meaningful). A dependent
    paw can use this to fail loudly on a misordered pipeline. *)
val active : Fennec_paw.Conn.t -> bool

(** A session value, if {!make} ran and the key is set. Typical login checks read a user id
    or account id here in downstream handlers or matched-route middleware. *)
val get : Fennec_paw.Conn.t -> string -> string option

(** The session map (reserved ["_"]-prefixed keys hidden). *)
val get_all : Fennec_paw.Conn.t -> (string * string) list

(** Set a value (returns the same conn, for piping). Use after successful login or any handler
    that changes request-to-request state; the session paw writes the refreshed signed cookie at
    the end of the response. *)
val set : Fennec_paw.Conn.t -> string -> string -> Fennec_paw.Conn.t

(** Remove one key. *)
val delete : Fennec_paw.Conn.t -> string -> Fennec_paw.Conn.t

(** Empty the session. *)
val clear : Fennec_paw.Conn.t -> Fennec_paw.Conn.t

(** The session paw. [secret] signs the cookie; [lifetime] is the max age (seconds, default
    1 day). With [store], the cookie holds a signed id and the data lives server-side;
    without it, the cookie holds the signed data. [secure] defaults to whether the request
    is https; the cookie is HttpOnly + SameSite=Lax by default.

    Put this paw before handlers that call {!get}, {!set}, {!delete}, or {!clear}. It only
    enables the session; it does not require a login by itself. Pair it with an auth paw or
    handler logic when protecting routes. *)
val make :
  secret:string ->
  ?cookie:string ->
  ?path:string ->
  ?lifetime:float ->
  ?same_site:Fennec_core.Cookie.same_site ->
  ?http_only:bool ->
  ?secure:bool ->
  ?store:store ->
  unit ->
  Fennec_paw.Paw.t
