(** Signed-cookie sessions (Plug.Session's default store). A small [string -> string]
    map is serialized into a cookie and HMAC-SHA256 signed with a server secret, so the
    client can read but not tamper with it (signed, not encrypted — do not store secrets
    in it). Verification is constant-time.

    Add {!plug} early in a pipeline, then read/write with {!get}/{!set} downstream. The
    plug loads + verifies the cookie inbound and, via a before_send hook, re-signs + resets
    it outbound — only when the session changed. *)

(** Sign a payload: ["<b64 payload>.<b64 hmac>"]. (Also usable as a generic signed token.) *)
val sign : secret:string -> string -> string

(** Verify a signed token (constant-time), returning the payload if the signature holds. *)
val verify : secret:string -> string -> string option

(** A session value, if {!plug} ran and the key is set. *)
val get : Fennec_paw.Conn.t -> string -> string option

(** The whole session map. *)
val get_all : Fennec_paw.Conn.t -> (string * string) list

(** Set a session value (returns the same conn, for piping). *)
val set : Fennec_paw.Conn.t -> string -> string -> Fennec_paw.Conn.t

(** Remove one key. *)
val delete : Fennec_paw.Conn.t -> string -> Fennec_paw.Conn.t

(** Empty the session. *)
val clear : Fennec_paw.Conn.t -> Fennec_paw.Conn.t

(** The session plug. [secret] signs the cookie (keep it secret + stable). The cookie is
    HttpOnly + SameSite=Lax by default; [secure] defaults to whether the request is https. *)
val plug :
  secret:string ->
  ?cookie:string ->
  ?path:string ->
  ?max_age:int ->
  ?same_site:Fennec_core.Cookie.same_site ->
  ?http_only:bool ->
  ?secure:bool ->
  unit ->
  Fennec_paw.Paw.t
