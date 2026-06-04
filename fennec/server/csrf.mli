(** CSRF protection (Plug.CSRFProtection equivalent). A per-session secret guards
    state-changing requests: embed {!token} in a form (a fresh MASKED encoding each render,
    so the value differs every time — defeating BREACH-style oracles), and {!plug} rejects
    an unsafe request whose submitted token doesn't unmask to the session secret
    (constant-time). Requires {!Session.plug} earlier in the pipeline. *)

(** A masked, embeddable token — different on every call, all valid for the same session
    secret (which is minted on first use). *)
val token : Fennec_paw.Conn.t -> string

(** Whether a submitted token unmasks to the session secret (constant-time; no mutation). *)
val verify : Fennec_paw.Conn.t -> string -> bool

(** The CSRF plug: verify the token on unsafe methods (from the [header] or a body [field]),
    answer 403 on a missing/invalid token, decline on [safe] methods. *)
val plug : ?field:string -> ?header:string -> ?safe:string list -> unit -> Fennec_paw.Paw.t
