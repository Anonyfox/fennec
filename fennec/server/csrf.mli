(** CSRF protection (Dream.csrf-grade). A per-session secret guards state-changing requests.
    {!token} returns a value that is MASKED fresh each render (BREACH-safe), carries an
    EXPIRY, and is HMAC-signed with the app secret; {!verify} returns a distinguishable
    {!outcome}; {!make} rejects unsafe requests whose token isn't [Ok]. Constant-time
    throughout. Requires {!Session.make} earlier in the pipeline. *)

(** Why a token did or didn't validate. [Expired]/[Wrong_session] occur in normal use (an
    aged-out form or session); [Invalid] indicates a bad signature or forged payload. *)
type outcome = Ok | Expired | Wrong_session | Invalid

(** A fresh, embeddable token signed with the app [secret], valid for [valid_for] seconds
    (default 3600). Mints the session secret on first use. *)
val token : secret:string -> ?valid_for:float -> Fennec_paw.Conn.t -> string

(** Validate a submitted token against the app [secret] and the session secret. *)
val verify : secret:string -> Fennec_paw.Conn.t -> string -> outcome

(** The CSRF paw: verify the token on unsafe methods (from the [header] or a body [field]),
    answer 403 unless it is [Ok], decline on [safe] methods. *)
val make :
  secret:string -> ?field:string -> ?header:string -> ?safe:string list -> unit -> Fennec_paw.Paw.t
