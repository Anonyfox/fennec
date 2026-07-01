(** SCRAM-SHA-256 server side (RFC 5802 / RFC 7677) — MongoDB's default auth mechanism, as a pure crypto
    state machine. No IO and no randomness: the server layer supplies the random server nonce (and the
    random salt at registration) and wraps these messages in the saslStart/saslContinue envelope.
    Validated against the published RFC 7677 test vector, so a real mongosh client interoperates.
    Stores no plaintext password — only (salt, iterations, StoredKey, ServerKey). SASLprep is identity
    for ASCII passwords and is not applied; non-ASCII passwords must be pre-normalised. *)

(** Raised on an unknown user, a nonce mismatch, an unexpected channel binding, or a bad password — the
    server maps it to a Mongo authentication-failure reply. The message is intentionally generic for the
    user-facing cases ("authentication failed") so it leaks neither which users exist nor why. *)
exception Auth_failed of string

(** A user's stored SCRAM-SHA-256 verifier. No plaintext password is recoverable from it. *)
type credential = { salt : string; iterations : int; stored_key : string; server_key : string }

val make_credential : salt:string -> iterations:int -> password:string -> credential
(** [make_credential ~salt ~iterations ~password] derives a verifier via PBKDF2-HMAC-SHA256. Run once,
    at registration, with a random [salt] (>= 16 bytes recommended) and [iterations] >= 4096. *)

(** Conversation state carried from {!server_first} to {!server_final}. *)
type t

val server_first :
  lookup:(string -> credential option) -> server_nonce:string -> string -> string * string * t
(** [server_first ~lookup ~server_nonce client_first] returns [(username, server_first_msg, state)].
    [server_nonce] must be fresh random. Raises {!Auth_failed} for an unknown user. *)

val server_final : t -> string -> string
(** [server_final state client_final] verifies the client's proof and returns the server-final message
    ([v=...]) proving the server also knows the password (mutual auth). Raises {!Auth_failed}. *)

(** {2 Client side} — for a follower / tool authenticating TO a SCRAM server. Pure: the caller supplies the
    random client nonce and drives the saslStart / saslContinue envelope. *)

val client_first_bare : user:string -> nonce:string -> string
(** ["n=<user>,r=<nonce>"] — the client-first-bare; send ["n,," ^ result] as the saslStart payload. *)

val client_final : password:string -> client_first_bare:string -> server_first:string -> string * string
(** From the password, our [client_first_bare], and the server's [server_first] reply, returns
    [(client_final_message, expected_server_signature)] — send the message as the saslContinue payload, then
    pass the signature to {!verify_server_final}. Raises {!Auth_failed} on a malformed server-first. *)

val verify_server_final : expected_server_sig:string -> string -> bool
(** Check the server-final [v=...] against the expected signature — the server proving it too knows the
    password (mutual auth). *)
