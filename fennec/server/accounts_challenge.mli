(** Short-lived, purpose-bound, single-use auth challenges.

    Challenges are the shared primitive behind magic links, OTP, email verification, password reset,
    OAuth/OIDC state+nonce, SAML request ids, passkey/WebAuthn ceremonies, MFA step-up, and recovery.
    The bearer token is returned to the caller once; stores receive only a hash of its secret part.

    A challenge token has the wire shape ["<id>.<secret>"]. The id is public lookup material. The
    secret is high-entropy bearer material and must be rendered only at the actual delivery boundary
    (email link, redirect state, WebAuthn challenge, etc.).

    {[
      let t = Accounts_challenge.make ~secret ~store:(Accounts_challenge.memory_store ()) () in
      match Accounts_challenge.create t ~purpose:Accounts_challenge.Password_reset () with
      | Ok issued ->
          Mailer.send (Accounts_challenge.token_to_string issued.token);
          Accounts_challenge.consume t ~purpose:Accounts_challenge.Password_reset issued.token
      | Error e -> Error e
    ]} *)

module Bson = Bson

(** A typed challenge purpose. Purpose separation prevents a token minted for one flow from being
    replayed in another flow. *)
type purpose =
  | Email_login
  | Email_verification
  | Password_reset
  | Passkey_registration
  | Passkey_assertion
  | OAuth_state
  | Oidc_state
  | Saml_request
  | Mfa_step_up
  | Recovery

(** Stable wire/debug name for a purpose. *)
val string_of_purpose : purpose -> string

(** Parse a purpose name produced by {!string_of_purpose}. *)
val purpose_of_string : string -> purpose option

(** Public metadata bound to a challenge.

    The metadata is intentionally generic but structured around auth flows: a challenge can be bound
    to a user, email, organization, provider/SSO connection, redirect target, and small extra values.
    Secrets do not belong here. *)
type metadata = {
  user_id : string option;
  email : string option;
  org_id : string option;
  connection_id : string option;
  redirect : string option;
  data : (string * Bson.t) list;
}

(** Empty metadata. *)
val empty_metadata : metadata

(** A stored challenge record. It contains no bearer secret and no secret hash. *)
type record = {
  id : string;
  purpose : purpose;
  metadata : metadata;
  created_at : float;
  expires_at : float;
  consumed_at : float option;
  revoked_at : float option;
  attempts : int;
  max_attempts : int option;
}

(** Bearer challenge token. Use {!token_to_string} only at wire/delivery boundaries. *)
type token = private string

(** Treat an incoming wire value as a challenge token. Verification still happens in {!consume}. *)
val token_of_string : string -> token

(** Render a token for email links, redirects, WebAuthn client data, etc. *)
val token_to_string : token -> string

(** A freshly issued challenge. *)
type issued = { token : token; record : record }

(** Challenge errors. *)
type error =
  | Invalid_token
  | Wrong_purpose
  | Expired
  | Already_consumed
  | Revoked
  | Too_many_attempts
  | Duplicate_id of string
  | Invalid_request of string
  | Store_error of string

(** Human-readable error text. *)
val string_of_error : error -> string

(** Store contract. Implementations must make [consume] atomic: purpose, hash, expiry,
    revoked/consumed state, attempt counters, and consumed-at must be checked/updated together. *)
type store = {
  insert : record -> secret_hash:string -> (unit, error) result;
  find : string -> (record option, error) result;
  consume : string -> purpose -> secret_hash:string -> now:float -> (record, error) result;
  revoke : string -> now:float -> (bool, error) result;
  revoke_user : ?purpose:purpose -> string -> now:float -> (int, error) result;
  revoke_email : ?purpose:purpose -> string -> now:float -> (int, error) result;
  gc_expired : now:float -> (int, error) result;
}

(** A process-local, mutex-guarded store for tests, examples, and single-process prototypes. *)
val memory_store : unit -> store

(** Challenge service configuration/state. *)
type t

(** Build a challenge service.

    [secret] hashes bearer token secrets before they enter the store and must be a long random
    string. [ttl] defaults to 10 minutes. [token_bytes] defaults to 32 bytes. [id_bytes] defaults to
    16 bytes. [now] exists for deterministic tests. *)
val make :
  secret:string ->
  store:store ->
  ?ttl:float ->
  ?token_bytes:int ->
  ?id_bytes:int ->
  ?now:(unit -> float) ->
  unit ->
  t

(** Create a new challenge. [ttl] overrides the service default for one challenge.
    [max_attempts] limits wrong-secret attempts for low-entropy flows such as OTP. Rare random id
    collisions are retried internally before [Duplicate_id] is returned. *)
val create :
  t ->
  purpose:purpose ->
  ?metadata:metadata ->
  ?ttl:float ->
  ?max_attempts:int ->
  unit ->
  (issued, error) result

(** Consume a token for [purpose]. On success the returned record has [consumed_at = Some _]. *)
val consume : t -> purpose:purpose -> token -> (record, error) result

(** Read a record by public id. This never exposes token material. *)
val find : t -> string -> (record option, error) result

(** Revoke one challenge by id. Returns [true] when an active record was changed. *)
val revoke : t -> string -> (bool, error) result

(** Revoke active challenges for a user, optionally restricted by purpose. *)
val revoke_user : t -> ?purpose:purpose -> string -> (int, error) result

(** Revoke active challenges for a normalized email, optionally restricted by purpose. *)
val revoke_email : t -> ?purpose:purpose -> string -> (int, error) result

(** Remove expired challenges. *)
val gc_expired : t -> (int, error) result
