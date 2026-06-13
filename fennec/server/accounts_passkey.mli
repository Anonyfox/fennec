(** Passkeys and WebAuthn ceremony verification.

    This module owns the Fennec-native WebAuthn server primitive: registration/assertion challenge
    state, client-data checks, authenticator-data parsing, COSE ES256 public-key extraction,
    signature verification, sign-counter policy, and typed credential facts for persistence. It does
    not choose users, persist credentials, merge identities, or issue Accounts session tokens.

    {[
      let t = Accounts_passkey.make ~challenge in
      let rp = Result.get_ok (Accounts_passkey.relying_party ~id:"app.example" ~name:"Acme" ()) in
      let u = Result.get_ok (Accounts_passkey.user ~id:"user-1" ~handle ~name:"ada" ()) in
      match Accounts_passkey.begin_registration t rp u with
      | Ok reg ->
          (* send reg.challenge to the browser; later verify the response *)
          Accounts_passkey.finish_registration t rp response ~token:reg.token ~user_id:"user-1"
      | Error e -> Error e
    ]} *)

module Challenge = Accounts_challenge
module Identity = Accounts_identity

(** Relying-party configuration. *)
type relying_party = {
  id : string;
  name : string;
  origins : string list;
  user_verification : bool;
}

(** Passkey helper state. *)
type t

(** Passkey errors. *)
type error =
  | Invalid_relying_party of string
  | Invalid_user of string
  | Invalid_state
  | Invalid_client_data of string
  | Invalid_attestation of string
  | Invalid_assertion of string
  | Unsupported_algorithm of string
  | Counter_rollback
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build a passkey helper around the shared challenge service. *)
val make : challenge:Challenge.t -> t

(** Build and validate relying-party policy.

    [id] is the WebAuthn RP ID, usually the registrable domain. [origins] are exact browser origins
    accepted for clientDataJSON. [user_verification] defaults to [false]; when [true], registration
    and assertion require the authenticator UV flag. *)
val relying_party :
  ?origins:string list -> ?user_verification:bool -> id:string -> name:string -> unit -> (relying_party, error) result

(** User facts needed to start registration. [handle] is the stable WebAuthn user handle. *)
type user = {
  id : string;
  handle : string;
  name : string;
  display_name : string;
}

(** Build and validate registration user facts. *)
val user : id:string -> handle:string -> ?display_name:string -> name:string -> unit -> (user, error) result

(** Challenge issued for a registration ceremony. *)
type registration = {
  challenge : string;
  token : Challenge.token;
  record : Challenge.record;
  rp : relying_party;
  user : user;
}

(** Challenge issued for an assertion ceremony. *)
type assertion_challenge = {
  challenge : string;
  token : Challenge.token;
  record : Challenge.record;
  rp : relying_party;
  user_id : string option;
  allowed_credentials : string list;
}

(** Create a single-use registration ceremony. *)
val begin_registration :
  t -> ?ttl:float -> ?redirect:string -> relying_party -> user -> (registration, error) result

(** Create a single-use assertion ceremony.

    [allowed_credentials] can be empty for discoverable-credential login. *)
val begin_assertion :
  t ->
  ?ttl:float ->
  ?user_id:string ->
  ?redirect:string ->
  ?allowed_credentials:string list ->
  relying_party ->
  (assertion_challenge, error) result

(** Persistable passkey credential facts. *)
type credential = {
  id : string;
  user_id : string;
  user_handle : string;
  public_key : X509.Public_key.t;
  sign_count : int32;
  backup_eligible : bool;
  backed_up : bool;
  transports : string list;
  created_at : float;
  last_used_at : float option;
}

(** Passkey credential store.

    Stores must enforce unique credential ids. [update] is used after successful assertions to
    persist the sign counter and last-used timestamp. *)
type store = {
  find : string -> credential option;
  list : ?user_id:string -> unit -> credential list;
  insert : credential -> (unit, string) result;
  update : credential -> (unit, string) result;
  delete : string -> (bool, string) result;
}

(** Mutex-guarded in-memory passkey credential store. *)
val memory_store : unit -> store

(** Browser registration response payload. *)
type registration_response = {
  id : string;
  raw_id : string;
  client_data_json : string;
  attestation_object : string;
  transports : string list;
}

(** Browser assertion response payload. *)
type assertion_response = {
  id : string;
  raw_id : string;
  client_data_json : string;
  authenticator_data : string;
  signature : string;
  user_handle : string option;
}

(** Verified assertion facts and updated sign counter. *)
type assertion = {
  credential : credential;
  user_present : bool;
  user_verified : bool;
  backup_eligible : bool;
  backed_up : bool;
}

(** Finish registration for the ["none"] attestation profile and ES256 credentials.

    The challenge is consumed only after client data, RP ID hash, flags, attested credential data, and
    COSE key are valid. *)
val finish_registration :
  t ->
  ?now:(unit -> float) ->
  relying_party ->
  registration_response ->
  token:Challenge.token ->
  user_id:string ->
  (credential, error) result

(** Finish assertion for ES256 credentials.

    The challenge is consumed only after client data, RP ID hash, flags, credential id, signature,
    and sign-counter policy are valid. *)
val finish_assertion :
  t ->
  ?now:(unit -> float) ->
  relying_party ->
  credential ->
  assertion_response ->
  token:Challenge.token ->
  (assertion, error) result

(** Canonical identity key for a passkey credential. *)
val identity : credential -> (Identity.key, error) result
