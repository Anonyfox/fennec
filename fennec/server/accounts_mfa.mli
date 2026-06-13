(** MFA and step-up authentication primitives.

    This module models assurance, fresh step-up requirements, TOTP, and backup codes. It deliberately
    does not persist enrolled factors, decide account policy, issue sessions, or mutate users. *)

module Challenge = Accounts_challenge

(** MFA helper state. *)
type t

(** MFA errors. *)
type error =
  | Invalid_config of string
  | Invalid_code
  | Code_mismatch
  | Replay
  | Insufficient_assurance
  | Stale_assurance
  | Invalid_state
  | Challenge_error of Challenge.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build an MFA helper.

    [secret] hashes backup/recovery codes before storage and must be a long random string. *)
val make : secret:string -> challenge:Challenge.t -> t

(** Authentication factor kind. *)
type factor =
  | Password
  | Email
  | OAuth
  | Oidc
  | Saml
  | Passkey
  | Totp
  | Backup_code
  | Recovery_code

(** Assurance level carried by a session or recent step-up. *)
type level =
  | Anonymous
  | Single_factor
  | Phishing_resistant_single_factor
  | Multi_factor
  | Phishing_resistant_multi_factor

(** Derive the assurance level for a set of verified factors. *)
val level_of_factors : factor list -> level

(** [satisfies ~required actual] is whether [actual] meets [required]. *)
val satisfies : required:level -> level -> bool

(** Verified assurance facts. *)
type assurance = {
  level : level;
  factors : factor list;
  authenticated_at : float;
}

(** Enrolled factor lifecycle. *)
type enrollment_status =
  | Pending
  | Active
  | Disabled

(** Stored MFA factor enrollment.

    [secret] is for factor-specific encrypted-or-app-protected material such as a TOTP secret.
    [backup_hashes] stores backup/recovery code hashes only. *)
type enrollment = {
  id : string;
  user_id : string;
  factor : factor;
  label : string option;
  status : enrollment_status;
  secret : string option;
  backup_hashes : string list;
  last_step : int64 option;
  created_at : float;
  confirmed_at : float option;
  disabled_at : float option;
}

(** Build assurance from verified factors. *)
val assurance : ?now:(unit -> float) -> factor list -> assurance

(** Route/action requirement. [max_age] bounds freshness in seconds when present. *)
type requirement = {
  level : level;
  max_age : float option;
}

(** Build a requirement. *)
val requirement : ?max_age:float -> level -> (requirement, error) result

(** Check whether current assurance satisfies a route/action requirement. *)
val require : ?now:(unit -> float) -> requirement -> assurance -> (unit, error) result

(** Single-use step-up challenge. *)
type step_up = {
  token : Challenge.token;
  record : Challenge.record;
  user_id : string;
  requirement : requirement;
}

(** Consumed step-up state. *)
type step_up_state = {
  user_id : string;
  requirement : requirement;
  data : (string * Bson.t) list;
  record : Challenge.record;
}

(** Issue a step-up challenge for [user_id].

    [data] carries signed challenge metadata for the caller's completion step. *)
val issue_step_up :
  t ->
  ?ttl:float ->
  ?redirect:string ->
  ?data:(string * Bson.t) list ->
  user_id:string ->
  requirement ->
  (step_up, error) result

(** Consume a step-up challenge after a second factor has verified. *)
val consume_step_up : t -> ?expected_user:string -> Challenge.token -> (step_up_state, error) result

(** TOTP configuration. *)
type totp = {
  secret : string;
  issuer : string option;
  account : string option;
  digits : int;
  period : int;
}

(** Generate a base32 TOTP secret. *)
val generate_totp_secret : ?bytes:int -> unit -> string

(** Build and validate TOTP configuration.

    [secret] is base32. [digits] defaults to 6 and must be 6-8. [period] defaults to 30 seconds. *)
val totp : ?issuer:string -> ?account:string -> ?digits:int -> ?period:int -> secret:string -> unit -> (totp, error) result

(** Build an otpauth URI for authenticator apps. *)
val provisioning_uri : totp -> string

(** Generate a TOTP code for [time]. *)
val totp_code : ?time:float -> totp -> string

(** Verify a TOTP code.

    [window] is the number of adjacent time steps accepted on each side. [last_step] rejects replay
    when the accepted step is not greater than the last stored step. The returned int64 is the
    accepted time step to persist. *)
val verify_totp : ?time:float -> ?window:int -> ?last_step:int64 -> totp -> code:string -> (int64, error) result

(** Generated backup codes and their storage hashes. *)
type backup_codes = {
  codes : string list;
  hashes : string list;
}

(** Generate user-visible backup codes and storage hashes. *)
val generate_backup_codes : t -> ?count:int -> ?bytes:int -> unit -> (backup_codes, error) result

(** Hash one backup/recovery code for storage. *)
val hash_code : t -> string -> string

(** Verify and consume one backup code hash.

    Returns the matched hash and the remaining hashes to persist. *)
val consume_backup_code : t -> hashes:string list -> code:string -> (string * string list, error) result

(** Build a normalized enrollment record. *)
val enrollment :
  ?now:(unit -> float) ->
  ?label:string ->
  ?status:enrollment_status ->
  ?secret:string ->
  ?backup_hashes:string list ->
  ?last_step:int64 ->
  ?confirmed_at:float ->
  ?disabled_at:float ->
  id:string ->
  user_id:string ->
  factor:factor ->
  unit ->
  (enrollment, error) result

(** Stored factor persistence. *)
type store = {
  find : string -> enrollment option;
  list : ?user_id:string -> ?factor:factor -> unit -> enrollment list;
  upsert : enrollment -> (unit, string) result;
  replace_if_current : current:enrollment -> enrollment -> (bool, string) result;
  delete : string -> (bool, string) result;
}

(** Mutex-guarded in-memory MFA enrollment store.

    Stores used in production must implement [replace_if_current] as an atomic compare-and-swap:
    replace the enrollment only when the stored record still matches [current]. Accounts uses this
    for replay-sensitive state such as TOTP counters and backup-code removal. *)
val memory_store : unit -> store
