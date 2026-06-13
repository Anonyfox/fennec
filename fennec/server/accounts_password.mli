(** Password hashing and policy primitives for Accounts.

    The high-level account/session operations remain on {!Fennec.Accounts}: [create_user],
    [login_with_password], and [set_password]. This module owns the reusable password-specific
    pieces those flows need: secure hashers and deterministic password policy checks. *)

(** A password hasher.

    Production applications can provide an Argon2id/scrypt/bcrypt hasher with this shape. Fennec's
    built-in hasher is dependency-light PBKDF2-HMAC-SHA256. *)
type hasher = {
  hash : password:string -> string;
  verify : password:string -> hash:string -> bool;
}

(** Built-in PBKDF2-HMAC-SHA256 hasher.

    [iterations] defaults to [210_000]. Hashes are encoded as
    [pbkdf2-sha256$iterations$salt$derived]. *)
val password_hasher : ?iterations:int -> unit -> hasher

(** Alias for callers that prefer the shorter noun. *)
val hasher : ?iterations:int -> unit -> hasher

(** Password policy validation errors. *)
type validation_error =
  | Too_short of int
  | Too_long of int
  | Missing_lowercase
  | Missing_uppercase
  | Missing_digit
  | Missing_symbol
  | Contains_email
  | Contains_username
  | Banned

(** Human-readable validation error text. *)
val string_of_validation_error : validation_error -> string

(** Concatenate validation errors into one user-facing sentence. *)
val describe_errors : validation_error list -> string

(** Password policy.

    The record is public so applications can inspect it, but construct values through {!policy},
    {!default_policy}, or {!strict_policy} to keep invariants checked. *)
type policy = {
  min_length : int;
  max_length : int option;
  require_lowercase : bool;
  require_uppercase : bool;
  require_digit : bool;
  require_symbol : bool;
  reject_email : bool;
  reject_username : bool;
  banned : string list;
}

(** Build a policy.

    Defaults are intentionally humane: minimum 8 characters, maximum 1024 characters, reject the
    submitted email/username when provided, reject a small built-in list of common bad passwords, and
    do not require character-class gymnastics. Raises [Invalid_argument] for impossible bounds. *)
val policy :
  ?min_length:int ->
  ?max_length:int option ->
  ?require_lowercase:bool ->
  ?require_uppercase:bool ->
  ?require_digit:bool ->
  ?require_symbol:bool ->
  ?reject_email:bool ->
  ?reject_username:bool ->
  ?banned:string list ->
  unit ->
  policy

(** Default password policy. *)
val default_policy : policy

(** Stricter ready-made policy: 12+ chars with lowercase, uppercase, digit, and symbol. *)
val strict_policy : policy

(** Validate a password against a policy.

    [email] and [username] are optional because not every flow has them. They are normalized with
    trimming/lowercasing before substring checks. *)
val validate :
  ?email:string ->
  ?username:string ->
  ?policy:policy ->
  string ->
  (unit, validation_error list) result

(** [true] when {!validate} accepts the password. *)
val is_valid : ?email:string -> ?username:string -> ?policy:policy -> string -> bool
