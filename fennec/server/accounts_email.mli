(** Email ownership, verification, magic links, and OTP helpers.

    This module is persistence- and transport-neutral. It normalizes email addresses, builds email
    identity keys, and wraps {!Accounts_challenge} for email verification, magic-link login, and
    one-time-code ceremonies. It does not send mail and it does not issue Accounts session tokens.

    {[
      let t = Accounts_email.make ~secret ~challenge in
      match Accounts_email.normalize " ADA@Example.COM " with
      | Error e -> prerr_endline (Accounts_email.string_of_error e)
      | Ok address -> (
          match Accounts_email.issue_login_link t (Accounts_email.binding address) with
          | Ok issued ->
              Mailer.send address (Accounts_challenge.token_to_string issued.token)
          | Error e -> prerr_endline (Accounts_email.string_of_error e))
    ]} *)

module Challenge = Accounts_challenge
module Identity = Accounts_identity

(** Normalized email address. *)
type address = private string

(** Email helper configuration. [secret] is used only to hash OTP codes into challenge metadata. *)
type t

(** Email errors. *)
type error =
  | Invalid_email of string
  | Invalid_token
  | Email_mismatch
  | Otp_mismatch
  | Invalid_otp_code
  | Invalid_otp_config of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** Build an email helper. [secret] must be a long random string. *)
val make : secret:string -> challenge:Challenge.t -> t

(** Normalize and validate an email address. Fennec lowercases addresses for account identity. *)
val normalize : string -> (address, error) result

(** Render a normalized address. *)
val address_to_string : address -> string

(** Build a normalized email identity key. *)
val identity : verified:bool -> address -> (Identity.key, error) result

(** Public challenge binding fields used by email ceremonies. *)
type binding = {
  email : address;
  user_id : string option;
  org_id : string option;
  connection_id : string option;
  redirect : string option;
}

(** Build binding metadata. *)
val binding :
  ?user_id:string ->
  ?org_id:string ->
  ?connection_id:string ->
  ?redirect:string ->
  address ->
  binding

(** Convert email binding to challenge metadata. *)
val metadata : ?data:(string * Bson.t) list -> binding -> Challenge.metadata

(** A challenge issued for email delivery. *)
type issued = {
  token : Challenge.token;
  record : Challenge.record;
  binding : binding;
}

(** Issue an email-verification challenge.

    Deliver {!Challenge.token_to_string} of [token] in an email link. Consuming the token proves
    control of the mailbox, not login by itself. *)
val issue_verification : t -> ?ttl:float -> binding -> (issued, error) result

(** Consume an email-verification challenge.

    [expected] prevents accidental cross-use when the caller already knows which address is being
    verified. A mismatch is rejected before consuming the challenge. *)
val consume_verification : t -> ?expected:address -> Challenge.token -> (Challenge.record, error) result

(** Issue a magic-link login challenge.

    Deliver {!Challenge.token_to_string} of [token] in an email link. The caller decides how a
    consumed challenge maps to a user/session. *)
val issue_login_link : t -> ?ttl:float -> binding -> (issued, error) result

(** Consume a magic-link login challenge. *)
val consume_login_link : t -> ?expected:address -> Challenge.token -> (Challenge.record, error) result

(** OTP challenge issued for email delivery.

    [code] is the only value that should be emailed. [token] is challenge state that should stay in
    the browser/session/form flow and be presented back with the code. *)
type otp = {
  token : Challenge.token;
  code : string;
  record : Challenge.record;
  binding : binding;
}

(** Issue an email OTP login challenge.

    [digits] defaults to 6. OTP attempts must still be rate-limited by the caller; wrong-code checks
    happen before challenge consumption so a typo does not burn the challenge. *)
val issue_otp : t -> ?ttl:float -> ?digits:int -> binding -> (otp, error) result

(** Consume an OTP challenge using the browser/session token plus emailed code. *)
val consume_otp : t -> token:Challenge.token -> code:string -> (Challenge.record, error) result
