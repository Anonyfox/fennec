(** Meteor-shaped Accounts client facade for Fur + Pulse/DDP apps.

    The server owns authentication and exposes the canonical Accounts DDP methods through
    {!Fennec.Accounts.Methods}. This client module gives browser code the familiar pieces:
    {!user}, {!user_id}, {!logging_in}, {!login_with_password}, explicit resume-token login, logout,
    password lifecycle calls, and MFA step-up completion. All results decode the stable server
    payloads into typed variants so components do not pattern-match raw BSON documents. *)

(** Safe public email entry from the Accounts user document. *)
type email = {
  address : string;
  verified : bool;
}

(** Safe public Accounts user document. Password hashes and provider tokens are never present. *)
type user = {
  id : string;
  username : string option;
  emails : email list;
  roles : string list;
  profile : Bson.t option;
  status : string option;
  created_at : float option;
  updated_at : float option;
}

(** Canonical client session payload. *)
type session = {
  user_id : string option;
  user : user option;
}

(** Login selector accepted by the built-in Accounts ["login"] DDP method. *)
type selector =
  | By_id of string
  | By_email of string
  | By_username of string

(** Successful login-like result, or a pending MFA branch. *)
type login_result =
  | Logged_in of {
      id : string;
      token : string;
      user : user option;
    }
  | Mfa_required of {
      user_id : string;
      mfa_token : string;
    }

(** Stable client error from a DDP method rejection or local payload decode failure. *)
type error = {
  code : string;
  reason : string;
}

(** Accounts client state. *)
type t

(** [connect ?path ?persist ?chrome ?token_key ()] opens a DDP client and wraps it with Accounts
    state. [token_key] defaults to ["fennec.accounts.loginToken"]; successful DDP logins store the
    resume token there, [logout] clears it, and construction automatically attempts resume when a
    token exists. Pass [~token_key:None] for cookie-only clients. *)
val connect : ?path:string -> ?persist:string -> ?chrome:bool -> ?token_key:string option -> unit -> t

(** [of_ddp ?token_key ddp] wraps an existing DDP client. *)
val of_ddp : ?token_key:string option -> Ddp_client.t -> t

(** Install the process/page default Accounts client. Calling [set_default] again with the same
    value is harmless; a different client replaces the default for tests or deliberate reconnects. *)
val set_default : t -> unit

(** The process/page default Accounts client. It connects to the default DDP endpoint on first use,
    refreshes ["currentUser"], and thereafter behaves like Meteor's global Accounts state. *)
val default : unit -> t

(** The underlying DDP client, for app subscriptions/method calls. *)
val ddp : t -> Ddp_client.t

(** Current user document as a Fur signal, like Meteor's [Meteor.user()]. *)
val user : t -> user option Fur.signal

(** Current user id as a Fur signal, like Meteor's [Meteor.userId()]. *)
val user_id : t -> string option Fur.signal

(** Current user id on the default Accounts client, like Meteor's [Meteor.userId()]. *)
val current_user_id : unit -> string option Fur.signal

(** Current user document on the default Accounts client, like Meteor's [Meteor.user()]. *)
val current_user : unit -> user option Fur.signal

(** Whether the default Accounts client is currently running an Accounts operation. *)
val current_logging_in : unit -> bool Fur.signal

(** [true] while an Accounts login/resume/signup/step-up/logout method is in flight. *)
val logging_in : t -> bool Fur.signal

(** Last Accounts method/decode error. Cleared by the next successful Accounts operation. *)
val last_error : t -> error option Fur.signal

(** Refresh the current user/session payload from the server's ["currentUser"] method. *)
val refresh_user : t -> (session, error) result option Fur.signal

(** Login with username/email/id plus password. *)
val login_with_password : t -> selector -> password:string -> (login_result, error) result option Fur.signal

(** Resume with a previously returned login token. *)
val login_with_token : t -> string -> (login_result, error) result option Fur.signal

(** Create a password user. Mirrors the built-in ["createUser"] DDP method. *)
val create_user :
  t ->
  ?username:string ->
  ?email:string ->
  ?profile:Bson.t ->
  password:string ->
  unit ->
  (login_result, error) result option Fur.signal

(** Clear the server-side DDP user binding and local resume token. *)
val logout : t -> (unit, error) result option Fur.signal

(** Bump [auth_epoch] for other clients and install the replacement token on this client. *)
val logout_other_clients : t -> (login_result, error) result option Fur.signal

(** Change the password for the current user. *)
val change_password : t -> old_password:string -> new_password:string -> (unit, error) result option Fur.signal

(** Complete a password reset token. *)
val reset_password : t -> token:string -> password:string -> (login_result, error) result option Fur.signal

(** Complete an initial enrollment token. *)
val enroll_account : t -> token:string -> password:string -> (login_result, error) result option Fur.signal

(** Verify an email token and finish the login branch, including MFA-required results. *)
val verify_email : t -> token:string -> (login_result, error) result option Fur.signal

(** Complete pending MFA step-up with a TOTP enrollment id/code. *)
val complete_login_step_up_totp :
  t -> mfa_token:string -> totp_id:string -> code:string -> (login_result, error) result option Fur.signal

(** Complete pending MFA step-up with one backup code. *)
val complete_login_step_up_backup :
  t -> mfa_token:string -> user_id:string -> code:string -> (login_result, error) result option Fur.signal

(** Decode helpers exposed for tests and custom low-level clients. *)
val decode_user : Bson.t -> (user, string) result
val decode_session : Bson.t -> (session, string) result
val decode_login_result : Bson.t -> (login_result, string) result
