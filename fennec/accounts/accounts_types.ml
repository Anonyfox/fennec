(* accounts_types.ml — the Accounts data model: the user/session/error/store/config types, the
   shared module aliases, the request-local Assigns keys, [default_config], and the trivial pure
   helpers (token coercion, status<->string, query encoding). Every other engine module opens this.
   Carved verbatim from accounts_base.ml. *)

module Conn = Paw.Conn
module Paw = Paw
module Assigns = Paw.Assigns
module H = Paw.Http
module Cookie = Paw.Cookie
module Session = Paw.Session (* signed-cookie session middleware (moved into fennec-paw) *)
module Bson = Bson
module Bson_json = Fennec_mongo_bson_json.Bson_json
module Json = Fennec_mongo_json.Json
module Mongo_runtime = Fennec_mongo_driver.Runtime
module Backend_dyn = Fennec_mongo_dynamic.Dynamic (* the runtime-selected storage backend (minimongo / Burrow / mongod) behind one Backend.S *)
module Backend_seam = Fennec_mongo_backend (* the pure seam — for its shared [query] constructor *)
module Identity = Accounts_identity
module Challenge = Accounts_challenge
module Throttle = Accounts_rate_limit (* brute-force rate limiting for the [login]/[createUser] methods *)
module Password = Accounts_password
module Email = Accounts_email
module OAuth = Accounts_oauth
module Oidc = Accounts_oidc
module Saml = Accounts_saml
module Passkey = Accounts_passkey
module Mfa = Accounts_mfa
module Org = Accounts_org
module Scim = Accounts_scim
module Roles = Accounts_roles
module Audit = Accounts_audit

type user_id = string
type email = { address : string; verified : bool }
type user_status =
  | Active
  | Suspended
  | Disabled
  | Deleted

type user = {
  id : user_id;
  username : string option;
  emails : email list;
  roles : Roles.Role.t list;
  profile : Bson.t option;
  services : (string * Bson.t) list;
  created_at : float;
  updated_at : float;
  auth_epoch : int;
  status : user_status;
}

type selector = By_id of user_id | By_email of string | By_username of string
type token = string

let token_of_string s = s
let token_to_string t = t

type auth_context = {
  user_id : user_id;
  session_id : string;
  strategy : string;
  factors : Mfa.factor list;
  issued_at : float;
  expires_at : float;
  auth_epoch : int;
}

type org_context = {
  org : Org.org;
  membership : Org.membership option;
}

type password_hasher = Password.hasher

type error =
  | User_not_found
  | Duplicate_email of string
  | Duplicate_username of string
  | Invalid_password
  | Password_not_configured
  | Strategy_not_found of string
  | Login_rejected of string
  | Invalid_user of string
  | Invalid_token
  | Store_error of string

let string_of_error = function
  | User_not_found -> "User not found"
  | Duplicate_email e -> "Email already exists: " ^ e
  | Duplicate_username u -> "Username already exists: " ^ u
  | Invalid_password -> "Incorrect password"
  | Password_not_configured -> "Password login is not configured"
  | Strategy_not_found s -> "Login strategy not found: " ^ s
  | Login_rejected r -> r
  | Invalid_user s -> s
  | Invalid_token -> "Invalid login token"
  | Store_error s -> s

type user_store = {
  find_user_by_id : user_id -> (user option, error) result;
  find_user_by_email : string -> (user option, error) result;
  find_user_by_username : string -> (user option, error) result;
  find_user_by_service : strategy:string -> service_id:string -> (user option, error) result;
  create_user : user -> password_hash:string option -> (user, error) result;
  update_user : user -> (user, error) result;
  password_hash : user_id -> (string option, error) result;
  set_password_hash : user_id -> string -> (unit, error) result;
  set_password_hash_and_bump : user_id -> string -> (int, error) result;
  bump_auth_epoch : user_id -> (int, error) result;
}

(* Public metadata for one live row in the login-token store. The hashed token itself is never
   exposed here — only the non-secret [session_id] (the [sid] already carried in every signed
   session) and timestamps. *)
type session_info = {
  session_id : string;
  user_id : user_id;
  created_at : float;
  expires_at : float;
  last_active_at : float;
  strategy : string;
}

(* Server-side login-token store ("active sessions"). Only HASHED tokens are persisted; the [sid] is
   the row key ([_id]). This is the source of truth for per-session revocation on the gated paths
   (resume / [verify_token] / opt-in per request). Time-sensitive ops take [~now] so callers (and
   tests) control the clock; expired rows read as absent. *)
type token_store = {
  record : session_info -> hashed:string -> (unit, error) result;
  find_live : sid:string -> hashed:string -> now:float -> (session_info option, error) result;
  list_for_user : user_id -> now:float -> (session_info list, error) result;
  touch : sid:string -> now:float -> (unit, error) result;
  revoke : sid:string -> (bool, error) result;
  revoke_user : user_id -> ?keep:string -> unit -> (int, error) result;
  gc_expired : now:float -> (int, error) result;
}

type store = {
  users : user_store;
  tokens : token_store;
  identities : Identity.store;
  challenges : Challenge.store;
  passkeys : Passkey.store;
  orgs : Org.store;
  mfa : Mfa.store;
  scim : Scim.store;
  audit : Audit.store;
  ensure_indexes : unit -> unit;
}

type login_attempt = { strategy : string; user : user option; allowed : bool; reason : string option }
type strategy = { name : string; login : credentials:Bson.t -> (user, error) result }

type external_identity = {
  key : Identity.key;
  email : string option;
  email_verified : bool;
  username : string option;
  profile : Bson.t option;
  service : (string * Bson.t) option;
}

type identity_login = {
  user : user;
  token : token;
  created : bool;
  linked : Identity.link option;
}

type mfa_totp_setup = {
  enrollment : Mfa.enrollment;
  totp : Mfa.totp;
  provisioning_uri : string;
}

type mfa_backup_setup = {
  enrollment : Mfa.enrollment;
  codes : string list;
}

type mfa_verification = {
  user_id : user_id;
  assurance : Mfa.assurance;
}

type passkey_registration_options = {
  registration : Passkey.registration;
  json : string;
}

type passkey_assertion_options = {
  assertion : Passkey.assertion_challenge;
  json : string;
}

type passkey_registration_finish = {
  credential : Passkey.credential;
  link : Identity.link;
}

type org_invite = {
  invite : Org.invite;
  token : string;
}

type password_reset = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

type enrollment = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

type login_step_up = {
  user : user;
  step_up : Mfa.step_up;
}

type login_completion =
  | Complete_login of user * token
  | Step_up_required of login_step_up

type identity_login_completion =
  | Complete_identity_login of identity_login
  | Identity_step_up_required of login_step_up

type session = {
  uid : user_id;
  sid : string;
  iat : float;
  exp : float;
  auth_epoch : int;
  strategy : string;
  factors : Mfa.factor list;
}

(* Central authentication policy — the typed twin of Meteor's [Accounts.config({...})]. [default_config]
   is SECURE-by-default (enumeration resistance on, sane token lifetimes); an app tunes it via [make
   ~config] or {!configure}. *)
type config = {
  require_verified_email : bool;
      (* a user must have at least one verified email before any session is issued (every strategy) *)
  forbid_client_account_creation : bool;
      (* the client [createUser] DDP method is rejected; server-side [create_user] still works (server-only signup) *)
  ambiguous_error_messages : bool;
      (* password login returns ONE indistinguishable error for unknown-account vs wrong-password (the
         on_login_failure hooks + the audit log still see the real reason) — pairs with the enumeration
         timing defense to close the login enumeration oracle. ON by default (secure); set [false] for
         Meteor-style specific errors. (Signup still reports a taken username/email — a UX necessity.) *)
  auto_send_verification_email : bool;
      (* auto-send a verification email when the client [createUser] DDP method creates a user with an
         email — Meteor's [sendVerificationEmail] config. Off by default (Meteor parity). Best-effort:
         a delivery failure never blocks signup. *)
  reset_token_lifetime : float;
      (* password-reset token TTL in seconds; default 3 days (Meteor's passwordResetTokenExpirationInDays) *)
  enroll_token_lifetime : float;
      (* enrollment-invite token TTL in seconds; default 30 days (Meteor's passwordEnrollTokenExpiration) *)
  verify_token_lifetime : float;
      (* email-verification token TTL in seconds; default 3 days *)
}

let default_config =
  {
    require_verified_email = false;
    forbid_client_account_creation = false;
    ambiguous_error_messages = true;
    auto_send_verification_email = false;
    reset_token_lifetime = 3. *. 86_400.;
    enroll_token_lifetime = 30. *. 86_400.;
    verify_token_lifetime = 3. *. 86_400.;
  }

type t = {
  secret : string;
  store : store;
  password_hasher : password_hasher option;
  password_policy : Password.policy option;
  policy : Roles.policy;
      (* the app's role→permission map — code-declared, immutable, held once. [can]/[require_permission]
         read it directly so callers never thread a [~policy] argument. The single RBAC policy: org
         permission checks (see [can_in]) resolve against it too. *)
  mutable config : config;
  mutable email_templates : Accounts_mailer.templates option;
  cookie : string;
  path : string;
  lifetime : float;
  validate_every_request : bool;
  mutable validate_login_hooks : (login_attempt -> (unit, string) result) list;
  mutable create_user_hooks : (user -> (user, string) result) list;
  mutable login_hooks : (user -> unit) list;
  mutable logout_hooks : (user_id option -> unit) list;
  mutable login_failure_hooks : (login_attempt -> unit) list;
  mutable before_external_login_hooks : (strategy:string -> identity:external_identity -> user:user -> bool) list;
  strategies : (string, strategy) Hashtbl.t;
  rate_limit : Throttle.t; (* throttles the [login]/[createUser] DDP methods by client IP + selector *)
}

let user_id_key : user_id option Assigns.key = Assigns.key "fennec.accounts.user_id"
let session_key : session option Assigns.key = Assigns.key "fennec.accounts.session"
let assurance_key : Mfa.assurance option Assigns.key = Assigns.key "fennec.accounts.assurance"
let org_context_key : org_context option Assigns.key = Assigns.key "fennec.accounts.org_context"


let option_exists f = function Some x -> f x | None -> false
let nonblank_opt s = let s = String.trim s in if s = "" then None else Some s

let encode_pairs kvs =
  String.concat "&" (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) kvs)

let int_of_string_default d s = match int_of_string_opt s with Some n -> n | None -> d
let float_of_string_default d s = match float_of_string_opt s with Some n -> n | None -> d

let string_of_user_status = function
  | Active -> "active"
  | Suspended -> "suspended"
  | Disabled -> "disabled"
  | Deleted -> "deleted"

let user_status_of_string = function
  | "active" -> Some Active
  | "suspended" -> Some Suspended
  | "disabled" -> Some Disabled
  | "deleted" -> Some Deleted
  | _ -> None

let user_can_login = function
  | Active -> true
  | Suspended | Disabled | Deleted -> false
