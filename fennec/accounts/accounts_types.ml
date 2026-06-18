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

(* Central password/email authentication policy — the typed twin of Meteor's [Accounts.config({...})].
   [defaults.password] is SECURE-by-default (enumeration resistance on, sane token lifetimes); an app
   tunes it via [start ~config] / {!configure}. This is the [password] sub-record of the umbrella
   {!config}; the older name {!default_config} aliases [defaults.password]. *)
type password_config = {
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

let default_password_config =
  {
    require_verified_email = false;
    forbid_client_account_creation = false;
    ambiguous_error_messages = true;
    auto_send_verification_email = false;
    reset_token_lifetime = 3. *. 86_400.;
    enroll_token_lifetime = 30. *. 86_400.;
    verify_token_lifetime = 3. *. 86_400.;
  }

(* ---- the umbrella declarative config: one record an app fills from [defaults] ---------------------

   Every field is optional via [defaults]; a dev writes [{ defaults with mail = Some {…} }] and nothing
   else. The sub-records each carry their own defaults so a partial override stays a one-liner. This is
   DATA — the {!Accounts_http.Wiring} seam folds it into a route table + a method gate. Omitting it (the
   {!defaults} value) reproduces today's behaviour exactly: every feature/method on, [defaults.password]
   password settings, routes mounted under [/auth]. *)

(* signed-cookie session shape (the framework's [make] knobs): cookie name, path scope, login-token
   lifetime (seconds), and whether to re-check the token store / [auth_epoch] on every request. *)
type session_config = {
  cookie : string;  (* the login cookie name; default ["_fennec_login"] *)
  path : string;  (* cookie path scope; default ["/"] *)
  lifetime : float;  (* login-token TTL in seconds; default one day *)
  validate_every_request : bool;
      (* re-check the token row + [auth_epoch] against the store on every request (immediate revocation
         at a per-request store-read cost). Off by default — the zero-read signed-cookie fast path. *)
}

let default_session_config = { cookie = "_fennec_login"; path = "/"; lifetime = 86_400.; validate_every_request = false }

(* transactional account email (Meteor's Accounts.emailTemplates): the [from] address, the site name
   rendered into the default templates, and optional per-flow template overrides. Present ([mail = Some
   …]) turns on the password/email HTTP routes in {!Accounts_http.Wiring}. *)
type mail_config = {
  from : string;  (* the [From:] address for all account email *)
  site_name : string option;  (* the product name rendered into the default templates *)
  templates : Accounts_mailer.templates option;  (* full template override; [None] ⇒ the defaults for [from]/[site_name] *)
}

(* passkeys / WebAuthn: the relying party (RP id + name + allowed origins). Present ([passkeys = Some …])
   mounts the passkey registration/assertion JSON routes. *)
type passkeys_config = { relying_party : Passkey.relying_party }

(* organizations: when [scim_prefix] is set, the SCIM 2.0 provisioning battery mounts at that prefix. *)
type orgs_config = { scim_prefix : string option }

(* where the auto-derived routes live. [auth_prefix] (default ["/auth"]) roots the password / email /
   passkey / provider routes; [me_path], when set, mounts [GET <me_path>] returning the session doc. *)
type routes_config = {
  auth_prefix : string;  (* route prefix for the derived auth endpoints; default ["/auth"] *)
  me_path : string option;  (* mount [GET <me_path>] (the JSON session doc) when set *)
}

let default_routes_config = { auth_prefix = "/auth"; me_path = None }

(* One configured SSO/identity provider in [config.providers]. Each variant bundles the EXISTING typed
   provider value with the token-exchange recipe the matching [*_callback_paw] already needs (OAuth/OIDC)
   or the trusted signing keys (SAML). {!Accounts_http.Wiring} mounts the authorize + callback routes
   per provider under [auth_prefix]. The preset constructors that synthesize the exchange (Accounts.OAuth.
   github &c.) are a later pass; today an app supplies the provider + its exchange explicitly. *)
type 'a provider =
  | OAuth_provider of {
      provider : OAuth.provider;
      exchange : OAuth.state -> code:string -> (external_identity, error) result;
      link_verified_email : bool;
      success : string;
      error : string;
    }
  | Oidc_provider of {
      connection : Oidc.connection;
      exchange : Oidc.state -> code:string -> (Oidc.principal, error) result;
      link_verified_email : bool;
      success : string;
      error : string;
    }
  | Saml_provider of {
      connection : Saml.connection;
      trusted_keys : X509.Public_key.t list;
      signing_key : X509.Private_key.t option;
      success : string;
      error : string;
    }
  constraint 'a = external_identity
(* the [constraint] pins the phantom so [provider] is a concrete (non-parametric-at-use) type once
   {!external_identity} is in scope; callers write [Accounts.provider]. *)

type config = {
  session : session_config;
  password : password_config;
  mail : mail_config option;  (* Some ⇒ password/email routes are wired *)
  passkeys : passkeys_config option;  (* Some ⇒ passkey routes are wired *)
  orgs : orgs_config option;  (* orgs.scim_prefix = Some p ⇒ SCIM battery at p *)
  rbac : Roles.policy option;  (* the app's role→permission map; None ⇒ the empty policy *)
  routes : routes_config;
  providers : external_identity provider list;  (* SSO providers to wire authorize+callback routes for *)
}

(* [defaults] — the zero-config umbrella: today's behaviour exactly. Every optional feature off (None),
   secure password defaults, the framework session/route defaults, no extra providers. A dev overrides
   one field at a time: [{ defaults with mail = Some { from; site_name = Some "Acme"; templates = None } }]. *)
let defaults =
  {
    session = default_session_config;
    password = default_password_config;
    mail = None;
    passkeys = None;
    orgs = None;
    rbac = None;
    routes = default_routes_config;
    providers = [];
  }

(* DEPRECATED alias kept for source compatibility: the old top-level [default_config] is now
   [defaults.password]. Same value, same type ([password_config]). *)
let default_config = defaults.password

type t = {
  secret : string;
  store : store;
  password_hasher : password_hasher option;
  password_policy : Password.policy option;
  policy : Roles.policy;
      (* the app's role→permission map — code-declared, immutable, held once. [can]/[require_permission]
         read it directly so callers never thread a [~policy] argument. The single RBAC policy: org
         permission checks (see [can_in]) resolve against it too. *)
  mutable config : password_config;
      (* the password/email policy (the [password] sub-record). Engine code reads it directly as
         [t.config.forbid_client_account_creation] &c.; {!configure} / [start ~config] set it. *)
  mutable settings : config;
      (* the full umbrella config applied via [start]. Defaults to {!defaults} (today's behaviour):
         every optional feature off, all methods on, routes under [/auth]. {!Accounts_http.Wiring}
         reads it to derive the route table + the DDP method gate. Engine paths that predate the
         umbrella keep reading the [config]/[cookie]/[path]/… fields, so the default path is unchanged. *)
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
