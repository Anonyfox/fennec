(** Accounts: signed-cookie identity, Mongo-shaped persistence, and Meteor-shaped auth words.

    Accounts is the framework-owned identity layer. It owns one Mongo/Minimongo-shaped persistence
    handle: users, identity links, short-lived challenges, audit, and index setup live behind
    {!Store.t}. The default browser session is a signed cookie containing only non-secret identity
    metadata, so normal request authentication is stateless and horizontal. Stores are touched for
    real account changes (create user, set password, consume reset tokens) and, when enabled, for
    immediate revocation checks.

    The public vocabulary intentionally follows Meteor where that vocabulary is good:
    [user_id], [set_user_id], [create_user], [login_with_password], [logout],
    [logout_other_clients], login hooks, and strategy-backed providers. The implementation is
    Fennec-native: typed paws for HTTP/SSR, a strategy interface for providers, and a store record
    instead of a hard dependency on one persistence layer.

    The substrate is always present, so a typical app only guards routes and reads the request user:

    {[
      let app =
        Fennec.Endpoint.make ~name:"web" ()
        |> Fennec.Endpoint.pipe_matched [ Fennec.Accounts.require_user () ]

      let handler conn =
        match Fennec.Accounts.user_id conn with
        | Some uid -> Fennec.Conn.text conn ("hello " ^ uid)
        | None -> Fennec.Conn.redirect "/login" conn
    ]}

    {1 Submodules}

    The first-party batteries ({!Password}, {!Email}, {!OAuth}, {!Oidc}, {!Saml}, {!Passkey},
    {!Mfa}, {!Org}, {!Scim}, {!Roles}, {!Audit}) compose through canonical user ids, identity links,
    challenges, and the one {!Store.t} instead of one giant configuration record. *)

module Conn = Paw.Conn
module Paw = Paw
module Bson = Bson

(** Shared identity-linking concepts for all account mechanisms. *)
module Identity = Accounts_identity

(** Shared short-lived challenge primitives for token/code/state/nonce ceremonies. *)
module Challenge = Accounts_challenge

(** Password hashing and policy primitives. High-level password login remains on this core module. *)
module Password = Accounts_password

(** Email ownership, verification, magic links, and OTP login. *)
module Email = Accounts_email

(** OAuth provider login using Authorization Code + PKCE. *)
module OAuth = Accounts_oauth

(** OpenID Connect login and enterprise OIDC SSO. *)
module Oidc = Accounts_oidc

(** SAML 2.0 enterprise SSO. *)
module Saml = Accounts_saml

(** Passkeys and WebAuthn. *)
module Passkey = Accounts_passkey

(** MFA and step-up authentication. *)
module Mfa = Accounts_mfa

(** Organizations, memberships, domains, and tenant auth policy. *)
module Org = Accounts_org

(** SCIM directory sync for enterprise provisioning/deprovisioning. *)
module Scim = Accounts_scim

(** Typed, string-backed app roles and permissions. *)
module Roles = Accounts_roles

(** Account and identity audit events. *)
module Audit = Accounts_audit

(** User ids are application/store ids. They are arbitrary stable strings. *)
type user_id = string

(** A normalized email address on a user record. *)
type email = { address : string; verified : bool }

(** Account lifecycle status. Non-[Active] users cannot start new sessions. *)
type user_status =
  | Active
  | Suspended
  | Disabled
  | Deleted

(** The framework-level user shape Accounts understands.

    [profile] and [services] are deliberately opaque BSON values. Accounts needs to index and update
    identity fields; application-specific user data remains application-owned. [auth_epoch] is a
    monotonic revocation/version number: bump it to invalidate already-issued signed sessions when an
    app needs immediate revocation. [roles] are optional app-wide authorization grants; org/team
    roles remain on {!Org.membership}. *)
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

(** Selectors accepted by password login. *)
type selector = By_id of user_id | By_email of string | By_username of string

(** A signed Accounts session token. Tokens are strings at HTTP/DDP boundaries, but the typed API
    keeps arbitrary strings from being passed to token-consuming functions by accident. *)
type token = private string

(** Authenticated request context derived from an accepted signed session.

    The base session context is intentionally limited to framework-owned facts that are always
    present for every Accounts login: canonical user id, session id, issuing strategy, verified MFA
    factors, timestamps, and revocation epoch. [factors] is empty for ordinary single-factor
    sessions and contains only signed factor names after {!complete_login_step_up}; it never carries
    factor secrets or challenge tokens. Higher-level modules such as {!Mfa} and {!Org} can layer
    assurance or tenant facts on top without bloating the stateless cookie for applications that do
    not need them. *)
type auth_context = {
  user_id : user_id;
  session_id : string;
  strategy : string;
  factors : Mfa.factor list;
  issued_at : float;
  expires_at : float;
  auth_epoch : int;
}

(** Tenant request context assigned by app routing or SSO callbacks.

    Accounts does not guess tenant state from URLs. Apps resolve the org once, assign this typed
    context, and use {!require_org} for route guards. *)
type org_context = {
  org : Org.org;
  membership : Org.membership option;
}

(** Treat an incoming cookie/DDP/storage string as a token. Verification still happens in
    {!verify_token} or {!login_with_token}. *)
val token_of_string : string -> token

(** Render a token for a custom wire/storage boundary. *)
val token_to_string : token -> string

(** A password hasher. This is the same shape as {!Password.hasher}. Production code should pass
    Argon2id/scrypt/bcrypt here; tests can pass a deterministic hasher. Accounts core never falls
    back to an insecure hash silently. *)
type password_hasher = Password.hasher

(** A built-in PBKDF2-HMAC-SHA256 password hasher.

    Argon2id remains the preferred adapter when an application wants that dependency, but this gives
    Accounts a secure, dependency-light password strategy out of the box. [iterations] defaults to
    [210_000]. Hashes are encoded as [pbkdf2-sha256$iterations$salt$derived]. *)
val password_hasher : ?iterations:int -> unit -> password_hasher

(** Errors returned by Accounts operations. They are intentionally stable and small so HTTP handlers,
    DDP methods, and UIs can map them without string parsing. *)
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

(** Human-readable error text. *)
val string_of_error : error -> string

(** Low-level user collection operations used by Accounts core.

    This record remains visible for tests and advanced inspection, but application code should use
    {!Store.t}. Accounts is Mongo-shaped by design: the production implementation is backed by
    MongoDB, and the fast test/client implementation uses Minimongo with the same BSON document
    schema and uniqueness rules. *)
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

(** Public metadata for one live session in the server-side login-token store ("active sessions").

    [session_id] is the [sid] carried in the signed session token — a non-secret random id, safe to
    show in a UI and to pass to {!revoke_session}. The hashed token is never exposed. *)
type session_info = {
  session_id : string;
  user_id : user_id;
  created_at : float;
  expires_at : float;
  last_active_at : float;
  strategy : string;
}

(** The server-side login-token store. Only HASHED tokens are persisted (the [session_id]/[sid] is
    the non-secret row key). It is the source of truth for per-session revocation on the token-
    presentation paths (resume, {!verify_token}, and per-request when [validate_every_request] is on).

    Application code uses {!list_sessions} / {!revoke_session} / {!revoke_other_sessions} rather than
    this record directly; it remains visible for tests and advanced inspection like {!user_store}.
    Time-sensitive operations take [~now] so callers control the clock; rows past [expires_at] read
    as absent. *)
type token_store = {
  record : session_info -> hashed:string -> (unit, error) result;
  find_live : sid:string -> hashed:string -> now:float -> (session_info option, error) result;
  list_for_user : user_id -> now:float -> (session_info list, error) result;
  touch : sid:string -> now:float -> (unit, error) result;
  revoke : sid:string -> (bool, error) result;
  revoke_user : user_id -> ?keep:string -> unit -> (int, error) result;
  gc_expired : now:float -> (int, error) result;
}

(** One Accounts persistence handle: users, login tokens, identity links, challenges, audit, and
    index setup.

    A normal app should create exactly one value and pass it to {!make}. Provider flows then use the
    built-in identity/challenge stores by default, so login/link/create behavior is one coherent
    Mongo transaction boundary instead of several userland knobs. *)
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

(** Accounts store constructors and facets. *)
module Store : sig
  type t = store
  type user = user_store

  (** In-process, mutex-guarded store for tests and examples. *)
  val memory : unit -> t

  (** Store used by the native framework path when no [MONGO_URL] is configured. Anonymous request
      identity remains [None], but database-backed Accounts operations fail with a clear
      [Store_error]. Applications normally do not construct this directly. *)
  val unavailable : ?message:string -> unit -> t

  (** The user collection facet. *)
  val users : t -> user

  (** The login-token ("active sessions") collection facet. *)
  val tokens : t -> token_store

  (** The canonical identity-link collection facet. *)
  val identities : t -> Identity.store

  (** The short-lived challenge collection facet. *)
  val challenges : t -> Challenge.store

  (** The passkey credential facet. *)
  val passkeys : t -> Passkey.store

  (** The organization, membership, and invite facet. *)
  val orgs : t -> Org.store

  (** The MFA enrollment facet. *)
  val mfa : t -> Mfa.store

  (** The SCIM directory state facet. *)
  val scim : t -> Scim.store

  (** The append-only audit facet. *)
  val audit : t -> Audit.store

  (** Create/verify backing indexes. The in-memory backend is already indexed by its maps, so this
      is a no-op there; native Mongo implementations run idempotent [createIndexes] commands. *)
  val ensure_indexes : t -> unit
end

(** A process-local, mutex-guarded store for tests, examples, and single-process prototypes.
    Alias for {!Store.memory}. *)
val memory_store : unit -> store

(** A login attempt passed to hooks. *)
type login_attempt = {
  strategy : string;
  user : user option;
  allowed : bool;
  reason : string option;
}

(** A pluggable login strategy. Strategies verify credentials and return a user. They may close over
    external clients, provider configuration, or the Accounts store. Accounts owns the common
    session issue, hooks, cookies, and [user_id] wiring after a strategy succeeds. *)
type strategy = {
  name : string;
  login : credentials:Bson.t -> (user, error) result;
}

(** External identity facts after a provider-specific module has already validated the ceremony.

    OAuth/OIDC/SAML/passkey/SCIM/email modules produce a canonical {!Identity.key}. This record is
    the small common shape Accounts needs to turn that key into the familiar login/link/create
    outcome. [email_verified] controls whether [email] may be used as explicit auto-link evidence;
    provider modules must set it only when the upstream provider asserted verification. *)
type external_identity = {
  key : Identity.key;
  email : string option;
  email_verified : bool;
  username : string option;
  profile : Bson.t option;
  service : (string * Bson.t) option;
}

(** Build external identity facts for {!login_with_identity}. [email] is normalized when present.
    Blank optional strings are dropped. *)
val external_identity :
  ?email:string ->
  ?email_verified:bool ->
  ?username:string ->
  ?profile:Bson.t ->
  ?service:string * Bson.t ->
  Identity.key ->
  external_identity

(** External identity facts for a verified email address, such as a consumed magic link or OTP. *)
val email_identity :
  ?username:string -> ?profile:Bson.t -> ?service:string * Bson.t -> Email.address -> external_identity

(** External identity facts for a provider OAuth subject after token exchange/profile validation. *)
val oauth_identity :
  ?email:string ->
  ?email_verified:bool ->
  ?username:string ->
  ?profile:Bson.t ->
  ?service:Bson.t ->
  OAuth.provider ->
  subject:string ->
  (external_identity, error) result

(** External identity facts for a validated OIDC principal. *)
val oidc_identity : ?username:string -> ?profile:Bson.t -> ?service:Bson.t -> Oidc.principal -> external_identity

(** External identity facts for a validated SAML principal. *)
val saml_identity : ?username:string -> ?profile:Bson.t -> ?service:Bson.t -> Saml.principal -> external_identity

(** External identity facts for a verified passkey assertion. *)
val passkey_identity : ?service:Bson.t -> Passkey.assertion -> (external_identity, error) result

(** External identity facts for a normalized SCIM user. SCIM is normally provisioning, but this is
    useful when SCIM external ids are also used as account-link evidence. *)
val scim_identity :
  ?username:string -> ?profile:Bson.t -> ?service:Bson.t -> Scim.connection -> Scim.user -> (external_identity, error) result

(** Result of an external identity login. *)
type identity_login = {
  user : user;
  token : token;
  created : bool;
  linked : Identity.link option;
}

(** TOTP enrollment setup returned once so the app can render the QR/provisioning URI. *)
type mfa_totp_setup = {
  enrollment : Mfa.enrollment;
  totp : Mfa.totp;
  provisioning_uri : string;
}

(** Fresh backup/recovery codes plus their persisted enrollment. *)
type mfa_backup_setup = {
  enrollment : Mfa.enrollment;
  codes : string list;
}

(** User-bound MFA verification result.

    This is deliberately stronger than a bare {!Mfa.assurance}: completing a pending login must prove
    the verified second factor belongs to the same user as the signed step-up challenge. *)
type mfa_verification = {
  user_id : user_id;
  assurance : Mfa.assurance;
}

(** Browser-ready passkey registration options. [json] is the response body to pass to browser
    WebAuthn client code; [registration] is the typed server-side state for tests/custom renderers. *)
type passkey_registration_options = {
  registration : Passkey.registration;
  json : string;
}

(** Browser-ready passkey assertion options. *)
type passkey_assertion_options = {
  assertion : Passkey.assertion_challenge;
  json : string;
}

(** Completed passkey registration. *)
type passkey_registration_finish = {
  credential : Passkey.credential;
  link : Identity.link;
}

(** Organization invite issued for application delivery. *)
type org_invite = {
  invite : Org.invite;
  token : string;
}

(** Password-reset challenge issued for application delivery.

    The token is returned once so the app can render it into an email/link. Accounts stores only the
    challenge secret hash and consumes it in {!reset_password}. *)
type password_reset = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

(** Enrollment challenge issued for initial password setup. *)
type enrollment = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

(** MFA step-up branch after the first factor or provider assertion has succeeded.

    [step_up.token] is a single-use challenge bound to [user.id] and the MFA requirement. Apps render
    it into the next form/JSON response, then consume it after verifying TOTP, backup-code, passkey,
    or another second factor. *)
type login_step_up = {
  user : user;
  step_up : Mfa.step_up;
}

(** A login completion that may stop before issuing a full session when active MFA factors require
    step-up. *)
type login_completion =
  | Complete_login of user * token
  | Step_up_required of login_step_up

(** External identity login completion. This keeps [created] and [linked] facts available when the
    login fully completes, while still giving MFA-aware callers a typed step-up branch. *)
type identity_login_completion =
  | Complete_identity_login of identity_login
  | Identity_step_up_required of login_step_up

(** Accounts configuration/state. *)
type t

(** Build an Accounts instance.

    [secret] signs browser session cookies/tokens and must be a long random string. [store] is the
    only persistence dependency. [password_hasher] enables the password strategy. [password_policy]
    validates create/change/set/reset password flows before hashing. [cookie] defaults to
    ["_fennec_login"]. [lifetime] defaults to one day and is the login-token expiration (Meteor's
    [loginExpirationInDays] equivalent): every issued session is recorded in [store.tokens] with
    [expires_at = issued_at + lifetime].

    {b Session source of truth.} The signed cookie stays a zero-read fast-path integrity proof (HMAC +
    expiry + [auth_epoch]). The token store is an {e additional} gate consulted on the token-
    presentation paths — {!verify_token} and {!login_with_token} {b always}, and {!paw} per request
    {e only} when [validate_every_request=true]. So {!revoke_session}/{!logout} take effect immediately
    on resume/API/opt-in-per-request, while a [validate_every_request=false] browser cookie keeps
    authenticating until it expires (the same boundary the [auth_epoch] already had). Recording a
    session is {e fail-closed}: if the token row cannot be persisted the login fails atomically.
    [validate_every_request] additionally re-checks [auth_epoch] against the store on each request;
    leave it [false] for the zero-read path, enable it for immediate per-request revocation/epoch
    enforcement. [rate_limit] throttles the [login] and [createUser] DDP methods against brute force
    (default: {!Accounts_rate_limit.make} — 5 attempts / 10 s per client IP and per account, à la
    Meteor); pass a custom limiter to retune or a disabled one to turn it off. *)
(** Central password/email authentication policy — the typed twin of Meteor's [Accounts.config({...})].
    This is the [password] sub-record of the umbrella {!config}; {!default_config} aliases
    [defaults.password]. *)
type password_config = {
  require_verified_email : bool;
      (** require at least one verified email before any session is issued (every strategy) *)
  forbid_client_account_creation : bool;
      (** reject the client [createUser] DDP method; server-side {!create_user} still works *)
  ambiguous_error_messages : bool;
      (** password login returns one indistinguishable error for unknown-account vs wrong-password (the
          {!on_login_failure} hooks and the audit log still see the real reason). On by default (secure);
          set [false] for Meteor-style specific errors. *)
  auto_send_verification_email : bool;
      (** auto-send a verification email when the client [createUser] DDP method creates a user with an
          email — Meteor's [sendVerificationEmail]. Best-effort (never blocks signup); off by default. *)
  reset_token_lifetime : float;
      (** password-reset token TTL in seconds; default 3 days (Meteor's [passwordResetTokenExpirationInDays]) *)
  enroll_token_lifetime : float;
      (** enrollment-invite token TTL in seconds; default 30 days (Meteor's [passwordEnrollTokenExpiration]) *)
  verify_token_lifetime : float;  (** email-verification token TTL in seconds; default 3 days *)
}

(** {1 The declarative config — one record, every field optional via {!defaults}}

    A userland app configures the whole Accounts subsystem from a single value: start from {!defaults}
    and override one field at a time ([{ Accounts.defaults with mail = Some { from; site_name = Some
    "Acme"; templates = None } }]). The umbrella is {b data}: {!Fennec.serve} hands it to {!start},
    which applies it to the process-native instance and auto-wires the routes + DDP methods it implies.

    {b Omitting it reproduces today's behaviour exactly} — every optional feature off, all methods on,
    [defaults.password] password settings, routes under [/auth]. The config only turns optional features
    (SSO / passkeys / SCIM / the password-email routes) {e on}; it never weakens a default security
    posture (the enumeration guard, throttle, and verified-email gate always apply). *)

(** The signed-cookie session shape — the framework's session knobs. *)
type session_config = {
  cookie : string;  (** the login cookie name; default ["_fennec_login"] *)
  path : string;  (** cookie path scope; default ["/"] *)
  lifetime : float;  (** login-token TTL in seconds; default one day *)
  validate_every_request : bool;
      (** re-check the token row + [auth_epoch] against the store on every request (immediate revocation
          at a per-request store read). Off by default — the zero-read signed-cookie fast path. *)
}

(** Transactional account email (Meteor's [Accounts.emailTemplates]). Setting [config.mail] turns on the
    password/email HTTP routes in the auto-wiring. *)
type mail_config = {
  from : string;  (** the [From:] address for all account email *)
  site_name : string option;  (** the product name rendered into the default templates *)
  templates : Accounts_mailer.templates option;
      (** full template override; [None] uses the defaults for [from] / [site_name] *)
}

(** Passkeys / WebAuthn. Setting [config.passkeys] mounts the passkey registration/assertion JSON routes. *)
type passkeys_config = { relying_party : Passkey.relying_party }

(** Organizations. When [scim_prefix] is set, the SCIM 2.0 provisioning battery mounts at that prefix. *)
type orgs_config = { scim_prefix : string option }

(** Where the auto-derived routes live. *)
type routes_config = {
  auth_prefix : string;  (** route prefix for the derived auth endpoints; default ["/auth"] *)
  me_path : string option;  (** mount [GET <me_path>] (the JSON session doc) when set *)
}

(** One configured SSO/identity provider for [config.providers].

    Each variant bundles the {b existing} typed provider value with the token-exchange recipe its
    matching callback route already needs (OAuth/OIDC) or the trusted signing keys (SAML); the
    auto-wiring mounts its authorize + callback routes under [routes.auth_prefix]. The preset
    constructors that {e synthesize} the exchange (e.g. [Accounts.OAuth.github]) are a later pass — for
    now an app supplies the provider and its exchange explicitly (the same [~exchange] the [*_paw]
    constructors take). The [external_identity] phantom keeps the type concrete at use sites. *)
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

(** The umbrella declarative config. Build one from {!defaults}. *)
type config = {
  session : session_config;
  password : password_config;
  mail : mail_config option;  (** [Some] ⇒ the password/email routes are wired *)
  passkeys : passkeys_config option;  (** [Some] ⇒ the passkey routes are wired *)
  orgs : orgs_config option;  (** [orgs.scim_prefix = Some p] ⇒ the SCIM battery at [p] *)
  rbac : Roles.policy option;  (** the app's role→permission map; [None] ⇒ the empty policy *)
  routes : routes_config;
  providers : external_identity provider list;
      (** SSO providers; the auto-wiring mounts authorize + callback routes per provider *)
}

(** The zero-config umbrella — today's behaviour exactly. Every optional feature off, secure password
    defaults, the framework session/route defaults, no extra providers. Override one field at a time. *)
val defaults : config

(** Secure password defaults: enumeration resistance on, reset 3 d / enroll 30 d / verify 3 d token
    lifetimes. Alias of [defaults.password]. *)
val default_config : password_config

val make :
  secret:string ->
  store:store ->
  ?password_hasher:password_hasher ->
  ?password_policy:Password.policy ->
  ?policy:Roles.policy ->
  ?cookie:string ->
  ?path:string ->
  ?lifetime:float ->
  ?validate_every_request:bool ->
  ?config:password_config ->
  ?rate_limit:Accounts_rate_limit.t ->
  unit ->
  t

(** Set the password/email policy after construction — the mutable twin of Meteor's [Accounts.config]. An
    app configures the framework-native instance ([current ()]) once in startup, e.g.
    [Accounts.configure (Accounts.current ()) { Accounts.default_config with require_verified_email = true }].
    For the full umbrella config (routes, providers, …) prefer {!start}. *)
val configure : t -> password_config -> unit

(** Apply the umbrella {!config} to the process-native instance (the same one {!current} returns) and
    auto-wire the routes + DDP methods it implies. Idempotent-ish: the last call wins. Called by
    {!Fennec.serve} when [?accounts] is given; an app may also call it directly in startup.

    With no config (the default {!defaults}), this is a no-op over today's behaviour — Accounts stays
    hard-wired, anonymous identity is [None], and the full route/method set is present. *)
val start : ?config:config -> unit -> unit

(** {1 Transactional account emails} — Meteor's [Accounts.sendVerificationEmail] family, delivered through
    {!Fennec_mail} (the [MAIL_URL] transport; with it unset, mail is logged to stdout). *)

(** The email templates — defaults + overrides, rendered with Fur (see {!Accounts_mailer}). *)
module Mailer = Accounts_mailer

(** Install the email templates ([from] + site name + per-flow Fur templates). Required before any
    [send_*_email] verb; build one with [Mailer.default ~from ()]. *)
val set_email_templates : t -> Mailer.templates -> unit

(** Issue an email-verification challenge for the user's primary address and send the rendered email.
    [?link] builds the click URL from the token (default [FENNEC_URL ^ "/verify-email?token=…"]). Returns
    [Error msg] on no-email-on-file / unset templates / transport failure. *)
val send_verification_email : t -> ?link:(token:string -> string) -> user_id -> (unit, string) result

(** Issue a password-reset challenge for [email] and send it. Non-enumerating: returns [Ok ()] even when
    no account has that address. [?link] defaults to [FENNEC_URL ^ "/reset-password?token=…"]. *)
val send_reset_password_email : t -> ?link:(token:string -> string) -> string -> (unit, string) result

(** Issue an enrollment (initial-password-setup) challenge for a user and send it. [?link] defaults to
    [FENNEC_URL ^ "/enroll-account?token=…"]. *)
val send_enrollment_email : t -> ?link:(token:string -> string) -> user_id -> (unit, string) result

(** Passwordless: issue a one-time sign-in CODE for [email], deliver it via the [login_code] template, and
    return the challenge token to pair with the code at login (Meteor's [requestLoginToken]). Always issues
    (passwordless permits signup) — naturally non-enumerating. [ttl] defaults to 10 minutes. The DDP
    [requestLoginToken] method wraps this. *)
val send_login_token_email : t -> ?ttl:float -> string -> (string, string) result

(** The process-native Accounts service.

    Fennec treats Accounts as its one identity/session substrate, not as a pluggable auth adapter.
    The native service is built once and memoized for the process (eagerly at boot via {!boot}) from the
    global framework Mongo state. A real [MONGO_URL] selects the native store — minimongo / embedded
    Burrow / mongod, all behind one backend-blind store; explicit [MONGO_URL=:memory:] selects minimongo
    for tests; a missing URL leaves anonymous identity as [None] and makes database-backed Accounts
    operations fail clearly. [FENNEC_ACCOUNTS_SECRET] supplies the stable cookie/token secret, otherwise
    an ephemeral process-local secret is minted. Userland does not pass Accounts through the framework.
    NOTE: memoized on first use, so it pins the [MONGO_URL] seen then — a test that needs a specific
    backend builds a store directly (e.g. {!Store.memory}) or runs as its own process. *)
val current : unit -> t

(** Native Accounts identity paw. It verifies the configured Accounts cookie and assigns the current
    request user id/auth context. With no login cookie, identity is simply [None]. *)
val native_paw : unit -> Paw.t

(** Eagerly build the memoized native store — call once at boot, inside [Fennec.serve]'s switch and after
    the data layer is up — so the engine opens and indexes are ensured before the first request rather
    than lazily on it. With no [MONGO_URL] this builds the cheap no-op store; accounts stays incremental
    opt-in (a request with no session cookie still resolves to [None]). *)
val boot : unit -> unit

(** Register a hook that can reject or observe login attempts. Hooks run after credentials verify and
    before a session is issued. Returning [Error reason] rejects the login. *)
val validate_login_attempt : t -> (login_attempt -> (unit, string) result) -> unit

(** Register a hook that can reject or alter new users before insertion. *)
val on_create_user : t -> (user -> (user, string) result) -> unit

(** Register an observer called after a successful login. *)
val on_login : t -> (user -> unit) -> unit

(** Register an observer called after logout. The user id is [None] when the caller was already
    anonymous. *)
val on_logout : t -> (user_id option -> unit) -> unit

(** Register an observer called when a login attempt FAILS — unknown account, wrong password, an inactive
    account, or a {!validate_login_attempt} rejection. The {!login_attempt} carries [allowed = false] and a
    [reason] tag ([user_not_found] / [invalid_password] / [account_inactive] / the rejection reason); use it
    for lockout, alerting, or metrics — the reactable twin of the audit log's [Login_failure] record. *)
val on_login_failure : t -> (login_attempt -> unit) -> unit

(** Register a veto fired just before an EXTERNAL login (OAuth/OIDC/SAML/identity) mints a session, with
    the [strategy], the resolved {!external_identity}, and the resolved {!user} — Meteor's
    [Accounts.beforeExternalLogin]. Any hook returning [false] aborts the login. Use it to gate SSO by
    email domain, org membership, or an allow-list. (Password/passwordless logins use {!validate_login_attempt}.) *)
val before_external_login : t -> (strategy:string -> identity:external_identity -> user:user -> bool) -> unit

(** Register or replace a login strategy. *)
val register_strategy : t -> strategy -> unit

(** Current request user id, if {!paw} accepted a signed login cookie. *)
val user_id : Conn.t -> user_id option

(** Current request auth context, if {!paw} accepted a signed login cookie. This is the typed
    request-local form of the signed session and does not perform a store read. *)
val auth_context : Conn.t -> auth_context option

(** Assurance derived from the current Accounts session or an explicit step-up assignment.

    Built-in strategies map to their natural first factor: password/email/OAuth/OIDC/SAML are
    single-factor, passkey is phishing-resistant single-factor. Custom strategies should call
    {!set_assurance} when they want MFA guards to trust their verified factors. *)
val assurance : Conn.t -> Mfa.assurance option

(** Attach verified assurance facts to the current request after a step-up or custom strategy. *)
val set_assurance : Conn.t -> Mfa.assurance -> Conn.t

(** Guard a route/action by MFA assurance. Missing or insufficient assurance returns [403], or
    redirects when [redirect] is supplied. *)
val require_assurance : ?redirect:string -> Mfa.requirement -> unit -> Paw.t

(** Attach an organization context to the current request. *)
val set_org_context : Conn.t -> ?membership:Org.membership -> Org.org -> Conn.t

(** Current organization context, if one was assigned. *)
val org_context : Conn.t -> org_context option

(** Current organization, if one was assigned. *)
val org : Conn.t -> Org.org option

(** Current organization membership, if one was assigned. *)
val membership : Conn.t -> Org.membership option

(** Convert a tenant login policy decision into an Accounts error. Use this inside
    {!validate_login_attempt} hooks or provider callback code after routing the user's tenant. *)
val require_org_strategy : Org.org -> Org.strategy -> (unit, error) result

(** Guard a tenant route. An assigned active org is enough when [permission] is absent; a permission
    check requires an active membership whose role grants it under the held {!policy} — the SAME
    code-declared policy as {!can}/{!require_permission} (no separate org RBAC). *)
val require_org : t -> ?redirect:string -> ?permission:string -> unit -> Paw.t

(** Replace a user's app-wide roles. Incoming roles may come from userland declarations, SSO claim
    mapping, SCIM mapping, or admin forms; storage uses canonical role names on the user document.
    Successful changes append [Audit.Role_change]. Pass [actor] and [request] for admin-console,
    SSO, or SCIM attribution; omitted actors are recorded as the Accounts system. No-op replacements
    do not emit audit events. *)
val set_roles :
  t -> ?actor:Audit.actor -> ?request:Audit.request -> user_id -> Roles.Role.t list -> (user, error) result

(** Parse and replace app-wide roles from external string values. Invalid names are rejected before
    persistence. Successful changes use the same [Audit.Role_change] contract as {!set_roles}. *)
val set_roles_from_strings :
  t -> ?actor:Audit.actor -> ?request:Audit.request -> user_id -> string list -> (user, error) result

(** Grant one app-wide role. Idempotent; only real changes append [Audit.Role_change]. *)
val grant_role : t -> ?actor:Audit.actor -> ?request:Audit.request -> user_id -> Roles.Role.t -> (user, error) result

(** Revoke one app-wide role. Idempotent; only real changes append [Audit.Role_change]. *)
val revoke_role :
  t -> ?actor:Audit.actor -> ?request:Audit.request -> user_id -> Roles.Role.t -> (user, error) result

(** Check a user's current app-wide role grants. *)
val has_role : t -> user_id -> Roles.Role.t -> (bool, error) result

(** Does the user's app-wide roles grant [permission] under the held {!policy} (passed to {!make})?
    Missing users, roles, and permissions deny. No [~policy] argument — the policy is code-declared once. *)
val can : t -> user_id -> Roles.Permission.t -> (bool, error) result

(** The scope a permission is evaluated in: [Global] (app-wide roles) or [Org org_id] (the user's active
    membership role in that org, on top of their global roles). One {!policy} decides both. *)
type scope = Global | Org of string

(** Like {!can} but in a [scope] — [can t uid permission] is [can_in t uid ~scope:Global permission]. The
    same code-declared {!policy} resolves org-scoped permissions, so there is no second RBAC model. *)
val can_in : t -> user_id -> scope:scope -> Roles.Permission.t -> (bool, error) result

(** Guard a route by app-wide role. The check is server-side and reads the current user record. *)
val require_role : t -> ?redirect:string -> Roles.Role.t -> unit -> Paw.t

(** Guard a route by app-wide permission under the held {!policy}. Server-side; reads the current user. *)
val require_permission : t -> ?redirect:string -> Roles.Permission.t -> unit -> Paw.t

(** Current request user, loaded from the store when a valid signed login cookie is present. This is
    a convenience for SSR/handlers that need the record; prefer {!user_id} when the id is enough. *)
val current_user : t -> Conn.t -> (user option, error) result

(** Browser/client session payload for SSR boot data, JSON ["me"] endpoints, and reactive clients.

    The document has stable top-level fields [userId], [user], [authContext], [assurance], and
    [org]. Missing values are encoded as BSON nulls. User documents intentionally exclude password
    hashes and provider tokens. *)
val session_doc : t -> Conn.t -> (Bson.t, error) result

(** GET route helper that returns {!session_doc} as canonical extended JSON.

    The helper runs {!paw} internally, so it works either as a standalone ["/me"] route or behind an
    existing Accounts paw. Responses are marked [Cache-Control: no-store]. *)
val session_paw : t -> path:string -> unit -> Paw.t

(** The Accounts paw. It reads the signed login cookie, verifies it, assigns [user_id] for downstream
    paws/handlers, and otherwise passes anonymous requests through. Put it early in the endpoint
    pipeline. *)
val paw : t -> unit -> Paw.t

(** A matched-route guard. Anonymous requests get [401] by default or [302] to [redirect] when
    provided. *)
val require_user : ?redirect:string -> unit -> Paw.t

(** Create a user. If [password] is provided, the Accounts instance must have a password hasher. *)
val create_user :
  t ->
  ?id:user_id ->
  ?username:string ->
  ?email:string ->
  ?password:string ->
  ?profile:Bson.t ->
  unit ->
  (user, error) result

(** Create a user (see {!create_user}) and send a verification email to [email] in one step — Meteor's
    [Accounts.createUserVerifyingEmail]. The user is created on success; delivery is best-effort (they can
    re-request verification), so the result carries the {!create_user} error. Needs {!set_email_templates}. *)
val create_user_verifying_email :
  t ->
  ?id:user_id ->
  ?username:string ->
  email:string ->
  ?password:string ->
  ?profile:Bson.t ->
  ?link:(token:string -> string) ->
  unit ->
  (user, error) result

(** Set a user's username, preserving uniqueness and normalizing the same way password login does. *)
val set_username : t -> user_id -> string option -> (user, error) result

(** Replace the opaque profile document. *)
val set_profile : t -> user_id -> Bson.t option -> (user, error) result

(** Add an email address. Duplicate addresses on another user are rejected case-insensitively. When
    [verified] is true, the verified email identity is attached in the same high-level operation. *)
val add_email : t -> ?verified:bool -> user_id -> string -> (user, error) result

(** Remove an email address and its verified email identity when present. The last usable credential
    is protected unless [allow_last] is true. *)
val remove_email : t -> ?allow_last:bool -> user_id -> string -> (user, error) result

(** Replace an existing email address. The new address is unverified unless [verified] is true. A
    verified-to-verified replacement keeps a usable email credential throughout the operation; a
    verified-to-unverified replacement still protects the last usable credential unless
    [allow_last] is true. *)
val replace_email :
  t -> ?allow_last:bool -> ?verified:bool -> user_id -> old_email:string -> new_email:string -> (user, error) result

(** Set lifecycle status and bump the revocation epoch. Non-[Active] users cannot start new
    sessions, and validated sessions are rejected after the epoch bump. *)
val set_user_status : t -> user_id -> user_status -> (user, error) result

(** Mark an account temporarily inactive. *)
val suspend_user : t -> user_id -> (user, error) result

(** Mark an account administratively disabled. *)
val disable_user : t -> user_id -> (user, error) result

(** Restore an account to [Active]. *)
val restore_user : t -> user_id -> (user, error) result

(** Mark an account deleted without removing its record or audit trail. *)
val delete_user : t -> user_id -> (user, error) result

(** Set a user's password and bump the revocation epoch in one store operation. Existing signed
    sessions become invalid when epoch validation is used; newly issued logins use the new hash. *)
val set_password : t -> user_id -> password:string -> (unit, error) result

(** Change a password after proving the current password. This does not issue a new login token; it
    only rotates the password hash and bumps the revocation epoch. *)
val change_password : t -> user_id -> old_password:string -> new_password:string -> (unit, error) result

(** Issue an email-verification challenge for an address already present on the user.

    The returned token should be delivered by the application. The address is normalized and bound
    together with [user_id], so consumption cannot verify a different user's email accidentally. *)
val issue_email_verification : t -> ?ttl:float -> user_id -> string -> (Email.issued, error) result

(** Consume an email-verification challenge, mark the user's matching email as verified, and attach
    the verified email identity. Identity conflicts are rejected before the user record is mutated. *)
val verify_email : t -> Challenge.token -> (user, error) result

(** Issue a password-reset challenge for [email].

    Missing emails return [Ok None] so handlers can keep a non-enumerating UX. Existing users return
    a token record for the application mailer to deliver. *)
val issue_password_reset : t -> ?ttl:float -> string -> (password_reset option, error) result

(** Consume a password-reset challenge, set the new password, bump the revocation epoch, and return
    a fresh signed session token for the changed user. *)
val reset_password : t -> Challenge.token -> password:string -> (user * token, error) result

(** Issue an initial-password enrollment challenge for a passwordless user. Existing password users
    are rejected so enrollment cannot silently rotate a credential. *)
val issue_enrollment : t -> ?ttl:float -> user_id -> (enrollment, error) result

(** Consume an enrollment challenge, set the first password, and return a fresh signed session. *)
val enroll_account : t -> Challenge.token -> password:string -> (user * token, error) result

(** MFA-aware variants used by route helpers and websocket methods.

    These consume the challenge and complete the underlying account mutation first. When the user
    has an active MFA enrollment they return [Step_up_required { user; step_up }] instead of
    issuing a full session token. [step_up.token] is single-use and bound to [user.id], letting the
    caller continue with a second-factor ceremony without inventing ad-hoc state. *)
val reset_password_completion : t -> Challenge.token -> password:string -> (login_completion, error) result
val verify_email_completion : t -> Challenge.token -> (login_completion, error) result
val enroll_account_completion : t -> Challenge.token -> password:string -> (login_completion, error) result

(** List identity links currently attached to a user. *)
val linked_identities : t -> user_id -> (Identity.link list, error) result

(** Unlink one identity from a user and bump the user's revocation epoch.

    By default the last usable credential cannot be removed. Pass [allow_last:true] only for
    administrative disable/delete flows that intentionally leave the user unable to login. *)
val unlink_identity : t -> ?allow_last:bool -> user_id -> Identity.key -> (Identity.link, error) result

(** Move identity links from one user id into another and bump both users' revocation epochs.

    This intentionally does not merge application-owned profile/data documents. Apps should do that
    in their own domain transaction, using the returned merge plan for audit/UI. *)
val merge_identities :
  t -> from_user_id:user_id -> into_user_id:user_id -> (Identity.merge_plan, error) result

(** Attach a validated external identity to an existing user without issuing a new session.

    Use this for "connect GitHub", "add SSO", and similar settings flows after the user is already
    authenticated, usually behind {!require_assurance}. Provider protocol validation still happens
    in the provider module; this function owns the common attach/update/audit path. *)
val link_identity :
  t -> ?now:(unit -> float) -> user_id -> external_identity -> (Identity.link option, error) result

(** Same as {!link_identity}, using the current request user id. *)
val link_current_identity :
  t -> ?now:(unit -> float) -> Conn.t -> external_identity -> (Identity.link option, error) result

(** Begin TOTP enrollment by storing a pending factor and returning a provisioning URI. *)
val enroll_totp :
  t ->
  ?issuer:string ->
  ?account:string ->
  ?label:string ->
  user_id ->
  (mfa_totp_setup, error) result

(** Confirm a pending TOTP enrollment with a valid code and activate the factor.

    [time] is useful for deterministic tests and custom clocking; production callers normally omit
    it. *)
val confirm_totp_enrollment : t -> ?time:float -> string -> code:string -> (Mfa.enrollment, error) result

(** Verify an active TOTP factor, persist the anti-replay counter, and return user-bound fresh
    assurance.

    [time] is useful for deterministic tests and custom clocking; production callers normally omit
    it. *)
val verify_totp_factor : t -> ?time:float -> string -> code:string -> (mfa_verification, error) result

(** Disable one MFA enrollment for the user. *)
val disable_mfa_enrollment : t -> user_id -> string -> (Mfa.enrollment, error) result

(** Generate and persist replacement backup codes for a user. Existing backup-code enrollment is
    replaced atomically at the Accounts store facet. *)
val regenerate_backup_codes : t -> ?count:int -> ?bytes:int -> user_id -> (mfa_backup_setup, error) result

(** Consume one backup code and return user-bound single-factor recovery assurance. *)
val consume_backup_code : t -> user_id -> code:string -> (mfa_verification, error) result

(** Verify a passkey assertion as a second factor, persist the updated credential counter, and
    return user-bound phishing-resistant assurance.

    The [token] and [json] are from {!begin_passkey_assertion} or
    {!mfa_passkey_assertion_options_paw}. Unlike {!finish_passkey_assertion}, this does not resolve
    identity links or create a login session; pair the returned verification with
    {!complete_login_step_up}. *)
val verify_passkey_factor :
  t ->
  Passkey.relying_party ->
  token:Challenge.token ->
  Fennec_mongo_json.Json.t ->
  (mfa_verification, error) result

(** Consume a login step-up challenge after a second factor has verified and issue the final signed
    session token.

    The [token] is the [step_up.token] returned by a login completion branch or built-in MFA route
    helper. [verification] usually comes from {!verify_totp_factor}, {!consume_backup_code}, or
    {!verify_passkey_factor}. The signed challenge records the target user and required assurance; the
    verification user is checked before challenge consumption, so this function fails closed on stale,
    replayed, wrong-user, or insufficient-factor attempts without burning another user's pending
    step-up. *)
val complete_login_step_up : t -> Challenge.token -> mfa_verification -> (user * token, error) result

(** Create or replace an organization record. *)
val create_org :
  t ->
  ?now:(unit -> float) ->
  ?status:Org.org_status ->
  ?domains:Org.domain list ->
  ?policy:Org.auth_policy ->
  id:string ->
  name:string ->
  unit ->
  (Org.org, error) result

(** Add or replace a membership after proving both the organization and user exist. *)
val add_org_member :
  t ->
  ?now:(unit -> float) ->
  ?status:Org.membership_status ->
  ?role:string ->
  ?external_id:string ->
  org_id:string ->
  user_id:user_id ->
  unit ->
  (Org.membership, error) result

(** Issue an organization invite. The raw token is returned once for delivery; only a hash is
    persisted. *)
val issue_org_invite :
  t ->
  ?now:(unit -> float) ->
  ?ttl:float ->
  org_id:string ->
  email:string ->
  role:string ->
  unit ->
  (org_invite, error) result

(** Accept a pending invite token for [user_id], creating the membership and marking the invite
    accepted. The target user must already carry the invited email address, preventing a logged-in
    browser from accidentally accepting an invite into the wrong account. *)
val accept_org_invite : t -> ?now:(unit -> float) -> string -> user_id:user_id -> (Org.membership, error) result

(** Authenticate with the password strategy and return the user plus a signed session token.
    MFA-enabled users return [Login_rejected "MFA step-up required"]; use
    {!login_with_password_completion} when the caller wants a typed branch. *)
val login_with_password : t -> selector -> password:string -> (user * token, error) result

(** MFA-aware password login. Password verification succeeds before [Step_up_required] is returned,
    but no full session token is issued until the app completes its step-up ceremony. *)
val login_with_password_completion : t -> selector -> password:string -> (login_completion, error) result

(** Authenticate through a registered custom strategy and return the user plus a signed session
    token. MFA-enabled users return [Login_rejected "MFA step-up required"]; use
    {!login_with_strategy_completion} for a typed branch. *)
val login_with_strategy : t -> string -> credentials:Bson.t -> (user * token, error) result

(** MFA-aware custom strategy login. *)
val login_with_strategy_completion : t -> string -> credentials:Bson.t -> (login_completion, error) result

(** Login, link, or create from a validated external identity.

    This is the shared Meteor-familiar account resolver for opt-in mechanisms such as magic links,
    OAuth/OIDC/SAML, passkeys, and SCIM. It checks in order:

    - [current_user_id], when present: attach the identity to the already logged-in user.
    - existing identity link: login that linked user.
    - verified email auto-link, only when [link_verified_email=true] and the identity email is
      verified.
    - JIT signup, only when [allow_signup=true].

    The provider-specific protocol work stays in the provider modules; this function owns the common
    user/session/linking outcome. By default it uses the identity facet inside the Accounts store;
    [identity_store] exists for focused tests and advanced migrations. *)
val login_with_identity :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  strategy:string ->
  external_identity ->
  (identity_login, error) result

(** MFA-aware external identity resolver. The identity may be linked or the user may be created
    before [Identity_step_up_required] is returned; no full session token is issued in that branch. *)
val login_with_identity_completion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  strategy:string ->
  external_identity ->
  (identity_login_completion, error) result

(** Consume a magic-link challenge and resolve the verified email identity. *)
val login_with_email_link :
  t ->
  ?identity_store:Identity.store ->
  Email.t ->
  ?expected:Email.address ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Challenge.token ->
  (identity_login, error) result

(** MFA-aware magic-link login. *)
val login_with_email_link_completion :
  t ->
  ?identity_store:Identity.store ->
  Email.t ->
  ?expected:Email.address ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Challenge.token ->
  (identity_login_completion, error) result

(** Consume an email OTP challenge and resolve the verified email identity. *)
val login_with_email_otp :
  t ->
  ?identity_store:Identity.store ->
  Email.t ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  token:Challenge.token ->
  code:string ->
  unit ->
  (identity_login, error) result

(** MFA-aware email OTP login. *)
val login_with_email_otp_completion :
  t ->
  ?identity_store:Identity.store ->
  Email.t ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  token:Challenge.token ->
  code:string ->
  unit ->
  (identity_login_completion, error) result

(** Resolve a validated OIDC principal through the common identity login policy. *)
val login_with_oidc :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Oidc.principal ->
  (identity_login, error) result

(** MFA-aware OIDC login. *)
val login_with_oidc_completion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Oidc.principal ->
  (identity_login_completion, error) result

(** Resolve a validated SAML principal through the common identity login policy. *)
val login_with_saml :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Saml.principal ->
  (identity_login, error) result

(** MFA-aware SAML login. *)
val login_with_saml_completion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Saml.principal ->
  (identity_login_completion, error) result

(** Resolve a verified passkey assertion through the common identity login policy. *)
val login_with_passkey :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Passkey.assertion ->
  (identity_login, error) result

(** MFA-aware passkey identity login. *)
val login_with_passkey_completion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Passkey.assertion ->
  (identity_login_completion, error) result

(** Persist a newly verified passkey credential and attach its identity to the credential's user.

    Call this after {!Passkey.finish_registration}. The credential id must be unique. *)
val register_passkey_credential : t -> Passkey.credential -> (Identity.link, error) result

(** Persist the updated passkey assertion counter and resolve the normal Accounts login.

    Call this after {!Passkey.finish_assertion}. *)
val login_with_passkey_assertion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Passkey.assertion ->
  (identity_login, error) result

(** MFA-aware passkey assertion login. The passkey counter is persisted before a step-up branch is
    returned, so replay protection still advances. *)
val login_with_passkey_assertion_completion :
  t ->
  ?identity_store:Identity.store ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?now:(unit -> float) ->
  Passkey.assertion ->
  (identity_login_completion, error) result

(** Begin passkey registration for a known Accounts user, returning browser-ready JSON. *)
val begin_passkey_registration :
  t -> Passkey.relying_party -> Passkey.user -> (passkey_registration_options, error) result

(** Finish passkey registration from the browser JSON response and attach the credential. *)
val finish_passkey_registration :
  t ->
  Passkey.relying_party ->
  user_id:user_id ->
  token:Challenge.token ->
  Fennec_mongo_json.Json.t ->
  (passkey_registration_finish, error) result

(** Begin passkey login/assertion, optionally scoped to a known user and credential allow-list. *)
val begin_passkey_assertion :
  t ->
  ?user_id:user_id ->
  ?allowed_credentials:string list ->
  Passkey.relying_party ->
  (passkey_assertion_options, error) result

(** Finish passkey login/assertion from the browser JSON response and issue the normal Accounts
    login result. *)
val finish_passkey_assertion :
  t ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  Passkey.relying_party ->
  token:Challenge.token ->
  Fennec_mongo_json.Json.t ->
  (identity_login, error) result

(** MFA-aware passkey assertion JSON completion. *)
val finish_passkey_assertion_completion :
  t ->
  ?current_user_id:user_id ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  Passkey.relying_party ->
  token:Challenge.token ->
  Fennec_mongo_json.Json.t ->
  (identity_login_completion, error) result

(** Resume from a signed session token and return the user plus a freshly issued replacement token.
    Unlike {!verify_token}, this loads the user and checks the current [auth_epoch], because explicit
    websocket/mobile resume already requires a store read. The presented token's session row must
    still be live (so a revoked/early-expired session cannot resume), and the token is {b rotated}:
    the replacement is recorded and the presented session's row is dropped, so the old resume token
    cannot be used again and the active-sessions list stays one row per resume chain. *)
val login_with_token : t -> token -> (user * token, error) result

(** Attach a freshly issued login cookie to the response. *)
val set_login_cookie :
  t ->
  Conn.t ->
  ?same_site:Paw.Cookie.same_site ->
  ?http_only:bool ->
  ?secure:bool ->
  token ->
  Conn.t

(** Expire the login cookie, run logout observers, and revoke the current request's session row in
    the token store (best-effort; available when {!paw} accepted the cookie upstream) so this
    device's resume token cannot be reused. *)
val logout : t -> Conn.t -> Conn.t

(** Bump the user's revocation epoch and remove all of the user's token-store rows. Existing signed
    sessions become invalid for configurations that validate epochs/rows and for any future session
    issue/check. *)
val logout_other_clients : t -> user_id -> (unit, error) result

(** Bump the user's revocation epoch and issue a fresh token for the current client, pruning every
    other session row (keeping the freshly issued one). Use this for Meteor-style
    ["logoutOtherClients"] semantics. *)
val logout_other_clients_and_refresh : t -> user_id -> (user * token, error) result

(** Verify a signed session token and return its user id. The signed token is checked statelessly
    (HMAC + expiry), and its session row in the token store is {b always} consulted — so
    {!revoke_session}, {!logout}, {!logout_other_clients}, and early expiry take effect here
    immediately, independent of [validate_every_request]. The [auth_epoch] is still only re-checked
    when [validate_every_request=true]. A successful verify refreshes the session's last-active time. *)
val verify_token : t -> token -> (user_id, error) result

(** List a user's live sessions (non-revoked, non-expired) for an "active sessions" UI. Each entry
    carries the [session_id] to pass to {!revoke_session} plus created/expiry/last-active metadata.
    The hashed token is never exposed. *)
val list_sessions : t -> user_id -> (session_info list, error) result

(** Revoke one session by its [session_id], scoped to [user_id] (a user cannot revoke another user's
    session by guessing an id). Returns [true] when a live session was found and removed. Unlike
    {!logout_other_clients} this is per-device and performs no epoch bump. Takes effect immediately on
    resume / {!verify_token} (and per request when [validate_every_request] is on); a
    [validate_every_request=false] browser cookie keeps authenticating via the zero-read path until it
    expires. *)
val revoke_session : t -> user_id:user_id -> session_id:string -> (bool, error) result

(** Revoke all of a user's sessions except [keep] (e.g. "log out my other devices", keeping the
    current session's id). Returns the number removed. *)
val revoke_other_sessions : t -> user_id:user_id -> keep:string -> (int, error) result

(** POST route helper for requesting a password-reset email.

    Reads [email_param] (default ["email"]), calls {!issue_password_reset}, invokes [send] only when
    the account exists, then redirects to [success]. Missing/invalid input redirects to [error].
    Unknown email still redirects to [success] to preserve non-enumerating UX. *)
val password_reset_request_paw :
  t ->
  ?email_param:string ->
  path:string ->
  success:string ->
  error:string ->
  send:(password_reset -> unit) ->
  unit ->
  Paw.t

(** POST route helper for completing password reset.

    Reads [token_param] (default ["token"]) and [password_param] (default ["password"]), consumes
    the reset token, sets the login cookie on success, and redirects to [success] or [error]. When
    active MFA exists, redirects to [mfa_required] when supplied or [error] otherwise; the redirect
    target receives [mfaToken] and [userId] query params. *)
val password_reset_paw :
  t ->
  ?token_param:string ->
  ?password_param:string ->
  ?mfa_required:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** POST route helper for completing initial password enrollment. When active MFA exists, redirects
    to [mfa_required] when supplied or [error] otherwise; the redirect target receives [mfaToken]
    and [userId] query params. *)
val enrollment_paw :
  t ->
  ?token_param:string ->
  ?password_param:string ->
  ?mfa_required:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** POST route helper for requesting verification of the current user's email.

    Requires a valid Accounts cookie already processed by {!paw}. Reads [email_param] (default
    ["email"]), calls {!issue_email_verification}, invokes [send], then redirects. *)
val email_verification_request_paw :
  t ->
  ?email_param:string ->
  path:string ->
  success:string ->
  error:string ->
  send:(Email.issued -> unit) ->
  unit ->
  Paw.t

(** GET route helper for consuming an email-verification token.

    Reads [token_param] (default ["token"]), verifies the email, issues a fresh login session, sets
    the login cookie, and redirects to [success] or [error]. When active MFA exists, redirects to
    [mfa_required] when supplied or [error] otherwise; the redirect target receives [mfaToken] and
    [userId] query params. *)
val email_verification_paw :
  t -> ?token_param:string -> ?mfa_required:string -> path:string -> success:string -> error:string -> unit -> Paw.t

(** POST route helper for requesting a magic email login link. The app owns delivery via [send]. *)
val email_login_link_request_paw :
  t ->
  ?email_param:string ->
  path:string ->
  success:string ->
  error:string ->
  send:(Email.issued -> unit) ->
  unit ->
  Paw.t

(** GET route helper for consuming a magic email login link, setting the Accounts cookie on
    success. When active MFA exists, redirects to [mfa_required] when supplied or [error] otherwise
    without setting a login cookie. The redirect target receives [mfaToken] and [userId] query
    params. *)
val email_login_link_paw :
  t ->
  ?token_param:string ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?mfa_required:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** POST route helper for requesting an email OTP. The app owns delivery via [send]. *)
val email_otp_request_paw :
  t ->
  ?email_param:string ->
  path:string ->
  success:string ->
  error:string ->
  send:(Email.otp -> unit) ->
  unit ->
  Paw.t

(** POST route helper for consuming an email OTP and setting the Accounts cookie on success. When
    active MFA exists, redirects to [mfa_required] when supplied or [error] otherwise without
    setting a login cookie. The redirect target receives [mfaToken] and [userId] query params. *)
val email_otp_paw :
  t ->
  ?token_param:string ->
  ?code_param:string ->
  ?allow_signup:bool ->
  ?link_verified_email:bool ->
  ?mfa_required:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** POST route helper for completing a pending login step-up with an active TOTP factor.

    Reads [mfa_token_param] (default ["mfaToken"]), [factor_param] (default ["factor"]), and
    [code_param] (default ["code"]). On success it consumes the step-up challenge, sets the Accounts
    cookie, and redirects to [success]. *)
val mfa_totp_paw :
  t ->
  ?mfa_token_param:string ->
  ?factor_param:string ->
  ?code_param:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** POST route helper for completing a pending login step-up with a backup code.

    Reads [mfa_token_param] (default ["mfaToken"]), [user_param] (default ["userId"]), and
    [code_param] (default ["code"]). The signed step-up challenge still enforces the target user, so
    a mismatched [userId] cannot complete another account's login. *)
val mfa_backup_code_paw :
  t ->
  ?mfa_token_param:string ->
  ?user_param:string ->
  ?code_param:string ->
  path:string ->
  success:string ->
  error:string ->
  unit ->
  Paw.t

(** JSON route helper for passkey registration options. Runs {!paw} internally and requires a
    current user. *)
val passkey_registration_options_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** JSON route helper for finishing passkey registration. The request body is the browser
    credential JSON plus ["token"] from {!passkey_registration_options_paw}. *)
val passkey_registration_finish_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** JSON route helper for passkey assertion/login options. If a session cookie is present, the
    allow-list is scoped to that user's credentials; otherwise discoverable credentials are allowed. *)
val passkey_assertion_options_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** JSON route helper for finishing passkey assertion/login. The request body is the browser
    assertion JSON plus ["token"] from {!passkey_assertion_options_paw}; success sets the Accounts
    cookie and returns [{id, token, created}]. MFA step-up returns HTTP 409 with
    [{mfaRequired: true, userId, mfaToken}] and no login cookie. *)
val passkey_assertion_finish_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** JSON route helper for passkey step-up options.

    If a session cookie is present, the allow-list is scoped to that user's credentials. Without a
    session it emits a discoverable-credential challenge, which keeps pending-login step-up from
    exposing credential ids before the [mfaToken] is consumed. *)
val mfa_passkey_assertion_options_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** JSON route helper for completing pending login step-up with a passkey assertion.

    The request body is the browser assertion JSON plus ["token"] from
    {!mfa_passkey_assertion_options_paw} and ["mfaToken"] from the login completion branch. Success
    sets the Accounts cookie and returns [{id, token}]. *)
val mfa_passkey_assertion_finish_paw : t -> Passkey.relying_party -> path:string -> unit -> Paw.t

(** SCIM 2-ish endpoint battery mounted at [prefix]. It serves discovery metadata
    ([ServiceProviderConfig], [ResourceTypes], [Schemas]) plus bearer-auth [Users] and [Groups]
    resources over the native SCIM/org/identity store facets, including GET/POST/PUT/PATCH/DELETE,
    provisioning Accounts users and org memberships from SCIM users. *)
val scim_paw : t -> prefix:string -> unit -> Paw.t

(** GET route helper that redirects to an OAuth provider.

    The helper derives the challenge service from [Accounts.t], binds the current user id when a
    session cookie is already present, and stores optional [redirect_param] (default ["redirect"]) in
    state for the callback. *)
val oauth_authorize_paw :
  t -> ?redirect_param:string -> path:string -> error:string -> OAuth.provider -> unit -> Paw.t

(** GET route helper for OAuth callbacks.

    It parses and consumes OAuth state, then calls [exchange] with the consumed state and provider
    code. [exchange] must perform token exchange/profile validation and return canonical
    {!external_identity} facts. On success the helper resolves/links the account, sets the login
    cookie, and redirects to the state redirect or [success]. *)
val oauth_callback_paw :
  t ->
  ?link_verified_email:bool ->
  path:string ->
  success:string ->
  error:string ->
  OAuth.provider ->
  exchange:(OAuth.state -> code:string -> (external_identity, error) result) ->
  unit ->
  Paw.t

(** GET route helper for OAuth callbacks that finish {b over DDP} (Meteor's popup handshake).

    Same provider exchange as {!oauth_callback_paw}, but instead of setting a cookie and redirecting
    it resolves the account (create/link/find + [before_external_login] veto) without minting a
    session, then returns a tiny bounce page that posts a single-use [{credentialToken,
    credentialSecret}] pair to the opener window (same-origin only) and closes. The SPA replays the
    pair through the ["login"] method's [{oauth}] form, which mints the session at that point and may
    still require MFA step-up. Mount this for popup-based OAuth inside a single-page app. *)
val oauth_callback_popup_paw :
  t ->
  ?link_verified_email:bool ->
  path:string ->
  OAuth.provider ->
  exchange:(OAuth.state -> code:string -> (external_identity, error) result) ->
  unit ->
  Paw.t

(** GET route helper that redirects to an OIDC provider. *)
val oidc_authorize_paw :
  t -> ?redirect_param:string -> path:string -> error:string -> Oidc.connection -> unit -> Paw.t

(** GET route helper for OIDC callbacks.

    [exchange] must exchange the authorization code, verify the ID token, validate claims against
    the consumed state/connection, and return an {!Oidc.principal}. The helper then uses
    {!login_with_oidc}, sets the login cookie, and redirects. *)
val oidc_callback_paw :
  t ->
  ?link_verified_email:bool ->
  path:string ->
  success:string ->
  error:string ->
  Oidc.connection ->
  exchange:(Oidc.state -> code:string -> (Oidc.principal, error) result) ->
  unit ->
  Paw.t

(** GET route helper that redirects to a SAML IdP with SP-initiated RelayState.

    [signing_key] signs the HTTP-Redirect URL when an IdP requires signed AuthnRequests. *)
val saml_authorize_paw :
  t ->
  ?redirect_param:string ->
  ?signing_key:X509.Private_key.t ->
  path:string ->
  error:string ->
  Saml.connection ->
  unit ->
  Paw.t

(** POST route helper for SAML ACS callbacks.

    Reads [RelayState] and [SAMLResponse], validates the response with [trusted_keys], resolves the
    Accounts login, sets the login cookie, and redirects. *)
val saml_callback_paw :
  t ->
  path:string ->
  success:string ->
  error:string ->
  Saml.connection ->
  trusted_keys:X509.Public_key.t list ->
  unit ->
  Paw.t

(** Register Meteor-shaped DDP/Pulse methods on a compatible reactive runtime:
    ["createUser"], ["currentUser"], ["login"], ["logout"], ["logoutOtherClients"],
    ["changePassword"], ["resetPassword"], ["verifyEmail"], ["enrollAccount"], and
    ["completeLoginStepUp"]. ["currentUser"] returns the safe session payload shape used by
    {!session_doc}; websocket-only context fields ([authContext], [assurance], [org]) are null.
    Login-like success results are [{id, token}], password signup [createUser] returns
    [{id, token, user}], MFA branches return [{mfaRequired, userId, mfaToken}], and
    ["logoutOtherClients"] returns a replacement [{id, token}] for the current connection after
    bumping [auth_epoch]. Browser clients that cannot receive a Set-Cookie on a websocket can still
    resume explicitly. HTTP/browser cookie helpers remain the preferred same-origin browser story. *)
module Methods (R : sig
  type doc = Bson.t

  type invocation = {
    user_id : string option;
    remote_ip : string option;
    is_simulation : bool;
    set_user_id : string option -> unit;
  }

  exception Error of { code : string; reason : string }

  val methods : (string * (invocation -> doc list -> doc)) list -> unit
end) : sig
  type invocation = R.invocation

  val register : t -> unit
end
