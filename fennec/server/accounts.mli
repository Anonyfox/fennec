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
    instead of a hard dependency on one persistence layer. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
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

(** One Accounts persistence handle: users, identity links, challenges, audit, and index setup.

    A normal app should create exactly one value and pass it to {!make}. Provider flows then use the
    built-in identity/challenge stores by default, so login/link/create behavior is one coherent
    Mongo transaction boundary instead of several userland knobs. *)
type store = {
  users : user_store;
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

  (** Minimongo-backed store. It is the fast reference backend for Accounts semantics and uses the
      same BSON codecs as native Mongo. *)
  val minimongo : unit -> t

  (** Native MongoDB-backed store using the same document schema as {!minimongo}. [prefix] defaults
      to ["accounts"] and creates collections such as ["accounts_users"],
      ["accounts_identities"], and ["accounts_challenges"]. Call {!ensure_indexes} during
      application startup before accepting auth traffic. *)
  val mongo : ?prefix:string -> Fennec_mongo_driver.Database.t -> t

  (** Store used by the native framework path when no [MONGO_URL] is configured. Anonymous request
      identity remains [None], but database-backed Accounts operations fail with a clear
      [Store_error]. Applications normally do not construct this directly. *)
  val unavailable : ?message:string -> unit -> t

  (** The user collection facet. *)
  val users : t -> user

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
    ["_fennec_login"]. [lifetime] defaults to one day. [validate_every_request] verifies the signed
    cookie's [auth_epoch] against the store on each request; leave it [false] for the zero-read
    stateless path, enable it for immediate account revocation. *)
val make :
  secret:string ->
  store:store ->
  ?password_hasher:password_hasher ->
  ?password_policy:Password.policy ->
  ?cookie:string ->
  ?path:string ->
  ?lifetime:float ->
  ?validate_every_request:bool ->
  unit ->
  t

(** The process-native Accounts service.

    Fennec treats Accounts as its one identity/session substrate, not as a pluggable auth adapter.
    The native service is created lazily from the global framework Mongo state. A real [MONGO_URL]
    selects the native Mongo-backed store; explicit [MONGO_URL=:memory:] selects the in-process
    Mongo-shaped store for tests; a missing URL leaves anonymous identity as [None] and makes
    database-backed Accounts operations fail clearly. [FENNEC_ACCOUNTS_SECRET] supplies the stable
    cookie/token secret, otherwise an ephemeral process-local secret is minted. Userland does not
    pass Accounts through the framework. *)
val current : unit -> t

(** Native Accounts identity paw. It verifies the configured Accounts cookie and assigns the current
    request user id/auth context. With no login cookie, identity is simply [None]. *)
val native_paw : unit -> Paw.t

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

(** Guard a tenant route. An assigned active org is enough when [permission] is absent; permission
    checks require an active membership and use {!Org.allows}. *)
val require_org : ?redirect:string -> ?permission:string -> ?role_allows:Org.role_allows -> unit -> Paw.t

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

(** Check a user's current app-wide role grants against a typed policy. Missing users, roles, and
    permissions deny. *)
val can : t -> user_id -> policy:Roles.policy -> Roles.Permission.t -> (bool, error) result

(** Guard a route by app-wide role. The check is server-side and reads the current user record. *)
val require_role : t -> ?redirect:string -> Roles.Role.t -> unit -> Paw.t

(** Guard a route by app-wide permission using a typed role policy. The check is server-side and
    reads the current user record. *)
val require_permission : t -> ?redirect:string -> policy:Roles.policy -> Roles.Permission.t -> unit -> Paw.t

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
    websocket/mobile resume already requires a store read. *)
val login_with_token : t -> token -> (user * token, error) result

(** Attach a freshly issued login cookie to the response. *)
val set_login_cookie :
  t ->
  Conn.t ->
  ?same_site:Fennec_core.Cookie.same_site ->
  ?http_only:bool ->
  ?secure:bool ->
  token ->
  Conn.t

(** Expire the login cookie and run logout observers. *)
val logout : t -> Conn.t -> Conn.t

(** Bump the user's revocation epoch. Existing signed sessions become invalid for configurations
    that validate epochs and for any future session issue/check. *)
val logout_other_clients : t -> user_id -> (unit, error) result

(** Bump the user's revocation epoch and issue a fresh token for the current client. Use this for
    Meteor-style ["logoutOtherClients"] semantics. *)
val logout_other_clients_and_refresh : t -> user_id -> (user * token, error) result

(** Verify a signed session token and return its user id. With [validate_every_request=false], this
    is the zero-read check and does not observe later [auth_epoch] bumps until token expiry. Use
    {!login_with_token} for explicit resume flows that should refresh a token and observe
    revocation immediately. *)
val verify_token : t -> token -> (user_id, error) result

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
    is_simulation : bool;
    set_user_id : string option -> unit;
  }

  exception Error of { code : string; reason : string }

  val methods : (string * (invocation -> doc list -> doc)) list -> unit
end) : sig
  type invocation = R.invocation

  val register : t -> unit
end
