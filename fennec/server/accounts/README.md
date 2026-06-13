# Accounts Modules

Accounts is the shared identity layer. The core `Fennec.Accounts` module owns users, signed session
tokens, cookies, `user_id` propagation into HTTP/SSR/DDP/Pulse, login hooks, logout, and revocation
epochs. The submodules in this directory are opt-in batteries that all converge on the same core
session model.

## Non-Negotiable Model

A single Fennec user may own any number of login mechanisms at the same time:

- password credential
- verified email addresses
- magic-link / OTP capability
- passkey/WebAuthn credentials
- OAuth provider subjects
- OIDC enterprise subjects
- SAML enterprise subjects
- MFA factors
- recovery credentials
- SCIM-provisioned enterprise identities

No module may assume one account equals one mechanism. Linking, unlinking, and conflict handling are
core concerns.

## Shared Outcome

Every fully completed login flow must produce the same result:

- a canonical `user_id`
- a signed `Accounts.token`
- an `Accounts.auth_context` describing the base signed session: user id, session id, strategy,
  issued/expiry timestamps, and revocation epoch
- optional higher-level request context from submodules, such as MFA assurance or org/tenant facts
- audit events
- optional cookie attachment for browser HTTP flows
- connection rebinding for DDP/Pulse flows

Flows that verify a first factor or provider assertion for a user with active MFA must branch before
issuing the signed token. The core exposes typed completion results for that:
`login_with_password_completion`, `login_with_strategy_completion`, `login_with_identity_completion`,
and the verified-value wrappers such as `login_with_oidc_completion`, `login_with_saml_completion`,
`login_with_email_link_completion`, `login_with_email_otp_completion`, and
`login_with_passkey_assertion_completion`. The step-up branch includes the user and a single-use
`Mfa.step_up` token bound to that user, so apps can continue with a second-factor form without
inventing temporary login state. Direct login helpers stay convenient for no-MFA apps and tests; they
reject step-up as a normal Accounts error instead of accidentally creating a session.

After TOTP, backup-code, passkey, or another second factor returns an `mfa_verification`, the app calls
`complete_login_step_up` with the branch token. That checks the verification user before consuming the
signed challenge, checks the required assurance, preserves the original first-factor strategy, and
issues the final signed session. The session stores only verified factor names, so later route guards
can satisfy MFA requirements without persisting server-side session state. Browser apps can mount the
stock `mfa_totp_paw`, `mfa_backup_code_paw`, and passkey MFA JSON paws; Pulse/DDP clients can use the
built-in `completeLoginStepUp` method.

The client side is first-party too. Link `fennec.accounts.client` in a Fur/Pulse app and use
`Fennec_accounts_client.user`, `user_id`, `logging_in`, `login_with_password`, token resume, signup,
logout, password lifecycle, email verification, and MFA completion. The client never switches on raw
BSON: login-like calls decode to `Logged_in {id; token; user}` or `Mfa_required {user_id; mfa_token}`,
and failures decode to `{code; reason}`. It also calls the built-in `currentUser` method to refresh the
same safe session payload shape as `session_doc`.

Named provider presets cover the common social setup without hiding protocol policy:
`OAuth.Providers.all` lists GitHub, Facebook, Discord, X, Spotify, Reddit, Amazon, and Bitbucket;
`Oidc.Providers.all` lists Google, Microsoft Entra ID, Apple, LinkedIn, Slack, GitLab, Twitch,
Salesforce, Dropbox, Auth0, Okta, and Keycloak. Use the named helper (`Providers.github`,
`Providers.google`, `Providers.okta`, etc.) for the one-line setup path. OAuth presets cover
authorization endpoint/scopes/state/PKCE. OIDC presets additionally encode the issuer used for
strict ID-token claim validation. Apps still own token exchange, profile fetching, JWKS caching, and
provider token storage.

Provider tokens, password reset tokens, magic-link tokens, passkey challenges, SAML request ids, and
OAuth state values must never be stored in the signed session cookie.

App-wide roles are native but opt-in. `Accounts.Roles.Role.t` and `Permission.t` are typed handles
backed by canonical strings, so apps define `admin`, `support`, `billing.read`, etc. once and pass
values to `grant_role`, `set_roles_from_strings`, `can`, `require_role`, and `require_permission`.
The user document stores `roles: ["admin", ...]`; SSO/SCIM/admin imports validate strings before
persistence; successful real mutations emit `Audit.Role_change` with actor/request attribution
when provided; route guards read the current server-side user document and deny by default. Org/team
roles remain on `Org.membership.role`.

## Merge Policy

Merge/linking policy is explicit and conservative:

- **Same verified provider subject**: same identity.
- **Same verified email from trusted provider**: candidate link, not automatically safe for every
  provider.
- **Same unverified email**: never auto-link.
- **Enterprise domain match**: may route to an org SSO connection, not by itself proof of account
  ownership.
- **SCIM-created user later logs in via SSO**: link by configured external id / SAML NameID / OIDC
  subject mapping, then email as secondary evidence.
- **Existing password user adds passkey/OAuth**: requires current session and optional step-up.
- **Two active users collide on a new identity**: reject and require explicit admin/user merge.
- **Unlink last usable credential**: reject unless a replacement credential is already verified.

## Flow Convergence

Provider modules do protocol work; core Accounts does account resolution. After an email magic link,
OAuth/OIDC/SAML callback, passkey assertion, or SCIM mapping has been validated, Accounts helper
constructors/wrappers produce:

- a canonical `Identity.key`
- optional email plus whether the provider verified it
- optional username/profile/service facts

Then call `Accounts.login_with_identity_completion` when MFA is supported, or
`Accounts.login_with_identity` when the caller only accepts completed sessions. The resolution order
is intentionally Meteor-familiar and small:

1. explicit current session: link this identity to the logged-in user
2. existing identity link: login that user
3. verified email auto-link, only when enabled by the app
4. JIT signup, only when enabled by the app/provider/org policy

The completed branch returns `identity_login`: the normal signed `Accounts.token`, the user, whether
the user was created, and the identity link when one was attached. Browser handlers attach the
cookie with `set_login_cookie`; DDP/Pulse methods can return the token and rebind the invocation
user id. The step-up branch returns no session token, only the user plus a bound `Mfa.step_up`
challenge token.

Complete verified-value wrappers:

- `login_with_email_link`
- `login_with_email_otp`
- `login_with_oidc`
- `login_with_saml`
- `login_with_passkey`

Each has a `_completion` sibling for MFA-aware routes.

Constructor helpers for profile-boundary/provisioning cases:

- `email_identity`
- `oauth_identity`
- `oidc_identity`
- `saml_identity`
- `passkey_identity`
- `scim_identity`

## Account Management

Core Accounts owns the mutations that must be consistent with sessions and identity links:

- `set_password`: rotate a password directly and bump the revocation epoch.
- `change_password`: prove the current password, then rotate and bump.
- `set_username`, `set_profile`, `add_email`, `remove_email`, and `replace_email`: user settings
  mutations that normalize inputs, preserve store uniqueness, keep verified email identities in
  sync, and protect the last usable credential unless explicitly overridden.
- `set_user_status`, `suspend_user`, `disable_user`, `restore_user`, and `delete_user`: lifecycle
  status mutations. Non-active users cannot start sessions; changes bump the revocation epoch.
- `issue_email_verification`: issue a user-bound token for an email already on the user.
- `verify_email`: consume the token, attach the verified email identity, and mark the email
  verified.
- `issue_password_reset`: issue a reset token without revealing whether a missing email exists.
- `reset_password`: consume the reset token, rotate the password, bump the epoch, and return a
  fresh signed session token.
- `issue_enrollment` and `enroll_account`: initial password setup for accounts created by SSO,
  SCIM/admin flows, or passwordless onboarding.
- `reset_password_completion`, `verify_email_completion`, and `enroll_account_completion`: MFA-aware
  forms that complete the mutation but return `Step_up_required` instead of a full session when an
  active factor exists.
- `linked_identities`: inspect the credentials attached to a user.
- `unlink_identity`: detach one credential, rejecting the last usable credential by default.
- `merge_identities`: move credential links from one user id into another; app-owned profile/data
  merge remains outside Accounts.
- `link_identity` and `link_current_identity`: attach validated provider facts to an existing user
  without issuing a session, for "connect provider" settings flows.
- `password_reset_request_paw`, `password_reset_paw`, `enrollment_paw`,
  `email_verification_request_paw`, and `email_verification_paw`: opt-in route helpers for
  conventional form flows, including optional MFA step-up redirects.
- `email_login_link_request_paw`, `email_login_link_paw`, `email_otp_request_paw`, and
  `email_otp_paw`: opt-in route helpers for passwordless email login. Apps provide delivery,
  redirects, presentation, and rate limiting. Consume helpers can redirect to an MFA route instead
  of setting a cookie; the redirect receives `mfaToken` and `userId`.
- `register_passkey_credential` and `login_with_passkey_assertion`: persist verified passkey
  credentials/counter updates before linking or logging in.
- `passkey_registration_options_paw`, `passkey_registration_finish_paw`,
  `passkey_assertion_options_paw`, and `passkey_assertion_finish_paw`: JSON passkey ceremony
  helpers that own challenge/counter/session plumbing while the app owns browser UI. Assertion
  finish returns HTTP 409 with `{mfaRequired, userId, mfaToken}` when the assertion is valid but
  step-up is still required.
- `scim_paw`: SCIM discovery plus bearer-auth Users/Groups endpoint battery over the Accounts
  store, including user provisioning, org membership sync, and user/group PATCH.
- `assurance`, `set_assurance`, and `require_assurance`: typed request assurance for MFA and
  step-up route guards.
- `enroll_totp`, `confirm_totp_enrollment`, `verify_totp_factor`,
  `regenerate_backup_codes`, `consume_backup_code`, and `disable_mfa_enrollment`: high-level MFA
  account lifecycle that persists enrollment state and anti-replay counters.
- `set_org_context`, `org_context`, `require_org`, and `require_org_strategy`: typed tenant context,
  RBAC route guards, and org login-policy integration.
- `create_org`, `add_org_member`, `issue_org_invite`, and `accept_org_invite`: high-level
  organization lifecycle. Invite tokens are returned once, stored only as hashes, and acceptance is
  bound to the invited email on the target account.
- `session_doc` and `session_paw`: stable current-session payloads for SSR boot data, JSON
  endpoints, and reactive clients.

The app still owns delivery and presentation: send the email, render the reset form, choose copy,
choose redirect targets, and provide the mail callbacks. Core does not bake in SMTP or templates.

## Store Direction

Accounts persistence is Mongo-shaped. The native framework path consumes the global Mongo URL
(`MONGO_URL`, or explicit `:memory:` in tests) and does not ask application code to pass a store.
When no Mongo URL exists, anonymous identity stays `None` but database operations fail clearly.
Lower-level tests and adapters can still create one `Accounts.Store.t` explicitly; provider flows use its
identity, challenge, passkey, org, MFA, SCIM, and audit facets automatically.

- `Store.minimongo ()`: fast in-memory reference backend for tests/examples.
- `Store.mongo ?prefix db`: native MongoDB backend using the same BSON schema.
- `Store.ensure_indexes store`: idempotent Mongo index setup; no-op for Minimongo.
- `Store.users`, `Store.identities`, `Store.challenges`, `Store.passkeys`, `Store.orgs`,
  `Store.mfa`, `Store.scim`, and `Store.audit`: low-level facets for tests, migrations, and modules
  that need a concrete sub-store.

The important invariants stay behind store operations: create user with initial password hash,
password hash plus auth epoch bump, single-use challenge consume, identity attach/detach/merge, and
identity login/link/create. MFA enrollment updates use compare-and-swap for replay-sensitive state
such as TOTP counters and remaining backup-code hashes. We do not expose SQL/Redis/custom
persistence as a first-class goal.

Core flows append security audit events for the account operations Accounts owns: login
success/failure, token resume, password change/reset, email verification, passkey
registration/assertion, identity link/unlink/merge, MFA enrollment/step-up, org policy/invite/member
changes, and logout. Hot-path audit is best-effort so a post-mutation append failure does not
corrupt the account flow; strict admin/export code can call `Audit.append` on `Store.audit` directly
and handle errors.

## Module List

- `Identity`: canonical identity links and merge decisions.
- `Challenge`: typed one-time challenges for token/state/nonce ceremonies.
- `Password`: complete password account surface.
- `Email`: verification, magic links, and OTP.
- `OAuth`: social/provider OAuth with Authorization Code + PKCE.
- `Oidc`: OpenID Connect and enterprise OIDC, including native JWKS parsing and RS256 ID-token
  verification.
- `Saml`: SAML 2.0 enterprise SSO.
- `Passkey`: WebAuthn/passkeys.
- `Mfa`: MFA, assurance, and step-up.
- `Org`: organizations, memberships, domains, RBAC hooks, tenant policy.
- `Scim`: enterprise directory provisioning/deprovisioning plus the Accounts-mounted SCIM endpoint
  battery.
- `Audit`: account/security audit events.

## Testing Bar

Every module needs:

- inline unit tests for pure parsing/validation/state transitions
- store contract tests for atomicity and uniqueness
- replay/expiry/consumed-token tests
- identity collision and merge tests
- DDP/Pulse method identity tests where the module exposes methods
- HTTP cookie/header tests where the module exposes paws/handlers
- negative tests for malformed input and provider mismatch
