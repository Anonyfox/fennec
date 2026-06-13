# Accounts

Accounts is Fennec's native identity layer: signed-cookie browser sessions, typed HTTP/SSR access to
`user_id`, Pulse/DDP method identity, and pluggable login strategies. The public words intentionally
track the useful Meteor vocabulary (`userId`, `setUserId`, `createUser`, `login`,
`logoutOtherClients`) while the implementation stays Fennec-native and Mongo-shaped.

## Goals

- One identity source for HTTP handlers, SSR, websocket/DDP methods, and publications.
- Stateless request authentication by default: the browser cookie carries signed, non-secret session
  metadata, so normal reads do not need a persistence hit.
- Mongo-native persistence: one Accounts store owns users, identity links, challenges, passkeys,
  organizations, MFA enrollments, SCIM state, and audit; production uses MongoDB,
  tests/dev/client-side pieces can use Minimongo with the same BSON schema.
- Native opt-in batteries for password, email links/OTP, OAuth, OIDC, SAML, passkeys, MFA,
  organizations, SCIM provisioning, and audit.
- Meteor-compatible method words where they are still good: `createUser`, `login`, `logout`,
  `logoutOtherClients`, `changePassword`, `resetPassword`, and `verifyEmail`.
- Explicit revocation tradeoff: immediate global invalidation requires checking persisted state.

## Current Implementation

`fennec/server/accounts.{mli,ml}` owns the core:

- `user` is the framework-level identity shape: `id`, `username`, `emails`, opaque `profile`,
  opaque provider `services`, timestamps, `auth_epoch`, and lifecycle `status`.
- `Store.t` is the one persistence handle. Its user facet contains lookup/update/password/revocation
  operations; identity stores canonical links; challenge stores hashed single-use token/state
  records; passkey stores WebAuthn credentials; org stores organizations, memberships, and invites;
  MFA stores factor enrollments; SCIM stores directory connections/users/groups; audit records
  security events.
- User creation receives the optional initial password hash in the same call, so Mongo persistence
  writes the user and password together.
- Password rotation uses one store operation, `set_password_hash_and_bump`, so adapters can update
  the password hash and revocation epoch atomically.
- `Store.minimongo ()` is the fast reference backend for tests and examples; `Store.mongo db` uses
  native MongoDB collections with the same codecs and idempotent index setup.
- `password_hasher` provides PBKDF2-HMAC-SHA256 using existing dependencies.
- `password_hasher` remains injectable so an Argon2id adapter can be supplied without changing the
  Accounts API.
- `token` is a private string type. Normal login-to-cookie flow stays direct, while custom
  storage/wire edges must explicitly use `token_to_string` / `token_of_string`.
- `strategy` is `{ name; login }`; a strategy verifies credentials and returns a user.
- `register_strategy` installs custom strategies.
- `external_identity` is the common post-provider shape: canonical identity key, optional verified
  email, optional username/profile, and optional provider service document. Constructors such as
  `email_identity`, `oauth_identity`, `oidc_identity`, `saml_identity`, `passkey_identity`, and
  `scim_identity` keep callback code terse without moving protocol validation into Accounts core.
- `login_with_password_completion`, `login_with_strategy_completion`, and
  `login_with_identity_completion` are the shared login completions for password/custom strategy and
  magic-link/OAuth/OIDC/SAML/passkey/SCIM flows.
  They verify the first factor or provider assertion, resolve the canonical user, and then either
  issue a full session or return a typed MFA step-up branch before a session exists. The branch
  carries both the user and a single-use `Mfa.step_up` challenge token bound to that user, so apps do
  not need to invent temporary login state.
- `login_with_identity` is the direct resolver for identity flows where the caller only wants a
  completed session. It uses the same resolution logic and rejects MFA step-up as a normal Accounts
  error so simple apps can stay simple.
  It logs in existing identity links, explicitly links the current user, optionally links by verified
  email, optionally JIT-creates a user, issues the normal signed Accounts token, and runs the same
  login hooks as password/custom strategies.
- `login_with_email_link`, `login_with_email_otp`, `login_with_oidc`, `login_with_saml`, and
  `login_with_passkey` are thin direct wrappers around the same resolver for the common
  verified-value cases. Their `_completion` siblings expose the MFA branch with created/linking
  facts preserved for provider callback UIs.
- Account-management helpers keep delivery/UI concerns outside core while owning the store
  mutation:
  - `set_username`, `set_profile`, `add_email`, `remove_email`, and `replace_email` cover the
    common user settings lifecycle. Verified email mutations keep the user record and verified
    email identity linked together, protect the last usable credential by default, and support
    verified-to-verified replacement without temporarily leaving the account credentialless.
  - `set_user_status`, `suspend_user`, `disable_user`, `restore_user`, and `delete_user` own the
    account lifecycle status. Non-active users cannot start new sessions; status changes bump
    `auth_epoch` so validated sessions are rejected immediately.
  - `issue_email_verification` creates a user-bound verification token for an address already on
    the user.
  - `verify_email` consumes that token, attaches the verified email identity, then marks the user
    email verified.
  - `issue_password_reset` returns `Ok None` for unknown email addresses so handlers can keep
    non-enumerating UX.
  - `reset_password` consumes the reset token, writes the new hash, bumps `auth_epoch`, and returns
    a fresh signed session.
  - `issue_enrollment` and `enroll_account` let an admin/SSO/passwordless-created account set its
    first password without conflating enrollment with password reset.
  - `reset_password_completion`, `verify_email_completion`, and `enroll_account_completion` are the
    MFA-aware forms: the mutation completes, but active MFA returns
    `Step_up_required { user; step_up }` instead of issuing a full session.
  - `change_password` proves the old password before the same hash+epoch rotation.
- Password creation/rotation/reset also ensures the per-user `Identity.password ()` link exists, so
  last-credential checks work across mixed password/provider accounts.
- `linked_identities`, `unlink_identity`, and `merge_identities` expose account linking operations
  at the Accounts layer. Unlink rejects the last usable credential by default and merge moves only
  credential links; application profile/data merge remains app-owned.
- `link_identity` and `link_current_identity` attach validated provider facts to an existing account
  without issuing a new session, for settings flows such as "connect GitHub" or "add SSO".
- `enroll_totp`, `confirm_totp_enrollment`, `verify_totp_factor`, `regenerate_backup_codes`,
  `consume_backup_code`, and `disable_mfa_enrollment` are the high-level MFA account lifecycle.
  They persist enrollment state, sealed TOTP secrets, anti-replay counters, backup-code hashes, and
  audit events.
- `create_org`, `add_org_member`, `issue_org_invite`, and `accept_org_invite` are the high-level
  org lifecycle. Invite tokens are returned once, stored only as hashes, and acceptance is bound to
  the invited email on the target account.
- Route helper paws wire the common HTTP edges without owning UI or SMTP:
  `password_reset_request_paw`, `password_reset_paw`, `enrollment_paw`,
  `email_verification_request_paw`, and `email_verification_paw`. Apps provide mail callbacks and
  redirect targets. Completion helpers can redirect to an MFA step-up route instead of setting a
  login cookie when active factors exist; the redirect includes `mfaToken` and `userId`.
- Passwordless email route helpers issue/consume magic links and OTPs:
  `email_login_link_request_paw`, `email_login_link_paw`, `email_otp_request_paw`, and
  `email_otp_paw`. They set the normal Accounts cookie on success; apps provide delivery, redirects,
  presentation, and rate limiting.
- Passkey JSON route helpers wrap browser ceremonies end to end:
  `passkey_registration_options_paw`, `passkey_registration_finish_paw`,
  `passkey_assertion_options_paw`, and `passkey_assertion_finish_paw`. They own challenge
  token/counter/session plumbing while browser JavaScript and UI remain app-owned. Assertion finish
  returns HTTP 409 with a small `{mfaRequired, userId, mfaToken}` JSON payload when the passkey
  assertion is valid but the account still needs step-up.
- `scim_paw ~prefix` mounts SCIM discovery plus bearer-auth Users/Groups endpoints over the
  Accounts store. It serves `ServiceProviderConfig`, `ResourceTypes`, and `Schemas`, handles
  GET/POST/PUT/PATCH/DELETE for provisioned resources, provisions Accounts users, attaches SCIM
  identities, syncs org memberships, and persists SCIM user/group directory state.
- Accounts is the native framework identity/session substrate. The runtime prepends the Accounts
  identity paw to every endpoint, live/DDP sessions inherit the request user id automatically, and
  built-in Accounts methods are registered on the reactive runtime by the Pulse server. Apps should
  not manually create or pass an Accounts service in the normal path.
- `paw` still exists as the low-level HTTP helper: it verifies the signed login cookie and assigns
  `user_id` on `Conn.t`. Use it only for tests/custom transports; `Fennec.serve` owns the default
  pipeline.
- `auth_context` exposes the accepted signed session on `Conn.t`: user id, session id, issuing
  strategy, timestamps, and auth epoch. This is the base request context every login mechanism
  shares; org and MFA modules layer tenant and assurance facts above it.
- `session_doc` gives SSR, JSON endpoints, and reactive clients one stable BSON payload for
  `userId`, `user`, `authContext`, `assurance`, and `org`. `session_paw ~path:"/me"` exposes the
  same payload as no-store extended JSON and runs `paw` internally, so the common browser "who am I"
  endpoint is one line.
- `assurance` exposes typed MFA assurance on `Conn.t`. Built-in strategies derive the obvious base
  factor from the signed session; custom strategies and successful step-up flows can call
  `set_assurance`.
- `require_assurance` guards matched routes/actions with an `Mfa.requirement`.
- `set_org_context`, `org_context`, `org`, `membership`, and `require_org` provide typed tenant
  context and RBAC route guards after app routing or SSO has resolved an organization.
- `require_org_strategy` turns `Org.decide_strategy` into the normal Accounts error type for login
  hooks and provider callbacks.
- `Roles` provides typed, string-backed app-wide roles and permissions. Global grants live on the
  user document as canonical `roles: [...]`; org/team roles stay on `Org.membership.role`.
- `set_roles`, `set_roles_from_strings`, `grant_role`, `revoke_role`, `has_role`, `can`,
  `require_role`, and `require_permission` are opt-in RBAC helpers. They deny by default and perform
  route checks server-side against the current user document.
- Successful role mutations append `Audit.Role_change` with `action`, `before`, `after`, and
  optional `role` metadata. Admin consoles, SSO callbacks, and SCIM handlers should pass `~actor`
  and `~request`; omitted actors are recorded as the Accounts system. Idempotent no-ops do not
  emit audit events.
- `require_user` is the matched-route guard.
- `set_password` writes a new hash and bumps `auth_epoch`.
- Core login/account-management flows append audit events for login success/failure, token resume,
  password change/reset, email verification, passkey registration/assertion, identity link/unlink/
  merge, MFA enrollment/step-up, org policy/invite/member changes, and logout. Audit append is
  best-effort for these hot paths; callers that need strict handling can use the audit store facet
  directly.
- The Pulse server installs `createUser`, `currentUser`, `login`, `logout`, `logoutOtherClients`,
  `changePassword`, `resetPassword`, `verifyEmail`, `enrollAccount`, and `completeLoginStepUp`
  automatically from the native Accounts runtime. `Methods(R).register` remains the low-level hook
  for tests/custom transports.
- `fennec.accounts.client` is the Fur/Pulse client facade: `current_user`, `current_user_id`,
  `current_logging_in`, `login_with_password`, token resume, signup, logout, password lifecycle,
  email verification, and MFA completion decode the server's canonical BSON payloads into typed
  OCaml variants.

The facade exports both:

- `Fennec.Accounts`
- `Fennec.Paw.Accounts`

They are the same module; `Paw.Accounts` exists so users discover auth next to the other route
middleware batteries.

## App Roles

Roles are optional. Apps that only need ownership checks can ignore them. When app-wide authorization
is useful, define typed handles once and use those values everywhere else:

```ocaml
module Roles = struct
  let admin = Fennec.Accounts.Roles.Role.admin
  let support = Fennec.Accounts.Roles.Role.v_exn "support"
end

module Perms = struct
  let admin_access = Fennec.Accounts.Roles.Permission.v_exn "admin.access"
  let billing_read = Fennec.Accounts.Roles.Permission.v_exn "billing.read"
end

let policy =
  Fennec.Accounts.Roles.policy
    [
      Fennec.Accounts.Roles.role Roles.admin [ Perms.admin_access; Perms.billing_read ];
      Fennec.Accounts.Roles.role Roles.support [ Perms.billing_read ];
    ]
```

Mutation helpers persist canonical strings on the user document:

```ocaml
let _ = Fennec.Accounts.grant_role accounts user_id Roles.admin
let _ =
  Fennec.Accounts.set_roles_from_strings accounts
    ~actor:(Fennec.Accounts.Audit.User admin_user_id)
    user_id
    [ "Admin"; "support" ]
```

Every successful real mutation records `Audit.Role_change` with `action`, `before`, `after`, and
optional `role` metadata. Pass `~actor`/`~request` when the change comes from an admin action,
provider callback, or SCIM endpoint so urgent access changes are attributable later. Repeating an
already-applied grant/revoke is a no-op and does not create audit noise.

Route guards stay server-authoritative:

```ocaml
Fennec.Endpoint.use_matched
  (Fennec.Accounts.require_permission accounts ~policy Perms.admin_access ())
  admin_endpoint
```

External mappings from SSO/SCIM/admin forms should enter through `Roles.Role.v` or
`set_roles_from_strings`. Invalid names fail before persistence. The client `fennec.accounts.client`
decodes public role names for UI hints, but server guards are the enforcement boundary.

## Session Model

The login cookie is signed, not encrypted. It contains no secrets:

```text
uid=<user id>
sid=<random session id>
iat=<issued epoch seconds>
exp=<expiry epoch seconds>
epoch=<user auth_epoch at issue time>
strategy=<login strategy>
factors=<comma-separated verified MFA factors, only after completed step-up>
```

Signed-only is the right default because the browser does not learn anything secret. Provider access
tokens, password reset tokens, and OAuth callback state must not live in this cookie.

`validate_every_request=false` is the zero-read path: a valid unexpired signature authenticates the
request until expiry. `validate_every_request=true` checks the user's current `auth_epoch` in the
store on each request. That enables immediate account disable/logout-all/password-change revocation,
at the cost of a read.

Explicit token resume is different: `login_with_token` always loads the user, checks the current
`auth_epoch`, and returns a freshly issued token. That is the right default for websocket/mobile
resume because the client is already asking the server to re-establish identity.

There is no way to have both zero persistent reads and immediate global revocation. The API makes
that tradeoff explicit instead of hiding it.

Completed MFA sessions remain stateless too. The final `complete_login_step_up` exchange stores only
verified factor names in the signed cookie, never factor secrets or challenge tokens. Later requests
derive route-guard assurance from the original login strategy plus those signed factor names.

## HTTP And SSR Flow

The Accounts substrate is always present and consumes the global framework Mongo state. A real
`MONGO_URL` stores Accounts collections in Mongo (`FENNEC_DB` selects the database, default
`fennec`); explicit `MONGO_URL=:memory:` uses the in-process Mongo-shaped store for tests. When
`MONGO_URL` is missing, anonymous identity is still `None` and the server boots with a warning, but
database-backed Accounts operations fail clearly instead of silently choosing a data location.
Accounts does not own a separate fallback policy. If the global URL points at real Mongo and the
driver/connection is broken, that is a real startup/runtime error instead of a silent data-location
change. `FENNEC_ACCOUNTS_SECRET` provides a stable cookie/token secret; when absent, Fennec mints an
ephemeral process-local secret for dev/test.

Typical server setup:

```ocaml
let app =
  Fennec.Endpoint.make ~name:"web" ()
  |> Fennec.Endpoint.pipe_matched [ Fennec.Accounts.require_user () ]

let () =
  Fennec.serve [ app ]
```

Handlers and SSR read:

```ocaml
match Fennec.Accounts.user_id conn with
| Some uid -> ...
| None -> ...
```

Handlers that need session metadata can read the same accepted cookie as a typed request context:

```ocaml
match Fennec.Accounts.auth_context conn with
| Some ctx -> (* ctx.user_id, ctx.session_id, ctx.strategy, ctx.auth_epoch *)
| None -> ...
```

For SSR boot data or a conventional `/me` endpoint, use the built-in session payload:

```ocaml
let me = Fennec.Accounts.session_paw accounts ~path:"/me" ()
```

The returned BSON/JSON includes `userId`, `user`, `authContext`, `assurance`, and `org`, with
password hashes and provider tokens intentionally absent.

MFA and org/tenant guards stay in the paw pipeline too:

```ocaml
let admin_routes =
  let requirement = Result.get_ok (Fennec.Accounts.Mfa.requirement Fennec.Accounts.Mfa.Multi_factor) in
  Fennec.Paw.seq
    [
      Fennec.Accounts.require_user ();
      Fennec.Accounts.require_assurance requirement ();
      Fennec.Accounts.require_org ~permission:"admin:write" ();
    ]
```

Applications resolve tenant state once, then assign it:

```ocaml
let attach_org org membership conn =
  Fennec.Accounts.set_org_context conn ~membership org
```

Custom strategies or completed step-up flows assign verified assurance explicitly:

```ocaml
let conn =
  Fennec.Accounts.set_assurance
    conn
    (Fennec.Accounts.Mfa.assurance [ Fennec.Accounts.Mfa.Password; Fennec.Accounts.Mfa.Totp ])
```

For a form/HTTP login endpoint, use the completion form when the app supports MFA:

```ocaml
match Fennec.Accounts.login_with_password_completion accounts (By_email email) ~password with
| Ok (Complete_login (_user, token)) ->
    conn |> Fennec.Accounts.set_login_cookie accounts token |> Fennec.Conn.redirect "/"
| Ok (Step_up_required step) ->
    (* render or redirect to the app's TOTP/passkey/backup-code step-up UI *)
    Render.mfa_form
      ~user_id:step.user.id
      ~token:(Fennec.Accounts.Challenge.token_to_string step.step_up.token)
| Error e ->
    Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

The shorter `login_with_password`, `login_with_strategy`, and direct identity wrappers are still
useful for apps without MFA or for tests; they reject active MFA as
`Login_rejected "MFA step-up required"` instead of silently issuing a session.

Account-management endpoints stay similarly small. The app owns email delivery and route rendering;
Accounts owns the challenge, identity, password hash, and revocation semantics:

```ocaml
match Fennec.Accounts.issue_password_reset accounts email with
| Ok None ->
    (* Same outward response as success: do not reveal whether the email exists. *)
    Fennec.Conn.redirect "/check-your-email" conn
| Ok (Some reset) ->
    Mailer.send_password_reset email (Fennec.Accounts.Challenge.token_to_string reset.token);
    Fennec.Conn.redirect "/check-your-email" conn
| Error e ->
    Fennec.Conn.text ~status:400 conn (Fennec.Accounts.string_of_error e)
```

```ocaml
match Fennec.Accounts.reset_password_completion accounts token ~password with
| Ok (Complete_login (_user, session)) ->
    conn |> Fennec.Accounts.set_login_cookie accounts session |> Fennec.Conn.redirect "/"
| Ok (Step_up_required step) ->
    Render.mfa_form
      ~user_id:step.user.id
      ~token:(Fennec.Accounts.Challenge.token_to_string step.step_up.token)
| Error e ->
    Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

For the conventional form endpoints, helper paws remove the repetitive plumbing while keeping the
same boundary:

```ocaml
let account_routes =
  Fennec.Paw.seq
    [
      Fennec.Accounts.password_reset_request_paw
        accounts
        ~path:"/forgot-password"
        ~success:"/check-your-email"
        ~error:"/forgot-password"
        ~send:(fun reset ->
          Mailer.send_password_reset
            reset.user
            (Fennec.Accounts.Challenge.token_to_string reset.token))
        ();
      Fennec.Accounts.password_reset_paw
        accounts
        ~path:"/reset-password"
        ~success:"/"
        ~mfa_required:"/mfa"
        ~error:"/reset-password"
        ();
      Fennec.Accounts.email_verification_paw
        accounts
        ~path:"/verify-email"
        ~success:"/"
        ~mfa_required:"/mfa"
        ~error:"/verify-email"
        ();
    ]
```

## External Identity Flow

Provider modules validate their own protocol and then converge on one Accounts call. For example,
an OIDC callback exchanges the authorization code outside Accounts core, verifies the raw ID token
with `Fennec.Accounts.Oidc.verify_id_token` against cached JWKS, and then resolves the account:

```ocaml
let facts =
  Fennec.Accounts.external_identity
    principal.identity
    ?email:principal.email
    ~email_verified:principal.email_verified
    ~service:("oidc", provider_doc)

match
  Fennec.Accounts.login_with_identity_completion
    accounts
    ~strategy:"oidc"
    ~link_verified_email:true
    ~allow_signup:connection.allow_jit
    facts
with
| Ok (Complete_identity_login r) ->
    conn |> Fennec.Accounts.set_login_cookie accounts r.token |> Fennec.Conn.redirect "/"
| Ok (Identity_step_up_required step) ->
    Render.mfa_form
      ~user_id:step.user.id
      ~token:(Fennec.Accounts.Challenge.token_to_string step.step_up.token)
| Error e ->
    Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

The resolver order is deliberately small and explicit:

1. `current_user_id` links the new identity to the already logged-in user.
2. An existing `Identity.store` link logs in that user.
3. Verified email can link only when `link_verified_email=true`.
4. New user creation happens only when `allow_signup=true`.

Unverified email never auto-links. Provider-subject equality is exact identity ownership, not an
email heuristic. Normal applications use the identity facet inside the Accounts store automatically;
the optional `~identity_store` argument exists for focused tests and controlled migrations.

For the common verified-value cases, app code can skip the record construction:

```ocaml
match
  Fennec.Accounts.login_with_oidc
    accounts
    ~allow_signup:connection.allow_jit
    ~link_verified_email:true
    principal
with
| Ok r ->
    conn |> Fennec.Accounts.set_login_cookie accounts r.token |> Fennec.Conn.redirect "/"
| Error e ->
    Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

The same shape exists for consumed email links/OTPs, SAML principals, and verified passkey
assertions. Use `login_with_oidc_completion`, `login_with_saml_completion`,
`login_with_email_link_completion`, `login_with_email_otp_completion`, or
`login_with_passkey_assertion_completion` for MFA-aware callback routes. OAuth remains split at the
profile boundary because the module intentionally does not fetch provider profiles or exchange
codes; after token exchange, call `oauth_identity` with the provider subject and optional verified
email/profile facts, then `login_with_identity_completion`.

OIDC has a slightly stronger first-party battery than plain OAuth: provider discovery, JWKS
retrieval/cache, and token endpoint HTTP remain adapter-owned, but Accounts owns native JWKS parsing,
RS256 ID-token signature verification, claim validation, and identity derivation.

Provider presets remove rote endpoint/scope setup while leaving exchange/profile policy explicit:

```ocaml
let github =
  Fennec.Accounts.OAuth.Providers.github
    ~client_id:(Env.required "GITHUB_CLIENT_ID")
    ~redirect_uri:"https://app.example/auth/github/callback"
    ()

let google =
  Fennec.Accounts.Oidc.Providers.google
    ~client_id:(Env.required "GOOGLE_CLIENT_ID")
    ~redirect_uri:"https://app.example/auth/google/callback"
    ()
```

`OAuth.Providers.all` exposes fixed OAuth authorization presets for GitHub, Facebook, Discord, X,
Spotify, Reddit, Amazon, and Bitbucket. Each named helper keeps a conservative login/account scope
set and still leaves token exchange and profile fetching to the app.

`Oidc.Providers.all` exposes OIDC presets for Google, Microsoft Entra ID, Apple, LinkedIn, Slack,
GitLab, Twitch, Salesforce, Dropbox, Auth0, Okta, and Keycloak. Static providers can be built from
the catalog directly; parameterized providers use their named constructors so strict ID-token issuer
validation receives exact runtime values:

```ocaml
let entra =
  Fennec.Accounts.Oidc.Providers.microsoft_entra
    ~tenant_id:(Env.required "ENTRA_TENANT_ID")
    ~client_id:(Env.required "ENTRA_CLIENT_ID")
    ~redirect_uri:"https://app.example/auth/entra/callback"
    ()

let okta =
  Fennec.Accounts.Oidc.Providers.okta
    ~domain:"acme.okta.com"
    ~client_id:(Env.required "OKTA_CLIENT_ID")
    ~redirect_uri:"https://app.example/auth/okta/callback"
    ()
```

The split is deliberate: OAuth presets are authorize URL + scopes + state/PKCE, while OIDC presets
also define the issuer that Accounts uses for claim validation after the app exchanges the code and
passes in a verified ID token/JWKS.

Passkey registration has one additional persistence step after `Passkey.finish_registration` verifies
the WebAuthn ceremony:

```ocaml
match Fennec.Accounts.register_passkey_credential accounts credential with
| Ok _link -> Fennec.Conn.redirect "/security" conn
| Error e -> Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

Passkey assertion can persist the updated counter and then issue the normal Accounts token:

```ocaml
match Fennec.Accounts.login_with_passkey_assertion_completion accounts assertion with
| Ok (Complete_identity_login r) ->
    conn |> Fennec.Accounts.set_login_cookie accounts r.token |> Fennec.Conn.redirect "/"
| Ok (Identity_step_up_required step) ->
    Render.mfa_form
      ~user_id:step.user.id
      ~token:(Fennec.Accounts.Challenge.token_to_string step.step_up.token)
| Error e -> Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
```

## MFA And Organizations

MFA setup is account-owned but UI-owned by the app:

```ocaml
match Fennec.Accounts.enroll_totp accounts user_id with
| Ok setup ->
    (* render setup.provisioning_uri as a QR code *)
    Render.totp_setup setup.provisioning_uri
| Error e ->
    Fennec.Conn.text ~status:400 conn (Fennec.Accounts.string_of_error e)
```

After the user enters a code, `confirm_totp_enrollment` activates the factor. Later step-up calls use
`verify_totp_factor`; successful verification returns an `mfa_verification` whose assurance can be
attached with `set_assurance` for explicit step-up routes and then guarded with `require_assurance`.
Backup codes use the same pattern: `regenerate_backup_codes` returns user-visible codes once and stores
only hashes; `consume_backup_code` removes the matched hash and returns user-bound recovery
verification.

For login step-up, the completion branch or built-in route helper gives the app an `mfaToken`.
After the second factor verifies, exchange that token and assurance for the final login session:

```ocaml
match Fennec.Accounts.verify_totp_factor accounts enrollment_id ~code with
| Error e ->
    Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e)
| Ok verification -> (
    match Fennec.Accounts.complete_login_step_up accounts mfa_token verification with
    | Ok (_user, session) ->
        conn |> Fennec.Accounts.set_login_cookie accounts session |> Fennec.Conn.redirect "/"
    | Error e ->
        Fennec.Conn.text ~status:403 conn (Fennec.Accounts.string_of_error e))
```

`complete_login_step_up` consumes the single-use challenge, checks the supplied assurance against the
signed requirement, verifies that the second factor belongs to the challenge user before consuming the
token, preserves the original first-factor strategy, and issues a signed session whose later requests
satisfy MFA route guards without a store read unless `validate_every_request=true`. Apps that want the
stock browser flow can mount `mfa_totp_paw`, `mfa_backup_code_paw`,
`mfa_passkey_assertion_options_paw`, and `mfa_passkey_assertion_finish_paw` instead of writing custom
completion routes.

Organizations follow the same "core owns state, app owns presentation" split:

```ocaml
match Fennec.Accounts.issue_org_invite accounts ~org_id:"acme" ~email ~role:"member" () with
| Ok invite ->
    Mailer.send_org_invite email invite.token
| Error e ->
    Fennec.Conn.text ~status:400 conn (Fennec.Accounts.string_of_error e)
```

`accept_org_invite` checks the hashed token, pending/expiry state, and that the target user carries
the invited email before creating membership. Tenant routing and policy are still typed primitives:
`Org.route_domain`, `require_org_strategy`, `set_org_context`, and `require_org`.

## Pulse/DDP Flow

`Fennec_ddp.Session.create` accepts `?user_id`, so a websocket session can start authenticated from
an already-verified HTTP cookie. In the normal framework path, `Fennec.serve` owns the native
Accounts runtime and `Fennec_pulse_server.Make(R).paw ()` derives the websocket user id from that
runtime automatically.

Mounting live data with native Accounts identity:

```ocaml
module Live = Fennec_pulse_server.Make (App_reactive)

let live = Live.paw ()
```

Inside methods, `inv.user_id` is populated before the first call. Inside publications, `pub.user_id`
is populated from the same connection identity and `pub.params` carries subscription arguments. A
login method can still call `set_user_id`, preserving Meteor's connection rebinding semantics.

The built-in Accounts DDP methods:

- `createUser {username?, email?, password, profile?}` -> `{id, token, user}` and connection user id
- `currentUser` -> `{userId, user, authContext, assurance, org}`. Over DDP, request-only fields are
  `null`; HTTP/SSR can use `session_doc` / `session_paw` for the richer same-shape payload.
- `login selector password` -> `{id, token}` and connection user id
- `login {user, password}` -> same
- `login {strategy, credentials}` -> custom strategy
- `login {resume}` -> token resume with replacement `{id, token}`
- `logout` -> clears connection user id
- `logoutOtherClients` -> bumps `auth_epoch`, returns a replacement `{id, token}` for the current
  websocket connection, and rebinds that connection user id
- `changePassword oldPassword newPassword` -> proves the current password and bumps `auth_epoch`
- `resetPassword token newPassword` -> consumes the reset token, returns `{id, token}`, and rebinds
  connection user id
- `verifyEmail token` -> marks the email verified, returns `{id, token}`, and rebinds connection
  user id
- `enrollAccount token password` -> consumes an initial-password enrollment token, returns
  `{id, token}`, and rebinds connection user id
- `completeLoginStepUp {mfaToken, totpId, code}` -> verifies TOTP, consumes the pending MFA challenge,
  returns `{id, token}`, and rebinds connection user id
- `completeLoginStepUp {mfaToken, userId, backupCode}` -> consumes one backup code, consumes the
  pending MFA challenge, returns `{id, token}`, and rebinds connection user id

When a login-like method verifies the first factor but active MFA exists, it returns
`{mfaRequired: true, userId, mfaToken}` and does not rebind the connection. Clients then verify a
second factor through the built-in `completeLoginStepUp` method or an HTTP endpoint that uses
`complete_login_step_up`.

The websocket method returns a signed token because websocket frames cannot set an HTTP-only browser
cookie. Same-origin browser flows should still prefer an HTTP login endpoint for the cookie story;
the DDP token is useful for explicit resume/custom clients.

Client code links `fennec.accounts.client` and wraps the normal DDP client:

```ocaml
module Accounts_client = Fennec_accounts_client

let accounts = Accounts_client.connect ~path:"/websocket" ~persist:"app" ()

let current_user = Accounts_client.user accounts
let loading = Accounts_client.logging_in accounts

let submit username password =
  ignore
    (Accounts_client.login_with_password accounts
       (Accounts_client.By_username username)
       ~password)
```

The client contract is deliberately small:

- `Logged_in {id; token; user}` means the connection is rebound; the token is stored for resume unless
  `~token_key:None` disabled token storage. If the result did not include `user`, the client calls
  `currentUser` to refresh the safe user payload.
- `Mfa_required {user_id; mfa_token}` means no session was created; call
  `complete_login_step_up_totp` or `complete_login_step_up_backup` after verifying the second factor.
- `Error {code; reason}` is the DDP method error or a local `client-decode` error when a server payload
  no longer matches the documented shape.

## Security Notes

- Account secrets require a long `~secret`; short secrets fail fast.
- Session cookies are `HttpOnly`, `SameSite=Lax`, path-scoped, and optionally `Secure`.
- The cookie contains no provider access tokens and no password material.
- Password hashes use salted PBKDF2-HMAC-SHA256 by default, with constant-time verification.
- Optional password policy is enforced consistently during create, direct set, change, and reset
  flows when configured on `Accounts.make`.
- Initial password hashes are passed through the store's create operation, not written as a
  second best-effort step after user insertion.
- Password changes are passed through one hash-and-epoch operation, not split into a hash write and
  revocation write.
- Production apps that want Argon2id should pass an Argon2id `password_hasher` adapter.
- High-level TOTP enrollment stores a sealed secret and rejects tampering before verification.
- TOTP counter updates and backup-code removal use store compare-and-swap so concurrent step-up
  retries cannot double-accept stale factor state.
- Reset/verification/magic-link tokens should be random, hashed in the store, TTL-bound, and
  single-use. They are not part of the session cookie.
- OAuth/OIDC adapters should use transient signed state/PKCE cookies and store provider tokens only
  in the account store. OIDC adapters should fetch/cache JWKS and pass raw ID tokens to the native
  verifier instead of hand-rolling JWT validation.
- Provider callbacks should call `login_with_identity` after protocol validation instead of each
  adapter inventing its own link/create/session rules.
- `logout_other_clients` bumps `auth_epoch`; immediate enforcement requires
  `validate_every_request=true` or another store-backed validation point.

## Native Batteries

The first-party Accounts batteries live next to the core module and are exposed through
`Fennec.Accounts.*`:

- `Store`: Mongo/Minimongo Accounts persistence: users, identity links, challenges, passkeys,
  organizations, MFA enrollments, SCIM directory state, audit, and idempotent index setup.
- `Identity`: canonical identity keys, link/detach/merge planning, and identity-link storage.
- `Challenge`: hashed TTL one-time challenges for OAuth/OIDC/SAML state, magic links, OTPs,
  reset tokens, passkey ceremonies, and step-up.
- `Password`: password hashing and policy checks.
- `Email`: email normalization, verification, magic links, OTP login primitives, and high-level
  passwordless route helpers.
- `OAuth`: Authorization Code + PKCE provider flow.
- `Oidc`: OpenID Connect authorization, JWKS parsing, RS256 ID-token verification, and claim
  validation.
- `Saml`: SAML 2.0 request/response validation with native XML/signature handling.
- `Passkey`: WebAuthn/passkey registration/assertion verification plus high-level JSON ceremony
  helpers.
- `Mfa`: assurance levels, step-up challenges, TOTP, and backup codes.
- `Org`: organizations, memberships, verified domains, tenant SSO policy, and RBAC hooks.
- `Scim`: enterprise provisioning/deprovisioning plans, user/group PATCH, SCIM identity keys, and the
  Accounts-mounted endpoint battery.
- `Audit`: append-only account/security event records and store contract.

These modules compose through canonical user ids, identity links, challenges, and the one Accounts
store instead of growing a giant `Accounts.make` configuration record.

## Mongo Store

Accounts persistence is deliberately Mongo-shaped:

- `Store.minimongo ()`: instant in-memory backend for tests/examples using real BSON documents.
- `Store.mongo ?prefix db`: native MongoDB backend over `fennec-mongo.driver`; collections default
  to `accounts_users`, `accounts_identities`, `accounts_challenges`, `accounts_passkeys`,
  `accounts_orgs`, `accounts_org_memberships`, `accounts_org_invites`,
  `accounts_mfa_enrollments`, `accounts_scim_connections`, `accounts_scim_users`,
  `accounts_scim_groups`, and `accounts_audit`.
- `Store.ensure_indexes store`: idempotently creates unique/sparse user indexes, identity lookup
  indexes, challenge expiry/user/email indexes, passkey credential indexes, org/domain indexes, MFA
  user indexes, SCIM tenant indexes, and audit query indexes for Mongo. It is a no-op for
  Minimongo.
- Invariants stay in store operations: create user with initial password hash, password hash plus
  auth epoch bump, challenge consume, identity attach/detach/merge, MFA enrollment compare-and-swap
  for replay counters/backup-code removal, and identity login/link/create.

Argon2id or provider-specific clients can still be passed as small adapters, but account persistence
itself is not an open-ended SQL/Redis abstraction.
