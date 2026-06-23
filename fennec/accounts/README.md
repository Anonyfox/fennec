# `fennec.accounts`

Meteor-style accounts, hard-wired into Fennec: passwords, signed-cookie sessions, email
verification / magic-links / OTP, MFA (TOTP + backup codes), passkeys (WebAuthn), OAuth, OIDC,
SAML, SCIM provisioning, organizations, and code-declared RBAC — all over one Mongo-shaped store
(`minimongo` / embedded `burrow://` / `mongod`, behind one backend-blind handle).

Accounts is not a pluggable adapter; it is the framework's identity substrate. `Fennec.serve`
prepends the identity paw to every endpoint and registers the built-in DDP methods automatically,
so a handler just reads the request user:

```ocaml
let handler conn =
  match Fennec.Accounts.user_id conn with
  | Some uid -> Paw.Conn.text conn ("hello " ^ uid)
  | None     -> Paw.Conn.redirect conn "/login"
```

The substrate is always present but inert until you turn features on: with no `MONGO_URL` and no
config, every request resolves to `Accounts.user_id conn = None`, database-backed operations fail
with a clear `Store_error`, and the full route/method set is still there (just unused). Turning a
feature on never weakens the default posture — the enumeration guard, the login throttle, and the
verified-email gate always apply.

The full public interface is `accounts.mli` (every value below is declared there); the per-module
deep-dives live under [`docs/`](docs/README.md).

---

## SSR + the reactive user (the anonymous, cacheable frame)

Fennec renders the page **server-side first**, then hydrates it in the browser. The contract for
identity across that boundary is deliberate and worth stating outright:

> **The SSR pass is the anonymous, cacheable frame; personalization snaps in on hydration.**

**The mechanism.** The SSR data fetchers are **user-blind** — they take the published-document
*params* only, never a `userId` (the server-side publication registry is keyed by name alone). So
during the server render every userId-scoped subscription resolves as if **`userId = None`**: the
always-on `__currentUser` publication yields no document, and the client-side
`Fennec_accounts_client.user` / `user_id` signals (Meteor's `Meteor.user()` / `Meteor.userId()`)
read `None`. The server-rendered HTML is therefore **identical for every visitor, signed-in or
not**.

Then the browser takes over: the WebSocket upgrade carries the (HttpOnly) login cookie, the server
seeds the session `userId`, the userId-scoped subscriptions **re-scope** to the real user, the merge
store fills in, and the signals recompute. Personalization **"snaps in"** over the anonymous frame —
no full re-render, just the reactive slots updating.

**Why this is the default (and a security property, not just a cache win).** Because the SSR frame
is byte-identical for everyone, it is uniformly **edge/CDN-cacheable**, and it **structurally cannot
leak one user's private data into a shared cached frame** — there is no per-user data in it to leak.
Per-user SSR would be the opposite trade: the first paint would already be personalized (no snap),
but the HTML would be uncacheable and one user's data would be one cache-key mistake away from
another user's screen. Fennec picks cacheable-and-safe by default.

**The userland rules.** Author components so the SSR frame is correct and public:

1. **Render the anonymous / `None` view for SSR.** Read the live signal and branch on it; the `None`
   arm is what the server emits and what every cached visitor sees first.

   ```ocaml
   module Accounts = Fennec_accounts_client

   let make () =
     let user = Accounts.current_user () in   (* SETUP: user option Fur.signal (live sub) *)
     <span className="user-badge">            (* RENDER (trailing body): re-runs when `user` fills in *)
       (match !user with
        | None   -> <a href="/login">"Sign in"</a>           (* the SSR / cacheable frame *)
        | Some u -> ("Hello, " ^ Option.value u.username ~default:"there"))
     </span>
   ```

2. **Reserve the layout so the snap is a fill-in, not a layout shift.** Give the personalized slot a
   fixed size (height / min-width) so the `None → Some` transition is a clean swap, not a reflow.
   The "snap" should be visible as content appearing, never as the page jumping.

3. **Auth-gated content renders its *gate*, never the protected content.** The SSR frame is
   public and cacheable, so a component behind a login must server-render a **skeleton / redirect /
   "sign in to continue"** placeholder and only reveal the protected content after hydration (when
   `user` becomes `Some` and, if you need it, a role check passes). Never put data you would not put
   on a billboard into the SSR output of a gated view — it will be cached and served to anonymous
   visitors. (For hard server-side enforcement of a *route*, the request-time guards —
   `Accounts.require_user` / `require_role` / `require_permission`, RBAC below — still apply; this
   rule is about what the *rendered component tree* exposes.)

The live signals (`user`, `user_id`, `logging_in`) and the verbs (`login_with_password`, `logout`,
…) are the browser-side `Fennec_accounts_client` facade (`fennec.accounts.client`); the server side
is `Accounts.user_id conn` in a handler. A worked end-to-end example of the skeleton→snap pattern —
a fixed-slot badge that renders "Sign in" for SSR and "Hello, …" after hydration — is
[`examples/site/frontend/components/user_badge.mlx`](../../examples/site/frontend/components/user_badge.mlx)
(mounted in the web app's `layout.mlx`; its SSR `None`-frame is asserted by the inline `let%test` in
that same file).

That is **Mode A** — a file-tree SPA page (`Paw.app (Fur_ssr.handler …)`), which is Conn-blind by
design. There is a second mode for the pages a classical server-rendered, authenticated app is built
from, and it makes the *opposite* trade.

### Mode B — the personalized server handler (seeded, no snap)

A **standalone handler** (`frontend/handlers/<name>.mlx`, mounted with `Paw.get "/path"
Handler.serve`) is the other half of the story. Unlike a Mode-A page, its server entry point —
`load : conn -> 'p outcome` — **has the Conn**. So it can read the request user *server-side*, render
the page **already personalized**, and **seed exactly that payload**; the client then decodes the same
seed and hydrates **byte-identical — no anonymous frame, no snap**. (`load` is server-only: the
`-handler` ppx strips it from the jsoo bundle, so the Conn/Accounts code never reaches the client.)

The contract, end to end:

> **A Mode-B `load` reads the userId off the Conn, renders personalized server-side, and seeds the
> payload — so the first paint is already the user's, and hydration is byte-identical.**

Inside `load`, both `Accounts.user_id conn` and `Accounts.current_user (Accounts.current ()) conn`
(*"a convenience for SSR/handlers"*) are in scope. The blessed protected-handler idiom is the
`Handler.guard` combinator — `match Accounts.user_id conn with Some uid -> … | None -> redirect login`,
named:

```ocaml
(* frontend/handlers/me.mlx — a personalized dashboard, authored as ONE .mlx *)
module Email = struct
  type t = { address : string; verified : bool } [@@deriving model]
end

(* the payload is EXACTLY the user-scoped data the page shows — this is what gets seeded + hydrated *)
type t = { username : string; roles : string list; emails : Email.t list } [@@deriving model]

(* SERVER ONLY (stripped from the client bundle). [guard] bounces anonymous requests to /login; past
   it, load the record and seed the trimmed payload via [render]. *)
let load conn =
  guard conn ~user:Accounts.user_id ~login:"/login" @@ fun _uid ->
  match Accounts.current_user (Accounts.current ()) conn with
  | Ok (Some u) ->
    let emails =
      List.map (fun (e : Accounts.email) -> { Email.address = e.address; verified = e.verified }) u.emails
    in
    render
      { username = Option.value u.username ~default:"(no username)";
        roles = Accounts.Roles.role_names u.roles;
        emails }
  | Ok None | Error _ -> redirect "/login"

(* ISOMORPHIC: payload -> vnode. SSRs on the server AND hydrates from the seed — sees only [t]. *)
let view (p : t) = (* … render p.username / p.roles / p.emails … *)
```

`guard` (like `render` / `redirect`) is part of the handler runtime the `-handler` ppx opens inside
`load`, so userland writes it bare. Its Conn type is abstract, so the caller passes the id extractor
(`~user:Accounts.user_id`). On `Some uid` it runs the body, on `None` it `redirect`s to `login` — a
server-side bounce *before* any personalized `render`.

**Mode A vs Mode B — the decision.**

| | **Mode A** — file-tree SPA page | **Mode B** — standalone handler |
|---|---|---|
| wired | `Paw.app (Fur_ssr.handler …)` | `Paw.get "/p" Handler.serve` |
| SSR sees | no Conn (user-blind) | the **Conn** (the request user) |
| first paint | anonymous, **cacheable** shell | **personalized**, authorized |
| personalization | reactive **snap** on hydration | **seeded** in the payload, no snap |
| use for | content / marketing / app-shells | dashboards / authed pages / classical SSR apps |

The nuance that ties the two together: in Mode B, **put user-scoped data in the payload** — that is
what is rendered personalized *and* seeded for byte-identical hydration; reactive subscriptions
(`Fennec_accounts_client`, the `__currentUser` signal) remain the **Mode-A** post-hydration mechanism
and stay the right tool for data that changes *live after* first paint. Pick Mode B when the page is
private and must be correct on the first byte; pick Mode A when the shell is public and the
personalization can fill in a beat later.

A worked, runnable example is
[`examples/site/frontend/handlers/me.mlx`](../../examples/site/frontend/handlers/me.mlx) (mounted at
`/me` in `examples/site/server.ml`, alongside the `/login` + `/logout` routes that make it live; its
personalized SSR frame is asserted by the inline `let%test_unit` in that same file).

---

## The DX: one declarative config object

A whole app is configured from a single value. Start at `Accounts.defaults` and override one field
at a time; hand it to `Fennec.serve ~accounts`. `defaults` reproduces today's behaviour exactly —
every optional feature off, secure password defaults, framework session/route defaults, no
providers — so the config only ever turns features *on*.

The umbrella record (`accounts.mli`, `type config`):

```ocaml
type config = {
  session   : session_config;
  password  : password_config;
  mail      : mail_config option;          (* Some ⇒ the password/email routes are wired *)
  passkeys  : passkeys_config option;      (* Some ⇒ the passkey routes are wired *)
  orgs      : orgs_config option;          (* orgs.scim_prefix = Some p ⇒ the SCIM battery at p *)
  rbac      : Roles.policy option;         (* the app's role→permission map; None ⇒ empty policy *)
  routes    : routes_config;               (* { auth_prefix; me_path } *)
  providers : external_identity provider list;   (* SSO; each mounts authorize + callback routes *)
}
```

### Step 0 — zero config

Accounts is on, anonymous identity is `None`, password login + sessions work, the DDP methods are
registered. Nothing to write — just don't pass `~accounts`.

```ocaml
let () = Fennec.serve [ web ]
```

### Step 1 — require a verified email before any session

```ocaml
let accounts =
  { Accounts.defaults with
    password = { Accounts.defaults.password with require_verified_email = true } }

let () = Fennec.serve ~accounts [ web ]
```

To actually send the verification / reset / enrollment emails, set `mail` — this also mounts the
password/email HTTP routes under `routes.auth_prefix`:

```ocaml
let accounts =
  { Accounts.defaults with
    mail = Some { from = "no-reply@acme.test"; site_name = Some "Acme"; templates = None } }
```

(`templates = None` uses the built-in Fur templates keyed off `from` / `site_name`; supply
`Accounts.Mailer.default ~from ()` to override.)

### Step 2 — add one OAuth provider (a one-liner)

A provider preset bundles the authorize/token/userinfo endpoints plus the default token-exchange
HTTP, so a provider is a single call. The presets return a `result` (endpoint config can be
malformed — a bad `redirect_uri`, an empty `client_id`), so unwrap it into the list. Prefer a *loud*
unwrap over a bare `Result.get_ok`: at boot a misconfig should fail with **why**, not a context-free
`Invalid_argument "Option.get"`. Each protocol module renders its own error, so pass the matching
`string_of_error` to a small helper:

```ocaml
(* fail loudly at startup with the actual reason; the renderer is per-protocol *)
let or_die to_s = function Ok v -> v | Error e -> failwith (to_s e)

let github =
  Accounts.OAuth.github
    ~redirect_uri:"https://app.acme.test/auth/github/callback"
    ~client_id:"…" ~client_secret:"…" ()
  |> or_die Accounts.OAuth.string_of_error

let accounts = { Accounts.defaults with providers = [ github ] }
```

The auto-wiring mounts `GET <auth_prefix>/github` (authorize) and
`GET <auth_prefix>/github/callback` for you.

### Step 3 — OIDC SSO, optionally mapping claims to app roles

`?role_map` turns the verified principal into app-wide role strings, applied after the login
succeeds (it is inert when omitted):

```ocaml
let google =
  Accounts.Oidc.google
    ~redirect_uri:"https://app.acme.test/auth/main/callback"
    ~client_id:"…" ~client_secret:"…"
    ~role_map:(fun principal ->
        match principal.email with
        | Some e when Filename.check_suffix e "@acme.test" -> [ "admin" ]
        | _ -> [])
    ()
  |> or_die Accounts.Oidc.string_of_error   (* same helper as Step 2, OIDC renderer *)

let accounts = { Accounts.defaults with providers = [ google ] }
```

Other OIDC presets: `Oidc.microsoft ~tenant_id`, `Oidc.okta ~domain`, `Oidc.auth0 ~domain`,
`Oidc.keycloak ~base_url ~realm`. SAML: `Saml.okta ~issuer ~sso_url ~entity_id ~acs_url
~trusted_keys` (the IdP POSTs a signed assertion to the ACS route — no token-exchange HTTP).
Escape hatches keep the exchange app-owned: `OAuth.custom ~exchange`, `Oidc.from_connection`,
`Saml.from_connection`.

### Step 4 — passkeys (WebAuthn)

Setting `passkeys` mounts the registration/assertion JSON routes:

```ocaml
let rp =
  Accounts.Passkey.relying_party ~id:"app.acme.test" ~name:"Acme" ()
  |> or_die Accounts.Passkey.string_of_error

let accounts = { Accounts.defaults with passkeys = Some { relying_party = rp } }
```

### Step 5 — orgs + RBAC

`rbac` declares the one role→permission policy the whole app evaluates against (route guards, `can`,
org-scoped checks — all read it). `orgs.scim_prefix` mounts the SCIM 2.0 provisioning battery:

```ocaml
let policy =
  Accounts.Roles.policy
    [ Accounts.Roles.role Accounts.Roles.Role.admin
        [ Accounts.Roles.Permission.v_exn "billing.write" ] ]

let accounts =
  { Accounts.defaults with
    rbac = Some policy;
    orgs = Some { scim_prefix = Some "/scim/v2" } }
```

Then guard routes with the no-argument forms (they read the configured singleton — see RBAC below):

```ocaml
let admin =
  Paw.Endpoint.make ~name:"admin" ()
  |> Paw.Endpoint.pipe_matched
       [ Accounts.require_user ();
         Accounts.require_permission (Accounts.Roles.Permission.v_exn "billing.write") () ]
```

### The whole thing, stacked

Each step is additive — the final config is just every override on one record:

```ocaml
let accounts =
  { Accounts.defaults with
    password  = { Accounts.defaults.password with require_verified_email = true };
    mail      = Some { from = "no-reply@acme.test"; site_name = Some "Acme"; templates = None };
    passkeys  = Some { relying_party = rp };
    orgs      = Some { scim_prefix = Some "/scim/v2" };
    rbac      = Some policy;
    routes    = { auth_prefix = "/auth"; me_path = Some "/me" };
    providers = [ github; google ] }

let () = Fennec.serve ~accounts [ web; admin ]
```

`Fennec.serve` hands the config to `Accounts.start`, which applies it to the process-native instance
(`Accounts.current ()`) and folds it through `Accounts.Wiring` into a route table + a method gate.
You can also call `Accounts.start ~config ()` directly outside `serve`.

### Configuration ordering

The umbrella `config` is applied to the process-native singleton on the **first** access — `Fennec.serve
~accounts` / `Accounts.start ~config` must run **before** the first `Accounts.current ()`. The
singleton's session fields (`cookie` / `path` / `lifetime` / `validate_every_request`) and its `policy`
are construction-time: they are frozen when the instance is first built, so a later `start` cannot
change them on an already-materialised singleton. Hooks (below) and the mutable password policy
(`Accounts.configure`) can be set afterwards; the session shape and RBAC policy cannot. In practice:
configure once, at startup, before any route handler or hook registration runs.

---

## Hooks

Meteor-style extensibility points, registered on the process-native instance (`Accounts.current ()`)
in startup. They are observe/veto callbacks, not config fields, so register them after `start` /
`Fennec.serve ~accounts` (see ordering, above).

```ocaml
let accounts = Accounts.current ()

(* Veto or observe a credential login AFTER the credentials verify, BEFORE a session is issued.
   Returning Error rejects the login (password / passwordless paths). *)
let () =
  Accounts.validate_login_attempt accounts (fun (attempt : Accounts.login_attempt) ->
      match attempt.user with
      | Some u when List.exists (fun (e : Accounts.email) -> e.address = "banned@acme.test") u.emails ->
        Error "account disabled"
      | _ -> Ok ())

(* Observe a successful login (metrics, last-seen, welcome side effects). *)
let () = Accounts.on_login accounts (fun (u : Accounts.user) -> Printf.printf "login: %s\n" u.id)

(* Observe a FAILED login — unknown account / wrong password / inactive / a validate veto.
   The attempt carries allowed = false and a reason tag; the reactable twin of the audit log. *)
let () =
  Accounts.on_login_failure accounts (fun (attempt : Accounts.login_attempt) ->
      Printf.printf "login failure (%s): %s\n" attempt.strategy
        (Option.value attempt.reason ~default:"?"))

(* Veto an EXTERNAL (OAuth / OIDC / SAML / identity) login just before it mints a session —
   Meteor's beforeExternalLogin. Any hook returning false aborts. This is the SSO-deny gate:
   a provider `role_map` only ENRICHES roles after login, it cannot reject; deny here instead. *)
let () =
  Accounts.before_external_login accounts (fun ~strategy:_ ~identity ~user:_ ->
      match identity.email with
      | Some e -> Filename.check_suffix e "@acme.test"   (* only this domain may SSO in *)
      | None -> false)
```

Other hooks: `Accounts.on_create_user` (reject/alter a new user before insertion, `user -> (user,
string) result`), `Accounts.on_logout` (`user_id option -> unit`), and `Accounts.register_strategy`
(register/replace a custom login strategy).

---

## Environment variables

| Variable | Purpose | Default / prod requirement |
| --- | --- | --- |
| `FENNEC_ACCOUNTS_SECRET` | The stable secret that signs session cookies/tokens (HMAC). | Optional in dev: unset ⇒ a fresh **ephemeral** per-process secret is minted, so every restart invalidates all sessions and multiple instances cannot share one. **Required in prod** (≥ 16 bytes) for durable, horizontally-shared sessions. Set-but-shorter-than-16-bytes is a hard startup error. |
| `FENNEC_URL` | Base URL for the email action links (`verify-email` / `reset-password` / `enroll-account` / login-code). | Defaults to `http://localhost`. Set it to your public origin in prod, or pass a per-call `~link` to the `send_*_email` verbs. |
| `MONGO_URL` | Selects the data backend the Accounts store rides (shared with the rest of the framework). | Unset ⇒ the store is **unavailable** (anonymous identity is `None`; DB-backed ops fail with a clear `Store_error`). `:memory:` ⇒ minimongo (tests); `burrow://<path>` ⇒ the embedded engine; a `mongodb://…` URL ⇒ `mongod`. |

---

## Architecture

The library is layered bottom-up; each layer opens only the ones below it. Two leaf sub-libraries
(`fennec.accounts.primitives`, `fennec.accounts.features`) hold the pure, HTTP-free,
storage-free logic; the engine, store, HTTP, and facade sit in `fennec.accounts` itself.

### `fennec.accounts.primitives` — the pure leaves (no cross-module deps)

| Module | Responsibility |
| --- | --- |
| `Accounts_identity` | external-identity keys (oauth/oidc/saml/passkey/scim subjects) + the link store |
| `Accounts_challenge` | single-use HMAC-verified, TTL'd, replay-safe tokens / codes / state / nonces |
| `Accounts_password` | PBKDF2-HMAC-SHA256 hashing + policy, constant-time verify |
| `Accounts_rate_limit` | the token-bucket brute-force throttle |
| `Accounts_roles` | typed `Role` / `Permission` + the `policy` (role → permission map) |
| `Accounts_audit` | the redacting append-only audit log |

### `fennec.accounts.features` — per-protocol auth, on top of the primitives (no HTTP, no store)

| Module | Responsibility |
| --- | --- |
| `Accounts_email` | email ownership, verification, magic-link, OTP |
| `Accounts_mfa` | TOTP + backup codes + step-up factors + assurance |
| `Accounts_org` | organizations, memberships, invites, domains, tenant auth policy |
| `Accounts_oauth` | OAuth2 + PKCE |
| `Accounts_oidc` | OpenID Connect (RS256 ID-token verification, over `oauth`) |
| `Accounts_passkey` | WebAuthn registration / assertion |
| `Accounts_saml` | SAML 2.0 request/response + XML signature |
| `Accounts_scim` | SCIM 2.0 provisioning (over `identity` + `org`) |

### The engine — `accounts_base.ml` (a thin glue: `include`s the modules below, in dep order)

| Module | Responsibility |
| --- | --- |
| `Accounts_types` | the data model (`user`, `email`, `token`, `auth_context`, the `provider` variant), aliases, helpers |
| `Accounts_secrets` | CSPRNG/hashing/constant-eq, the signed-session codec + revocation gate, the MFA-secret seal (touch with care) |
| `Accounts_identity_bridge` | `external_identity` + per-provider identity glue + service factories |
| `Accounts_runtime` | `make` / `configure` / hooks / audit + the strategy→audit/mechanism maps (breaks the lifecycle↔login cycle) |
| `Accounts_request` | the cookie-verifying paw + the `require_*` guards + `can` / `can_in` |
| `Accounts_lifecycle` | user / password / email / role / MFA-enrollment / org / identity-link writes |
| `Accounts_login` | session issue + the `login_with_*` family + step-up + the session store verbs |
| `Accounts_http` | the `*_paw` route constructors + SCIM provisioning + the JSON/BSON helpers |

### Store, HTTP, methods, config, facade

| Module | Responsibility |
| --- | --- |
| `Accounts_codec` | the record↔BSON codecs (the plumbing under `Store`) |
| `Accounts_collection_store` | the backend-blind collection-store builder |
| `Accounts_native` | the `Store` over any runtime-selected backend, the process-native singleton (`current` / `boot` / `start`), and `Wiring` (config → route table + method gate) |
| `Accounts_methods` | the Meteor-shaped DDP `Methods` functor (`createUser` / `login` / `logout` / …) |
| `Accounts_session` | the HTTP session-view serializers (`session_doc` / `session_paw`) for SSR/`/me` |
| `Accounts_provider_presets` | the one-liner provider constructors (`OAuth.github`, `Oidc.google`, `Saml.okta`, …) |
| `Accounts_http_client` | the ambient fail-closed outbound-HTTPS transport the presets call (installed by `Fennec.serve`) |
| `Accounts_profile` | the optional Sift-typed view of `user.profile` (`profile_codec` / `typed_profile` / `set_typed_profile`) |
| `Accounts_mailer` | the transactional email templates (rendered with Fur, delivered through `fennec.mail`) |
| `accounts.ml` | the public facade: a 38-line glue that re-exports the engine + satellites to satisfy `accounts.mli` |

### The two public faces

The top-level `Accounts.*` namespace carries the ~40 daily-drivers: `user_id`, `create_user`,
`login_with_password`, `logout`, `require_user` / `require_role` / `require_permission`,
`verify_token`, `list_sessions` / `revoke_session`, the `send_*_email` verbs, and the config
(`defaults` / `start` / `Wiring`). Everything a 95%-of-apps newcomer never touches —
the `*_paw` route constructors (for custom URL layouts; the config mounts the defaults), the
MFA-aware `*_completion` login branches, the protocol identity resolvers + `external_identity`
builders, identity-link administration, the passkey ceremony primitives, the low-level codecs, and
the deprecated explicit-instance guard shims — is grouped under **`Accounts.Advanced`**. Nothing was
removed from the library; `Advanced` is a discoverability grouping in `accounts.mli`.

---

## Security model

Match the code, not the marketing — these are the actual guarantees (`accounts_secrets.ml`,
`primitives/accounts_password.ml`, `primitives/accounts_rate_limit.ml`).

- **Password hashing.** PBKDF2-HMAC-SHA256, **600 000 iterations** by default (the OWASP-2023
  floor), random per-user salt, stored as `pbkdf2-sha256$<iters>$<salt>$<derived>`. Verification is
  **constant-time** (`constant_eq` accumulates a difference over the full length).

- **Sessions.** The browser session is a **signed cookie** carrying only non-secret identity
  metadata (HMAC-SHA256 over the encoded session). Normal request authentication is stateless and
  horizontal — zero store reads on the fast path. The token store keeps only **hashed** tokens
  keyed by a non-secret `session_id`; it is the source of truth for **per-session revocation**.
  `verify_token` (and resume) **always** consult that row, so `revoke_session` / `logout` /
  `logout_other_clients` take effect immediately on those paths; `validate_every_request` (off by
  default) additionally re-checks the row + the `auth_epoch` on *every* request. Recording a session
  is **fail-closed**: if the row cannot be persisted, the login fails atomically.

- **MFA-secret seal.** TOTP secrets are written through `seal_mfa_secret` — an authenticated
  HMAC-keystream seal (versioned `v1`, MAC verified constant-time) keyed by the instance secret.
  Unsealing is **fail-closed**: any value that is not a valid `v1` seal reads as absent, never as
  plaintext, so a tampered or unsealed stored value can't defeat the seal.

- **Enumeration resistance.** Password login returns one indistinguishable error for
  unknown-account vs wrong-password by default (`password.ambiguous_error_messages`; the hooks and
  audit log still see the real reason). `issue_password_reset` / `send_reset_password_email` return
  success even for addresses with no account, so the UX cannot be used as an account oracle.
  *Deliberate trade-off:* signup is **not** non-enumerating — `create_user` / the `createUser` method
  return a distinct `Duplicate_email` / `Duplicate_username` so the form can say "that's taken"
  (Meteor parity; a usable-signup-UX vs strict-enumeration-resistance call). Front a public signup with
  the throttle (already on) and, if you need it, a CAPTCHA.

- **Brute-force throttle.** A token-bucket limiter (`Accounts_rate_limit.make`) guards the `login`
  and `createUser` DDP methods — **5 attempts / 10 s** by default (Meteor's `DDPRateLimiter`),
  keyed on **both** the client IP and the login selector, so neither a single IP nor a single
  account can be hammered. A throttled attempt spends nothing; retune or disable via the limiter.

- **SSO transport.** The provider presets' default token-exchange runs over the framework's
  prod-safe outbound HTTPS client (tls-eio + x509 + ca-certs, peer **verified against the OS trust
  store, never downgraded**), captured once at boot. OIDC presets verify the **RS256 ID-token
  signature + iss/aud/exp/nonce** before trusting a principal; OAuth/OIDC carry PKCE + state; SAML
  verifies the XML signature against pinned `trusted_keys`. Email is trusted as verified only when
  the provider attests it (`email_verified`). `before_external_login` is the per-login veto for
  domain/org allow-lists. *Note:* a provider `role_map` is post-login **enrichment** — it maps the
  verified principal to app roles *after* the session is minted, so it can never **deny** a login (and
  a failure to apply it is non-fatal, surfaced to the audit log as a `Role_change` / `Failure` event).
  To gate who may SSO in at all, return `false` from `before_external_login`.

The crypto in `Accounts_secrets` and `Accounts_password` was carried **verbatim** through the
refactor — the enumeration / replay / seal guarantees did not move.

---

## Testing

The house pattern is **inline `let%test` colocated** with the code it exercises (the `fennec-hunt`
runner), run against in-memory stores (minimongo). The leaves and features test themselves; the
engine-level integration + HTTP/DDP-system suite lives in **`accounts_integration_test.ml`** — a
dedicated test module rather than inline, because its shared in-memory fixture sits at the
`Accounts_native` (`Store`) layer, *above* the whole engine, so it cannot be pushed down into the
engine leaves. Two system tests live under [`test/`](test/):

- `test/accounts_backends_test.ml` — drives the **public** Accounts API over the embedded
  `burrow://` engine (a temp dir, no `mongod`) and asserts backend parity: password login + token +
  username login, case-insensitive unique email/username, sparse username indexes, per-session
  revocation, and RBAC-guard equivalence. Runs under `dune runtest`; fails the build if accounts
  ever breaks on burrow.
- `test/acme_pebble_test.ml` — an end-to-end ACME proof against a local pebble server, gated on
  `FENNEC_ACME_TEST_DIR` (skips otherwise); run manually / in CI with pebble up.

Run the suite:

```sh
dune runtest fennec/accounts
```

which covers the engine integration suite (1221 tests) plus `fennec/accounts/features` (950),
`fennec/accounts/primitives` (78), and the burrow parity exe. `dune build @check` and
`dune build @all` cover the whole workspace.
