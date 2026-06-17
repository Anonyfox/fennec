# Accounts.OAuth

`Fennec.Accounts.OAuth` owns the provider-neutral OAuth 2.0 Authorization Code + PKCE ceremony:
provider configuration, authorization URL generation, single-use state, callback parsing, and stable
provider-subject identity keys.

The module deliberately does not perform HTTP token exchange, fetch provider profiles, store provider
tokens, create users, merge identities, or issue Accounts session tokens. Those steps belong to app
code or provider-specific adapters built on top of this primitive.

## Baseline

- Authorization Code flow only.
- PKCE S256 always.
- State is required, TTL-bound, purpose-bound, and single-use.
- Redirect URI is bound into state metadata and sent exactly in the authorization URL.
- Provider access/refresh tokens never live in Accounts session cookies.
- Provider identity is keyed by provider subject, not by email.

## Provider Configuration

`provider` validates and normalizes provider settings:

```ocaml
let github =
  Fennec.Accounts.OAuth.provider
    ~name:"github"
    ~authorize_url:"https://github.com/login/oauth/authorize"
    ~client_id:(Sys.getenv "GITHUB_CLIENT_ID")
    ~redirect_uri:"https://app.example.com/oauth/github/callback"
    ~scopes:[ "read:user"; "user:email" ]
    ()
```

The provider name is lowercased and becomes the identity namespace. Scopes are trimmed and blank
scopes are dropped. Extra authorization parameters can be supplied with `~extra_params`; standard
OAuth/PKCE parameters such as `state`, `redirect_uri`, `scope`, and `code_challenge` are reserved
and rejected there.

## Authorization

`authorize` creates a high-entropy PKCE verifier/challenge pair, creates a
`Challenge.OAuth_state` record, stores the PKCE verifier in challenge metadata, and returns a URL for
the browser redirect.

```ocaml
let oauth = Fennec.Accounts.OAuth.make ~challenge in

match Fennec.Accounts.OAuth.authorize oauth ~redirect:"/dashboard" github with
| Ok authorization ->
    Fennec.Conn.redirect conn authorization.url
| Error err ->
    Fennec.Conn.text ~status:400 conn (Fennec.Accounts.OAuth.string_of_error err)
```

State metadata includes:

- provider name
- redirect URI
- PKCE verifier
- optional user id for linking flows
- optional org id for enterprise routing
- optional post-login redirect

The raw state token is rendered only as the OAuth `state` query parameter. The stored challenge keeps
only the hashed token secret through `Accounts.Challenge`.

## Callback

Callback handlers first parse the query string:

```ocaml
match Fennec.Accounts.OAuth.parse_callback conn.req.query_string with
| Ok (Code { code; state }) -> ...
| Ok (Callback_error { error; description; state = _ }) -> ...
| Error err -> ...
```

Then consume state:

```ocaml
match Fennec.Accounts.OAuth.consume_state oauth ~expected_provider:"github" state with
| Ok state ->
    (* exchange [code] with [state.code_verifier] and [state.redirect_uri] *)
| Error err -> ...
```

`expected_provider` rejects a callback routed to the wrong provider handler before consuming the
state challenge. Correct callbacks still consume atomically, so replay fails.

## Identity Rules

Provider identity comes from the provider's stable subject:

```ocaml
let key = Fennec.Accounts.OAuth.identity github ~subject:profile_sub
```

Link by provider subject first. Email can assist merge only when the provider explicitly reports it
as verified and the application's connection policy trusts that provider. Provider emails can be
missing, unverified, reused, or changed; they must not replace subject-based identity.

## Token Storage

Access tokens, refresh tokens, expiry, granted scopes, and provider profile snapshots are
application-store data. They should be encrypted or otherwise protected according to the app's data
layer. They must never be placed in the signed Accounts login cookie.

## Edge Cases

- User cancels consent: `parse_callback` returns `Callback_error`.
- Callback replay: second `consume_state` fails as already consumed.
- State expiry: callback fails closed.
- Callback routed to the wrong provider: `expected_provider` rejects without burning state.
- Provider returns no email: link or create by provider subject only.
- Provider returns unverified email: do not auto-link by email.
- Provider email changes: keep the same provider-subject identity.
- Existing password user has the same verified email: merge only under explicit trusted-provider
  policy.
- Existing OAuth identity belongs to another user: store-level identity uniqueness must reject or
  enter an explicit merge/account-link flow.

## Tests

- Provider names/scopes normalize.
- Blank provider fields are rejected.
- Reserved extra authorization parameters are rejected.
- PKCE verifier and challenge are OAuth-safe and standards-shaped.
- Authorization URL contains response type, client id, redirect URI, state, PKCE challenge, S256,
  scopes, and extra parameters.
- Callback parser handles code and provider errors.
- State is purpose-bound, TTL-bound, and single-use.
- Wrong-provider precheck does not consume state.
- Provider-subject identity delegates to `Accounts.Identity`.
