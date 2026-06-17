# Accounts.Oidc

`Fennec.Accounts.Oidc` owns the deterministic OpenID Connect ceremony around Authorization Code +
PKCE + nonce: connection configuration, authorization URL generation, single-use state, callback
parsing, RS256 ID-token verification against JWKS, verified-claims validation, and canonical
OIDC/email identity evidence.

The module deliberately does not fetch discovery documents, cache JWKS, exchange authorization
codes, persist provider tokens, create users, merge identities, or issue Accounts session tokens.
Provider-specific adapters perform discovery/JWKS retrieval and token exchange, then pass the JWKS
and raw ID token to `verify_id_token` or pass already-verified claims to `validate_claims`.

## Baseline

- Authorization Code flow only.
- PKCE S256 always.
- `openid` scope is always present.
- Nonce is required and bound into challenge metadata.
- State is TTL-bound, purpose-bound, and single-use through `Challenge.Oidc_state`.
- Issuer and audience/client id are exact checks.
- Redirect URI, client id, issuer, connection id, nonce, and PKCE verifier are bound into state.
- Provider access/refresh tokens never live in Accounts session cookies.
- Stable identity is OIDC issuer + connection id + subject, not email.

## Connection Configuration

`connection` validates and normalizes an OIDC connection:

```ocaml
let main =
  Fennec.Accounts.Oidc.connection
    ~id:"main"
    ~issuer:"https://idp.example"
    ~authorize_url:"https://idp.example/oauth2/v1/authorize"
    ~client_id:(Sys.getenv "OIDC_CLIENT_ID")
    ~redirect_uri:"https://app.example.com/oidc/main/callback"
    ~scopes:[ "email"; "profile" ]
    ~domains:[ "example.com" ]
    ~org_id:"org_123"
    ()
```

Connection ids are lowercased and become part of the identity namespace. Scopes are trimmed and
`openid` is inserted when missing. Extra authorization parameters can be supplied with
`~extra_params`; standard OAuth/OIDC parameters such as `state`, `nonce`, `scope`, `redirect_uri`,
and `code_challenge` are reserved and rejected there.

`domains` is an admission/routing policy for enterprise connections. Domain ownership alone is not
login proof and must not replace issuer/subject identity.

## Authorization

`authorize` creates PKCE material, generates a nonce, stores both in a single-use
`Challenge.Oidc_state`, and returns the provider redirect URL.

```ocaml
let oidc = Fennec.Accounts.Oidc.make ~challenge in

match Fennec.Accounts.Oidc.authorize oidc ~redirect:"/dashboard" main with
| Ok authorization ->
    Fennec.Conn.redirect conn authorization.url
| Error err ->
    Fennec.Conn.text ~status:400 conn (Fennec.Accounts.Oidc.string_of_error err)
```

State metadata includes:

- connection id
- issuer
- client id
- redirect URI
- PKCE verifier
- nonce
- optional user id for linking flows
- optional org id for enterprise routing
- optional post-login redirect

## Callback

Callback handlers parse the query string, then consume state:

```ocaml
match Fennec.Accounts.Oidc.parse_callback conn.req.query_string with
| Ok (Code { code; state }) ->
    begin match Fennec.Accounts.Oidc.consume_state oidc ~expected_connection:"main" state with
    | Ok state -> (* exchange [code] using [state.code_verifier] and [state.redirect_uri] *)
    | Error err -> ...
    end
| Ok (Callback_error { error; description; state = _ }) -> ...
| Error err -> ...
```

`expected_connection` rejects callbacks routed to the wrong connection before consuming state.
Correct callbacks consume atomically; replay fails.

## Claims Validation

If the adapter already verified and parsed the ID token, call `validate_claims`:

```ocaml
let claims =
  {
    Fennec.Accounts.Oidc.issuer = "https://idp.example";
    subject = "00u123";
    audience = [ Sys.getenv "OIDC_CLIENT_ID" ];
    expires_at = 1_800_000_000.;
    not_before = None;
    issued_at = Some 1_799_999_900.;
    nonce = Some state.nonce;
    email = Some "ada@example.com";
    email_verified = Some true;
    hosted_domain = Some "example.com";
    tenant = None;
    groups = [];
  }

match Fennec.Accounts.Oidc.validate_claims main state claims with
| Ok principal -> ...
| Error err -> ...
```

For the common native path, parse the provider JWKS once, cache it in the adapter, and verify the
raw ID token with Accounts:

```ocaml
let keys = Fennec.Accounts.Oidc.jwks_of_string jwks_json |> Result.get_ok in

match Fennec.Accounts.Oidc.verify_id_token main state keys id_token with
| Ok principal -> ...
| Error err -> ...
```

Validation checks:

- RS256 signature over the JWT signing input when `verify_id_token` is used
- matching `kid` when the ID-token header provides one
- exact issuer
- consumed state issuer/connection/client/redirect URI
- audience contains the connection client id
- expiry with leeway
- not-before and issued-at future skew with leeway
- nonce equals consumed state nonce
- configured domain policy, when present
- OIDC subject can form a canonical `Accounts.Identity` key
- email claim can form a canonical email identity when present

`principal.identity` is the durable login identity. `principal.email_identity` is optional merge
evidence. It is verified only when the provider claim explicitly says `email_verified = true` and
the application trusts the connection.

## Enterprise SSO

Enterprise OIDC connections can carry `org_id`, `domains`, and `allow_jit`.

- `org_id` is bound into state and returned on the validated principal.
- `domains` can route or admit a login attempt, but issuer/subject remains the identity.
- `allow_jit` is configuration data for higher account creation/linking policy.
- SCIM may provision a user before first OIDC login; subject/SCIM matching belongs in the store or
  account-linking layer.

## Edge Cases

- JWKS key rotation: the adapter owns retrieval/cache refresh; `jwks_of_string` and
  `verify_id_token` own key parsing/signature verification.
- Unsupported algorithms or non-RSA keys: rejected by the native verifier.
- Multiple connections share issuer but different client ids: connection id participates in the
  identity namespace and client id is checked.
- IdP sends huge group/role claims: keep token exchange/profile adapters responsible for limiting
  persisted profile size.
- User removed from org but still has local session: revoke/bump Accounts auth epoch or enforce org
  membership at request time.
- OIDC subject changes during IdP migration: treat as an explicit identity merge/admin migration.
- Callback routed to wrong connection: rejected without consuming state.
- Callback replay: rejected as already consumed.
- Provider omits email: identity still works by issuer/connection/subject.
- Provider sends unverified email: email identity is unverified merge evidence only.

## Tests

- Connection ids, scopes, and domains normalize.
- Blank connection fields and reserved extra params are rejected.
- Authorization URL contains response type, client id, redirect URI, state, scope, nonce, PKCE
  challenge, and S256.
- Callback parsing delegates to the OAuth parser.
- State is purpose-bound, TTL-bound, and single-use.
- Wrong-connection precheck does not consume state.
- Verified claims derive OIDC and verified email identities.
- Issuer, audience, nonce, expiry, and domain mismatches fail closed.
- JWKS parsing accepts RSA keys and rejects malformed keys.
- Signed ID-token verification accepts valid RS256 tokens and rejects tampering.
