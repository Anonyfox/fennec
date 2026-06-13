# Accounts.Passkey

`Fennec.Accounts.Passkey` implements the native WebAuthn/passkey ceremony primitive for
phishing-resistant login and MFA. It owns challenge state, RP/origin policy, client-data checks,
authenticator-data parsing, `none` attestation verification, COSE ES256 public-key extraction,
assertion signature verification, sign-counter policy, and canonical passkey identity evidence.

The module deliberately does not persist credentials, choose users, merge identities, or issue
Accounts session tokens. It returns typed credential/assertion facts for the Accounts store and
linking layer.

## Supported Profile

- Registration and assertion ceremonies.
- Exact RP ID hash validation.
- Exact origin allowlist validation.
- User-present flag required.
- User-verified flag required when RP policy enables `user_verification`.
- Registration with `fmt = "none"` attestation.
- COSE ES256/P-256 public keys.
- Assertion ECDSA/SHA-256 signature verification.
- Single-use, purpose-bound challenges through `Challenge.Passkey_registration` and
  `Challenge.Passkey_assertion`.
- Discoverable login support by allowing assertion challenges with no allowlist.
- Sign-counter rollback rejection when both stored and new counters are non-zero.
- Backup eligibility/state flags are preserved.

Attestation trust chains, enterprise attestation policy, non-ES256 algorithms, user persistence,
credential uniqueness, last-factor removal policy, and session issuance stay outside this module.

## Relying Party

```ocaml
let rp =
  Fennec.Accounts.Passkey.relying_party
    ~id:"example.com"
    ~name:"Example"
    ~origins:[ "https://example.com" ]
    ~user_verification:true
    ()
```

`id` is the WebAuthn RP ID. `origins` are exact browser origins accepted from `clientDataJSON`.
`user_verification` controls whether the UV authenticator flag is mandatory.

## Registration

Start a ceremony:

```ocaml
let user =
  Fennec.Accounts.Passkey.user
    ~id:"user_123"
    ~handle:"stable-user-handle"
    ~name:"ada@example.com"
    ()

match Fennec.Accounts.Passkey.begin_registration passkeys rp user with
| Ok reg -> ...
| Error err -> ...
```

`reg.challenge` is the browser challenge string. `reg.token` is the opaque single-use server token
to keep with the ceremony boundary.

Finish after the browser returns credential data:

```ocaml
match
  Fennec.Accounts.Passkey.finish_registration passkeys
    rp
    response
    ~token:reg.token
    ~user_id:"user_123"
with
| Ok credential -> ...
| Error err -> ...
```

The response fields are decoded byte strings, except `id`, which is the browser's base64url
credential id. The module checks `id` against `raw_id`, validates client data, parses CBOR
attestation, extracts the public key, checks flags and RP ID hash, then consumes the challenge.
Invalid responses do not burn the challenge.

## Assertion

Start a ceremony:

```ocaml
match
  Fennec.Accounts.Passkey.begin_assertion passkeys
    ~user_id:"user_123"
    ~allowed_credentials:[ credential.id ]
    rp
with
| Ok assertion -> ...
| Error err -> ...
```

Pass an empty `allowed_credentials` list for discoverable credential login.

Finish after the browser signs:

```ocaml
match
  Fennec.Accounts.Passkey.finish_assertion passkeys
    rp
    credential
    response
    ~token:assertion.token
with
| Ok verified -> ...
| Error err -> ...
```

The module checks client data, RP ID hash, flags, allowed credential id, optional user handle,
signature over `authenticatorData || SHA256(clientDataJSON)`, and sign-counter rollback before
consuming the challenge. The returned credential carries the updated sign count, backup flags, and
last-used timestamp.

## Credential Facts

`credential` contains:

- credential id
- user id
- user handle
- public key
- sign count
- backup eligibility/state
- transports
- created/last-used timestamps

`identity credential` returns the canonical `Accounts.Identity.Passkey` key.

## Route Helpers

The high-level Accounts module includes JSON paws for browser ceremonies:

- `passkey_registration_options_paw`: requires the current Accounts session and returns browser
  registration options plus the single-use server token.
- `passkey_registration_finish_paw`: consumes browser credential JSON, verifies the ceremony,
  stores the credential, and attaches the passkey identity.
- `passkey_assertion_options_paw`: returns login/assertion options; when a session is present it
  scopes the allow-list to that user's credentials, otherwise it allows discoverable credentials.
- `passkey_assertion_finish_paw`: verifies the assertion, persists the updated counter, resolves the
  Accounts user, sets the signed login cookie, and returns the normal login payload.

Apps still own the browser-side WebAuthn JavaScript, UI copy, and step-up policy, but not the
challenge/counter/session plumbing.

## Edge Cases

- Wrong origin: rejected without consuming the challenge.
- Wrong RP ID hash: rejected.
- Challenge replay: final consume fails closed.
- Credential id mismatch: rejected.
- Credential not in the allowlist: rejected.
- User handle mismatch: rejected.
- Signature mismatch: rejected.
- Counter rollback: rejected when both counters are non-zero.
- Always-zero authenticators: accepted without rollback protection, matching common authenticator
  behavior.
- User verification required by policy: UV flag is enforced.

## Tests

Inline tests cover RP/user validation, registration challenge metadata, `none` attestation with
ES256 credential extraction, wrong-origin retry without challenge burn, assertion signature
verification, counter update, counter rollback rejection, and high-level malformed JSON handling.
