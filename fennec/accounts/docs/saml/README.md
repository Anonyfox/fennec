# Accounts.Saml

`Fennec.Accounts.Saml` implements the native SAML 2.0 enterprise SSO path for statically linked
Fennec deployments. It covers SP-initiated request state, AuthnRequest rendering, HTTP-Redirect URL
generation, optional signed AuthnRequest redirects, RelayState replay protection, HTTP-POST
`SAMLResponse` verification, assertion validation, and canonical SAML/email identity evidence.

The module is deliberately a constrained SAML profile, not a generic XML security toolkit. It accepts
the common Web Browser SSO shape used by modern IdPs and rejects unsupported XML/signature forms
closed. This keeps the Accounts surface auditable, portable across opam build matrices, and free of
runtime `xmlsec`/dynamic-library deployment requirements.

## Supported Profile

- SP-initiated login with HTTP-Redirect AuthnRequest and HTTP-POST response.
- One `Response` with status `urn:oasis:names:tc:SAML:2.0:status:Success`.
- Exactly one bearer `Assertion`.
- Exactly one enveloped XML Signature, either on the `Response` or the `Assertion`.
- RSA-SHA256 signature method.
- SHA-256 digest method.
- Exclusive XML canonicalization without comments.
- Same-document `#ID` reference with strict ID uniqueness.
- Enveloped-signature transform followed by exclusive-c14n transform.
- Pinned/trusted IdP public keys supplied by the caller.
- PEM certificate bundles or public keys can be loaded directly with `trusted_keys_of_pem`.
- `SAMLResponse` payloads are size-limited by default before XML parsing.
- No encrypted assertions, DTDs, external entities, CDATA, comments-as-data, or unsigned safe mode.

IdP metadata import, certificate-chain policy, user creation, identity merging, and Accounts session
issuance stay outside this module. The caller owns those application and storage decisions.

## Connection Configuration

`connection` validates and normalizes one SAML service-provider connection:

```ocaml
let corp =
  Fennec.Accounts.Saml.connection
    ~id:"corp"
    ~issuer:"https://idp.example/saml"
    ~sso_url:"https://idp.example/sso"
    ~entity_id:"https://app.example.com/saml/metadata"
    ~acs_url:"https://app.example.com/saml/acs"
    ~org_id:"org_123"
    ~domains:[ "example.com" ]
    ~external_id_attribute:"employee_id"
    ~trust_email:true
    ~allow_jit:true
    ()
```

Connection ids are lowercased and become the SAML identity namespace. Domains are admission hints,
not identity proof. `trust_email` controls whether assertion emails become verified email evidence;
leave it false unless the IdP connection policy is explicitly trusted.
`allow_jit` is returned on the verified principal so account-linking code can enforce provisioning
policy without threading separate connection state through the callback.

Trusted IdP keys can be loaded from PEM once at startup:

```ocaml
let idp_keys =
  match Fennec.Accounts.Saml.trusted_keys_of_pem pem with
  | Ok keys -> keys
  | Error err -> ...
```

## Login Start

`issue_request` creates a public AuthnRequest ID and a private RelayState challenge token:

```ocaml
let saml = Fennec.Accounts.Saml.make ~challenge in

match Fennec.Accounts.Saml.issue_request saml ~redirect:"/dashboard" corp with
| Ok request ->
    let location = Fennec.Accounts.Saml.redirect_url request in
    (* Redirect the browser to [location]. *)
| Error err -> ...
```

The request state is TTL-bound, purpose-bound, and single-use through `Challenge.Saml_request`.
`authn_request_xml` renders the XML request. `redirect_url` raw-deflates and base64-encodes it as
`SAMLRequest`, then includes RelayState as the opaque challenge token.

For IdPs that require signed SP requests, use `signed_redirect_url` with the SP private key:

```ocaml
match Fennec.Accounts.Saml.signed_redirect_url request ~signing_key with
| Ok location -> ...
| Error err -> ...
```

This signs the HTTP-Redirect binding query with RSA-SHA256 over `SAMLRequest`, `RelayState`, and
`SigAlg`.

## ACS Callback

Use `consume_response` for the normal ACS path:

```ocaml
match
  Fennec.Accounts.Saml.consume_response saml
    ~expected_connection:"corp"
    corp
    ~trusted_keys:idp_keys
    ~relay_state
    ~saml_response
with
| Ok principal -> ...
| Error err -> ...
```

`consume_response` first loads RelayState metadata without consuming it, verifies XML and signature,
extracts assertion facts, validates issuer/audience/recipient/destination/request/time/domain policy,
then consumes RelayState. Invalid XML, bad signatures, wrong connection routes, and assertion
mismatches leave RelayState usable for a corrected retry. A valid response succeeds only if final
single-use consume wins.

`SAMLResponse` is rejected before XML parsing when it exceeds the built-in response size limit. The
limit can be overridden on `verify_response` or `consume_response` for unusual IdPs, but normal apps
should keep the default.

`consume_state`, `verify_response`, and `validate_assertion` remain exposed for advanced integrations
that need to split the ACS pipeline, but the one-call helper is the safest default.

## Principal

Successful validation returns:

- `identity`: durable SAML identity, scoped by connection id and backed by configured external id or
  NameID.
- `email_identity`: optional email merge/contact evidence.
- `email`: normalized email when present.
- `allow_jit`: connection provisioning policy for the account-linking layer.
- `org_id`: org carried by the request state.
- `session_index`: IdP session index when present.
- `signature_key_fingerprint`: SHA-256 fingerprint of the trusted key that verified the response.
- `attributes`: normalized assertion attributes.
- `assertion`: the extracted assertion facts for audit or hooks.

Email is never the primary SAML identity. Prefer a stable external id attribute for enterprise
directories; otherwise NameID becomes the stable subject.

## Edge Cases

- Wrong connection route: rejected before consuming RelayState.
- Expired/replayed RelayState: final consume fails closed.
- Tampered response: digest or signature verification fails.
- Oversized response: rejected before XML parsing.
- Multiple signatures: rejected.
- Duplicate XML IDs: rejected.
- Unsigned response/assertion: rejected.
- Non-success SAML status: rejected.
- Encrypted assertion: rejected until native decrypt support exists.
- IdP certificate rollover: pass both old and new public keys during the rollover window.
- Transient NameID: configure a stable external id attribute.
- Email changes in IdP: SAML identity remains external id or NameID, not email.
- IdP-initiated login: intentionally not part of this primitive because it needs different replay and
  routing controls.
- SCIM-created user: link by configured external id in the store/linking layer.

## Tests

Inline tests cover connection normalization, request state, single-use RelayState, wrong-route
precheck, expiry, PEM key loading, assertion validation, AuthnRequest/redirect rendering, signed
AuthnRequest redirects, signed response verification, key fingerprint audit, response size limits,
tampering, wrong keys, unsigned responses, duplicate IDs, multiple signatures, non-success status,
encrypted assertions, and invalid-response retry before final consume.
