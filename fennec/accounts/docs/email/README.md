# Accounts.Email

`Fennec.Accounts.Email` owns email-address normalization plus the email-specific challenge flows for
verification, magic-link login, and one-time-code login.

The module is persistence- and transport-neutral. It does not send mail, create users, attach
identities, reveal account existence, or issue session tokens. It builds normalized email identities
and wraps `Fennec.Accounts.Challenge` so application code can decide how a consumed email proof maps
to registration, login, merge, or recovery.

## Scope

- normalize and validate email addresses
- build verified or unverified email identity keys
- bind email/user/org/connection/redirect metadata into challenges
- issue and consume email-verification challenges
- issue and consume magic-link login challenges
- issue and consume numeric email OTP challenges

Account mutations stay in the higher Accounts layer or user code:

- add/remove email address
- mark primary email
- attach/detach identity
- merge users
- issue the final account session token
- send the actual email

## Address Normalization

`normalize` trims and lowercases addresses, then requires exactly one `@`, non-empty local/domain
parts, and no ASCII whitespace.

```ocaml
match Fennec.Accounts.Email.normalize " Ada@Example.COM " with
| Ok email -> Fennec.Accounts.Email.address_to_string email
| Error err -> Fennec.Accounts.Email.string_of_error err
```

Fennec intentionally treats email identity as normalized lowercase account identity. Applications
that need display-preserving email text should store that separately from account identity.

## Identity Rules

`identity ~verified address` delegates to `Fennec.Accounts.Identity.email`.

- Verified email is safe account-link evidence.
- Unverified email is contact data or a pending claim, not proof.
- Verified and unverified claims for the same normalized address share the same stable identity key.
- Only verified email identities satisfy `Accounts.Identity.same_verified_email`.
- Provider profile emails should become verified email evidence only when the provider explicitly
  asserts verification and the connection policy trusts that provider.

This module does not enforce uniqueness. Stores should normally enforce one verified email identity
per account graph, while unverified duplicate contact data can remain an application policy choice.

## Challenge Binding

Email ceremonies use a shared binding:

```ocaml
let binding =
  Fennec.Accounts.Email.binding
    ~user_id:"user_123"
    ~org_id:"org_456"
    ~connection_id:"email"
    ~redirect:"/dashboard"
    email
```

The binding becomes challenge metadata. A consumed challenge returns the full challenge record, so
callers can use the bound user, email, org, connection, redirect, and custom data without trusting
request parameters from the callback.

## Verification

Verification uses `Challenge.Email_verification`.

```ocaml
let email = Fennec.Accounts.Email.normalize "ada@example.com" |> Result.get_ok
let binding = Fennec.Accounts.Email.binding ~user_id:"user_123" email
let issued = Fennec.Accounts.Email.issue_verification email_service binding
```

Deliver `Challenge.token_to_string issued.token` in the verification email. Consuming the token
proves mailbox control for the bound address; it does not log the user in by itself.

`consume_verification ?expected` rejects expected-address mismatch before challenge consumption, so
rendering code can safely bind a form or route to a specific address without burning the token on a
wrong-address request.

## Magic Link Rules

- Magic links use `Challenge.Email_login`.
- Only the challenge store sees the hashed secret half; the raw token is rendered at delivery time.
- The link binds intended email, optional user id, redirect target, and org/connection hints.
- The challenge token is not a session cookie and should not be treated as a long-lived session.
- Consuming the challenge returns proof metadata; the caller decides whether and how to issue an
  `Accounts.token`.

Purpose separation is strict. A magic-link token cannot verify email, and a verification token cannot
log in a user.

## OTP Rules

- OTP login also uses `Challenge.Email_login`.
- `issue_otp` returns both a challenge token and a numeric code.
- Email only the `code`.
- Keep the `token` in the browser/session/form flow and submit it back with the code.
- The code is stored only as an HMAC inside challenge metadata.
- Code comparison is constant-time.
- Wrong-code checks happen before challenge consumption, so a typo does not burn the challenge.

Because wrong codes do not consume the challenge, callers must rate-limit OTP attempts. Use
request-level rate limiting and, for production stores, challenge attempt caps or equivalent
per-email/per-IP counters. Responses should avoid revealing whether an account exists.

```ocaml
let otp = Fennec.Accounts.Email.issue_otp email_service binding |> Result.get_ok in
send_email ~to_:email ~body:otp.code;

match Fennec.Accounts.Email.consume_otp email_service ~token:otp.token ~code:submitted_code with
| Ok record -> (* issue login session from record.metadata *)
| Error err -> (* generic invalid-code response *)
```

## Route Helpers

The high-level Accounts module includes opt-in paws for the common browser edges:

- `email_login_link_request_paw`: POST endpoint that issues a login-link token and calls an app
  `send` callback.
- `email_login_link_paw`: GET endpoint that consumes the token, resolves the Accounts user, sets the
  signed login cookie, and redirects.
- `email_otp_request_paw`: POST endpoint that issues an OTP code and calls an app `send` callback.
- `email_otp_paw`: POST endpoint that consumes token+code, resolves the Accounts user, sets the
  signed login cookie, and redirects.

Apps still own the form UI, mail delivery, redirect targets, rate limiting, and any org policy that
decides whether email login is allowed for a domain.

## Edge Cases

- User changes email while a verification challenge is pending: compare the consumed metadata with
  current user state before marking verified.
- Magic link opened on a different device: the challenge is transport-neutral; the caller chooses
  whether cross-device login is allowed.
- Link scanner consumes email links: use an intermediate confirmation page before consuming login
  challenges when that risk matters.
- Multiple outstanding verification emails: either allow all until expiry or revoke older active
  challenges by user/email/purpose before issuing a new one.
- OAuth login claims an email that exists unverified locally: do not auto-link unless the provider
  email is explicitly verified and trusted.
- Enterprise domain routes email to SSO: reject or redirect email-login initiation according to the
  org/connection policy before issuing a challenge.
- OTP brute force: enforce per-email/per-IP throttles and short TTLs.

## Tests

- Normalization trims/lowercases valid addresses.
- Malformed addresses are rejected.
- Email identity delegates to `Accounts.Identity`.
- Verification tokens are purpose-bound and single-use.
- Magic-link tokens cannot verify email.
- Expected-email mismatch does not consume a token.
- Expired email challenges fail closed.
- OTP issue returns a numeric code.
- OTP consume requires token plus matching code.
- Wrong OTP code does not consume the challenge.
- Invalid OTP digit configuration and code shape are rejected.
- Accounts route helpers issue/consume magic links and OTPs without exposing account existence.
