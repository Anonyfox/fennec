# Accounts.Challenge

`Fennec.Accounts.Challenge` provides the shared primitive for short-lived auth ceremonies: magic
links, OTP, email verification, password reset, OAuth/OIDC state and nonce, SAML request ids,
passkey challenges, MFA step-up, and recovery. It issues a bearer token once, stores only a hash of
the secret half, and consumes the token atomically.

The token wire shape is:

```text
<id>.<secret>
```

The id is public lookup material. The secret is high-entropy bearer material and should only be
rendered at the delivery boundary, such as an email link, redirect state, WebAuthn challenge, or
one-time-code transport.

## Rules

- Store only hashed challenge secrets.
- Every challenge has a typed purpose.
- Every challenge has expiry.
- Consuming a challenge is atomic and single-use.
- Purpose mismatch is invalid even if the secret matches.
- User, email, org, connection, redirect target, and assurance metadata must be bound into the
  challenge record.
- Email metadata is normalized to lowercase/trimmed form on creation and revocation.
- Wrong-secret attempts are counted before a challenge can be consumed, and optional `max_attempts`
  locks low-entropy flows.
- Rare random id collisions are retried internally before `Duplicate_id` is surfaced.

## Purpose Separation

A password-reset token must not verify email. An OAuth state token must not log in a user. A passkey
registration challenge must not satisfy a passkey assertion. Purpose is included in the token-secret
hash, and stores must reject any consume call whose requested purpose differs from the stored
purpose.

## Store Contract

`Challenge.store` supports:

- `insert`
- `find`
- `consume`
- `revoke`
- `revoke_user`
- `revoke_email`
- `gc_expired`

`consume` must check existence, purpose, hash, expiry, consumed-at, revoked-at, attempt counters, and
the consumed-at update in one atomic operation. Database-backed stores should implement that as a
single transaction or conditional update. Returning `Ok record` means the challenge is already marked
consumed.

`find` returns only the public record. It must never expose the raw token secret or stored secret
hash.

`revoke_user` and `revoke_email` are bulk policy tools for "latest link wins", password reset
cleanup, identity unlinking, org lockout, and account recovery cleanup. They only affect active
records and can be restricted by purpose.

`gc_expired` deletes expired records. Expired records are also rejected during `consume`, so garbage
collection is operational cleanup rather than a correctness dependency.

## Memory Store

`memory_store` is mutex-guarded and useful for inline tests, examples, and single-process prototypes.
Production adapters should persist records and secret hashes in the chosen application data layer.
The framework-level API is intentionally persistence-neutral.

## Edge Cases

- User requests two magic links: either both remain valid until use, or the caller revokes older
  active challenges by purpose/user/email before issuing the new one.
- Clock skew around expiry: inject `now` in tests and keep production clocks consistent.
- Replay after successful consume: rejected as `Already_consumed`.
- Token leaked through referrer/logs: keep token rendering at the delivery boundary and avoid logging
  full URLs containing challenge secrets.
- OTP brute force: use `max_attempts` and pair this primitive with caller-level rate limiting.
- OAuth callback after state expiry: rejected as `Expired`.
- Browser opens the same magic link twice: first consume wins; later attempts are rejected.
- SAML response for an unknown request id: rejected as `Invalid_token`.

## Tests

- Purpose mismatch rejected.
- Expired challenge rejected.
- Double consume rejected.
- Revoked challenge rejected.
- Wrong-secret attempts are counted and capped.
- User/email bulk revocation respects purpose filters.
- Expired garbage collection preserves live records.
- Hashed storage never stores raw token.
- Metadata is preserved and checked.
- Random id collision retry is covered without weakening the store uniqueness contract.
- Atomic consume is covered by the store contract; database stores should add adapter-level
  concurrent-consume tests.
