# Accounts.Mfa

`Fennec.Accounts.Mfa` provides the shared assurance and step-up primitives used by
account adapters. It is intentionally storage-neutral: applications persist enrolled
factors, accepted TOTP counters, and remaining backup-code hashes in their account
store, while this module handles validation, challenge binding, and replay checks.

## Responsibilities

- Derive session assurance from verified factor kinds.
- Check route or method requirements, including freshness windows.
- Issue and consume single-use step-up challenges through `Accounts.Challenge`.
- Generate and verify TOTP secrets using RFC 6238 SHA-1 semantics.
- Generate, hash, normalize, verify, and consume backup or recovery codes.

## Assurance

Assurance levels are ordered from anonymous to phishing-resistant multi-factor:

- `Anonymous`
- `Single_factor`
- `Phishing_resistant_single_factor`
- `Multi_factor`
- `Phishing_resistant_multi_factor`

`level_of_factors` derives the level from the distinct verified factors. Passkeys are
treated as phishing-resistant. A `requirement` combines the minimum level with an
optional `max_age`, so sensitive routes and methods can ask for fresh step-up without
forcing logout or a full login restart.

## Step-Up Flow

1. Build a `requirement` for the protected action.
2. Call `issue_step_up` for the current user.
3. Verify the selected second factor in the UI or method flow.
4. Call `consume_step_up` with `expected_user` before granting the action.

The challenge is purpose-bound, single-use, TTL-bound, and carries the user id plus
requirement metadata. A wrong expected user does not consume the challenge, which lets
callers reject confused-session attempts without destroying a valid pending step-up.

## TOTP

`generate_totp_secret` returns a base32 secret. `totp` validates base32 input, digit
count, and period. `provisioning_uri` returns an `otpauth://` URI for authenticator
apps. `verify_totp` accepts a bounded adjacent-step window and returns the accepted
counter; callers persist that counter and pass it back as `last_step` to reject replay.

The high-level `Fennec.Accounts.enroll_totp` helper seals the TOTP secret before persistence and
unseals it only for confirmation/verification. This low-level module treats the secret as opaque so
the same verifier can be used by custom storage or migration code.

## Backup Codes

Backup codes are generated as short base32 strings and stored only as HMAC-SHA256
hashes under the MFA helper secret. Verification normalizes whitespace, hyphens, and
case. `consume_backup_code` returns the matched hash and the remaining hash list to
persist atomically.

The Accounts store facet persists replay-sensitive changes with compare-and-swap semantics. TOTP
confirmation, TOTP verification, backup-code consumption, and factor disabling replace the enrollment
only when the stored record still matches the state that was verified, so concurrent retries fail
closed instead of double-accepting a code.

## Edge Cases Covered

- Stale or too-weak assurance rejects protected actions.
- Step-up challenges are purpose-bound and single-use.
- Wrong-user step-up attempts fail without consuming valid state.
- TOTP follows RFC 6238 test vectors.
- TOTP replay is rejected once the accepted counter is persisted.
- Stale TOTP/backup-code enrollment state fails closed through store compare-and-swap.
- High-level Accounts TOTP enrollment persists a sealed secret and fails closed on tampering.
- Backup codes are single-use and tolerant of copy/paste formatting.

## Out Of Scope

This module does not enroll or unenroll factors, decide organization policy, remember
devices, audit factor changes, or mutate user records. Those belong in the account
store, organization policy, and identity-linking layer.
