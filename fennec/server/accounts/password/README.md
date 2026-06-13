# Accounts.Password

`Fennec.Accounts.Password` owns password-specific primitives for the broader Accounts layer:
hashing, verification, and deterministic password policy validation.

High-level account/session flows remain on `Fennec.Accounts`:

- `create_user ?password`
- `login_with_password`
- `set_password`

Those flows delegate to this module for the built-in hasher. Keeping the primitive here gives users
one obvious place for password policy, while avoiding a module cycle between `Accounts.Password` and
the full `Accounts` service.

## Hashing

`password_hasher ?iterations ()` returns a hasher with:

```ocaml
type hasher = {
  hash : password:string -> string;
  verify : password:string -> hash:string -> bool;
}
```

The built-in hasher is PBKDF2-HMAC-SHA256 with random 16-byte salts and constant-time derived-key
comparison. Hashes are encoded as:

```text
pbkdf2-sha256$iterations$salt$derived
```

The default iteration count is `210_000`. Applications that want Argon2id/scrypt/bcrypt can pass a
custom value with the same `hasher` shape to `Fennec.Accounts.make`.

The top-level `Fennec.Accounts.password_hasher` remains as a compatibility alias for
`Fennec.Accounts.Password.password_hasher`.

## Policy

`policy ()` builds a deterministic password policy. The default policy is intentionally humane:

- minimum 8 characters
- maximum 1024 characters
- reject a small built-in list of common bad passwords
- reject the submitted email or username when those values are provided
- no mandatory lowercase/uppercase/digit/symbol classes

`strict_policy` is available for teams that explicitly want character-class requirements:

- minimum 12 characters
- lowercase required
- uppercase required
- digit required
- symbol required

Validation is pure:

```ocaml
match Fennec.Accounts.Password.validate ~email ~username password with
| Ok () -> ...
| Error errors ->
    let message = Fennec.Accounts.Password.describe_errors errors in
    ...
```

The policy layer does not hash, persist, rate-limit, or reveal account existence. It is safe to run
before calling `create_user`, `set_password`, or a reset-token consume flow.

## Reset And Rotation

Password rotation is handled by `Fennec.Accounts.set_password`, which hashes the new password and
calls the store's atomic `set_password_hash_and_bump` operation. Existing sessions are invalidated
according to the Accounts epoch-validation policy.

Password reset should be built from:

- `Fennec.Accounts.Challenge` with purpose `Password_reset`
- this module's `validate`
- `Fennec.Accounts.set_password`

Reset request endpoints must not reveal whether an email exists. Challenge consumption is
single-use, purpose-bound, and TTL-bound in `Accounts.Challenge`.

## Security Notes

- There is no silent insecure hash fallback. Password flows without a configured hasher fail before
  persistence.
- Initial password hashes are stored atomically through `store.create_user ~password_hash`.
- Password changes use `store.set_password_hash_and_bump`, not a split hash write plus revocation
  write.
- The built-in banned-password list is intentionally small. Real breached-password screening should
  be an application hook or adapter.
- Password policy checks are not rate limits. Pair login/reset endpoints with request-level rate
  limiting.

## Edge Cases Covered

- Invalid PBKDF2 hashes fail closed.
- Invalid iteration and policy bounds fail fast.
- Default policy rejects short/common passwords.
- Default policy rejects passwords containing submitted email/username.
- Strict policy reports missing character classes.
- Custom policies can disable email/username rejection.
- Error rendering is stable and concise.
