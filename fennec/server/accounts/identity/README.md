# Accounts.Identity

`Fennec.Accounts.Identity` is the canonical identity-key layer shared by all account mechanisms. It
prevents password, email, OAuth, OIDC, SAML, passkey, SCIM, MFA, and recovery modules from inventing
different normalization and merge rules.

The module is intentionally persistence-neutral. It does not attach or merge users by itself. Stores
use its opaque `key` values, `stable_key`, `scope`, and verification helpers to implement atomic
lookup, attach, detach, and merge operations.

## Core Model

An identity key has:

- `kind`: password, email, OAuth, OIDC, SAML, passkey, SCIM, recovery.
- `scope`: `Global` or `Per_user`.
- optional `namespace`: provider, issuer/connection, org id, or SSO connection.
- normalized `subject`: email address, provider subject, credential id, external id, etc.
- optional `verification`: currently used by email identities as evidence, not uniqueness.

`Global` keys are safe for database uniqueness constraints and lookup indexes. `Per_user` keys only
describe local credential presence inside one user record and must not be globally unique.

## Constructors

- `password ()`: local password credential presence, `Per_user`.
- `email ~verified address`: normalized lowercase email, `Global`.
- `oauth ~provider ~subject`: lowercased provider plus exact trimmed subject, `Global`.
- `oidc ~issuer ~connection ~subject`: exact trimmed issuer, lowercased connection, exact trimmed
  subject, `Global`.
- `saml ~connection ~name_id ?external_id ()`: lowercased connection plus `external_id` when present,
  otherwise NameID, `Global`.
- `passkey ~credential_id ?user_handle ()`: credential id, `Global`; user handle is validated but
  does not participate in uniqueness.
- `scim ~org_id ~external_id`: lowercased org id plus exact external id, `Global`.
- `recovery ~name`: conservative ASCII recovery mechanism name, `Per_user`.

Provider subjects, OIDC subjects, SAML NameIDs, passkey credential ids, and SCIM external ids are not
lowercased because providers may define them as case-sensitive opaque identifiers.

## Stable Keys

`stable_key` is the storage/index representation. It is ASCII and length-framed, so raw provider
values cannot collide through separators:

```text
fennec-identity-v1 + scope + kind + namespace + subject
```

Callers should not parse `stable_key`; it is for exact equality, ordering, and database uniqueness.
Use `describe` for logs/debug output. Verification state is intentionally not part of `stable_key`:
verified and unverified claims for the same normalized email address are the same identity, but only
verified claims are safe auto-link evidence.

## Auto-Linking

The only built-in cross-identity auto-link signal is:

```ocaml
same_verified_email a b
```

It returns `true` only when both keys are verified email identities for the same normalized address.
Unverified email never auto-links. Provider profile emails must be converted to `email ~verified:true`
only when the provider explicitly asserted verification.

Exact provider-subject equality is still a normal lookup path: an OAuth/OIDC/SAML/passkey/SCIM key
with the same `stable_key` belongs to the same existing identity. That is not the same as
auto-linking a new provider account by a profile email.

## Link Plans

`link ~user_id key ~created_at` builds a storage link. Stores can use `plan_link` before attaching:

- `Attach`: no existing global link blocks the attach.
- `Already_linked`: the key is already attached to the same user.
- `Conflict`: a global key is already attached to another user.

`Per_user` keys do not produce cross-user conflicts because they are local credential facts, not
global lookup identities.

`usable_for_login` marks credentials that can satisfy login or recovery. Verified email, provider
subjects, passkeys, SCIM identities, password presence, and recovery credentials are usable.
Unverified email is not.

`plan_detach` rejects removing the last usable credential by default. Applications can pass
`allow_last:true` only for explicit admin/recovery flows that have another safety guarantee.

## Merge Plans

`plan_merge ~from_user_id ~into_user_id ~source ~target` computes the identity-link rewrite needed
to merge one user into another:

- `move`: source links missing from the target and safe to rewrite to the target user.
- `keep`: links already present on the target.
- `conflicts`: source links whose stable key is owned by a third/non-target user.

The plan is deterministic by `stable_key`, but it is still only a plan. Stores must apply the merge
atomically with their own uniqueness constraints.

## Identity Store

`Accounts.Identity.store` is the companion-store contract for identity links. It deliberately stays
separate from the core `Accounts.store` user record so account storage does not become one giant
shape.

The contract exposes:

- `find`: global-key lookup by `stable_key`; per-user keys are not globally findable.
- `list`: deterministic link listing, optionally scoped to one user id.
- `attach`: atomic attach with global uniqueness checks.
- `detach`: atomic detach with last-usable-credential protection.
- `merge`: atomic source-user-to-target-user link rewrite when no conflicts exist.

`memory_store` is mutex-guarded and intended for tests, examples, and single-process prototypes.
Persistent adapters should preserve the same semantics with database uniqueness constraints and
transactions or conditional writes.

## Store Contract Direction

Future persistent account stores should use `Accounts.Identity` this way:

- Store `stable_key` for every `Global` identity link.
- Enforce uniqueness on `stable_key`.
- Keep `Per_user` identities inside the user/security settings record, not in a global unique index.
- Attach/detach/merge identity links in one transaction or equivalent conditional write. The
  `store` contract models this directly.
- Reject detaching the last usable login credential unless an explicit recovery/admin policy allows
  it.
- When merging users, rewrite all global identity links atomically to the target user id.

## Edge Cases Covered

- Normalization is stable and idempotent.
- Unverified email shares the same stable identity as the address but does not auto-link.
- Same verified email auto-link evidence ignores case.
- Provider/connection names normalize while opaque subjects do not.
- OIDC issuer participates in uniqueness.
- SAML prefers stable external id over NameID when present.
- Passkey uniqueness is credential-id based, not user-handle based.
- SCIM external ids are scoped by org id.
- Password/recovery presence is per-user, not globally unique.
- Stable keys are length-framed and separator-collision safe.
- Link planning is idempotent for already attached identities.
- Global identity collisions are explicit conflicts.
- Per-user credential facts can exist on many users.
- Detach planning protects the last usable credential by default.
- Merge planning moves missing links, keeps target duplicates, and reports third-user conflicts.
- The memory store enforces global uniqueness and keeps per-user credential facts independent.
- The memory store mutates only when attach/detach/merge plans allow the operation.
