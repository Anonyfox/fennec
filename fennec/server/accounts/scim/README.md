# Accounts.Scim

`Fennec.Accounts.Scim` provides the provisioning core for enterprise directory
sync. SCIM is not login: it creates, updates, deactivates, and groups enterprise
users before or after they authenticate through SAML, OIDC, OAuth, passkeys, or
other account mechanisms.

## Responsibilities

- Model a tenant SCIM connection with hashed bearer-token authentication.
- Normalize SCIM users and groups into storage-friendly records.
- Derive the canonical `Accounts.Identity.scim` key for external ids.
- Convert active/inactive SCIM users into `Accounts.Org` memberships.
- Apply normalized user and group PATCH operations.
- Produce idempotent user and group sync plans.
- Compute group membership deltas by SCIM user external id.

## Connection Auth

`connection` hashes the bearer token immediately and stores only the SHA-256 hash.
`authenticate` compares the presented token hash in constant time. HTTP middleware or SCIM endpoint
code should authenticate first, then call the pure provisioning functions in this module.

## Identity Rules

`external_id` is required for users and groups. User external ids are strong identity
evidence only inside the tenant organization, so `identity` scopes them through
`Accounts.Identity.scim ~org_id`. Email addresses are normalized and useful profile
data, but SCIM must not blindly merge global users by email.

## User Sync

`plan_user` compares the stored resource with the incoming SCIM user and returns one
of:

- `Create_user`
- `Update_user`
- `Deprovision_user`
- `No_user_change`

This makes duplicate request retries safe: an identical replay produces
`No_user_change`. Changing `external_id` for an existing user is rejected because it
would change the identity key. When `allow_deprovision` is false, an incoming
`active=false` update preserves the current active state while still allowing other
profile fields to update.

## PATCH Semantics

`apply_user_patch` supports scalar fields (`userName`, `active`, `displayName`, `externalId`) and
set-like list fields (`emails`, `groups`). Removing `externalId` or `userName` is rejected.
Removing `active` maps to `active=false`, matching SCIM's deprovisioning shape.

`apply_group_patch` supports `displayName`, `externalId`, and set-like `members`. Removing
`externalId` or `displayName` is rejected. The higher Accounts `scim_paw` helper parses SCIM PATCH
JSON into these typed operations for `/Users/:externalId` and `/Groups/:externalId`.

## Groups

Groups are tracked by external id and member external ids. `plan_group` is idempotent
for create/update/delete. `membership_delta` returns deterministic add/remove lists so
stores can update memberships atomically and audit the exact directory change.

## Edge Cases Covered

- Bearer tokens are never stored in plaintext.
- SCIM users can exist before first SSO login.
- Duplicate create/update retries become no-ops when state is unchanged.
- User and group external ids cannot be changed in-place.
- Deprovisioning can be disabled per connection.
- Email changes update the same SCIM identity instead of creating a new one.
- Group membership changes are computed by external id, not global user id.

## HTTP Surface

The pure module stays transport-neutral. The higher `Fennec.Accounts.scim_paw ~prefix` battery
mounts the JSON bearer-auth endpoint over the Accounts store and serves `/Users`, `/Users/:id`,
`/Groups`, and `/Groups/:id` with GET/POST/PUT/PATCH/DELETE. User writes provision Accounts users,
SCIM identities, and org memberships through the shared store.

## Out Of Scope

This module does not implement HTTP routing, JSON parsing, persistence, audit storage,
session revocation, identity merging, or SCIM schema discovery endpoints. Those layers
should consume the normalized records and plans from this module.
