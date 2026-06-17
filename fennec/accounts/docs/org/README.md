# Accounts.Org

`Fennec.Accounts.Org` provides the tenant-side primitives for B2B identity:
organizations, memberships, verified domains, SSO routing, login policy, and small
RBAC hooks. It is deliberately storage-neutral. Applications persist records and wire
them into account creation, SCIM, audit, and session issuance.

## Responsibilities

- Normalize organization ids, role names, and email/domain routing suffixes.
- Model organization and membership lifecycle states.
- Model tenant SSO and MFA policy.
- Route verified domains to exactly one active organization.
- Resolve the SSO connection ids allowed for a routed tenant/domain.
- Decide whether a login strategy is allowed by tenant policy.
- Provide a small role-to-permission hook for route and method guards.

## Domain Routing

`route_domain` accepts either an email address or domain. It lowercases and validates
the suffix, ignores suspended/deleted organizations, and ignores unverified domains.
If multiple active organizations verify the same domain, it returns
`Domain_ambiguous`; callers must ask the user to choose instead of silently picking an
organization.

Domain-level `connection_ids` take precedence over the org-level SSO policy. When a
verified domain does not name its own connections and the org requires SSO,
`connection_ids_for_route` returns the policy connections.

## Tenant Policy

`Sso_optional` permits local and federated login strategies. `Sso_required` blocks
local strategies unless `allow_password_fallback` is explicitly enabled for migration
or break-glass scenarios. Federated strategies must use a connection id owned by the
organization.

`mfa_policy` is carried on the org policy but not enforced here. The session layer can
combine it with `Accounts.Mfa` assurance checks to protect routes and methods.

## Memberships

Memberships attach a global user id to an organization id with a normalized role and
lifecycle state. `require_membership` verifies that both the organization and
membership are active. SCIM deactivation should normally disable or remove a
membership, not delete the global user.

## RBAC Hook

`allows` checks an active membership against a role predicate. The built-in predicate
is intentionally small: owner/admin allow everything, member allows read permissions,
and every other role denies by default. Applications can pass a custom predicate
without changing account storage or routing behavior.

## Edge Cases Covered

- Unverified and inactive-domain routes are ignored.
- Duplicate verified domains across active orgs are reported as ambiguous.
- Required SSO blocks password/email login unless fallback is explicit.
- Required SSO allows only owned OAuth/OIDC/SAML connection ids.
- Disabled memberships do not grant tenant access or RBAC permissions.
- Multi-org users can be represented by independent active memberships.

## Out Of Scope

This module does not persist organizations, mutate users, merge identities, import IdP
metadata, sync SCIM resources, audit membership changes, or issue sessions. Those
layers consume the normalized records and decisions from this module.
