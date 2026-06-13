# Accounts.Audit

`Fennec.Accounts.Audit` defines the shared append-only security event vocabulary
for account and identity operations. It is deliberately storage-neutral:
authentication modules build events, backing stores append them, and security UI or
export code can query them later.

## Responsibilities

- Provide stable event kinds for login, logout, tokens, password, email, passkey,
  OAuth/OIDC/SAML, identity links, MFA, SCIM, org policy, and challenge operations.
- Represent actors as anonymous, user, or system actors.
- Represent mechanisms and provider/connection names without storing credentials.
- Carry safe request facts: request id, IP, and user agent.
- Carry success/failure outcomes with stable machine-readable failure codes.
- Sanitize metadata so secret-bearing keys are redacted before storage.
- Provide a small append-only memory store for tests and simple deployments.

## Event Shape

An event contains:

- `id`
- timestamp
- kind
- actor
- optional target user id
- optional org id
- optional mechanism
- optional connection id
- optional request context
- outcome
- sanitized metadata

The `to_fields` projection is deterministic and excludes empty optionals. It is
intended for logs, tests, simple stores, and export adapters. It is not a JSON
serializer.

## Secret Safety

Audit events must never contain passwords, raw tokens, provider access tokens, bearer
secrets, raw cookies, private keys, challenge secrets, SAML responses, OTPs, or
credential material. `sanitize_metadata` redacts values when metadata key names look
secret-bearing, and event construction always sanitizes metadata before storing it on
the event record.

## Store Contract

The memory store models the intended persistence contract:

- append-only writes
- duplicate event ids rejected
- stable append-order reads
- simple filters by target user, org, and kind

Production stores should preserve those semantics and push target-user, org, and kind filters into
the backing query path so indexed audit lookups do not scan the full event collection. Retention,
pagination, export, and tamper-evidence remain deployment concerns.

## Edge Cases Covered

- Failed login before a user is known can be recorded with `Anonymous`.
- SCIM and scheduled work can be recorded with `System`.
- Merge/link/unlink operations have distinct event kinds.
- Metadata with token/password/secret-like keys is redacted.
- Duplicate append retries are rejected by event id.

## Out Of Scope

This module does not decide which auth paths must emit which events, persist to a
database, sign audit logs, implement export formats, or revoke sessions. Those layers
consume this event vocabulary and store contract.
