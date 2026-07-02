# Trails — the API service archetype

A link shortener + click-stats service with **no web UI at all**: versioned JSON endpoints, API-key
auth, signed outbound webhooks, and an http test suite that doubles as the living spec. This is the
"straight backend service" shape — machine-facing fennec, where the consumer is `curl`, a script, or
another program.

## The category

API-first services are their own family, distinct from websites: internal microservices, public
data APIs, integration glue, webhook relays, cron/background services (template galleries carry
whole Backend and Cron categories — [Vercel](https://vercel.com/templates); the "dynamic web
application" taxonomies all include pure-API backends —
[Octahedroid](https://octahedroid.com/blog/web-applications-types-and-examples-know-2025)).
The defining traits:

- **The contract is the product.** Stable versioned paths (`/v1/…`), machine-readable errors with
  correct status codes, and idempotent semantics where retries happen. Humans read the docs;
  machines read the responses.
- **AuthN is a header, not a session.** API keys / bearer tokens, per-key rate limits, and a
  key-management story (issue, list, revoke).
- **Events flow out too**: webhooks with HMAC signatures, retries, and an idempotency key — the
  half of API design most toys skip and most real integrations live on.

## The app

**Trails** — foxes follow scents; users follow trails. The service:

- `POST /v1/trails` `{url, slug?}` → a short trail (`https://host/t/:slug`); custom or generated slug.
- `GET /t/:slug` → 301 to the target, recording the click (the ONE non-`/v1` route — the redirect
  itself, which is the product).
- `GET /v1/trails` / `GET /v1/trails/:slug` → listing + per-trail stats (clicks, last-clicked,
  referrer tally).
- `DELETE /v1/trails/:slug` → gone (410 afterwards, not 404 — deliberate semantics to document).
- Webhooks: register a URL per key; a click past a threshold (or every N clicks) POSTs a signed
  event. `POST /v1/webhooks/test` fires a sample delivery.
- `GET /v1` → self-description: every endpoint, one line each, as JSON (the API documents itself).
- `GET /healthz` via the stock paw.

## How it works (the fennec shape)

Axes: **no rendering** (JSON only) · CRUD + counters · **API keys, no user accounts** · webhooks
out · async = webhook delivery via the outbox.

- One endpoint, `Server.json_on_error` as the error funnel (framework 404/405/500 are JSON here),
  `Body_limit`, and `Rate_limit` keyed by API key.
- A small **api-key paw**: reads `Authorization: Bearer`, constant-time compares against hashed
  keys in the `keys` collection, assigns the key onto the conn; `/t/:slug` and `/healthz` are the
  only unauthenticated routes. Keys are minted by a tiny CLI-ish route (`POST /v1/keys` guarded by
  a root token from the environment) — key management without a UI.
- Trails + clicks are two collections with Sift codecs; the click write is fire-and-forget fast
  (the redirect must not wait on stats — measure it).
- Outbound webhooks ride the **effects outbox**: the click handler enqueues, the resident worker
  delivers with the intent id as the idempotency key and HMAC-signs the body (the same
  `Webhook.verify` counterpart consumers would use — point at it in the docs route).

Target layout (note: no `web/` at all):

```
examples/api/
  server.ml            # endpoint, paws, routes — the whole surface
  store/               # trails.ml, keys.ml, clicks.ml (codecs + collections)
  webhooks.ml          # signing + the outbox handler registration
  test/
    http/              # THE spec: every endpoint, every error shape, auth matrix, webhook flow
    system/            # boot + healthz + a real end-to-end shorten→redirect→stats round-trip
```

## Implementation hints

- **Error shape discipline**: one JSON error envelope (`{"error","status",…}`) everywhere —
  including framework errors (405 keeps `Allow`), malformed JSON bodies (400 with a parse hint),
  and auth failures (401 vs 403 distinguished). Write the http tests FIRST for the error matrix.
- **Status-code semantics worth demonstrating**: 201 + `Location` on create, 409 on slug collision,
  410 after delete, 301 (not 302) for the redirect, `Retry-After` on rate-limit 429s.
- **Key hygiene**: store only a hash; show the key once at mint; prefix keys (`trl_…`) so they're
  greppable in leaks; constant-time compare (the pattern accounts already uses).
- **Webhook correctness**: sign with HMAC-SHA256 over the raw body, send `X-Trails-Signature` +
  an id + timestamp; document replay-window verification; deliveries retry via the outbox and give
  up to a dead-letter log line — state at-least-once + idempotency-key semantics honestly.
- **The redirect hot path**: one indexed point-read + an enqueued count — verify with a quick
  micro-measure that stats add no visible latency; it's this app's covenant moment.
- **Self-description**: `/v1` returns the route list with method, path, auth, and a one-liner —
  cheap, and it makes the API explorable from `curl` alone (agents love this).
- **Rate limits**: per-key token bucket on writes; the unauthenticated redirect gets a generous
  per-IP limit; both covered by http tests asserting the 429 + headers.
- **Testing**: the http cut IS the deliverable — auth matrix (no key / bad key / revoked key),
  CRUD happy paths, every error shape, webhook signature verification (receive the delivery in the
  test's own listener). The system cut boots the binary and runs one full round-trip.
- Discover first: `fennec discover "return JSON errors for an API"`,
  `fennec discover "verify a webhook signature"`, `fennec discover "rate limit by key"`.

## Sources

[Vercel — templates gallery (Backend / Cron categories)](https://vercel.com/templates) ·
[Octahedroid — web application types 2025](https://octahedroid.com/blog/web-applications-types-and-examples-know-2025) ·
[AppVerticals — types of web applications](https://www.appverticals.com/blog/types-of-web-applications/)
