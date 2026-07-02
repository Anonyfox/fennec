# Denhunt — the marketplace / two-sided archetype

A job board for the desert: employers post dens-with-benefits, seekers apply with a CV upload,
moderators keep it clean. The **two-sided platform** shape — distinct account roles with different
views of the same data, listing lifecycles driven by time, and the trust machinery (moderation,
anti-spam) every open-submission site needs.

## The category

Two-sided platforms are the family where *users produce the inventory*: marketplaces for goods and
services, directories and listings (real estate, local business), job boards, booking/reservation
systems, review sites ([HubSpot](https://blog.hubspot.com/website/types-of-websites) lists job
boards, directories, booking, and review sites as distinct types; listing-site patterns in
[Fireart's directory examples](https://fireart.studio/blog/13-examples-of-inspirational-listing-websites/)
and [Directorist's template survey](https://directorist.com/blog/best-directory-website-templates/);
booking's canonical layers — search, availability, pricing, booking —
in [PHPTravels' reservation design](https://phptravels.com/blog/hotel-reservation-system-design)).
The defining traits:

- **Asymmetric roles**: the supply side (posts, manages, pays) and the demand side (searches,
  filters, applies/books) see different apps over shared data.
- **Listings have lifecycles**: draft → published → expired/filled, driven by clocks as much as
  clicks — cron is architecture here, not a nicety.
- **Trust is a feature**: moderation queues, spam guards, report buttons; an open-submission site
  without them is a spam site within a week.
- **Search IS the product** on the demand side: facets, freshness, and zero-result grace.

A job board is the leanest complete instance of the family (two roles, applications as the
transaction, no payments needed) — the mechanics generalize to every marketplace.

## The app

**Denhunt** — "find your next burrow." Features:

- Public/demand side: `/` (fresh listings), faceted search (region, den-type, remote-friendly,
  salary band in acorns), `/job/:slug`, apply with note + CV upload; a saved-search with email
  alerts for logged-in seekers.
- Supply side: employer accounts with an org profile, post-a-listing form (draft → submit for
  review), a dashboard of their listings + applications received, mark-as-filled.
- Moderation: an admin queue — new listings land `pending`, a moderator approves/rejects (with a
  reason that's emailed); a report button on public listings feeds the same queue.
- Lifecycle: listings auto-expire after 30 days (`[@cron]`), with a "renew" email 3 days before.

## How it works (the fennec shape)

Axes: SSR public side + lightly-reactive dashboards · CRUD + uploads · **accounts with roles**
(seeker / employer / moderator via the RBAC policy) · forms + a few live queries · async = the
fleet's richest cron + email story.

- Three faces on two endpoints: public + seeker views, the employer dashboard, and the moderation
  queue (role-gated routes on the admin endpoint).
- Collections: `listings` (with status: draft/pending/published/rejected/expired/filled),
  `applications`, `orgs`, `saved_searches`, `reports`. Status transitions as one module of guarded
  functions — the state machine in one place.
- CV upload via multipart into file storage keyed by application; served back only to the listing's
  employer (authorization on the download route — a deliberate security beat).
- Applications trigger a notification email to the employer (`[@after apply]`); saved-search
  alerts run as a daily `[@cron]` digest (new matches since last run).
- Search: indexed fields + typed filters composed from the query string; facet counts computed
  server-side.

Target layout:

```
examples/marketplace/
  server.ml
  web/
    apps/main/         # public + seeker + employer (role-aware nav)
    apps/admin/        # moderation queue
    components/        # listing_card.mlx, facet_bar.mlx, application_row.mlx, status_chip.mlx …
    handlers/          # post_listing.mlx, apply.mlx, moderate.mlx (each view + submit)
    store/             # listings.ml, applications.ml, orgs.ml, saved_searches.ml, reports.ml
  workflows/           # transitions.ml, expiry.ml ([@cron]), alerts_digest.ml ([@cron])
  test/
    http/              # role matrix (who sees/does what), facets, upload limits, transitions
    browser/           # post → moderate → appears public → apply → employer sees it
    system/            # the expiry cron: freeze time boundaries via the tick-driven scheduler
```

## Implementation hints

- **The role matrix is the spec**: write it as a table first (guest/seeker/employer/moderator ×
  every action), then encode it as http tests — the marketplace's correctness IS this matrix.
- **Status machine discipline**: transitions only through the guarded module (illegal transition =
  error, tested); every transition records who/when (the audit trail moderation depends on).
- **Uploads done right**: `Body_limit` sized for CVs, extension/content-type allowlist (pdf/txt),
  stored outside the webroot, downloads authorized per-employer + `Content-Disposition` attachment
  (the injection-safe helper exists — use it).
- **Anti-spam floor**: honeypot + per-account posting rate limit + new-employer listings always
  `pending` (auto-approve is a config you DON'T enable in the example — say why).
- **Search niceties**: facet counts that update with selections, "0 results" state with loosened
  suggestions, freshness sort default, URL-carried facet state (shareable searches — the
  saved-search feature is literally "bookmark this URL server-side").
- **The cron pair**: expiry runs daily (published + 30d → expired; email at 27d); the alerts digest
  dedupes per seeker (max one email/day) — both drive through the scheduler's deterministic
  `tick` in tests (time as an argument; no sleeping tests).
- **Salary transparency nicety**: band required (the modern norm), rendered prominently, filterable.
- Discover first: `fennec discover "restrict a route to a role"`,
  `fennec discover "accept a file upload"`, `fennec discover "run a job daily at a time"`.

## Sources

[HubSpot — 28 types (job board, directory, booking, review)](https://blog.hubspot.com/website/types-of-websites) ·
[Fireart — listing website examples](https://fireart.studio/blog/13-examples-of-inspirational-listing-websites/) ·
[Directorist — directory templates 2025](https://directorist.com/blog/best-directory-website-templates/) ·
[PHPTravels — reservation system design](https://phptravels.com/blog/hotel-reservation-system-design)
