# Packboard — the SaaS archetype

A realtime team kanban, sold per-pack: organizations with invited members and roles, plan gates,
onboarding, settings, a weekly digest — the **full SaaS canon** in one vertical-shaped tool. This is
the fleet's summit app: everything the previous seven established (SSR, collections, accounts,
realtime, forms, workflows, cron, email) composed into the shape most products ship as in 2026.

## The category

SaaS is the default shape of the modern software business, and its starter-kit canon is remarkably
uniform across every ecosystem — auth, teams/orgs, roles, billing, settings, an admin panel, and a
marketing/SEO face ([the SaaS starter-kit index](https://saas-starter-kits.com/),
[SaaS Pegasus' boilerplate guide](https://www.saaspegasus.com/guides/saas-boilerplates-and-starter-kits/),
[SaaSykit's Laravel kit survey](https://saasykit.com/blog/10-best-laravel-starter-kits-for-2025)).
The 2025/2026 twist is **vertical SaaS**: the same canon aimed at one niche — "a kanban built only
for landscaping companies charges 3× because it speaks the user's language"
([Flowjam — indie SaaS ideas 2025](https://www.flowjam.com/blog/indie-hackers-saas-ideas-2025-10-you-can-launch-fast)).
The defining traits:

- **The org is the tenant**: data belongs to organizations, not users; membership + roles gate
  everything; invites are the growth loop.
- **Plans gate features, billing is a provider**: the plan/entitlement logic is yours; the payment
  rails are Stripe/Paddle — so the example builds the entitlement half and designs the provider
  seam (same decision as `shop/`).
- **The product is a loop, not a page**: onboarding → daily use (the actual tool) → digest emails
  pulling users back → settings/admin as the maintenance face.

## The app

**Packboard** — "how fox packs ship." Vertical flavor: kanban for den-construction crews. Features:

- Marketing face: `/` pricing + pitch (reuses the `brochure/` muscles), signup.
- Onboarding: create your pack (org) → name your first board → invite two packmates (email
  invites with accept links) — a real first-run, not a blank screen.
- The tool: boards → columns → cards, **fully realtime** (drag a card, the pack sees it move),
  card assignees, due dates, a per-board activity feed, member presence.
- Roles: alpha (owner — billing/settings/danger zone), builder (member), scout (read-only) —
  through the accounts RBAC policy.
- Plans: Free (1 board, 5 members) vs Pro (unlimited) — enforced server-side as entitlements; the
  upgrade page ends at the documented provider seam (`awaiting_provider` + the integration guide
  comment), mirroring `shop/`'s payments stance.
- The loop-closer: a Monday-morning `[@cron]` digest per pack (what moved, what's due).
- Settings: pack profile, members table (role changes, remove), personal profile, and the danger
  zone (delete pack — a workflow that cascades correctly).

## How it works (the fennec shape)

Axes: all of them — SSR marketing + live SPA tool · multi-tenant reactive data · **accounts +
orgs + RBAC** · methods (tool) + forms (settings/onboarding) · workflows + cron + outbox email.

- Two endpoints: marketing (public SSR) and the app (auth-gated SPA). The accounts org feature
  carries packs/membership/invites; the RBAC policy declares the three roles once, guards read
  `can`/`require_role` everywhere.
- **Tenancy rule #1**: every query filters by the active org, enforced in ONE place — a scoped
  collection accessor the components must go through, so "forgot the org filter" is structurally
  hard. This module is the app's most important 30 lines.
- Boards/columns/cards/activity are Sift-coded collections published per-org (subscription
  parameterized by the active pack); moves are methods with optimistic simulation (the `chat/`
  muscles, now with structure).
- Entitlements: one `plan.ml` module — `can_add_board`, `can_invite` — consulted by methods AND
  rendered into the UI (disabled + upgrade hint), so limits are one source of truth.
- Invites: token + email via outbox, accept route joins the org (existing account) or lands on
  signup-then-join (new) — the flow every SaaS fumbles; do it properly and test both paths.

Target layout:

```
examples/saas/
  server.ml            # marketing + app endpoints, accounts config (orgs on, RBAC policy)
  web/
    apps/marketing/    # pricing/pitch (SSR)
    apps/main/         # the tool (SPA)
    components/        # board.mlx, column.mlx, card.mlx, member_table.mlx, digest_preview.mlx …
    handlers/          # onboarding.mlx, settings/*.mlx, invite_accept.mlx
    store/             # boards.ml, cards.ml, activity.ml + scoped.ml (THE tenancy accessor)
    plan.ml            # entitlements, one source of truth
  workflows/           # delete_pack.ml, weekly_digest.ml ([@cron "0 7 * * 1"])
  test/
    http/              # tenancy isolation matrix, entitlement limits, invite both-paths
    browser/           # two browsers, one pack: drag in A, moves in B; onboarding first-run
    system/            # digest cron via deterministic tick; delete-pack cascade
```

## Implementation hints

- **Tenancy isolation is the security story**: the http matrix MUST include cross-org probes —
  a member of pack A requesting pack B's board by id gets 404 (not 403 — don't confirm existence).
  One test per collection through the scoped accessor.
- **Drag-and-drop without a library**: column moves as explicit methods (`move_card ~to_column
  ~before`) with fractional-order keys (or index rebalancing — document the choice); optimistic
  move + settle. Keyboard fallback (select card → move buttons) keeps it accessible and testable.
- **The activity feed is `[@after]` hooks** on the mutating workflows — the reactions system
  demonstrated on product features rather than plumbing.
- **Invite hygiene**: single-use tokens with expiry, re-invite resends (no duplicate rows), the
  accept flow works logged-in, logged-out-with-account, and brand-new — three http tests.
- **The digest**: per-pack aggregation since last digest, skipped when nothing happened (nobody
  loves empty digests), one email per member honoring a per-user opt-out — cron + outbox + honest
  batching in one workflow, driven by `tick` in the system test.
- **Danger zone done right**: type-the-pack-name confirm, the delete workflow cascades
  (boards/cards/activity/invites/membership) in one transaction, and the audit log records it.
- **Presence + assignee avatars** reuse `chat/` patterns; the fox-avatar generator moves into a
  shared component.
- **Plan gates in the UI**: disabled states explain themselves ("Free packs get 1 board — upgrade")
  — never a silent failure; the same message the server returns when bypassed.
- Discover first: `fennec discover "organizations with member roles"`,
  `fennec discover "declare an RBAC policy"`, `fennec discover "run a weekly cron job"`.

## Sources

[SaaS starter-kit index](https://saas-starter-kits.com/) ·
[SaaS Pegasus — boilerplates guide](https://www.saaspegasus.com/guides/saas-boilerplates-and-starter-kits/) ·
[SaaSykit — 10 best Laravel starter kits 2025](https://saasykit.com/blog/10-best-laravel-starter-kits-for-2025) ·
[Boilerplatelist — top Laravel SaaS boilerplates](https://boilerplatelist.com/collections/top-laravel-saas-boilerplates/) ·
[Flowjam — indie SaaS ideas 2025 (vertical SaaS)](https://www.flowjam.com/blog/indie-hackers-saas-ideas-2025-10-you-can-launch-fast) ·
[Vercel — SaaS templates](https://vercel.com/templates/saas)
