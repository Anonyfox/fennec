# PWA — installable, offline-booting apps (as-built)

Status: **as-built (living record).** Tiers 1–3 of the PWA story, framework-owned end to end. The
PWA opt-in (one `Pwa.v` declaration + `~persist` on the client) IS the user's consent to offline
persistence. Guide order: METHODS.md (methods/offline semantics) → this file (what survives reloads).

## Tier 1 — installability + offline shell (`fennec.pwa`)

`Pwa.v name ~icons …` generates the manifest, the service worker, and the registration head snippet
(`Pwa.head_html` — include in the document template). `Pwa.paw cfg ~assets ~precache` serves
`<scope>manifest.webmanifest` + `<scope>sw.js` (with `Service-Worker-Allowed`). The framework owns
the asset graph, so the precache list and the cache VERSION are exact: `version_of` digests the
precached assets' contents; activation deletes older caches — an atomic swap per deploy. Fetch
strategy: precached assets cache-first; **navigations network-first with offline fallback to the
last seen copy of the page** — whose embedded SSR seed restores its data through the normal
seed → tentative → quiesce path. Updates are user-confirmed: the new worker WAITS;
`Pwa_client.update_available ()` (a Fur signal) flips; `Pwa_client.apply_update ()` swaps + reloads.
Multiple PWAs per origin = one scope per app subpath; multitenant-by-host = separate origins.

## Tier 2 — persistent data cache (`Ddp_client.connect ~persist:"<ns>"`)

Each subscription's data snapshots to local storage (debounced on deltas + immediately at every
`ready`) via `Merge_store.snapshot_sub` — the SEED format, carrying only the sub's OWN fields. On
the next boot, `subscribe` restores the snapshot as a seed: warm data instantly, fully offline,
TENTATIVE until the next live `ready` re-confirms (quiescence prunes what died while away). Storage
is namespaced (`fennec:<ns>:…`) — subpath apps on one origin never collide. **Identity hook:** call
`Ddp_client.purge_storage` on logout/user-switch; one user's cache must never leak to the next.
Backend: localStorage, synchronous BY DESIGN (restores happen at exact boot points — no async races
to reason about); the ~5MB quota fits "what you were subscribed to + the outbox"; IndexedDB is the
named seam if an app outgrows it (the interface would go async — a deliberate later decision).

## Tier 3 — persistent write outbox

Buffered methods survive reloads as `(name, params, seed)` (`Outbox` codec — closures don't
survive, so results are fire-and-forget post-reload, Meteor's semantics). On boot the outbox
re-issues each entry with a FRESH id and its ORIGINAL seed, and `Method.stub_replay` re-runs its
stub — the deterministic seed streams remint byte-identical optimistic ids, so the rows reappear
exactly as they were, before the socket even opens. The normal flush + `updated` reveal then
resolves them with the proven server-wins reconciliation. At-least-once across sessions: write
idempotency stays the app's concern, now with a longer fuse. The outbox key is **TAB-scoped** (a
sessionStorage-persisted suffix): two tabs sharing one persist namespace never clobber or
double-execute each other's pending writes — sessionStorage is per-tab and survives that tab's
reloads, exactly the right lifetime. Data snapshots ARE shared across tabs (any tab's snapshot is
valid data; last-writer-wins is benign), and a stopped subscription deletes its snapshot.

## Out (named seams)

Push notifications (VAPID infra), SW background sync while the page is closed (DDP-in-worker or an
HTTP method-batch bridge), IndexedDB storage, precache manifests emitted at build time by the CLI.
