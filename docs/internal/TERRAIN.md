# TERRAIN — fennec vs the modern web framework landscape

Status: **analysis snapshot (June 2026).** Capabilities + DX comparison against the twelve stacks a
team would realistically choose instead. Written from the as-built fennec (Pulse methods/offline/PWA/
delta-resync complete; Accounts in flight). Honesty rule: every "fennec leads" claim below is backed
by shipped, tested code; every gap is named, not euphemized.

The fennec baseline all comparisons run against — the ENTIRE full-stack vertical for a live feature:

```ocaml
(* shared (compiles into server AND browser bundle) *)
let add_task = Method.define "addTask" ~args:(Codec.a1 Codec.string) ~result:Codec.string
    ~stub:(fun sim title -> ignore (sim.Method.insert "tasks" (Bson.doc [("title", Bson.str title)])))

(* server *)
RData.publish "tasks" (fun _ -> RData.Cursor (RData.cursor tasks ()));
RData.handle add_task (fun _inv title -> RData.Collection.insert tasks (Bson.doc [("title", Bson.str title)]))

(* component — server-renders, hydrates, stays live, works offline *)
let docs = Ddp_client.find client "tasks" () in
<ul>(items_of (Fur.get docs))</ul>
<button onClick=(fun _ -> ignore (Ddp_client.call_m client add_task title))>"Add"</button>
```

Iteration loop: bytecode server + ~0.2s jsoo relink + CLI livereload; `fennec test` runs
unit → property → HTTP → real-Chrome → system in one command; deploy is one static binary with
auto-ACME HTTPS and Host-routed multitenancy. Renames/arity/type drift across the wire are compile
errors. Offline/optimistic/PWA/resync need zero app code.

---

## 1. Meteor (the inspiration)

**Where it shines:** npm — any React/Vue/Svelte frontend, any package. A decade of production lore,
accounts-ui maturity, Galaxy hosting. Lower entry bar: untyped JS methods are faster to slap down.

**Our take vs theirs:** same verbs (`publish`/`subscribe`/`methods`/`call`), same wire (we decode
real Meteor frames byte-for-byte in CI). Divergences are deliberate: typed method values (drift =
compile error vs runtime 404), allow/deny refused outright (they regret it publicly), one shared
backend observe per query (their NO_MERGE-strategy debate solved structurally), multicore in one
process (Node = one core per process, oplog work duplicated per instance), delta resync on reconnect
(Meteor re-streams the full subscription — never fixed), offline + PWA built in (Meteor needs
ground:db-style community packages, mostly unmaintained).

**Feel:** Meteor feels loose and fast on day 1, haunted by 2am runtime errors at month 6. fennec
feels stricter on day 1 — the codec/labeled-args ceremony is real — and then the compiler carries
the hundred-method scale Meteor teams fear.

## 2. Rails

**Where it shines:** the highest CRUD throughput in the industry. `rails g scaffold Post title`
→ model+migration+controller+views in seconds. ActiveRecord migrations as a first-class discipline.
Hotwire/Turbo gives sprinkle-realtime without writing JS. Rails 8 ships auth generators, Solid
Queue/Cache/Cable (no Redis), Kamal deploys. Convention culture + the deepest gem shelf (Devise,
Sidekiq, ActiveStorage) + guides that teach the craft.

**Our take vs theirs:** Rails broadcasts *patches* (Turbo Streams: "append this HTML"); fennec syncs
*data* — the client holds a queryable live cache, so a second widget over the same data is free,
offline works, optimistic UI is structural. Rails has no types anywhere; a renamed column is a
production exception. One fennec binary vs Ruby runtime + asset pipeline.

**Feel:** nothing matches `rails g` for the first week. fennec has **no generators** — an honest
gap (a `fennec g resource` writing the Method/handler/component triple would close most of it).
Rails iteration is reload-and-refresh fast; the type feedback fennec gives at edit time, Rails
gives at runtime — often in production.

## 3. Phoenix / LiveView (the closest philosophical rival)

**Where it shines:** the BEAM. Process-per-user isolation, supervision, **multi-node distribution
out of the box** — Presence and PubSub fan out across a cluster with zero infra. LiveView's
server-held UI needs no client state at all; minimal-diff updates over the socket. Ecto changesets
are a genuinely great validation model. `iex` + recompile is a joyful loop.

**Our take vs theirs:** opposite answer to the same question. LiveView keeps UI state on the
server → a dropped connection means a dead UI and lost form state; reconnect re-mounts. fennec
keeps a synced cache on the client → offline keeps working, optimistic writes queue, reload
survives. LiveView needs JS hooks the moment you want client-side interactivity; fennec components
ARE client code. Typed end-to-end vs Dialyzer-optional.

**Their structural advantage we don't have:** clustering. fennec is multicore on ONE node; Phoenix
PubSub spans nodes natively. Our seam: a Fanout backend over a shared bus (the mux makes per-node
cost low, but cross-node invalidation is unbuilt).

## 4. Next.js

**Where it shines:** gravity. React's ecosystem, RSC streaming, Vercel's deploy polish,
image/font/edge optimizations, and the largest example/StackOverflow/LLM-training corpus in
existence — AI assistants write Next.js fluently, which is now a real DX factor.

**Our take vs theirs:** Next has **no data layer**. The fennec vertical above requires, in Next:
Prisma (DB) + tRPC or server actions (RPC) + TanStack Query (cache/optimistic) + Pusher/Ably or
WebSocket DIY (live) + next-pwa (offline) — five libraries, five config files, and the
cache-invalidation glue is yours. Server Actions are the closest analog to Methods: RPC-ish, but
revalidation is path/tag-based page refetching, not delta sync; optimistic UI is a `useOptimistic`
hook you reconcile by hand. Hydration mismatch bugs are a whole genre; fennec's seed-exact
hydration kills the class. node_modules vs zero deps.

**Feel:** Next iteration is fast (Fast Refresh) and the tooling is luxurious; the complexity tax
arrives as config sprawl and "which rendering mode am I in?" — fennec has exactly one model
(SSR → hydrate → live), and it's the same model offline.

## 5. Remix / React Router 7

**Where it shines:** web fundamentals — loaders/actions over forms, progressive enhancement,
works-without-JS. The cleanest mental model in React-land.

**Our take vs theirs:** a loader is fennec's SSR seed — except our seed stays **live** after load
and the action's effects push to every open client. Same five-library story as Next for realtime.

## 6. SvelteKit

**Where it shines:** the compiler. Svelte 5 runes are signals — the same reactivity model as Fur —
with the most polished template DX in the industry (formatter, LSP, errors). Tiny bundles, no VDOM.

**Our take vs theirs:** philosophically the closest *frontend*; the gap is again the data layer
(none built in). Honest DX comparison: a `.svelte` file edits nicer than an `.mlx` file today —
mlx's formatter/editor tooling is younger and it shows. Our compensation is that the same file
type-checks against the server's actual data shapes.

## 7. Nuxt (Vue)

Same verdict as Next with Vue's gentler learning curve and auto-import magic. Nitro's deploy
targets are excellent. No live-data answer; useFetch + your own socket glue.

## 8. Django

**Where it shines:** the **admin** — an auto-generated CRUD UI over your models that ships with
every app, still unmatched 20 years on. ORM + migrations, a forms/validation framework, i18n,
sessions/auth in the box, and a stability culture (deprecation policy measured in years).

**Our take vs theirs:** Django is sync-first; realtime is Channels (a bolt-on with its own
deployment shape), and the frontend story is "templates, or DRF + a SPA" — the split-stack tax.
fennec's whole premise inverts that. But the admin is a real gap: Pulse has the primitives (live
`find` over any collection + typed methods) to generate one, and it would be a flagship feature.

## 9. Laravel

**Where it shines:** the DX-polish king. Eloquent reads like prose, Blade/Livewire (their
LiveView-lite), first-party everything: Sanctum (auth), Cashier (billing), Horizon (queues), Reverb
(websockets), Forge/Vapor (deploy products). The lesson Laravel teaches: a framework wins by
shipping *products around the framework*.

**Our take vs theirs:** untyped, and the realtime is broadcast-events (you re-fetch), not a synced
cache. But their queue/jobs story (Horizon) highlights our gap: **fennec has no background-job
framework** — Eio fibers ad hoc, no persistence/retry/scheduling. For pro apps that's a must-build.

## 10. ASP.NET Core / Blazor (the closest *typed* competitor)

**Where it shines:** C# end-to-end like us. Blazor Server ≈ LiveView; Blazor WASM ≈ our jsoo client.
EF Core, SignalR, a debugger/profiler/IDE story OCaml can't match, enterprise integration depth.

**Our take vs theirs:** Blazor WASM ships a .NET runtime to the browser — multi-MB payloads against
our compact jsoo bundle; first-paint via prerender is clunkier than seed-hydration. SignalR is a
*transport* (you build the sync); Pulse is the sync. Where they win decisively: tooling. Visual
Studio's debugging of full-stack C# is years ahead of OCaml's LSP + printf reality.

## 11. Spring Boot

**Where it shines:** enterprise gravity — DI, Actuator observability, Spring Security's depth,
Kafka/batch/cloud integrations. If the job is "integrate with 14 enterprise systems," Spring wins.

**Our take vs theirs:** startup time, annotation magic, and ceremony are the cost; the web/realtime
DX (WebFlux/STOMP) is workmanlike. fennec is a different sport — but Actuator-style observability
(metrics/health/tracing OOTB) is a gap worth noting; we have a Metrics paw, not a story.

## 12. FastAPI + React (the de-facto modern split stack)

**Where it shines:** Pydantic models → automatic OpenAPI docs → generated TS clients; Python's ML
ecosystem next door. Simple, honest, everywhere.

**Our take vs theirs:** the type boundary is *regenerated* (openapi-typescript, drift windows in
CI) where ours is *shared* (one value, no codegen, no window). Two repos/builds/deploys vs one.
Their OpenAPI docs are a gap on our side for public-API products: Codec carries the schema
information; emitting OpenAPI from method declarations is buildable and unbuilt.

---

## The honest scoreboard

**Where fennec leads — and nobody else ships it OOTB:**
1. **The live-synced, offline-first client cache** with optimistic UI, write outbox, PWA, identity
   purge, and delta resync. Meteor is closest and lacks offline/resync/multicore; everyone else
   assembles it from 4–6 libraries and owns the reconciliation bugs.
2. **One typed language across the entire vertical including the wire** — only Blazor competes, at
   a runtime-download cost an order of magnitude higher.
3. **Operational self-containment**: one static binary, auto-ACME HTTPS incl. wildcard/on-demand,
   Host-multitenancy, multicore — no nginx, no Vercel, no Redis for the core story.
4. **The test pyramid in one tool** (unit→property→HTTP→real Chrome→system, `fennec test`).
5. **Proof culture**: the data layer's guarantees are pinned by adversarial tests, not docs prose.

**Where we're at parity in kind, behind in polish:** template/editor DX (SvelteKit, Blazor),
guides-and-tutorial culture (Rails, Django, Laravel), error-message friendliness.

**Where we're genuinely behind (the post-Accounts roadmap, in rough value order):**
1. **Generators/scaffolding** — `rails g` is the first-week experience; a `fennec g resource`
   emitting the Method/handler/component triple is cheap and high-leverage.
2. **Background jobs** — no queue/retry/schedule story (Sidekiq/Oban/Horizon/Solid Queue all set
   the bar). Must exist for pro apps.
3. **Admin UI** — Django's flagship; Pulse's live find + typed methods are exactly the primitives
   to generate one.
4. **Multi-node fan-out** — Phoenix PubSub's cluster story; our seam is a Fanout bridge over a
   shared bus.
5. **Mailers, file storage, i18n** — the Rails/Laravel batteries we lack.
6. **OpenAPI emission** from method declarations (the FastAPI lesson).
7. **Observability** — Actuator-grade metrics/health/tracing OOTB.
8. **The ecosystem/AI-familiarity deficit** — unfixable by code: LLMs and new hires speak Rails and
   Next fluently; OCaml+mlx is low-resource. Countermeasures: examples corpus, guides, and DX so
   regular that little needs to be known.

## How iteration *feels*, side by side

| | edit→see | wrongness surfaces | full-stack rename | live feature cost |
|---|---|---|---|---|
| fennec | ~1s (bytecode + 0.2s relink) | **compile time, both sides** | compiler walks you | 0 extra libs |
| Meteor | seconds (slower at scale) | runtime | grep + prayer | 0 (no offline/types) |
| Rails | instant reload | runtime/production | grep + tests | Turbo patches only |
| Phoenix | fast (iex) | runtime + Dialyzer-maybe | grep + tests | LiveView (online-only) |
| Next.js | instant (Fast Refresh) | TS-partial (wire untyped) | TS helps per-side | 4–6 libraries |
| Django | instant reload | runtime | grep + tests | Channels bolt-on |
| Blazor | moderate (hot reload flaky) | **compile time, both sides** | IDE refactor (best-in-class) | SignalR + DIY sync |

The one-sentence version: **fennec's bet is that the data layer is the framework** — everyone else
makes it the app's problem (or, in Meteor's case, made it the framework's problem first and stopped
evolving). The bet's cost is ecosystem loneliness; its payoff is that the hardest parts of a modern
app (sync, offline, optimism, types across the wire) are guarantees instead of glue.
