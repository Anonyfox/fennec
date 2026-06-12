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

**Where we're genuinely behind, with the deliberate ordering:**
- **Generators/scaffolding** — real gap vs `rails g`, deliberately LAST: generators codify surface,
  and ours is still moving. Until then the agent fastlane + discover (below) cover the same need.
- **Background jobs, mailers, storage, i18n, OpenAPI, deeper observability** — later features, not
  the current story. Named, not forgotten.
- **Multi-node fan-out — explicit NON-GOAL.** The scaling story is: a high-performance baseline ×
  vertical scaling on the multicore effects runtime (one process saturates a big box; the observe
  multiplexer makes live-query cost per-query, not per-viewer). When horizontal is ever needed, it
  is the boring kind: more servers behind a load balancer — and fennec is unusually good at boring
  LB because servers are STATELESS per session by design: a client landing on a different node
  heals completely from client state (resubscribe + delta resync + outbox flush; sticky sessions
  not required), and with mongod change streams every node observes the database directly — the
  database IS the bus, no Redis adapter, no cluster protocol.
- **Resilience model, vs BEAM's "let it crash":** fennec's answer is typed error elimination first,
  fiber containment second. A method exception → a 500 Result (connection lives); a publication
  exception → Nosub (client stops loading); a callback exception is contained inside the Fanout
  drainer; a dead socket tears down its session's observers (proven, RX2). The blast radius of a
  crash is one fiber/one request — and crucially there are almost no long-lived stateful processes
  TO supervise, because state lives in collections, not in process heaps. That is the precise
  counter-position to supervision trees: Phoenix needs them because state lives in processes; we
  removed the patient instead of building the hospital.

## AI-first development — the deliberate second audience

fennec explicitly optimizes for two developers: the human and the coding agent. Two shipped
features make the agent a first-class citizen rather than a tolerated guest:

**`fennec dev --agent --attach` (the fastlane).** One guarded post-tool hook bridges the dev loop
into the agent harness: after every edit, the agent's next model step receives the dev verdict —
build result, affected surface (backend/component/route/styles/tests), unit-test tally, focused
compiler diagnostics with a code frame, and last-good serving state. The agent stops tailing logs,
stops running `dune build` after every edit, stops guessing whether the change took: the framework
closes its feedback loop natively, deadline-bounded, with `fennec agent status` as cheap recovery.

**`fennec discover "<task>"` (pre-edit orientation).** A source-derived, evidence-backed
orientation card for task-shaped questions ("build login with signed cookies", "SSR page with a
client counter"): which public path to use, which examples prove it, what to inspect next. It
attacks the agent failure mode directly — stale training priors, hallucinated APIs, expensive blind
`rg` exploration — by replacing familiarity with instrumentation. Because fennec owns its source
(`.mli` docs, examples, tests, the dune graph), the index is generated truth, not curated prose.

**What this does to the "exotic language" penalty:** the honest assessment. The penalty is real —
LLMs write Rails and Next from memory; they cannot write mlx from memory. But the penalty's COST
model changes completely under instrumentation: an agent in a Rails repo writes plausible code and
discovers wrongness at runtime (if ever); an agent in a fennec repo writes its first attempt from a
discover card, gets a typed compiler verdict injected within seconds, and converges in one or two
cycles. Strict types + instant ground-truth feedback is exactly the loop agents thrive in —
OCaml's strictness flips from adoption liability to vibecoding asset. What remains genuinely
unsolved: zero-shot generation quality outside the loop (a one-shot snippet on a forum will be
worse than a one-shot Next snippet), and that is a corpus problem time and examples mitigate, not a
design problem. No other framework ships a native agent harness; the closest analogs (llms.txt,
MCP docs servers, repo maps) are docs-entry conventions, not loop closure.

## DX across the scale spectrum — how it actually feels

**Tier 1: ship-fast solo — landing page + a little logic, hours to prod.**
The honest ranking today: Next+Vercel and Rails 8 win the first hour (`npx create-next-app` /
`rails new` + a deploy product); fennec has no `create-fennec-app` yet and an opam toolchain warmup,
so minute 0–30 is slower. From the first deploy on, the order inverts: fennec prod is `scp` one
static binary to any box — auto-HTTPS (ACME), PWA, offline, multitenancy included — no Vercel bill,
no node_modules, no Ruby runtime, and the experiment that grows up never hits a rewrite cliff
(the landing page and the eventual product are the same architecture). Verdict: behind on
first-hour ceremony, ahead on first-deploy and every hour after.

**Tier 2: the product app — 2–8 people, real users, app-like features.**
fennec's strongest tier, and it isn't close. The moment a product needs live data + optimistic UI +
offline — i.e. the moment it becomes an *app* — Next teams assemble the five-library stack and own
its reconciliation bugs; Rails teams discover Turbo patches don't give them a queryable client
cache; Phoenix teams write JS hooks for client interactivity and lose state on disconnects. In
fennec these are the zero-glue baseline, refactors are compiler-walked across the whole vertical,
and `fennec test` runs unit→browser→system in one command. The day-to-day feel: features are
mostly *declarations* (a Method, a publish, a component), and the scary parts (sync, conflicts,
reconnects) are someone else's proven code — ours.

**Tier 3: logic-heavy enterprise platform.**
The types compound: at hundreds of methods, wire integrity at compile time is the difference
between refactoring weekly and refactoring never (only Blazor offers this, at WASM-payload and
SignalR-is-just-transport cost). OCaml's variants + exhaustive matching + functors model gnarly
domains better than anything in Rails/Django/Laravel-land, and the resilience model above holds
under the load. The thin parts at this tier are the periphery — jobs/observability/i18n (later
features) and integration breadth (Kafka/SAP/etc. clients — Java and C# ecosystems are deeper) —
plus hiring, which the agent-first story partially converts from "find OCaml devs" into "your
agents are productive on day one, your seniors review."

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
evolving). The bet's cost is ecosystem loneliness — which the agent fastlane + discover are built
to convert from a familiarity problem into an instrumentation problem; its payoff is that the
hardest parts of a modern app (sync, offline, optimism, types across the wire) are guarantees
instead of glue, for human and agent developers alike.
