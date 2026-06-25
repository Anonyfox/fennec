# The non-web core — Collections, Workflows, Reactions

**Status: IMPLEMENTED** (Jun 25 2026). The runtime, the ppx, and the example all ship; this document is
both the design rationale and the as-built reference. What's where:

| Concept | Module | Lib |
| --- | --- | --- |
| Transparent transaction | `Fennec_mongo_dynamic.Tx` (+ `Minimongo.tx_snapshot/restore`) | `fennec-mongo.dynamic` |
| Workflows runtime | `Workflow.exec` (what `[@workflow]` lowers to) | `fennec.pulse.workflow` |
| Schedules + the claim | `Schedule` (every/cron + `_fennec_cron`) | `fennec.pulse.workflow` |
| `[@workflow]` + `@after`/`@before`/`@cron`/`@every` + circuit-breaker | the ppx | `fennec.pulse.workflow.ppx` |
| Collection methods (create / save / delete / find_one / where / all / count) | generated on the model by `[@@deriving collection]` over the isomorphic `Coll_writer` seam (server backend installed by `Fennec_pulse_app`) | `fennec.pulse.live.client` + `fennec.pulse.app` |
| Userland homes | `collections/` + `workflows/` (peers of `web/`) | the app |

**Atomic rollback works on all three backends now** — in-memory (`:memory:`, snapshot-and-restore),
**burrow** (the durable dev/embedded default — one held LMDB parent txn via `begin_txn`/`commit_txn`/
`abort_txn`), and **real replica-set Mongo** (a libmongoc client-session transaction —
`session_start` / `*_s` ops / `session_commit`|`abort`). Each routes a workflow's reads + writes through
its native transaction, commits on return, and rolls back on `raise` — invisibly, no userland change.
Read-your-writes is **complete** (every read — find/count/distinct/aggregate — sees the workflow's pending
writes); the non-transactional path stays byte-identical on every backend. The circuit-breaker is compile-time for the `@after`/`@before` graph
(intra-module via the ppx, cross-module via module dependency cycles) plus a runtime re-entrancy guard for
body-level cascades. See `fennec/pulse/workflow/README.md` for the API and the example's
`collections/` + `workflows/` READMEs for the 2-minute teach.

**Scope:** the *non-web* half of a Fennec app — data ground truth, business logic, and everything that
reacts to or is scheduled around it. The `web/` layer (Phoenix-style userland interface) is its peer; web
code is a thin caller of this layer.

---

## 0. Why this exists

The web layer was the easy half. The non-web side is where every framework swamps: DDD rots into a
boundary-layer lasagna; Rails oscillates between fat-model god-objects and `app/services/` sprawl; Phoenix
contexts spent 8 years on "where does this go?"; Meteor had great isomorphic collections wrapped in
magic-folder security; Wasp/EdgeDB built the right idea on the wrong vehicle (a new language; a
replace-your-database platform) and paid for it ($5M post-mortem; a shutdown).

Nearly every one of those failures traces to **DSL/codegen tax** or **discipline-reliance** — and Fennec's
substrate dissolves both: business code is *ordinary OCaml over ambient capabilities*, the dangerous things
are *unrepresentable* (compile errors), and the heavy lifting (transactions, exactly-once, scheduling,
cycle-checking) is **transparent**, the way Eio makes IO read synchronous while a handler does the work.

The non-negotiables, from those failures and the project's own principles: **one cohesive concept per file;
no architecture-driven file sprawl; no reliance on developer discipline (mechanically enforced); declarative
via ppx, never a new DSL; the web layer is one thin interface; composition is ignorant of folder structure;
and nothing an OCaml beginner couldn't read.**

---

## 1. The model in one breath

> **Collections** are the data ground truth (declarative, one concept per file). **Workflows** are business
> processes — *ordinary OCaml functions over ambient data and effects*, multiple to a module, sized by the
> dev. **Reactions & schedules** are *annotations* on workflow functions — `[@after fn]` / `[@before fn]`
> and `[@cron …]` / `[@every …]` — the only lifecycle hooks, each a **real typed reference** to its target
> (so go-to-references *is* the graph view), wired and **circuit-checked** by the ppx. There is *no*
> data-change hook: Fennec owns its database and a collection's only writes are named transitions, so every
> change goes through an explicit function and `@after` catches it.
>
> There is **no `db`, no dependency injection, no `Flow` object, no monad, no binding operators, and no
> control-flow vocabulary**. Data and mail *just exist* (Meteor-style ambient). Control flow is `raise`.
> The framework runs each workflow invocation in **one transparent transaction** (read-your-writes; commit
> on return; roll back on `raise`) and delivers fire-and-forget effects **exactly-once post-commit**. A
> cyclic reaction graph is a **compile error**. Scheduled jobs run **at-most-once across replicas**.

Everything below is detail.

---

## 2. Collections — the data ground truth

One persisted data concept, fully contained in one file — a Sift `[@@deriving collection]` model (we call
it *Collection*, not "entity," to stay familiar). It owns:

- **schema + validity-as-types** — illegal states unrepresentable (`status : [Pending|Paid|Cancelled]`,
  `lines : line list [@min_len 1]`). Validity lives in the types and Sift refinements, not in code.
- **single-aggregate transitions are the write API** — a collection exposes `create`, `delete`, and named
  transitions (`Order.pay` refuses a non-pending order); it does **not** expose raw field mutation to
  userland. So a meaningful state change can *only* happen through a named function — which is exactly where a
  reaction hooks (`[@after Order.pay]`), and why `@after` catches every payment **by construction** (no
  data-change hook needed). Events are a property of the *transition*, not something a workflow emits by hand.
  The moment a rule spans two collections it is a Workflow — so a Collection can never grow into a god-module.
- **declarative policy** — a pure total `(row, actor) -> bool` (deny-by-default; an op with no policy is a
  compile error; `~as_admin` bypass; inputs limited to `(row, actor)`, never cross-collection joins). This
  is the thing the TS sync-engine wave (Zero, Electric) *retreated* from — for codegen-ceiling reasons OCaml
  doesn't have, since the policy is a plain function that dual-compiles to server authority and client
  prediction.

Server-authoritative; the *shape* is isomorphic (Sift compiles both sides). **Server-side reads are
one-shot** (`find` / `find_one` / `count`) — a workflow reads, computes, writes; it never sits subscribed.
**Live data is a *publication*** (the existing `Pulse.publish`: a client subscribes, the browser's Fur
signals update) — a client-facing concern, *not* a core read primitive. Liveness there is opt-in and
**bounded** (the over-reactivity guard); its cross-replica propagation is §8.

---

## 3. Workflows — ordinary functions over ambient data

A workflow is an **ordinary OCaml function**. Data, mail, and external services are **ambient** — they just
exist (this is how Pulse already works: the backend resolves from a fiber-local switch installed at boot; no
`db` is ever threaded). So a workflow reads, writes, branches, sends, and returns, top to bottom:

```ocaml
(* orders.ml — order processes. Ambient data + mail. Nothing threaded. *)
let place cart =
  let lines  = Cart.resolve cart in                 (* raises on bad input *)
  Inventory.reserve lines;                            (* ambient write *)
  let order  = Orders.create ~customer:cart.customer ~lines in  (* ambient write; returns the order *)
  Payments.charge ~idem:(Idem.of order) ~cents:order.total;     (* ambient external call (the cliff, §7) *)
  Orders.mark_paid order;                             (* ambient write; the transition is hookable (§4) *)
  order
```

Four properties make this safe *without* anything in the body:

1. **A module holds as many public process functions as cohere** — `Orders.place`, `Orders.cancel`,
   `Orders.refund` — with sub-steps as private functions. The dev sizes the module by cohesion, exactly as a
   component file may hold a private sub-component. "One thing per file" holds: for workflows the *thing* is
   a process area. (This dissolves the one-tiny-file-per-use-case sprawl: related verbs cluster.) **And a
   single workflow function may be *long*** when the process genuinely has many sequential steps: a linear
   top-to-bottom read is the *simplest* form it can take — the whole story in one place, in order.
   Splitting it into sub-functions purely to shorten it is itself an anti-pattern (the lasagna in
   miniature): it scatters one story across call sites and hides the order you must follow. Factor a step
   out **only** when it is independently reused or a genuinely distinct sub-process — **never for line
   count.**
2. **The framework runs each invocation in one transparent transaction** (§5): ambient writes batch into a
   real transaction with **read-your-writes**, commit on normal return, **roll back on any `raise`**.
3. **Imperative read-then-use just works.** The dominant pattern — *use a write's return value in the next
   calls* (`let order = Orders.create … in Payments.charge ~for:order`) — is plain `let … in`, no special
   semantics. Re-reading your own pending writes works too, because it's a real transaction, not a deferred
   buffer.
4. **Tests are trivial: just call the function.** Pulse already gives a unit test a fresh in-memory minimongo
   ambiently (`:memory:`, keyed per suite) — and *plain functions* (not just registered methods) are
   testable today. No DI, no mocks; assert on ambient data and `Mail.captured ()`:

```ocaml
let%test "place produces a paid order and one receipt email" =
  Catalog.seed [ Catalog.product "widget" ~price:500 ];
  let mail = Mail.captured () in
  let o = place { customer="ada"; items=["widget",2] } in
  o.status = Paid && Mail.count mail = 1
```

**Control flow is `raise`.** No framework vocabulary — `if` / `match` / `raise` of a domain (or stdlib)
exception. The web edge maps exceptions to responses (sensible default: unknown → 500 + log, tx already
rolled back; per-app mappings for the ones you want rendered nicely). The workflow body has no `Flow.*`.

---

## 4. Reactions & schedules — the lifecycle annotations

A **workflow module is a recognized kind** (like components and handlers), and it is the *only* kind allowed
to carry lifecycle annotations. The ppx assembles them, wires them (leaf-first, §6), and **runs the
circuit-check**. There are four — and each names its target as a **real symbol**, which is what makes the
whole graph navigable (see "Discoverability" below).

```ocaml
(* orders.ml — processes + their reactions + schedules, all ordinary functions *)

(* @after fn — a reaction AFTER a function (or a collection transition) commits; receives its result. *)
let[@after place] send_receipt (order : Order.t) =
  Mail.send (Receipt.render order)              (* an EFFECT: post-commit, async, exactly-once via idem *)

let[@after Order.pay] reindex (order : Order.t) =
  Search.index order                            (* a DERIVATION: runs in the transition's transaction, converges *)

(* @before fn — a GUARD before a function, inside its transaction; may veto by raising. Use sparingly. *)
let[@before refund] within_window (order : Order.t) =
  if Clock.age order.paid_at > days 30 then raise Refund.Too_late

(* @cron / @every — a scheduled workflow. The type system FORCES no input: the generated runner calls
   [f ()], so a function that needs args simply won't compile as a cron. *)
let[@cron "*/5 * * * *"] expire_abandoned () =
  Orders.find ~where:Order.(pending_longer_than ~min:15)
  |> List.iter (fun o -> Inventory.release o.lines; Orders.cancel o)
```

**Why `@after` is enough — and there is no data-change hook.** Because **Fennec owns its database and a
collection's only writes are named transitions** (§2), every state change goes through a known function. So
`[@after fn]` (and `[@after Order.pay]` on a *transition* for "whenever paid, by any in-app path") covers
every reaction. There is no `@on_change` / observe-the-stream primitive — and so none of its cost: no
cross-replica change feed, no "which change" predicate, no old/new diffing. The one thing it excludes is a
write made *outside* the app (a legacy system on a shared DB, a manual `mongosh`); that is a rare, explicit
**escape hatch** (an opt-in oplog tap) and the *only* place Meteor's change-stream cost reappears.

**Each reaction is one of two delivery disciplines** (the §8 split applied to the hook) — and which one is
*part of the declaration*, not a guess:
- a **derivation** (maintains a derived collection / search index — a *view* of the data) runs **in the
  triggering transaction**, is idempotent, and **converges** (replay-safe; rebuilds after a restart). It may
  *block* the operation, by necessity.
- an **effect** (email, webhook, charge, analytics — it *escapes* the system) runs **post-commit,
  asynchronously (never blocks the response), exactly-once** via its idem key (the outbox, §7–§8).

**`[@before fn]` is a guard, not a reaction** — it runs *before* the function, *inside* its transaction, and
may **veto by raising** (→ the whole operation rolls back). Because a hidden veto is the spookiest thing a
hook can do (Rails' `before_save`-returns-false), the bias is: **a guard that is part of one operation's
logic should be a visible call at the top of the function body; cross-cutting auth/rate-limit belongs in
edge middleware.** `@before` earns its keep only for a domain pre-condition that genuinely spans many
workflows and must be enforced uniformly. It may read and veto; it must not fire effects (it is pre-commit).

**Ordering.** Multiple hooks on one target are **independent and unordered** — that keeps them additive and
composable. If one genuinely must precede another, declare it (`hook_b [@after hook_a]`) — just another edge
the circuit-check governs.

**`[@cron]` / `[@every]`** is a scheduled workflow; the no-input form is type-forced. The cleanup cron is also
the cliff's compensation (§7).

### Discoverability is the toolchain, not a tool

Callbacks rot when behavior fires *invisibly* (the Rails lesson). We avoid that **not** with a bespoke
"show me the hooks" command, but by making every hook a **real typed reference to its target**:

- **Go-to-references on `Order.pay`** lists the transition *and* every `[@after Order.pay]` / `[@before …]` —
  the "what fires around this" view, through the find-references your editor already has.
- **Rename** the target → its hooks update; **delete** it → its orphaned hooks are **compile errors** (no
  silent orphans, unlike Rails). There are **no stringly event names** to typo.
- The workflow wiring is **materialized as a generated manifest** (route_gen-style: `wiring_gen` scans
  `workflows/` → `wiring.ml`, in the tree — not a hidden runtime registry), force-linking every module so a
  pure `@cron`/`@every` can't silently fail to register — *one openable file*. Find-references (above) gives
  the per-target graph today; a fuller rendered edge-graph overview can build on the same file.
- **Bounded blast radius:** after-hooks are post-commit, isolated, idempotent, async; `@before` can only
  veto in-transaction (no other reach). So an unnoticed hook cannot cause Rails-style damage even before you
  go looking.

Naming (`@after`/`@before`/`@cron`/`@every`) is bikeshed-open; the shapes are fixed.

---

## 5. The transparent transaction context (the one build item)

This is the only genuinely new machinery (atomicity does **not** exist in Pulse today — each write commits
independently). The seam is proven: Pulse already binds a **fiber-local per-invocation context** via
`Eio.Fiber.with_binding` (for seeded IDs), and the backend resolves through one `Dynamic.collection` choke
point. A workflow context hooks the same way:

When the framework invokes a workflow (as a web action, a method, an `[@after]` reaction, or a `[@cron]`
job), it binds a fiber-local **transaction** for that invocation. Ambient calls route into it transparently:

- **ambient DB writes** → applied *within* the transaction (so later reads see them — read-your-writes),
  **committed on normal return, rolled back on `raise`.**
- **fire-and-forget effects** (`Mail.send`, webhooks, analytics — no result the flow uses) → recorded in the
  same transaction (a transactional outbox) and **delivered exactly-once post-commit** via the idem key.
- **synchronous external calls** (`Payments.charge` — returns a result the flow uses) → the framework
  **commits the pending writes before the call** (it knows Payments is external, not a collection), then
  continues (§7).

The dev writes ordinary ambient code; the context *plans* the invocation by intercepting ambient calls — and
that same plan is what gives, for free, deterministic replay, dry-run, and introspection. This is the Convex
insight (the runtime sees all reads/writes) via Eio-style ambient interception rather than an explicit plan.

**Honest backend matrix** (atomicity needs a transactional backend):

| backend | atomic workflows |
|---|---|
| minimongo (dev/test) | yes — working-copy commit/rollback |
| burrow (embedded) | yes — WAL |
| replica-set Mongo (normal prod) | yes — native transactions |
| standalone non-replica Mongo | degrades: writes apply immediately, no auto-rollback → lean on `@cron` cleanup |

The context only opens a transaction when a workflow does 2+ writes; a single-write workflow is already
atomic, so there's no transaction overhead for the trivial case.

---

## 6. The circuit-breaker — no cyclic reactions (compile error)

The reaction graph must be acyclic, and it's enforced at **compile time with zero runtime machinery** — the
spike proved this. The mechanism: reactions are wired as values, leaf-first; the wiring type is **abstract**,
so the only way to build a node is an application. An acyclic graph needs plain `let`s; a *cycle* forces
`let rec node_a = wire […b…] and node_b = wire […a…]`, and a function application is rejected as a `let rec`
right-hand side:

```
Error: This kind of expression is not allowed as right-hand side of let rec
```

So a cyclic `[@after]` graph (a `@after` b while b `@after` a, directly or through any chain) **does not
compile**. The ppx generates this leaf-first wiring from the `[@after]` annotations — so the dev writes
co-located hooks and still gets the free guarantee. (The ppx **must** emit the leaf-first `let` chain, never
a runtime registry, or it throws the guarantee away.) Proven across all three dangerous shapes:
node ⇄ node, reaction → workflow → reaction, derived A ↔ B. This catches the brutal *emergent* cycles
(independent hooks closing a loop); pure direct-call recursion stays ordinary visible `let rec`.

---

## 7. The cliff & compensation

An external call that returns a result the flow uses (charge a card) cannot be inside the DB transaction —
that's the cliff. It needs **no inline ceremony**: the context commits the pending writes *before* the call
(it knows the call is external), the workflow stays **forward-only**, and on failure it just `raise`s — the
already-committed work is left in a recoverable state (e.g. the order stays `Pending`).

**Compensation is a declared reaction, not inline code.** A `[@cron]` cleanup (`expire_abandoned`, §4)
releases/cancels abandoned-Pending state. The forward workflow reads clean; the undo is a policy. Inline
compensation (a `try/with` around the external call) remains available as a rare escape for the few flows
that genuinely need immediate undo — but it is not the default you reach for.

The honest exactly-once seam past the cliff: the effect-intent is recorded in the same transaction (outbox);
a relay delivers it; the idem key makes the handler idempotent — **exactly-once-effect = at-least-once-delivery
+ idempotent-handler** (there is no true exactly-once delivery).

---

## 8. At-most-once across replicas — synced schedules & exactly-once effects

Most hooks are single-run by construction: `@after`/`@before` run **in-process on the replica that ran the
target**, and a derivation (`@after` a transition) runs **inside that transition's transaction** (on whichever
replica did the write, updating the shared derived collection atomically) — so none of them need a claim. Two
things genuinely cross replicas and share one **claim**: **scheduled jobs** (every replica's timer fires the
slot) and the **outbox relay** (every replica could drain the shared queue — also how `@after` *effects* reach
the world exactly-once). A third — **live publications** — needs cross-replica *propagation* but no claim (see
the end of this section).

**The claim = a per-tick unique insert** (Meteor's "synced cron" trick, made transparent by the annotation):

- On each scheduled slot, every replica attempts `insert { job; slot }` into an internal `_fennec_cron`
  collection with a **unique index on `(job, slot)`**. Exactly one wins; the rest get a duplicate-key and
  skip. The job name comes from the ppx (`Module.fun`); `slot` is the scheduled time, **rounded to the
  cron's granularity** so minor clock skew between replicas maps to the same slot.
- **No deadlock** — insert-or-fail is instant and holds no lock; there is nothing to wait on.
- **No orphan** — keyed per *slot*, not a global "running" flag: a winner that crashes mid-run forfeits only
  *that* tick; the next tick is a new slot and a fresh election. (A flag without a slot is exactly what
  orphans forever — the slot key is the fix.)
- **Self-pruning** — a TTL index on `slot` deletes old claims; the collection never grows.
- **At-most-once** by default (a crash skips a tick; the next recovers — right for cleanup/digests).
  At-least-once is a lease variant (claim-with-expiry + idempotent job) — the documented upgrade, not the
  default, because a lease reintroduces the orphan/double-run questions.
- **Engages only with a shared DB** (real Mongo). Embedded burrow / in-memory is a single process → no
  cluster → the job runs directly. Transparent either way; the dev only writes `[@cron]`.

**One primitive, two uses:** a cron claims `(job, slot)`; the outbox relay claims `(effect-id)` before
delivering. Synced schedules and exactly-once effects ride the same unique-insert claim — `@after`/`@before`
and in-transaction derivations need none of it.

### Live publications across replicas — propagation, not a claim

A browser's subscription lives on one replica; a write that affects it may happen on another. Because every
write is app-originated (§2), **the write path knows the precise delta and emits it post-commit as a change
event** — the *same* outbox/effect mechanism — which subscriber-replicas relay to their clients. So it is a
*propagation* tier, not a claim, and it is **precise, targeted fanout — not oplog tailing**:

- **single server** (burrow-embedded / one node): in-process — **free**.
- **multi-replica with live subscriptions**: the change events go over a **pub/sub bus** (Redis/NATS).
- the *only* thing that would force blind oplog tailing is reacting to **external** writes (the §4 escape
  hatch) — quarantined, opt-in, the one place Meteor's cost reappears.

The real defense is keeping the live surface small: most reads are one-shot `find`; liveness is opt-in and
bounded; server reactions are in-tx derivations. Only genuinely-live client data pays.

---

## 9. Typing & safety — what is mechanically prevented

The point is that the dangerous things are *unrepresentable*, not discouraged:

- **No half-commit / no mid-transaction effect** — ambient writes are one transaction; an aborting `raise`
  *is* the rollback; external effects defer to post-commit (§5).
- **No silent failure** — exceptions *propagate by default* (you can't `ignore` them the way you can a
  `result`). The only swallow risk is a catch-all `with _ -> ()`; a lint bans bare `with _` in workflow
  code. (Exceptions are *stronger* than `result`-threading on the swallow axis.)
- **No cyclic reactions** — §6, `let rec` over the abstract wiring is rejected.
- **No exactly-once footgun** — an effect requires a derived `~idem` key (a random uuid won't construct one);
  delivery is at-least-once + idempotent (§7–§8).
- **No ill-formed cron** — `[@cron]` forces a no-input function via the generated `f ()` (§4).
- **No forgotten authorization** — a Collection op with no policy is a compile error (§2).
- **No unbounded live publication** — a publication declares its bound (§2).

**Refuse (ceremony that rots — the Gel/Wasp "don't boil the ocean" trap):** a plan/saga *monad* or binding
operators in userland; rank-2 phantom regions to make the cliff airtight (Eio/Mirage/Jane Street all stop at
structural confinement); GADT/multi-axis typestate for persisted workflows; effect handlers or free monads
as a *userland* surface; a tagless "capability typeclass that lists effects"; session types; full IVM; a new
control-flow vocabulary. Each was considered and rejected for adding ceremony without buying a guarantee the
simpler design lacks.

**The honest ceiling (where OCaml can't mechanize — keep the cheap substitute):** no linear types, so types
can't stop *double-spend* — keep the idem key. No data-race freedom, so the reactive store is confined to one
domain (Eio fibers are cooperative; the commit critical section is suspension-free) rather than type-checked.
No typed effects, so the cliff is *structural* (external services aren't collections) + a lint, not a type.
These are exactly where Eio, MirageOS, and Jane Street draw the line too.

---

## 10. How the web layer uses it

`web/` is a thin caller — a page/handler invokes a workflow and renders the result; the framework establishes
the workflow's transaction context and maps any `raise`d exception to a response:

```ocaml
let submit conn =
  let o = Orders.place (Checkout_form.read conn) in   (* ambient; framework wraps it transactionally *)
  Conn.redirect conn (Paths.order o.id)
  (* a raised domain exception → the edge maps it (4xx + message); unknown → 500 + log, tx rolled back *)
```

When web logic itself becomes nontrivial it gets its own one-concept-per-file unit *in `web/`* — logic lives
where its concept lives; folders mark the kind/realm, never the call graph.

---

## 11. Settled vs. open

**Settled:** the three kinds; workflows as ordinary ambient functions (no `db`/DI/`Flow`/monad); multiple
public functions per module, dev-sized; `raise` as the only control flow; **the app owns its database and a
collection's writes are named transitions** — so every change goes through a known function and there is **no
data-change hook**; the lifecycle annotations `[@after]` / `[@before]` / `[@cron]` / `[@every]`, each a **real
typed reference** so the standard toolchain (go-to-references, rename, the compiler) *is* the graph view — no
bespoke command; the per-hook delivery discipline (derivation = `@after` a transition, in-tx + converge;
effect = post-commit + async + exactly-once); hooks on a target are independent/unordered unless an explicit
edge is declared; the circuit-breaker (compile error on cyclic reactions, holding across before+after as one
happens-before graph); read-your-writes within the transparent transaction; **at-most-once schedules +
exactly-once effects via the one unique-insert claim, and live publications via write-emitted post-commit
change-events** (in-process single-server; a pub/sub bus multi-replica) — no oplog tailing; tests by just
calling the function against ambient in-memory data.

**Built (was open, now shipped):**
- **The transparent transaction context — on ALL THREE backends.** The fiber-local + `Dynamic.collection`
  seam holds an optional per-backend native transaction handle, opened lazily on the first write and
  finalized on commit/rollback: **in-memory** (snapshot/restore), **burrow** (one held LMDB parent txn via
  `begin_txn`/`commit_txn`/`abort_txn`; observers revealed at commit), and **real replica-set Mongo** (a
  libmongoc client-session transaction — `session_start`, session-appended `*_s` writes + find/aggregate
  reads + a `command_s` for multi-doc update/delete/count/distinct, `session_commit`/`abort`). Read-your-writes
  is complete — every read (find/count/distinct/aggregate) sees the workflow's pending writes. Commit-on-return,
  rollback-on-raise, read-your-writes, nested-flatten, serialized — the no-transaction path stays
  byte-identical on every backend. Proven end-to-end: the engine `test_txn` + dynamic `test_tx_burrow`
  (burrow) and `cli/test/test_mongo_txn` against a managed single-node replica set (Mongo) — each covers
  commit, rollback of insert/update/remove, and read-your-writes.
- **Folder layout** — settled: top-level `collections/` and `workflows/`, flat peers of `web/`. Built in
  the example; server-only, `(include_subdirs unqualified)`, no client mirror.
- **`@before` reach** — built as an in-transaction guard that may `raise` to veto; use it for
  cross-cutting domain pre-conditions, a visible call at the top of the body for the rest.
- **The wiring manifest (force-link).** A generated `wiring.ml` (route_gen-style: `wiring_gen` scans
  `workflows/` and references every module) so a pure `@cron`/`@every` that nothing else calls still links
  and registers at boot. The server force-links them all with ONE `Wiring.link ()` — no naming each by
  hand, no discipline to forget. Proven by the example's `maintenance.ml` (an unreferenced hourly sweep
  that registers anyway) + `test_wiring`.

**Open — deferred follow-ons:**
- **The external-write escape hatch** (an opt-in oplog tap, for a non-Fennec writer on a shared DB) — the one
  feature that reintroduces change-stream cost; keep it quarantined and rare.
- **The multi-replica pub/sub bus** for live publications (Redis/NATS) — only when a second replica serves
  live subscriptions; single-server needs none.
- **`fennec new` scaffolding** of `collections/`/`workflows/` — the convention is taught by the example +
  READMEs; the minimal scaffold stays a hello-world for now.
- **The manifest's rendered reaction-graph** — the force-link manifest ships (above); rendering the full
  `@after`/`@cron` edge graph INTO that openable file (beyond the module list) is the remaining nicety.
  Find-references already gives it per-target.
- **At-least-once schedules** (the lease variant) — deferred until a job genuinely needs it.
- **Annotation names** (`@after`/`@before`/`@cron`/`@every`) — bikeshed.

**Sequencing (Gel's gravestone):** *do not boil the ocean.* Build the core — ambient workflows + the
transparent transaction + `@after`/`@cron` + the claim — first; keep layers independently replaceable; stay
a **library the user owns** (escape hatch = stop using the ppx, keep your OCaml), never a lock-in platform.

---

## Appendix A — the spike

A throwaway compiled spike (plain OCaml, no ppx, no framework) validated the circuit-breaker — the abstract,
leaf-first-wired node type whose cyclic wiring is rejected as a `let rec` right-hand side — across all three
dangerous shapes (node ⇄ node, reaction → workflow → reaction, derived A ↔ B). That wiring is the mechanism
the ppx generates from `[@after]` annotations; the guarantee is identical whether hand-written or generated.
The transparent transaction context and the synced-claim were validated by reading the live Pulse runtime
(ambient fiber-local backend resolution; minimongo per-suite isolation; unique indexes on Mongo + burrow),
not yet built.
