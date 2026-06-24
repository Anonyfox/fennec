# The non-web core — Collections, Workflows, Reactions

**Status:** design RFC. Not implemented. The circuit-breaker was validated by a throwaway compiled spike;
the rest is grounded in the *actual* ambient Pulse runtime (the data layer resolves its backend from a
fiber-local Eio switch installed at `Fennec.serve` boot; minimongo gives tests a fresh in-memory DB; mail
is ambient). This is the reference the implementation builds against.

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
> dev. **Reactions & schedules** are *annotations* on workflow functions — `[@after fn]`, `[@cron …]`,
> `[@every …]` — the only lifecycle hooks, wired and **circuit-checked** by the ppx.
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
- **single-aggregate transitions** — operations whose invariant is about *this one* aggregate
  (`Order.pay` refuses a non-pending order). A transition that represents a meaningful state change is also
  where a reaction hooks (`[@after Order.pay]`), so events are a property of the *transition*, not something
  a workflow emits by hand. The moment a rule spans two collections it is a Workflow — so a Collection can
  never grow into a god-module.
- **declarative policy** — a pure total `(row, actor) -> bool` (deny-by-default; an op with no policy is a
  compile error; `~as_admin` bypass; inputs limited to `(row, actor)`, never cross-collection joins). This
  is the thing the TS sync-engine wave (Zero, Electric) *retreated* from — for codegen-ceiling reasons OCaml
  doesn't have, since the policy is a plain function that dual-compiles to server authority and client
  prediction.

Server-authoritative; the *shape* is isomorphic (Sift compiles both sides). Reads are **live-or-not by
type**: `find` is the one-shot default; `watch : … -> 'a Live.t` opts into reactivity and *requires* a
bound (an unbounded live query won't typecheck) — the over-reactivity guard, felt at the call site.

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
   a process area. (This dissolves the one-tiny-file-per-use-case sprawl: related verbs cluster.)
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
to carry lifecycle annotations. The ppx assembles them, wires them, and **runs the circuit-check** (§6).
There are exactly three:

```ocaml
(* still orders.ml — processes + their reactions + their schedules, all ordinary functions *)

(* @after fn — a reaction: runs AFTER that function commits, typed to its result. Co-located. *)
let[@after place] send_receipt (order : Order.t) =
  Mail.send (Receipt.render order)

(* hook a transition, not a process, to catch "whenever paid, by any path" *)
let[@after Order.pay] award_loyalty (order : Order.t) =
  Loyalty.add order.customer ~points:(order.total / 100)

(* @cron / @every — a scheduled workflow. The function MUST take no input, enforced by the type system:
   the generated runner calls [f ()], so a function that needs args simply won't compile as a cron. *)
let[@cron "*/5 * * * *"] expire_abandoned () =
  Orders.find ~where:Order.(pending_longer_than ~min:15)
  |> List.iter (fun o -> Inventory.release o.lines; Orders.cancel o)
```

- **`[@after fn]`** runs after the named function *commits*, receiving its *result* (typed: `send_receipt`'s
  `Order.t` must match `place`'s return, or it won't compile; args are also available if needed). Hook a
  *process* or a *transition*. Reactions run **post-commit** (a rolled-back workflow fires none) and each in
  its own transaction context, so they compose and stay atomic.
- **`[@cron expr]` / `[@every dur]`** is a scheduled workflow. The type system *forces* the no-input form —
  ill-shaped cron jobs don't compile. Same workflow machinery; just a timer trigger.
- **The cleanup cron is the cliff's compensation** (§7): `expire_abandoned` handles "charge failed → order
  left Pending" with no inline `try/with` — the forward workflow stays clean; the undo is a declared rule.

Naming (`@after`/`@cron`/`@every`) is bikeshed-open; the shape is fixed.

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

`@after` reactions run **in-process on the replica that ran the function**, so they are single-run by
construction. Two things *do* cross replicas and need a claim: **scheduled jobs** (every replica's scheduler
fires) and the **outbox relay** (every replica could drain it). One mechanism handles both.

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

The **outbox relay uses the same claim** (claim an effect row before delivering), so cron-dedup and
exactly-once-effects share one internal primitive.

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
- **No unbounded live query** — `watch` requires a bound (§2).

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
public functions per module, dev-sized; `raise` as the only control flow; `[@after]` / `[@cron]` / `[@every]`
as the only lifecycle annotations; the circuit-breaker (compile error on cyclic reactions); read-your-writes
within the transparent transaction; exactly-once effects and at-most-once schedules via the unique-insert
claim; tests by just calling the function against ambient in-memory data.

**Open — deliberately not yet decided:**
- **The transparent transaction context is the one build item** — doesn't exist today; the fiber-local +
  `Dynamic.collection` seam is proven; each backend needs its batch-commit. (This is the make-or-break to
  build first.)
- **Folder layout** — a horizontal `web/` + a non-web peer (`data/`? `core/`?) vs vertical feature slices.
  Leaning horizontal peer, domain-organized inside; `web/` is already horizontal. Concepts settled; the
  top-level shape is not.
- **`@after` passes the result by default** (args available if a reaction needs them) — confirm.
- **Annotation names** (`@after`/`@cron`/`@every`) — bikeshed.
- **At-least-once schedules** (the lease variant) — deferred until a job genuinely needs it.

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
