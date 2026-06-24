# The non-web core — Collections, Workflows, Reactions

**Status:** design RFC. Not implemented. The shapes below were validated by a throwaway compiled
spike (plain OCaml, no ppx, no framework deps); the essential code + compiler output is in Appendix A.
This is the reference the eventual implementation builds against.

**Scope:** how the *non-web* half of a Fennec app is structured — the data ground truth, the business
logic, and everything that reacts to data (publications, derived views, analytics, effects). The `web/`
layer (Phoenix-style userland interface; see the web-layer rename) is already carved; this is its peer.

---

## 0. Why this exists

The web layer was the easy half — Phoenix proved web-vs-non-web is a durable split, and Fennec's
`web/` is exactly that. The hard half is the *non-web* side, and it is where most frameworks swamp:

- **DDD / Clean / Hexagonal** rot into a lasagna of domain-boundary layers — one use-case smeared
  across 5–11 files, "where's the actual logic," shotgun surgery on every change.
- **Rails** oscillates forever between fat-model god-objects and `app/services/` one-method-command
  sprawl; the service object is "an anemic domain model," and it enforces nothing, so it decays.
- **Phoenix contexts** produced 8 years of "where does this function go?" and an unsolved
  "who owns the shared schema" question.
- **Meteor** had isomorphic collections + methods (good) wrapped in magic load-order folders whose
  *names* were the security model (`server/` code that "still ships to the client" if misplaced).
- **Wasp / EdgeDB-Gel** built the *right* idea (one declarative place per concept) on the *wrong*
  vehicle (a new language; an all-or-nothing replace-your-database platform) and paid for it — Wasp's
  own $5M post-mortem ("a new language was the mistake"); Gel shutting down ("boiling the ocean").

The non-negotiables this design holds to, derived from those failures (and from the project's own
principles): **one concept per file; no architecture-driven file sprawl; no reliance on developer
discipline (it must be mechanically enforced); declarative via ppx, never a new DSL; the web layer is
one thin interface to a transport-agnostic core; composition is ignorant of folder structure.**

The deepest reason Fennec can attempt the co-located, single-file-per-concept model that others
abandoned: nearly every abandonment traces to **DSL/codegen tax** or **discipline-reliance**, and
Fennec's substrate dissolves both — a rule is a *plain total OCaml function* that dual-compiles to
native authority and jsoo prediction for free, and the realm boundary is *mechanical* (the dual-compile
strip + js_gate), not a folder convention. (Full research: the `nonweb-layer-research` memo.)

---

## 1. The model in one breath

> **Three kinds of file. One wiring law. One cliff.**
>
> **Collections** = data ground truth (declarative). **Workflows** = business processes (plain
> functions). **Reactions** = things that fire off a change (one typed-`sink` model, two kinds).
> Composition is 100% by module reference — *ignorant of folders*. The only mechanically-enforced
> lines are the realm boundary (server vs client, via dual-compile) and the public `.mli` wall. A
> circular reaction graph is **a compile error** (unconstructable, not detected). The atomic↔external
> boundary is a **visible, typed seam**.

That is the whole architecture. Everything below is detail.

---

## 2. Collections — the data ground truth

A **Collection** is one persisted data concept, fully contained in one file. (We call it *Collection*,
not "entity," to stay familiar — it is a Sift `[@@deriving collection]` model.) It owns:

- **schema + validity-as-types** — illegal states unrepresentable (`status : [Pending|Paid|Cancelled]`,
  `lines : line list [@min_len 1]`). Validity is in the *types and Sift refinements*, not in code.
- **single-aggregate transitions** — operations whose invariant is about *this one* aggregate live
  here (`Order.cancel` refuses a paid order, enforced by the match). The instant a rule spans two
  collections it is a Workflow, so a Collection can never grow into a god-module.
- **declarative policy** — `(row, actor) -> bool` (deny-by-default; an op with no policy is a compile
  error; `~as_admin` bypass; inputs limited to `(row, token)`, never cross-collection joins). This is
  the one thing the TS sync-engine wave (Zero, Electric) *retreated* from — for reasons (codegen
  ceilings, portability hacks) OCaml dissolves: the policy is just a pure total function that
  dual-compiles to server authority and client prediction.
- **the events it announces** — payload-typed (`Order.placed : Order.t event`).
- **its read-surface** — publications are per-collection, so they co-locate here.

Server-authoritative; the *shape* is isomorphic (Sift compiles both sides), the *write authority* is
server-side (stripped from the client like `web/`'s `Data.local` fetchers already are).

**The line that matters:** static validity of *one* thing → Collection (declarative). A *process* over
*things* → Workflow (imperative). "Make illegal states unrepresentable" is a Collection concern;
"place an order" is a Workflow.

---

## 3. Workflows — business processes

A **Workflow** is one business process, a *plain function named by the verb* (`place_order`, not
`OrderService`). It reads top-to-bottom as a recipe. Three rules:

1. **Cross-collection invariants are synchronous calls inside one transaction.** The workflow *is* the
   consistency boundary (Vernon's "user-aggregate affinity" sanctions one atomic transaction across
   aggregates for a single user action). If a step fails, the transaction aborts — nothing partial, no
   saga, no compensation. **This is what dissolves the invariant snowball:** invariants are a property
   of a transaction, not a pyramid of layers. Complexity only returns at the external cliff (§6).
2. **Intermediate, memory-only types live in the workflow that uses them** — they are not Collections.
3. **Downstream reactions arrive as explicit arguments** (typed sinks). The workflow depends on the
   *shape* (`Order.t sink`), never on which reactions exist, and **cannot emit mid-transaction** (the
   `Tx.run` closure is handed no sink) — reactions only run on the committed result.

```ocaml
let place_order ~(on_placed : Order.t Flow.sink) ~customer ~lines : (Order.t, error) result =
  let outcome =
    Flow.Tx.run (fun tx ->
        let* () = Inventory.reserve tx ~lines in   (* cross-collection invariant, atomic *)
        let* order = Order.insert tx ~customer ~lines in
        Ok order)
  in
  match outcome with
  | Ok order -> Flow.emit on_placed order; Ok order   (* emit ONLY post-commit *)
  | Error _ as e -> e
```

---

## 4. Reactions — one sink model, two kinds

Everything that fires off a data change is a **Reaction**, and there is *one* mechanism — a typed
`sink` — with two kinds, distinguished only by **trigger source**:

| kind | trigger | examples | delivery discipline |
|---|---|---|---|
| **side-effect** | a domain event (a workflow `emit`s) | email, "track order placed", outbound webhook | **fire-once**, post-commit, outbox-backed |
| **read-projection** | a collection's **change stream** (every write) | publication, derived collection, analytics rollup | **converge** to `f(source)`; idempotent, replay-safe |

```ocaml
(* side-effect: a handler of a domain event *)
let email_confirmation : Order.t -> unit = fun o -> Mail.send (Email.confirmation o)

(* read-projection: a consumer of the change stream; sink = client | another collection | engine *)
let publish_orders   : Order.t Flow.change -> unit = function Added o -> stream_to_client o | _ -> ()
let derive_revenue ~revenue_changes : Order.t Flow.change -> unit = function
  | Added o -> Revenue.bump ~changes:revenue_changes ~customer:o.customer ~add:o.cents | _ -> ()
```

**This is the answer to "publications are per collection, but where do analytics calls go?":** a
publication and a derived/analytics *view* are the same thing — a read-projection on the change stream,
co-located with the collection. "Call analytics *when an order is placed*" is the *other* thing — a
side-effect on a domain event. Same `sink` discipline underneath; the split is read-projection vs
side-effect, and that split *is* the rule for where it lives.

**The one nuance not to paper over:** the two kinds share *wiring and typing*, but **not delivery**.
A read-projection must converge (idempotent, replay-safe — materialized-view semantics); a side-effect
must fire exactly once (don't email twice on replay). So each reaction is tagged `derivation` vs
`effect`. Unifying them at the *type* level is the elegance; conflating them at the *runtime* level
reintroduces "analytics fired twice" / "view drifted." (Formalizing this tag is an open item, §9.)

---

## 5. The wiring law — circular dependencies are *unconstructable*

This is the load-bearing innovation, and it needs **zero framework machinery** — it falls out of the
OCaml value-recursion checker.

`type 'a sink` is **abstract**; the only way to build one is `Flow.sink : ('a -> unit) list -> 'a sink`
(a function application). Reactions are wired at a composition root by passing sinks as explicit
dependencies, leaf-first:

```ocaml
let low_sink    : Inventory.low Flow.sink = Flow.sink [ audit_low ]                       (* leaf *)
let placed_sink : Order.t Flow.sink       = Flow.sink [ email; restock ~on_low:low_sink ] (* uses it *)
```

Acyclic wiring needs **no recursion** — plain leaf-first `let`s. A *cycle* forces a forward reference,
i.e. `let rec a = sink […b…] and b = sink […a…]`. And `sink (…)` is a function application, which OCaml
**refuses** as the right-hand side of `let rec`:

```
Error: This kind of expression is not allowed as right-hand side of let rec
```

So **you only ever reach for `let rec` when you have a real cycle, and that is exactly the case the
compiler rejects.** No event bus, no registry, no runtime cycle detector. Circular reaction graphs are
*unconstructable*. Proven in the spike across all three dangerous shapes: `sink ⇄ sink`,
`reaction → workflow → reaction`, and `derived collection A ↔ B`.

**Precise scope (be honest):** this catches **hidden, emergent cycles** — the brutal-at-runtime kind
that arise from independent subscriptions, *because such a cycle must route through ≥1 sink to be
hidden*. It does **not** catch a loop closing purely through direct function calls with no sink in it —
but that is ordinary `let rec` recursion you can see and reason about locally, terminating on data like
any recursion. The boundary is exactly right: emergent reaction cycles are impossible; visible
call-recursion stays a normal tool.

Why abstract `sink` is essential: if `sink` were a concrete `'a -> unit`, a determined author could
write the cycle as raw mutually-recursive *functions* (`let rec f x = … and g y = …`, which OCaml
allows). Abstractness forces every sink through the `Flow.sink` application, so the cycle always lands
on the rejected `let rec`-over-application form.

---

## 6. The cliff — the atomic↔external boundary, made visible and exactly-once

Internal work is atomic (one Mongo transaction). An external call (charge a card, hit a vendor) **cannot**
be in that transaction. That boundary — the cliff — is where atomicity ends and most frameworks get
vague. Two type-level forcing functions keep it honest:

1. **DB writes need `tx`** (available only inside `Tx.run`). External effects need an **idempotency key
   in their type** (`charge : idem:string -> …`) — you cannot fire an exactly-once effect without
   declaring its key.
2. The cliff is the **visible seam between two `Tx.run` blocks.** For a single external call, the
   workflow stays a *plain function* — atomic phase → external(idem) → settle | compensate — with
   compensation as the ordinary error path. No step-DAG, no saga machinery:

```ocaml
let place_and_charge ~sku ~qty ~amount : (Order.t, error) result =
  let* order = Flow.Tx.run (fun tx ->
      let* () = Inventory.reserve tx ~sku ~qty in
      Order.insert tx ~sku ~qty ~amount) in
  (* ── CLIFF: order committed; now the external, exactly-once charge ── *)
  match charge ~idem:(Printf.sprintf "order-%d" order.id) ~amount with
  | Ok () -> Flow.Tx.run (fun tx -> Order.mark_paid tx order)               (* settle, atomic *)
  | Error e ->
    let* _ = Flow.Tx.run (fun tx ->                                          (* compensate, atomic *)
        let* () = Inventory.release tx ~sku ~qty in Order.cancel tx order) in
    Error (`Charge_failed e)
```

The heavy **declarative step-DAG only earns its keep at 3+ external steps** with ordering/compensation
constraints — deferred until a real case demands it. The durable version of exactly-once: record the
effect-intent in the *same* transaction (transactional outbox) and a worker performs it by idem key —
runtime infra, not a composition concern; the idem-key is already the seam.

---

## 7. The typing guarantees (what is a compile error)

The point of the whole design is that the dangerous things are *unrepresentable*, not *discouraged*:

- **No event-dispatch holes.** Payload type flows `event → sink → handler`; there is no string lookup at
  dispatch, so a typo'd name (silent drop) and a drifted payload (runtime cast) cannot be written.
- **No circular reaction graphs.** §5 — `let rec` over `sink` applications is rejected.
- **No exactly-once footgun.** An external effect requires an `~idem` key in its type.
- **No mid-transaction effects.** The `Tx.run` closure is handed no sink; reactions are post-commit.
- **No forgotten authorization.** A Collection op with no policy is a compile error (deny-by-default).
- **No DB write outside a transaction** (writes require `tx`).

---

## 8. Mechanical safety — out-of-the-box footgun elimination

§5–§7 give the structural guarantees. This section is the broader toolkit: how a *long, linear* workflow
(length is fine — Go-style; splitting for length is itself an anti-pattern) is made **unable to silently
break**, mechanically, with no new ceremony. The wins come from abstract types + capability-passing + a
plan value + a handful of lints + Eio's structured concurrency — all mainline OCaml 5. Validated against a
research round (compensation, atomicity, typed effects, error handling, typestate, exactly-once, reactive cost).

**The one tweak that subsumes the most — workflows *describe*, the framework *performs*.** A workflow body
is `snapshot -> plan`: it may only *read* the snapshot and *return* a typed `plan` (writes + scheduled
effects); the framework owns `commit`. Because `plan` is opaque and no IO capability is in scope,
**half-commit, mid-transaction effect, and accidental multi-transaction splitting become type errors** —
Datomic/Convex's "a mutation is a transaction," enforced here at *compile time* by the body's only output
being data (Convex needs a runtime sandbox for the same property). The body still reads long-and-linear; it
appends to a plan instead of performing. Limit: no auto-retry of an arbitrary closure (no determinism
enforcement without a sandbox) — retry the pure plan-production only, never effect dispatch.

### Adopt (ranked by leverage ÷ ceremony)

| footgun killed | mechanism | cost |
|---|---|---|
| half-commit / mid-tx effect / multi-tx split | **Plan model** (`snapshot -> plan`, opaque, framework commits) | structural; the spine |
| **forgot to compensate** past the cliff | **`step ~forward ~undo`** — can't obtain the forward result without its undo; undos auto-run LIFO on failure, discarded on success; back with Eio `Switch.on_release_cancellable` | ~1 labelled arg per step |
| **effect fired twice** | **abstract `Idem.t` derived from the triggering change** (a random uuid won't construct one) + transactional outbox + unique-index dedup (free on Mongo & Burrow) | low |
| **wrong delivery discipline** | **`Effect` vs `Derivation` as two types**, different verbs; `Effect.enqueue` requires an `Idem.t` and has no `perform_now`; a `Derivation` materializer is pure by **capability-withholding** (no clock/RNG/IO/tx in scope) | low; resolves the §10 delivery-tag |
| **derived view drifted** | pure materializer + free `rebuild` / `scrub` / `reconcile` (recompute-from-source diff) | low (the 80/20; not full IVM) |
| **illegal step order** (charge-before-reserve) | **parse-don't-validate stage types** — `Unvalidated→Validated→Priced→Paid`; `ship` demands a `Paid` only payment mints; `let*`-chained | low; composes with Sift |
| external call inside a tx (**the cliff**) | **capability-passing** — external effects are values passed in, never ambient; not threaded into the plan body + a lint flagging `Http`/`Unix` inside it | near-free |
| **silent error swallow** | engineered must-use: `result`-returning steps (drop one → warnings 10/26, free); **ban polymorphic `ignore` / bare `let _ =`** in workflow code; `-warn-error +5+10+26` across all profiles; closed error ADT default, polyvar rows annotated-closed at the `.mli`; ban error catch-alls + lossy `map_error` | lints + dune flags + a convention |
| **over-reactivity** (the 9 TB/day class) | **live-is-a-type**: `find` one-shot (the default) vs `watch : … -> 'a Live.t`; `watch` *requires* a `~limit`/`~window` (unbounded-live unrepresentable); no-op writes emit no invalidation (ppx-derived equality); per-query cost a first-class value + build lint | low |
| **data races / leaked subscriptions** | single-domain store + **suspension-free commit** (Eio: no lock needed if single-domain + no fiber switch → the write-then-fire cycle is atomic for free); subscriptions tied to the connection `Switch` (auto-teardown) | architectural rule |

The last four rows are purely **additive** (abstract types, lints, Eio) — no ceremony. The first three are
structural refinements to §3–§6.

### Refuse (ceremony that rots — avoiding footguns is not the same as piling on type theory)

Every research thread independently flagged these as the Gel/Wasp "don't boil the ocean" trap:
- **Rank-2 phantom regions** to make the cliff airtight — Eio, MirageOS, and Jane Street all deliberately
  stop at structural confinement + a lint; the airtight version needs `'a.` annotations everywhere and
  rank-2-only-in-records. ~95% of the safety at ~5% of the cost; the residual is a *malicious-insider*
  threat, not the model.
- **GADT / multi-axis typestate** for persisted workflows — breaks `[@@deriving model]`, opaque errors,
  cartesian impl explosion. Parse-don't-validate gives ~80% of the benefit and serializes fine.
- **OCaml 5 effect handlers / free monads as the *enforcement* layer** — OCaml effects are *untyped* (a
  wrong effect is a runtime error, not a compile one). Use the opaque plan + capability instead.
- **Tagless-final "capability typeclass that lists effects"** — De Goes' "unconstrain rather than
  constrain"; it is theater (the ambient `Http.post` sits one line below).
- **Session types, full IVM / differential dataflow, stringly `(_, string) result`, designing around
  OxCaml modes** — academic, boil-the-ocean, or not-mainline.

### The honest ceiling (where OCaml can't mechanize — keep the cheap substitute)

- No **linear types** → types can't stop *double-spend* (a stage value reused twice). **Keep the
  idempotency key**; don't let stage types fool you into dropping it.
- No **data-race freedom** in the type system → confine the store to one domain, don't pretend threads are checked.
- No **typed effects** → *withhold* the clock/IO capability (so it's unreachable) rather than *tracking*
  effects in types; a lint covers the residual ambient hole.

These are exactly where Eio, MirageOS, and Jane Street stop too — Fennec draws the line in good company.

---

## 9. How the web layer uses it

`web/` stays a thin interface: a page/handler **calls a workflow** and renders the result; nothing more.
When web logic itself becomes nontrivial (a multi-step wizard's server coordination), it becomes its
own one-concept-per-file thing **in `web/`** — because its concept *is* web-interaction. The rule is
uniform: **logic lives where its concept lives; folders mark the kind/realm, never the call graph.**

---

## 10. Settled vs. open

**Settled (validated by the spike + the mechanical-safety round; aligned with the principles):**
- The three kinds (Collections / Workflows / Reactions) and folder-ignorant, by-reference composition.
- The wiring law (circular = compile error) and its scope.
- The cliff shape (plain function + compensation; idem-keyed effects; step-DAG deferred).
- The typing guarantees of §7 and the mechanical-safety toolkit of §8.
- Reactions = one sink model, two kinds (side-effect vs read-projection).

**Resolved by §8 (were open):**
- **The `derivation` vs `effect` delivery tag** → two distinct types — `Effect` (requires an `Idem.t`,
  outbox-delivered, fire-once) vs `Derivation` (a pure, capability-withheld materializer with free
  `rebuild`/`scrub`/`reconcile`). The dev cannot pick the wrong discipline.
- **Cliff hardening** → capability-passing (external effects are values, never ambient) + a lint flagging
  `Http`/`Unix` inside the tx body; rank-2 phantom regions deliberately *rejected* as ceremony-that-rots
  (the residual is a malicious-insider threat, not the design's model).

**Still open — deliberately not yet decided (see `nonweb-layer-research` for the evidence on each):**
- **Folder layout.** A horizontal `web/` + a non-web peer (`data/`? `core/`?) vs vertical feature
  slices. Leaning: a horizontal peer, *domain-organized inside* (the least-premature commitment for a
  tangled, client-replicated domain) — but `web/` is already horizontal, and reversing it into feature
  slices is on the table. **Concepts are settled; the folder name and top-level shape are not.**
- **Policy home.** On the Collection (this RFC's lean) vs a separate typed policy referenced by both —
  Instant's "schema and authz evolve on different axes" is the live counter-argument.
- **The ppx ergonomics layer.** A later ppx may hoist `let%on E` / co-located policy/schedule into the
  composition root — but it **must regenerate the same leaf-first `let` chain**, never a runtime
  registry, or it throws away the free acyclicity guarantee (§5).
- **Multi-step sagas** (the step-DAG) — deferred until a real 3+-external-step case appears.

**The sequencing discipline (Gel's gravestone):** *do not boil the ocean.* Ship the core (Collections +
Workflows + the sink wiring) first, keep every layer independently replaceable, and stay a **library the
user owns** — the escape hatch is "stop using the ppx, keep your OCaml," never a lock-in platform.

---

## Appendix A — the spike

A self-contained compiled spike (plain OCaml, no ppx, no framework) validated the two load-bearing
properties. The seam (`flow.mli`), condensed:

```ocaml
type 'a event                                   (* payload-typed; name+codec for observability/DDP *)
val event : string -> 'a event

type 'a sink                                    (* ABSTRACT — the acyclicity guard (see §5) *)
val sink : ('a -> unit) list -> 'a sink
val emit : 'a sink -> 'a -> unit

type 'a change = Added of 'a | Changed of 'a | Removed of 'a   (* read-projections ride the same sink *)

module Tx : sig type t  val run : (t -> ('a, 'e) result) -> ('a, 'e) result end
```

Proven, compiled and run:
- **Positive:** a workflow commits atomically and emits post-commit; a failed reserve aborts the whole
  transaction and emits *nothing*; a read-projection fans a change stream into a publication + a derived
  collection + an analytics rollup; a single-aggregate transition refuses an illegal state by the match.
- **Negative (all three rejected at compile time with "not allowed as right-hand side of let rec"):**
  a `sink ⇄ sink` cycle; a `reaction → workflow → reaction` cycle; a `derived collection A ↔ B` cycle.

The spike was throwaway (lived in `/tmp/flow-spike`); these shapes are the canonical reference.
