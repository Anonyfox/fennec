# METHODS — the one blessed write path (as-built)

Status: **as-built (living record).** Methods are how a fennec client changes data — the only way,
by decree. There is **no allow/deny** and never will be: the rule machinery for direct client
mutations is the part of Meteor its own community spent a decade regretting; fennec's security story
is one sentence instead — *every client-originated change is a method you wrote on the server.*

The wording is Meteor's (`methods` / `call` / `apply` / `Error {code; reason}` / `userId` /
`isSimulation` / `setUserId`); the guarantees are OCaml's. This file is the canonical guide; the
precise per-value contracts live in the `.mli`s (`fennec.pulse.method`, `reactive.mli`,
`ddp_client.mli`).

---

## 1. Three layers, strictly stacked

**Layer 0 — the wire primitive (Meteor-exact, untyped).** `R.methods [(name, fun inv args -> …)]`,
`call`/`apply`, DDP `method`/`result`/`updated`, byte-compatible with stock Meteor clients (golden
frames). The escape hatch and the interop floor.

**Layer 1 — typed method values.** A method is ONE shared value:

```ocaml
(* shared module (compiles native + jsoo) *)
let add_task : (string, string) Method.t =
  Method.define "addTask" ~args:Codec.(a1 string) ~result:Codec.string

(* server *)
RData.handle add_task (fun inv title -> Collection.insert tasks (doc [ "title", str title ]))

(* browser component *)
let r = Ddp_client.call_m client Site_methods.add_task "Buy milk"
(* : (string, string * string) result option Fur.signal *)
```

Because both sides reference the *same value*, a renamed method, a changed arity, or a drifted type
is a **compile error across the whole app** — the stringly-mismatch bug class doesn't exist. And the
**codec is the validation**: a malformed call decodes to a `400` *before* the handler runs (what
Meteor needs check/ValidatedMethod for). `Codec` is value-level combinators — no ppx, no functors —
so the hundredth method costs what the first did, at compile time and at runtime.

**Layer 2 — opt-in latency compensation (`?stub`).** See §3.

## 2. Identity and semantics

- Each call runs with `invocation = { user_id; is_simulation; set_user_id }`. `set_user_id` rebinds
  the **connection** for subsequent calls — a login method's job (verify however you like, then
  `inv.set_user_id (Some uid)`); off-connection it is a no-op. This is the identity *hook*, not an
  accounts system.
- Methods from one connection run **serially, in order** (the dispatch fiber) — Meteor's default.
  (`unblock` is deferred.)
- **At-least-once**: an unacknowledged method (sent, no `result`) re-sends **verbatim** after a
  reconnect (same id, same seed), after the resubscriptions — so in-flight `call_result`/`call_m`
  signals always eventually resolve. Write idempotency is the app's concern, exactly as in Meteor.
- **The write fence**: the server emits `updated` only after every data delta the method's writes
  caused has been *delivered* to that session (`Fanout.on_drained` → `Backend.fence` →
  `Reactive.fence` → the session). Exact on the in-memory backend; **best-effort over mongod**
  (change-stream delivery is asynchronous; the resume-token fence is the marked seam — Meteor uses
  oplog positions for the same job).

## 3. Optimistic UI (latency compensation)

Opt-in per method, because the stub is client code by nature:

```ocaml
let add_task : (string, string) Method.t =
  Method.define "addTask" ~args:Codec.(a1 string) ~result:Codec.string
    ~stub:(fun sim title -> ignore (sim.insert "tasks" (doc [ "title", str title ])))
```

Mechanics — deliberately built on machinery the data layer already proves elsewhere:

- The stub runs **immediately** in the browser against `Sim.writes` — a virtual subscription drawn
  from the merge store's **negative precedence band**: its fields win over every real subscription
  instantly, and a later simulation wins over an earlier one. The UI updates now.
- **Rollback is `sub_stopped`.** When the server's `updated` arrives (behind the write fence), the
  client drops the sim sub and the per-field precedence fallthrough reveals server truth — no
  bespoke undo machinery exists or is needed. A rejected method reverts the UI the same way, with
  the typed error in the `call_m` signal.
- **Insert ids converge**: the call carries a `randomSeed`; stub and handler mint ids from the same
  `(seed, collection)` splitmix64 stream (pure OCaml — bit-identical native and jsoo; the server
  binds it fiber-locally for the handler's extent, so concurrent methods never cross). The
  optimistic row and the real row are *one row* — no duplicate-then-vanish.
- Deletes tombstone via `sim_hide` (precedence can't express absence): dropping the sim restores the
  doc unless a real removal landed meanwhile.
- A **throwing stub** is logged and skipped; the call still reaches the server (truth).

Rules of thumb for stubs: keep them a cheap prediction of the handler's collection writes; match the
handler's **per-collection insert order** (streams are per collection, so cross-collection
interleaving differences are fine); don't duplicate server-only logic — handlers are not
simulations. Known caveats: a **MONGO (ObjectId)** collection's optimistic insert swaps ids once at
reveal (STRING, the default, converges); over **mongod** the fence is best-effort (above).

## 4. Performance and the dev loop

- **Compile**: methods are values. No ppx, no per-method functors, no `.cmi` cascades — hundreds of
  handlers stay cheap. Shard declarations into per-domain shared modules (`methods/tasks.ml`,
  `methods/billing.ml`, …) so an edit recompiles one module.
- **Dev server**: dune's dev profile already links js_of_ocaml in **separate compilation** mode —
  measured here: editing a stub and relinking the app bundle takes **~0.2s**. Release builds use
  whole-program + optimization automatically. Nothing to configure.
- **Runtime**: dispatch is a registry lookup + the handler; codecs run once per call at the edge;
  the fence is a handful of already-idle drain checks at user-action rate.

## 5. Offline mode (built in — no plugin, no userland plumbing)

A network drop degrades gracefully with **zero app code**. While disconnected: `find` keeps
rendering the cache (last known truth), stubs keep applying instantly (full optimistic UI offline),
and every method call **buffers in order** (the unacked list — the same at-least-once machinery as
reconnect resend). When the socket returns, ONE handshake heals everything, in this order: `connect`
(session resume) → resubscribe every live subscription (resync + `ready`-quiescence drop what the
server stopped sending) → flush the buffered methods verbatim, oldest first. Their `updated`s then
resolve the waiting simulations exactly as if the network had never blinked.

Deliberately, **nothing else queues offline**: the server session dies with the socket, so raw
frames buffered client-side could only double-send — subs rebuild from `t.subs`, methods from the
buffer. **Silent network death** (wifi gone, no FIN — the case browsers take minutes to notice) is
caught by a DDP heartbeat: a ping every 15s with a 10s deadline force-closes the dead socket and the
reconnect loop takes over (≤ ~25s detection).

Affordances are two Fur signals on the client — `status` ([`Connected | `Connecting | `Waiting]) and
`pending_writes` (buffered count) — for "reconnecting…" banners and "saving… (N)" hints; both are
pinned (`Connected`/0) on SSR so the first paint never flashes an offline state. Scope, by design:
the **running page** — buffers do not survive a reload (durability across reloads = a different
feature, IndexedDB persistence, deliberately out).

## 6. Deferred (deliberately, with seams marked)

- `this.unblock` (concurrent methods per connection).
- Alea-exact seed derivation for **stock Meteor clients'** optimistic UI (fennec↔fennec converges
  today; stock clients fall back to non-optimistic correctness).
- Publication-side `userId` (the session carries it; threading it into `publish` is a later, small
  step).
- A resume-token write fence for mongod.
