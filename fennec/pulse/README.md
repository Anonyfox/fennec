# fennec.pulse

**The isomorphic reactive data layer** — typed collections, live queries over DDP, SSR seeding, and
transactional workflows, Meteor's model rebuilt on OCaml types. One `Sift` codec per model drives
everything: validation, BSON/JSON, the Mongo `$jsonSchema` the database itself enforces, typed
filters, and the client's cache — the server and the browser run the *same* query engine
(minimongo), so a subscription is a synced subset, not a REST round-trip.

## The userland surface

```ocaml
(* one typed collection, defined once by its Sift codec (see sift/README.md) *)
let tickets = App.collection Ticket.def

let () =
  App.publish Ticket.def;                              (* live DDP + the SSR seed, one call *)
  App.method_ Ticket.close (fun inv id ->              (* a typed method with its invocation *)
      App.update Ticket.def ~where:[ Filter.eq "_id" id ] Update.(set "open" false))
```

Reads and writes are typed end-to-end: `App.insert def value` validates through the codec;
`where:[%q { open = true }]` / `Filter.*` compile to the same selector semantics on every backend
(`:memory:` / `burrow://` / `mongodb://` — one `MONGO_URL`, see [mongo/README.md](../../mongo/README.md)).
A **workflow** (`let[@workflow] place_order arg = …`) runs in one transaction — commit on return,
rollback on raise, read-your-writes, on every backend natively — and its **reactions**
(`[@after place_order]`, `[@before …]` guards) plus the effects **outbox** and the **scheduler**
(`[@cron "0 3 * * *"]` / `[@every 60.]`) hang off it declaratively.

## How the live pipeline works

```
write → Backend.observe_changes → Reactive (per-query live sets, keyed by Query_key)
      → Merge_store (per-session union of subscriptions, minimal added/changed/removed)
      → DDP over the websocket → the client's minimongo cache → Fur signals re-render
```

- **`Reactive`** ([reactive.ml](reactive.ml)) — live queries over the storage seam's
  `observe_changes`; publications, subscriptions, method dispatch, the `__currentUser` pub.
- **`Merge_store`** ([live/merge_store.ml](live/merge_store.ml)) — Meteor's merge box: what each
  session actually sees (the union of its subscriptions), diffed to minimal DDP messages.
- **`fennec/ddp`** — the wire itself (EJSON, message codec, SockJS framing, session), fully `.mli`'d
  and shared by server and client.
- **`live/client`** — the browser DDP client over the same minimongo; `live/sim.ml` simulates
  latency compensation for tests.

## Package map

| Lib | Role |
|---|---|
| `fennec.pulse` | the engine: `Reactive` (live queries, pubs, methods) · `Typed` (codec-bound collections) · `Query_key` |
| `fennec.pulse.app` | the facade userland opens: `collection` / `publish` / `method_` / `insert` / `update` / `where` / `transaction` / `serve_ddp` |
| `fennec.pulse.live` (+ `.live.client`) | the merge box + DDP server glue; the browser/native DDP clients |
| `fennec.pulse.method` | typed method definitions (shared by server dispatch + client stubs) |
| `fennec.pulse.workflow` (+ `.ppx`) | transactional workflows, reactions, the effects outbox (exactly-once via idempotent delivery), the at-most-once cluster scheduler — each `.mli`'d, each `tick`-driveable in tests |
| `fennec.pulse.sift` (4 libs) | the typed-BSON toolkit every model rides on — [sift/README.md](sift/README.md) |
| `js_gate` | the build-time gate: a native unit reaching the browser closure fails the BUILD, not the runtime |

## Design points

- **No `Pulse.start`.** `Fennec.serve` boots the data layer from `MONGO_URL` alone (the ambient
  switch, the wire endpoint, accounts) — zero wiring in userland.
- **Exactly-once, honestly.** The outbox is at-least-once delivery + the intent id as the
  idempotency key — stated, not pretended away. The scheduler is at-most-once per (job, slot) via a
  lock-free unique-insert claim — no lease to orphan.
- **Pay only for what you use.** No `[@cron]` jobs → no scheduler fiber; no effect handlers → no
  outbox worker; the js_gate keeps native code out of bundles at compile time.
- **Deterministic tests at every layer.** The workflow/outbox/schedule loops are `tick`-driveable
  with time as an argument; the DDP client has a native build for socket-free tests; integration
  suites live in `mongo/test` + `test/` folders per the repo testing ladder.
