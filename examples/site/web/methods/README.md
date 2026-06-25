# `web/methods/` — typed RPC methods

A **method** is how a client changes data: a typed remote procedure call (Meteor's `Meteor.methods`).
It is the RPC transport, the peer of `web/handlers/` (which is HTTP). One method per file; the file's
name is the method (`add_task.mlx` → `Add_task.add_task`).

Like everything under `web/`, a method file is **dual-compiled**: the server keeps the real handler; the
client keeps only what it needs to *call* the method (and optionally predict it). You never wire any of
that — you write a function.

## A method is a typed function

```ocaml
(* web/methods/add_task.mlx *)
let add_task (title : string) : Task.t = Task.create { Task.id = ""; title; body = "" }
```

That is the whole method. The fur ppx reads the signature and does the rest:

- **The wire contract is derived from the types.** `string -> Task.t` becomes the argument and result
  codecs automatically (primitives map directly; a `[@@deriving model] t` uses its codec; `list`/`option`
  wrap). A wrongly-typed call is a *compile error*; a malformed argument is rejected before the handler
  runs. You write no `~args`/`~result`, no codecs, no name string.
- **The body is server-only, stripped from the client *categorically*.** It lives in a slot the client
  build simply doesn't include — so whatever it touches (workflows, secrets, C-backed libraries) can
  never reach the bundle. This isn't a per-line annotation you can forget; it's the whole function.
- **No `inv` parameter.** The current user / request context is ambient (`Auth.*` / `Accounts.*`), not a
  threaded argument.

A binding that is already a value (e.g. a hand-written `Rpc.method_ …`) is left untouched — the
combinator stays available as an escape hatch.

## Calling a method

From a component, with the same Meteor feel — and full type safety:

```ocaml
ignore (Pulse.call Add_task.add_task "Buy milk")   (* a wrong argument type is a compile error *)
```

## `let%optimistic` — instant UI (latency compensation)

By default a method is server-only: the new row appears once the server's write propagates over the
reactive subscription (one round-trip). Add an **optimistic slot** and the client predicts the result
*immediately*, against its local cache:

```ocaml
let add_task (title : string) : Task.t = Task.create { Task.id = ""; title; body = "" }
let%optimistic add_task title          = Task.create { Task.id = ""; title; body = "" }
```

- **It's a separate, client-only function** — so it can differ intelligently from the handler (predict
  only the visible row while the server also charges a card, sends mail, runs a workflow), and *no server
  code can leak into it* (the split is the safety boundary). It's plain OCaml in the verbs you already
  use — it can read stores/signals like any component snippet. No `ignore`, no `sim`.
- **The guess is just a guess; the server is authoritative.** Insert ids mint from the call seed on both
  sides, so a string-id collection's optimistic row converges with the real one when the server's write
  arrives — no duplicate-then-vanish. Omit the slot entirely and you simply lose the instant feedback.

## `let%authorize` — a server-side guard

A guard that runs *before* the handler, server-side (authorization is the server's call, never the
client's). It raises to deny:

```ocaml
let place_order (cart : Cart.t) : Order.t = Orders.place cart
let%authorize place_order _cart = Auth.require_role `Customer   (* raises if not allowed *)
```

It receives the same arguments as the method (authorize editing a specific resource), and can use any
server-only check. Omit it for a public method.

## Escalating to a workflow

A method is the thin web edge. When its body grows past a couple of writes — multiple collections that
must change **atomically**, after-effects, retries, scheduling — don't fatten the method: extract the
logic into a `[@workflow]` (in `workflows/`) and have the method call it:

```ocaml
let place_order (cart : Cart.t) : Order.t = Orders.place cart   (* Orders.place is a [@workflow] *)
```

Why this is the right move, not just tidy:

- **The refactor is trivial** — both are plain functions; cut the body into a `let[@workflow]` and call it.
- **You gain machinery for free** — a workflow runs in the transparent transaction (atomic rollback),
  composes other workflows, fires `@after` reactions, and can be `@cron`-scheduled. The clearest signal
  to escalate is *"these writes must be atomic"* — that need is the workflow boundary.
- **It stays leak-proof** — the method's handler (and the workflow it calls, and everything that touches)
  is stripped from the client wholesale.

So the layering reads: **collection methods** (`Task.create`) for direct CRUD → **methods** for the typed
RPC verb → **workflows** for the orchestrated process. Edge → process; transport → logic.

## Good to know

- **Offline-first.** A call made while disconnected is queued and replayed on reconnect; the optimistic
  guess (if any) shows immediately. No userland code.
- **Server truth wins.** The optimistic guess is reconciled to the server's data when its write arrives;
  validation and authorization are enforced server-side regardless of what the client predicted.
- **One method per file.** The filename-named function is the method; other top-level bindings are
  server-only helpers.
