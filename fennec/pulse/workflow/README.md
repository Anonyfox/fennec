# fennec.pulse.workflow — the non-web core runtime

The runtime behind Fennec's **Collections / Workflows / Reactions** (the design RFC is
[`docs/internal/CORE-LAYER.md`](../../../docs/internal/CORE-LAYER.md)). It is plain OCaml — the
`[@after]`/`[@before]`/`[@cron]`/`[@every]` ppx (`fennec.pulse.workflow.ppx`) is only sugar over it,
and the **write API** (`Collection`) lives one layer up in `fennec.pulse.app` so a userland file opens
one module.

## The model in one breath

- **Collections** are the data ground truth — a Sift `[@@deriving collection]` record. Their writes are
  the data verbs (`create` / `save` / `delete`), never raw field mutation; a "transition" is just a
  `[@workflow]` that reads, changes, and `save`s.
- **Workflows** are ordinary functions tagged `[@workflow]` that run inside **one transparent
  transaction** (commit on return, roll back on `raise`, read-your-writes). No `db` is threaded
  anywhere — data and mail just exist (ambient).
- **Reactions** are annotations on functions: `[@after f]` / `[@before f]` and `[@cron …]` / `[@every …]`.

## Userland — what you write

```ocaml
open Ticket   (* the model's fields AND its methods (create/save/where/find_one) in scope *)

let[@workflow] open_ticket subject =                 (* a normal function; the tx is invisible *)
  let t = create { id = ""; subject; status = "open" } in
  ignore (Ticket_event.create { Ticket_event.id = ""; ref_ = t.id });
  t                                                  (* both writes commit together, or neither does *)

let[@workflow] close (t : t) =                       (* a transition: read, change, save *)
  if t.status <> "open" then failwith "not open";    (* raise = veto *)
  save { t with status = "closed" }

let[@after close] notify (t : t) = Mail.send …              (* post-commit effect, isolated *)
let[@cron "0 * * * *"] sweep () =                           (* at-most-once across replicas *)
  where [%q status = "open"] |> List.iter (fun t -> ignore (close t))   (* typed Meteor query *)
```

You call them like any function: `open_ticket "…"`, `close ticket`. The data verbs are the **methods on
the collection** (`Ticket.create` / `save` / `delete` / `find_one` / `where` / `all` / `count`) that
`[@@deriving collection]` generates — they run server-side over an isomorphic seam the framework installs
at boot. `[%q …]` is the typed Sift query (compile-checked, dotted subpaths typed).

**On length.** A workflow may be *long* when the process simply has many sequential steps — a top-to-bottom
read is the clearest form it can take (the whole story, in order, in one place). Splitting it into
sub-functions just to shorten it scatters that one story across call sites and hides the order — an
anti-pattern. Factor a step out only when it is reused or a distinct sub-process, **never for line count**.

## API — what the ppx targets (you rarely call these directly)

```ocaml
val transaction : (unit -> 'a) -> 'a               (* one block, one transaction; re-entrant (nested joins) *)

(* the verbs above are the generated collection methods over the Coll_writer seam; the same logic is
   also exposed flat on Fennec_pulse_app (create/save/delete/get/all/find) as the installed backend +
   an escape hatch, e.g. when you hold only a `Def.t`. *)

module Workflow : sig
  (* what `let[@workflow] f arg = body` lowers to: run befores + body in a tx, fire afters post-commit *)
  val exec : name:string -> befores:('a -> unit) list -> afters:('r -> unit) list -> 'a -> (unit -> 'r) -> 'r
  exception Cyclic_reaction of string              (* the runtime half of the circuit-breaker *)
end

module Schedule : sig
  val every : float  -> name:string -> (unit -> unit) -> unit
  val cron  : string -> name:string -> (unit -> unit) -> unit   (* 5-field UTC crontab *)
end
```

For each `[@workflow] f` the ppx emits two typed hook lists and a wrapped function that *uses* them, so
OCaml **infers** the hook types from `f`'s argument and result — and `[@after f] h` becomes a normal,
type-checked call `__after__f h`. No `Obj`, no stringly registry: reactions are **real typed
references** (go-to-references lists everything that fires around a workflow, rename is safe, deleting a
target makes its hooks a compile error).

## The transparent transaction

`[@workflow]` (and `transaction`) wraps the work in `Fennec_mongo_dynamic.Tx`. Atomic rollback is
implemented for the **in-memory (`:memory:`) backend** — what `fennec test` and the dev seed exercise —
by snapshotting each touched collection on first write and restoring it (emitting compensating change
events so live observers converge) on `raise`. Outermost transactions serialize under one Eio mutex;
nested workflows join the enclosing transaction, so a workflow calling a workflow is **one atomic
unit**, and after-hooks fire once, after the *outermost* commit. On **burrow** (the durable dev /
embedded default) rollback is just as real: the transaction holds ONE LMDB parent txn for the whole
workflow — reads and writes route through it (read-your-writes, since an LMDB write txn reads its own
pending writes), `commit` makes it durable in one fsync, `raise` aborts it. On **real Mongo** the
bracket is still transparent and writes commit-on-success; a client-session transaction is the one
remaining follow-on. None of this is visible in userland — you write `raise`.

## The circuit-breaker (reaction cycles can't infinite-loop)

Three layers, so a cycle is caught as early as possible:

1. **intra-module** — the ppx builds the local `@after`/`@before` edge graph and rejects a cycle at
   compile time (`cyclic reaction chain p -> q -> p`).
2. **cross-module** — `[@after w]` emits a reference to `w`, so a hook's module depends on its target's
   module: a cross-module reaction cycle is a *module dependency cycle* dune rejects (and a directly
   self-referential workflow is a `let rec` over an application, which OCaml rejects).
3. **runtime** — `Workflow.call` carries a fiber-local re-entrancy guard: a workflow re-entered within
   its own reaction cascade (a body that loops back) raises `Cyclic_reaction`, contained by the
   after-hook isolation — it halts the loop instead of overflowing the stack.

## At-most-once schedules across replicas

`Schedule` runs `@cron`/`@every` jobs at-most-once cluster-wide: each due tick a replica tries to claim
`(job, slot)` with a unique insert into `_fennec_cron` (unique index on `(job, slot)`); only one
replica's insert wins per slot. Lock-free (no lease → no deadlock, no orphan if a replica dies),
self-pruning by timestamp, UTC slots, and "always succeeds" on a single server. The scheduler fiber is
forked by `Fennec.serve` at boot (a no-op when there are no jobs; never in the console).

## Tests

- `test/` — Workflow (post-commit after, veto, nested-flatten + outer-rollback) and Schedule (cron
  matcher, claim contention, tick semantics).
- `ppx/test/` — the desugar, behaviourally; a negative build proves the @after-edge cycle is a compile
  error.
- `../app/test_core/` and `mongo/dynamic/test/` — the transaction context + Collection write-API end to
  end over `:memory:`.
