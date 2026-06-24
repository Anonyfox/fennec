# fennec.pulse.workflow — the non-web core runtime

The runtime behind Fennec's **Collections / Workflows / Reactions** (the design RFC is
[`docs/internal/CORE-LAYER.md`](../../../docs/internal/CORE-LAYER.md)). It is plain OCaml — the
`[@after]`/`[@before]`/`[@cron]`/`[@every]` ppx (`fennec.pulse.workflow.ppx`) is only sugar over it,
and the **write API** (`Collection`) lives one layer up in `fennec.pulse.app` so a userland file opens
one module.

## The model in one breath

- **Collections** are the data ground truth — a Sift `[@@deriving model]` record + a `Def.v`. Their
  writes are **create / delete / named transitions**, never raw field mutation.
- **Workflows** are ordinary functions that run inside **one transparent transaction** (commit on
  return, roll back on `raise`, read-your-writes throughout). No `db` is threaded anywhere — data and
  mail just exist (ambient).
- **Reactions** are annotations on functions: `[@after f]` / `[@before f]` and `[@cron …]` / `[@every …]`.

## API

```ocaml
(* one block, one transaction — commit on return, roll back on raise (re-entrant: a nested call joins) *)
val transaction : (unit -> 'a) -> 'a               (* = Fennec_mongo_dynamic.Tx.run *)

module Workflow : sig
  type ('a, 'r) t
  val make   : string -> ('a -> 'r) -> ('a, 'r) t  (* wrap a function as a workflow *)
  val call   : ('a, 'r) t -> 'a -> 'r              (* run it: before-guards + body in a tx, after-hooks post-commit *)
  val before : ('a, 'r) t -> ('a -> unit) -> unit  (* in-transaction guard; raise to veto (rolls back) *)
  val after  : ('a, 'r) t -> ('r -> unit) -> unit  (* post-commit reaction, isolated; receives the RESULT *)
  exception Cyclic_reaction of string              (* the runtime half of the circuit-breaker *)
end

module Schedule : sig
  val every : float  -> name:string -> (unit -> unit) -> unit
  val cron  : string -> name:string -> (unit -> unit) -> unit   (* 5-field UTC crontab *)
end

(* in fennec.pulse.app — the transitions-as-write-API over a Sift Def.t: *)
module Collection : sig
  val get : 'a Def.t -> string -> 'a option
  val all : 'a Def.t -> 'a list
  val find : 'a Def.t -> where:Filter.t list -> 'a list
  val create     : 'a Def.t -> ('a, 'a) Workflow.t
  val delete     : 'a Def.t -> ('a, unit) Workflow.t
  val transition : 'a Def.t -> string -> ('a -> 'a) -> ('a, 'a) Workflow.t
end
```

The ppx desugars `let[@after w] h x = …` to `let () = Workflow.after w h` (and similarly for the
others), so reactions are **real typed references** — go-to-references on a workflow lists everything
that fires around it, rename is safe, and deleting a target makes its orphaned hooks a compile error.

## The transparent transaction

`Workflow.call` (and `transaction`) wraps the work in `Fennec_mongo_dynamic.Tx`. Atomic rollback is
implemented for the **in-memory (`:memory:`) backend** — what `fennec test` and the dev seed exercise —
by snapshotting each touched collection on first write and restoring it (emitting compensating change
events so live observers converge) on `raise`. Outermost transactions serialize under one Eio mutex;
nested `call`s join the enclosing transaction, so a workflow calling a workflow is **one atomic unit**,
and after-hooks fire once, after the *outermost* commit. On **burrow / mongo** the bracket is
transparent and writes commit-on-success; backend-native rollback (an LMDB parent-txn / a mongo
session) is a scoped follow-on.

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
