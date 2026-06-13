# INDEXES — declarative, reconciled, full-stack (design → as-built)

Indexes are the next data-story milestone. The bar: Meteor works (a decade of `createIndex` at
startup), but has three real gaps we close.

## Meteor's gaps, and our answers

1. **Imperative, scattered.** Meteor calls `Tasks.createIndex(...)` in startup code, away from the
   model. → **Declarative, co-located, typed:** `Index.declare collection Index.[ asc Fields.done_;
   unique (asc Fields.email) ]` right under the model. Field handles ⇒ a renamed field is a
   compile error in the index too.
2. **No drift management.** Remove a `createIndex` call and the index lingers in mongod forever
   (orphan); you must remember to `dropIndex`. → **Boot reconciliation:** at startup we diff the
   DECLARED set against what mongod actually has, create the missing, and DROP the orphans we
   created — tracked by a deterministic fennec name prefix (`fx_…`) so we never touch `_id_` or a
   hand-made index. Idempotent; re-running is a no-op; identical declared sets across instances
   converge (multi-instance safe — create/drop are idempotent server-side).
3. **No dev/test parity.** Meteor's minimongo ignores `unique`, so a uniqueness bug only appears in
   production. → **Minimongo enforces unique** in-engine: an insert/update violating a declared
   unique index fails the same way prod does, so tests and the in-memory backend catch it.

## Lifecycle (graceful, full-stack)

- **Where:** `Index.declare` registers (global, like the publication/method registries); reconcile
  runs inside `T.attach` (boot, Eio context) — declaring + attaching = indexes ensured.
- **Graceful:** a failed build (e.g. a unique index over already-duplicate data) is caught, logged
  loudly with the index name + reason, and the server continues (a missing index degrades perf, not
  correctness — crashing is worse). `~strict` flips it to fail-fast for prod gates.
- **Backend-virtual:** `Backend.S` gains `ensure_index`/`drop_index`/`index_names`. Native →
  mongod create/drop/list. Mini → unique-constraint enforcement (parity) + name tracking; non-unique
  in-memory indexes are declared-but-no-op (the in-memory scan doesn't need them — actual in-memory
  index acceleration is a noted perf seam).
- **Names encode the spec** (fields + directions + unique), so a changed declaration yields a new
  name → the old is dropped and the new created automatically (a "migration" with no migration file).

## Surface

```ocaml
type t = { id : string; email : string; team : string } [@@deriving collection ~name:"users"]
let () = Index.declare collection Index.[ unique (asc Fields.email); asc Fields.team ]
```
That's the whole story — declared once, reconciled at boot, enforced in-memory and in mongod.
