# fennec-mongo

**The MongoDB data plane in OCaml — three interchangeable storage tiers behind one signature and one
URL.** The same query / update / projection / sort / aggregation semantics everywhere: one pure `Query`
engine shared by the in-memory and embedded tiers, differential-tested against a real `mongod` so all
three agree on what a query means.

| Tier | Lib | What it is |
|---|---|---|
| **In-memory** | `fennec-mongo.minimongo` | pure OCaml collection + reactive observe; compiles to JS — the browser cache and the test backend |
| **Embedded** | `fennec-mongo.burrow` | "SQLite for MongoDB": a durable LMDB engine *in your process* — indexes, planner, transactions, oplog, and a mongosh-compatible wire endpoint |
| **Hosted** | `fennec-mongo.driver` | a statically-linked libmongoc client to a real `mongod` |

All three sit behind **one seam** (`Backend.S` in `fennec-mongo.backend`), selected at boot by `MONGO_URL`
with no type change downstream:

```sh
MONGO_URL=:memory:                          # minimongo — tests, dependency-free
MONGO_URL=burrow:///abs/path/app            # embedded, durable; add host:port for a mongosh endpoint:
MONGO_URL=burrow://localhost:27017/data/app #   … mongosh/Compass connect as to a hosted mongod (SCRAM via user:pass)
MONGO_URL=mongodb://localhost/app           # a real mongod via libmongoc
```

`fennec dev` generates a zero-config `burrow://` URL (durable dev data + a live mongosh endpoint);
`fennec test` sets `:memory:`. A missing `MONGO_URL` fails clearly — never a hidden fallback.

## Package map

| Lib | Role |
|---|---|
| `fennec-mongo.bson` | the BSON value type: constructors, typed accessors, `equal`/`compare` |
| `fennec-mongo.query` | the pure engine: `Matcher` · `Modifier` · `Projection` · `Sorter` · `Aggregate` · `Expr` · `Geo` · `Diff` · `Id` |
| `fennec-mongo.minimongo` | the in-memory collection + the `Fanout` observe monitor |
| `fennec-mongo.json` / `.bson_json` | JSON and extended-JSON codecs |
| `fennec-mongo.gridfs` | GridFS as a pure functor — runs over any of the three tiers |
| `fennec-mongo.wire` | the pure wire protocol: spec-exact BSON bytes, OP_MSG/OP_QUERY framing, SCRAM-SHA-256 (server *and* client sides) |
| `fennec-mongo.burrow` (+ `.store` `.codec` `.binary` `.lmdb`) | the embedded engine stack, bottom-up: LMDB FFI → typed store → memcomparable keys / record codec → the engine |
| `fennec-mongo.ffi` / `.driver` | libmongoc externals + the typed driver (client, collections, live change streams) |
| `fennec-mongo.mongod` | a managed local `mongod` lifecycle (spawn/adopt/health) |
| `fennec-mongo.backend` | the ONE storage seam: `Backend.S` + `query` + the in-memory impl — pure, ships to JS |
| `fennec-mongo.dynamic` | the native facade: driver + Burrow backends, the `Dynamic` MONGO_URL selector, transparent transactions (`Tx`), the mongosh wire endpoint, wire client + replication |

The precise API is inline in the `.mli`s (the two libs of raw externals — `.ffi`, and the seam whose body
*is* a signature — are their own interface).

## The in-memory tier (Minimongo)

No indexes to declare — every query is a brute-force scan, so geo, `$regex`, and aggregation just work.
Ideal for tests and the browser. **Thread-safe on OCaml 5 multicore**: reads snapshot and mutations
commit under a per-collection lock, and change events deliver in commit order outside all locks (via the
bundled `Minimongo.Fanout` monitor) — observers may re-entrantly mutate, and a slow observer blocks nothing.

```ocaml
module C = Minimongo

let () =
  let users = C.create () in
  let _ = C.insert users (Bson.doc [ ("name", Bson.str "Ada"); ("age", Bson.int 36) ]) in

  (* selectors / updates / projections are just [Bson.t] — the shapes you'd type in the shell *)
  let names =
    C.fetch (C.find users
      ~selector:(Bson.doc [ ("age", Bson.doc [ ("$gte", Bson.int 18) ]) ])
      ~sort:(Bson.doc [ ("age", Bson.int (-1)) ])
      ~fields:(Bson.doc [ ("name", Bson.int 1); ("_id", Bson.int 0) ]) ()) in

  let _ = C.update users (Bson.doc [ ("name", Bson.str "Ada") ])
                         (Bson.doc [ ("$inc", Bson.doc [ ("age", Bson.int 1) ]) ]) in

  let avg = C.aggregate users
    [ Bson.doc [ ("$group", Bson.doc [ ("_id", Bson.null); ("avgAge", Bson.doc [ ("$avg", Bson.str "$age") ]) ]) ] ] in

  (* observe — fires immediately on every matching mutation *)
  let h = C.observe_changes (C.find users ()) ~added:(fun id _ -> Printf.printf "added %s\n" id) () in
  ignore (names, avg); h.C.stop ()
```

### Feature coverage

- **Selectors** — comparison · logical · element (`$exists`/`$type`) · evaluation (`$mod`/`$regex`) · array (`$all`/`$elemMatch`/`$size`) · bitwise · geospatial (`$geoWithin`/`$near`/`$geoIntersects`, all shapes). Equality is numeric-cross-type + array-aware; range is type-scoped.
- **Updates** — `$set` `$unset` `$inc` `$mul` `$min` `$max` `$rename` `$setOnInsert` `$push` (`$each`/`$position`/`$sort`/`$slice`) `$addToSet` `$pull` `$pullAll` `$pop` `$bit`.
- **Projection** — nested include/exclude, `$slice`, `$elemMatch`.
- **Aggregation** — `$match`/`$project`/`$addFields`/`$unset`/`$sort`/`$limit`/`$skip`/`$count`/`$unwind`/`$group`/`$sortByCount`/`$sample`/`$replaceRoot`/`$lookup`/`$unionWith`/`$facet`/`$bucket`, the full `Expr` language (field paths, `$$ROOT`, arithmetic/comparison/boolean/conditional/string/array/type), and the accumulators. `$lookup` / `$unionWith` resolve the foreign collection through a supplied resolver (`aggregate ?lookup`); the framework's `fennec.pulse` wires it across a reactive instance's named collections, so **in-memory joins span collections** — on the server and (over the subscribed subset) on the client.
- **Not supported** (a layer up or out of scope): `$where` / `$text`; positional `$` / `$[]` / `$[<id>]` + `$currentDate` (need the selector / arrayFilters / a clock); `$near` distance *sorting* (the filter is in). `$sample` is a deterministic head sample (no RNG). An unknown operator never hides a doc; an unknown stage passes through. (`$jsonSchema` validators live a layer up — enforced by Burrow and mongod.)

## The embedded tier (Burrow)

A native, in-process, on-disk engine on vendored LMDB: single-writer **group-committed durability**
(~20k+ durable writes/s at 200 writers), lock-free MVCC readers, secondary indexes
(single/compound/unique/sparse/multikey) behind a selectivity-scoring **planner** with sort-via-index,
`$jsonSchema` validation, workflow transactions, online index builds, hot backup, and an **oplog** with
its consumer trio — resumable change streams (`db.coll.watch()`), point-in-time recovery, and
async-replication primitives (an idempotent follower + a SCRAM-authenticated wire transport). The same
`Query` semantics as minimongo, differential-tested against it and against real mongod; in-process it
beats a local mongod on every scenario we bench.

The wire endpoint makes it operable like a database: real **mongosh / Compass / drivers** connect
(SCRAM-SHA-256, RBAC roles, TLS, audit sink, `explain`/`serverStatus`, a slow-command log). Design +
module map: [burrow/README.md](burrow/README.md) · production scope and the embedded/control-plane
boundary: [burrow/PRODUCTION.md](burrow/PRODUCTION.md).

## The hosted tier (driver + managed mongod)

A statically-linked **libmongoc** driver (`fennec-mongo.ffi` externals + the typed `fennec-mongo.driver`),
live change streams, and a managed `mongod` lifecycle (`fennec-mongo.mongod`) for adopting/spawning a
local server per test suite. The driver is native-only and **degrade-safe** at build time — if libmongoc
can't be built, the pure tiers still work. `fennec-mongo.dynamic` puts all three tiers behind `Dynamic`
(one `Backend.S`), adds transparent workflow transactions (`Tx.run` — native rollback on every tier), and
hosts the wire endpoint + replication transport.

## Native + browser

`Bson` and `Minimongo` are Stdlib-only; the `Query` engine's only dependency beyond `Bson` is the
pure `re`. So the whole engine compiles to JavaScript via js_of_ocaml — the *same* `Minimongo` runs
server-side and as a client-side cache. Inject the id / RNG source for a clean JS build
(`Minimongo.create ?gen_id`, `Query.Id.random_id ?rng`).

```sh
opam install fennec-mongo    # the pure trio is dep-light; a BSON-only consumer never links the native archive
```
