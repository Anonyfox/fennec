# fennec-mongo

**A MongoDB you can run in memory — on the server and in the browser.** Pure OCaml: a BSON value
type, the full Mongo query / update / projection / sort / aggregation engine, and an in-memory
Minimongo collection with a reactive observe engine. The same source compiles native and to
JavaScript (js_of_ocaml).

No indexes to declare — every query is a brute-force scan over the documents, so geo, `$regex`, and
aggregation just work. Ideal for **tests and the browser**; `fennec dev` auto-starts/adopts a real
`mongod` when available, while explicit `MONGO_URL=:memory:` keeps tests dependency-free. **Thread-safe on OCaml 5 multicore**: reads
snapshot and mutations commit under a per-collection lock, and change events deliver in commit order
outside all locks (via the bundled `Minimongo.Fanout` monitor) — observers may re-entrantly mutate,
and a slow observer blocks nothing. The precise API is inline in the `.mli`s.

## Quickstart

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

## Modules

| Module | Role |
|---|---|
| `Bson` | the value type + constructors, typed accessors, `equal` / `compare`, `to_string` |
| `Minimongo` | the collection: `create` · `insert` · `update` · `remove` · `find` · `fetch` · `find_one` · `count` · `aggregate` · `observe_changes` |
| `Query.*` | the engine, reached *through* Minimongo (direct only when collection-less): `Matcher` · `Modifier` · `Projection` · `Sorter` · `Aggregate` + `Expr` · `Geo` · `Id` |

## Feature coverage

- **Selectors** — comparison · logical · element (`$exists`/`$type`) · evaluation (`$mod`/`$regex`) · array (`$all`/`$elemMatch`/`$size`) · bitwise · geospatial (`$geoWithin`/`$near`/`$geoIntersects`, all shapes). Equality is numeric-cross-type + array-aware; range is type-scoped.
- **Updates** — `$set` `$unset` `$inc` `$mul` `$min` `$max` `$rename` `$setOnInsert` `$push` (`$each`/`$position`/`$sort`/`$slice`) `$addToSet` `$pull` `$pullAll` `$pop` `$bit`.
- **Projection** — nested include/exclude, `$slice`, `$elemMatch`.
- **Aggregation** — `$match`/`$project`/`$addFields`/`$unset`/`$sort`/`$limit`/`$skip`/`$count`/`$unwind`/`$group`/`$sortByCount`/`$sample`/`$replaceRoot`/`$lookup`/`$unionWith`/`$facet`/`$bucket`, the full `Expr` language (field paths, `$$ROOT`, arithmetic/comparison/boolean/conditional/string/array/type), and the accumulators. `$lookup` / `$unionWith` resolve the foreign collection through a supplied resolver (`aggregate ?lookup`); the framework's `fennec.pulse` wires it across a reactive instance's named collections, so **in-memory joins span collections** — on the server and (over the subscribed subset) on the client.
- **Not supported** (a layer up or out of scope): `$where` / `$jsonSchema` / `$text`; positional `$` / `$[]` / `$[<id>]` + `$currentDate` (need the selector / arrayFilters / a clock); `$near` distance *sorting* (the filter is in). `$sample` is a deterministic head sample (no RNG). An unknown operator never hides a doc; an unknown stage passes through.

## Beyond in-memory

A native, statically-linked **libmongoc driver** (`fennec-mongo.ffi`), a managed `mongod` lifecycle
(`fennec-mongo.mongod`), and an extended-JSON codec (`fennec-mongo.bson_json`). The driver is
native-only and **degrade-safe** at build time — if libmongoc can't be built, the pure in-memory
engine still works. The framework's `fennec.pulse.mongo` exposes both behind one `Backend.S` / `Dynamic`, so
**Pulse** (the framework's reactive data layer — collections, DDP, live queries) runs over either
with no type change; `fennec dev` auto-starts/adopts a managed local mongod when available and
`fennec test --mongo` launches per-suite mongods, all wired through `MONGO_URL`.

## Native + browser

`Bson` and `Minimongo` are Stdlib-only; the `Query` engine's only dependency beyond `Bson` is the
pure `re`. So the whole engine compiles to JavaScript via js_of_ocaml — the *same* `Minimongo` runs
server-side and as a client-side cache. Inject the id / RNG source for a clean JS build
(`Minimongo.create ?gen_id`, `Query.Id.random_id ?rng`).

```sh
opam install fennec-mongo    # the pure trio is dep-light; a BSON-only consumer never links the native archive
```
