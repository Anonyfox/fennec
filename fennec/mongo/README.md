# fennec-mongo

**A MongoDB you can run in memory — on the server and in the browser.** Pure OCaml: a BSON value
type, a complete MongoDB query / update / projection / sort engine (with geospatial, `$regex`, and
the aggregation pipeline), and an in-memory Minimongo collection with a reactive observe engine. The
same source compiles native (via the regular toolchain) and to JavaScript (via js_of_ocaml).

## Why it exists

Real MongoDB's hard parts — the storage engine, indexes, sharding — are about scaling persistent
data. An **in-memory collection has none of those constraints**, so almost the entire *feature*
surface is just pure computation over a list of documents. That has a useful consequence:

> **No indexes are required.** A 2dsphere index, a compound index — in real Mongo these are
> performance structures. Here every query is a brute-force scan, so geo queries, `$regex`, and
> aggregation "just work" with nothing to declare.

The result is a lean, fast, dependency-light Mongo that behaves like the real thing on the common
path — ideal for **tests, dev, and the browser** — so you only reach for a real `mongod` when you
need the storage engine itself, not its query features.

## Status

Ships the **pure trio**: `Bson` + the `Query` engine + in-memory `Minimongo`. A native,
libmongoc-backed driver (same wire features, real persistence) is a planned addition to this
package. The pure trio's API is stable.

## Install

```
opam install fennec-mongo
```

In your `dune`:

```lisp
(libraries fennec-mongo.bson fennec-mongo.minimongo)   ; the common case
; add fennec-mongo.query only to use the engine (Matcher/Aggregate/…) directly
```

## Quickstart

```ocaml
module C = Minimongo

let () =
  let users = C.create () in

  (* insert — an _id is minted if you omit one *)
  let _ = C.insert users (Bson.doc [ ("name", Bson.str "Ada"); ("age", Bson.int 36) ]) in
  let _ = C.insert users (Bson.doc [ ("name", Bson.str "Linus"); ("age", Bson.int 54) ]) in

  (* find with a selector + sort + projection *)
  let names =
    C.fetch
      (C.find users
         ~selector:(Bson.doc [ ("age", Bson.doc [ ("$gte", Bson.int 18) ]) ])
         ~sort:(Bson.doc [ ("age", Bson.int (-1)) ])
         ~fields:(Bson.doc [ ("name", Bson.int 1); ("_id", Bson.int 0) ])
         ())
  in
  (* names = [ {name: "Linus"}; {name: "Ada"} ] *)

  (* findOne / update *)
  let _ada = C.find_one users ~selector:(Bson.doc [ ("name", Bson.str "Ada") ]) () in
  let _ = C.update users
            (Bson.doc [ ("name", Bson.str "Ada") ])
            (Bson.doc [ ("$inc", Bson.doc [ ("age", Bson.int 1) ]) ]) in

  (* aggregate — pure transforms over the documents *)
  let avg =
    C.aggregate users
      [ Bson.doc [ ("$group", Bson.doc [ ("_id", Bson.null);
                                         ("avgAge", Bson.doc [ ("$avg", Bson.str "$age") ]) ]) ] ]
  in
  ignore (names, avg);

  (* react to changes — fires immediately on every matching mutation *)
  let h = C.observe_changes (C.find users ())
            ~added:(fun id _fields -> Printf.printf "added %s\n" id) () in
  let _ = C.insert users (Bson.doc [ ("name", Bson.str "Grace") ]) in
  h.C.stop ()
```

Selectors, update documents, and projections are all just `Bson.t` documents — exactly the shapes
you'd write in a MongoDB shell.

## Module map

**Tier 1 — what most code uses:**

| Module | Role |
|---|---|
| `Bson` | the value type + constructors (`doc`/`str`/`int`/`bool`/`float`/`array`/…), typed accessors, `equal`/`compare`, `to_string` |
| `Minimongo` | the collection: `create` / `insert` / `update` / `remove` / `find` / `fetch` / `find_one` / `count` / `aggregate` / `observe_changes` / `observe` |

**Tier 2 — the `Query` engine** (reached *through* `Minimongo`; call directly only for standalone /
collection-less use): `Matcher` (selector matching), `Modifier` (updates), `Projection`, `Sorter`,
`Aggregate` + `Expr` (pipeline + expressions), `Geo` (geo predicates), `Id` (id generation).

## Feature coverage

**Query selectors** — comparison (`$eq` `$ne` `$gt` `$gte` `$lt` `$lte` `$in` `$nin`), logical
(`$and` `$or` `$nor` `$not`), element (`$exists` `$type`), evaluation (`$mod` `$regex`), array
(`$all` `$elemMatch` `$size`), bitwise (`$bitsAllSet`/`AllClear`/`AnySet`/`AnyClear`), and geospatial
(`$geoWithin` `$near` `$nearSphere` `$geoIntersects` with `$geometry`/`$box`/`$center`/
`$centerSphere`/`$polygon`/`$maxDistance`/`$minDistance`). Equality is numeric-cross-type and
array-aware (a scalar matches an array element); range comparison is type-scoped.

**Update operators** — `$set` `$unset` `$inc` `$mul` `$min` `$max` `$rename` `$setOnInsert` `$push`
(with `$each`/`$position`/`$sort`/`$slice`) `$addToSet` `$pull` `$pullAll` `$pop` `$bit`.

**Projection** — nested dotted include/exclude, `$slice`, `$elemMatch`.

**Aggregation stages** — `$match` `$project` `$addFields`/`$set` `$unset` `$sort` `$limit` `$skip`
`$count` `$unwind` `$group` `$sortByCount` `$sample` `$replaceRoot`/`$replaceWith` `$lookup`
`$unionWith` `$facet` `$bucket`. **Expressions** — field paths, `$$ROOT`/`$$CURRENT`/`$$<var>`,
arithmetic, comparison, boolean, conditional (`$cond`/`$ifNull`/`$switch`), string, array
(`$map`/`$filter`/`$arrayElemAt`/…), type/conversion; **accumulators** `$sum`/`$avg`/`$min`/`$max`/
`$first`/`$last`/`$push`/`$addToSet`/`$count`.

## Intentionally not supported

- **`$where` / `$jsonSchema`** — arbitrary JavaScript / schema validation.
- **`$text`** — needs a text index + scoring.
- **Positional update operators** `$` / `$[]` / `$[<id>]` and **`$currentDate`** — need the query
  selector / arrayFilters / a clock; handled one layer up.
- **Positional projection** `$`.
- **`$near` proximity *sorting*** — the distance *filter* (`$maxDistance`/`$minDistance`) is
  implemented; result ordering by distance is a cursor concern.
- **`$sample`** is a deterministic head sample (the pure engine has no RNG).
- Date-expression operators (`$year`/`$dateToString`/…).
- An unknown query operator never hides a document; an unknown aggregation stage passes input
  through unchanged.

## Server and browser

Everything here is pure OCaml — `Bson` and `Minimongo` are Stdlib-only, and the `Query` engine's
only dependency beyond `Bson` is the pure [`re`](https://github.com/ocaml/ocaml-re) library — so the
whole engine compiles to JavaScript via **js_of_ocaml** and the *same* `Minimongo` runs on the server
and as a client-side cache. Injection seams keep a JS build clean: `Minimongo.create ?gen_id` and
`Query.Id.random_id ?rng` let you supply the id / randomness source (e.g. `Math.random`).
