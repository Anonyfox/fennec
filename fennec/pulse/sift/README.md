# Sift — fennec's typed-data toolkit

Sift is how fennec says **what a value is**. You write the shape of a record once, and from that single
description Sift gives you everything fennec needs of a document:

- **BSON** encode / decode (the form the storage layer speaks),
- **validation** (the same checks on the server and in the browser, for forms),
- **JSON** I/O — plain JSON for HTTP APIs, extended JSON (`$oid`/`$date`) for mongosh interchange,
- **Mongo** selectors / updates / sorts / indexes / `$jsonSchema`,
- **structural diff** for reactive sync, **DDP method arguments**, and `$jsonSchema` **reflection**.

It is built directly on `Bson.t` (the tree the storage layer already uses) — no neutral data model, no
format abstraction, no separate zero-copy engine. Simple to read, simple to reason about. Everything is
**pure and jsoo-safe**, so the *same* codec runs server-side and in the browser.

> Sift lives inside fennec as the `fennec.pulse.sift` submodule. It is not a standalone package — it is
> the building block the framework brings for typed Mongo work, end to end.

## The three libraries

| Library | What it is |
|---|---|
| `fennec.pulse.sift` | the codec: the shape language + encode/decode/validate/JSON/diff/args/reflection. The flat `Sift.*` API. |
| `fennec.pulse.sift.mongo` | the pure Mongo toolkit built on a codec: `Filter` / `Update` / `Sort` / `Index` / `Projection` / `Json_schema` / `Def`. No runtime — just data → Bson artifacts. |
| `fennec.pulse.sift.ppx` (`.rules`) | the DX: the `[@@deriving model]` / `[@@deriving collection]` derivers and the `[%q]`/`[%fields]`/`[%sort]`/`[%set]`/`[%index]` query DSL. |

## The DX (what you actually write)

```ocaml
type t = {
  id : string;                                    (* a field named id/_id ⇒ the _id (ObjectId-aware) *)
  title : string; [@non_empty] [@max_len 200]     (* inline validators stack *)
  done_ : bool;                                    (* trailing _ stripped for the wire key → "done" *)
  tags : string list;                              (* list ⇒ tolerates absence (opt_list) *)
  note : string option;                            (* option ⇒ absent/null = None, None omits the key *)
}
[@@deriving collection ~name:"tasks"]
```

That one line generates: `module Fields` (typed field handles), `let codec` (the validating codec), `let
collection` (the declaration), and the reactive read verbs `find` / `project`. Then query it with the
DSL, resolved against `Fields` so a wrong field or value is a compile error:

```ocaml
let urgent = find ~where:[%q done_ = false && priority > 3] ~sort:[%sort priority desc] ()
let titles = project [%fields title; author / name] ~where:[%q done_ = false] ()
let () = update tasks ~where:[%q id = the_id] [%set done_ = true]
```

`[@@deriving model]` is the same minus the store tail — for forms, JSON APIs, and method arguments.

## Internal module map (for contributors)

Inside `fennec.pulse.sift` (you use the flat `Sift.*` surface; these are the pieces behind it):

- `Shape` — the GADT type representation + the error / refinement-hint types.
- `Combinators` — the builder DSL: `string`/`int`/`req`/`opt`/`obj`/`seal`/`check`/the validators/variants.
- `Codec` — the codec value `'a t`, encode-side `validate`, pretty-printing.
- `Engine` — normalization, the refinement-check phase, `needs_checks`, `pretty`.
- `Bson_engine` — the shape-directed `Bson.t` read / write / size. **This is encode/decode.**
- `Json_writer` / `Json_reader` — plain relaxed JSON (the HTTP wire form).
- `Derived` — free shape-derived `equal` / `compare` / `hash` / `default` / `arbitrary`.

The ppx (`fennec.pulse.sift.ppx.rules`) is three small modules — `Sift_ppx_rules` (the `model` deriver +
the shared `model_core`), `Sift_mongo_ppx_rules` (the query DSL), `Collection_deriver` (the `collection`
tail). `fur.ppx` links all three so every `.mlx`/`.ml` on `(pps fennec.fur.ppx)` gets the whole DX in
one ppx pass.

## Design rules (please keep)

- **`Bson.t`, not a neutral model.** Encode/decode is one tree pass. The `bson` codec is the dynamic
  escape (any raw `Bson.t`).
- **Pure + jsoo-safe.** No `Unix`, no `Bigarray` tricks, nothing that won't cross-compile to JavaScript —
  the codec is shared verbatim by the server and the browser.
- **The shape is the single source of truth.** Decode, encode, validation, JSON, diff, reflection and the
  Mongo toolkit all derive from one `'a Sift.t`. Add a feature at the shape level and every derivation
  gets it for free.
- **Mongo queries *are* Bson.** `sift.mongo` produces `Bson.t` selectors/updates; it owns no runtime.
