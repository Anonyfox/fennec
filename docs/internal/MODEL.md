# MODEL — typed collections (design rationale)

> **Practical guide / current API:** see [`fennec/pulse/sift/README.md`](../../fennec/pulse/sift/README.md)
> and the per-module `.mli` files. This doc is the *why* — the design reasoning behind the typed-collection
> layer, kept for contributors. It is built and shipping.

The problem this layer closes: without it every userland touchpoint speaks `Bson.t` — field names as bare
strings, per-component defensive matching, silent drift on rename. That was Meteor's forever-weak spot
(simple-schema / collection2 / astronomy — three generations of runtime bolt-ons fighting the absence of
types). We have what they never had: **a compiler on both sides of the wire.** The rule for the layer:
*let OCaml play its strengths inside; keep the surface a beginner writes flat and obvious.*

## The keystone — one declaration drives everything

`'a Sift.t` wraps a **GADT type representation** (`Shape`), so a field spec carries structure, not just a
pair of opaque encode/decode functions. From that single description the framework derives:

1. **the codec** — encode/decode with field-named, path-tagged errors;
2. **the mongod `$jsonSchema` validator** — the DATABASE rejects foreign writes that violate the shape
   (`Json_schema.validator`; minimongo enforces the identical rule in-engine). Nobody has this from one
   declaration — Prisma doesn't validate foreign writes, Mongoose validates only its own;
3. **typed field handles** (`Fields.x`) powering `Filter` / `Update` / `Sort` / `Index` / `Projection`;
4. **derived pretty-printing** (`Sift.show`), and later, for free, OpenAPI emission + a generic admin UI
   (the Django-admin primitives are exactly field names + types + codecs).

Naming follows the house rule: Meteor-family words stay literal (`collection` / `Method` / `publish` /
`subscribe`), themed names are for fennec-original layers. The decorator is `[@@deriving collection]`.

## What the dev writes (the entire surface)

One model module — a plain OCaml record plus one attribute, compiled into both the server and the browser
bundle:

```ocaml
(* store/task.ml — the WHOLE model, common case: zero field annotations *)
type t = { id : string; title : string; done_ : bool; tags : string list }
[@@deriving collection ~name:"tasks"]
```

The deriver puts the reactive READ verbs straight on the module (`Task.find` / `Task.project` — Meteor's
collection object), generates the `Fields` handles + the codec, and a `collection` declaration. Queries
read as expressions via the DSLs (resolved against `Fields` in scope, so a wrong field/value is a compile
error):

```ocaml
open Task
let live  = Task.find ~where:[%q done_ = false] ~sort:[%sort title asc] ()
let cards = Task.project [%fields title] ()
let () = update tasks ~where:[%q id = tid] [%set done_ = true]   (* Update modifiers, typed *)
```

Convention over annotation — the deriver applies the rules a reader would guess: `id`/`_id` → `"_id"`
(ObjectId-coerced); a trailing underscore is a keyword escape, stripped for the wire key (`done_` →
`"done"`); `option` decodes absent as `None`, `list` as `[]`. `[@key "wire"]` overrides the key. The
combinators remain the truth; **the ppx is only the pen** — its expansion is the hand-written builder
form, golden-tested byte-for-byte (`test_collection_ppx`).

## Validation — opt-in, stackable, airtight by path

Checks are opt-in (`[@check]` / the inline catalog) and STACK in declaration order, each with its own
message; errors COLLECT (every failing field, not first-fail — forms need the full list). Two kinds:

- **Structured refinements** (`min_len`/`max_len`/`pattern`/`min`/`max`/`one_of`/…) carry their meaning in
  the shape, so they ALSO translate into the mongod `$jsonSchema` (minLength, pattern, enum, …) — the
  database enforces them against foreign writers.
- **Arbitrary predicates** (`[@check fun v -> …]`) run at every app boundary but can't be pushed into
  mongod — documented honestly as app-side-only.

**Airtight = every path a value travels is covered:** ① method args (decode + checks → 422 before the
handler); ② server writes (`encode_checked` validates → an invalid value can't reach the DB through the
typed layer); ③ optimistic stubs (same checks; `Sift.validate` for instant offline form feedback); ④
foreign writers (structured refinements via the installed `$jsonSchema`); ⑤ reads of legacy/garbage docs
(decode runs checks; failures surface under the skip-count-warn policy, never a silent mis-render).

### The catalog (each entry is a shape node; presets are named nodes)

| need | surface | $jsonSchema |
|---|---|---|
| string length / emptiness | `[@min_len n]` `[@max_len n]` `[@non_empty]` | minLength/maxLength |
| string shape | `[@matches "re"]`; presets `[@email]` `[@url]` `[@slug]` | pattern |
| enumeration | `[@one_of ["draft";"live"]]` | enum |
| normalization | `[@trim]` `[@lowercase]` — run BEFORE checks, both directions | — |
| numeric bounds | `[@min n]` `[@max n]` `[@positive]` | minimum/maximum |
| float sanity | nan/inf REJECTED by default | bsonType double |
| list shape | `[@min_items n]` `[@max_items n]` `[@unique_items]` | minItems/maxItems/uniqueItems |
| optional keys | `'a option` — absent/null → `None`; `None` omits the key | required omission |
| dynamic-key maps | `Sift.str_map` (Mongo subdocs as dicts) | additionalProperties |
| nested records | a field typed `M.t` where `M` also derives — paths nest (`address.zip: …`) | properties (recursive) |
| polymorphic docs | OCaml variants over a discriminator (`Sift.variant ~tag`) | oneOf per case |
| cross-field rules | record-level `checking (fun t -> pred) "msg"` | — |
| anything else | `[@check fun v -> pred]` | — |

Named NON-members (each has a better home): **uniqueness** → `Index.unique` (a DB guarantee, not a
predicate); **foreign-key existence** → handler logic (a codec check must stay pure — no IO);
**authorization** → methods (the blessed path, see METHODS.md).

## Taste decisions (each a Meteor scar avoided)

- **`Filter`/`Update`/`Sort` are functions, not a parser** — typed against each handle, compiling to the
  same Bson the engine runs; `.raw` keeps the full Mongo surface reachable. No string DSL, no magic
  operators.
- **Projections are objects, not phantom records** — `[%fields title]` yields `< title : _ >`; the full
  record is never built, so a projected-away field is *unmentionable*, not `undefined`. (See
  `projection.mli`.)
- **`_id` is the author's choice** — include `Sift.doc_id` in the record (typed `string`, ObjectId-coerced)
  or omit it; no forced wrapper.
- **Malformed-doc policy is one deliberate default** — typed reads SKIP docs that fail decode, count them,
  warn once; the UI never crashes on foreign garbage. `find_results` exposes the per-doc verdict for code
  that must care. Writes never skip (encode is total).
- **Evolution without migrations** — `opt` fields + defaults are forward-tolerant readers (additive first;
  rename = add+backfill+drop; the validator updates by deploy). No migration framework.

## What we deliberately do NOT build

No ORM, no relations DSL (joins stay `$lookup`), no lazy-loading proxies, no identity map, no migration
framework. Each is a tar pit with a worse replacement already in the stack.
