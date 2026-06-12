# MODEL — typed data modeling over collections (design)

Status: **design, pre-implementation.** The last untyped hole in the vertical: today every userland
touchpoint speaks `Bson.t` — field names as bare strings, per-component defensive matching, silent
drift on rename. This was Meteor's forever-weak spot (simple-schema / collection2 / astronomy: three
generations of runtime bolt-ons fighting the absence of types, each wrapping writes and each with
seams). We have what they never had: a compiler on both sides of the wire. The design rule for this
layer: **let OCaml play its strengths inside; keep the surface a beginner writes flat and obvious.**

## What the dev writes (the entire surface)

One model module, by convention, shared (compiles into server and browser bundle alike) — a PLAIN
OCaml record plus one deriving attribute (the official surface):

```ocaml
(* store/task.ml — this is the WHOLE model (common case: ZERO field annotations) *)
type t = { id : string; title : string; done_ : bool; tags : string list }
[@@fennec.model "tasks"]

let () = Model.index model [ Index.asc Fields.done_; Index.unique [ Index.asc Fields.title ] ]
```

Convention over annotation — the deriver applies the rules a reader would guess:

- a field named `id`/`_id` maps to `"_id"` with ObjectId coercion (no [@id] needed);
- a trailing underscore is ALWAYS an OCaml keyword escape (`done_`, `type_`, `end_` — `done` is a
  reserved word, the underscore is the standard community convention, not ours), so the deriver
  auto-strips it for the wire key: `done_` → `"done"`. House style: prefer non-colliding domain
  words first (`completed` beats `done_`); escape only when the vocabulary genuinely collides;
- `option` fields decode absent as `None`; `list` fields decode absent as `[]` (Mongo-idiomatic);
- the collection name stays an EXPLICIT string — deriving it from the module name needs English
  pluralization inflection, a Rails scar (surprising on person/status) we refuse to import.

Attributes exist only for deviations: `[@check fun s -> ...]` (validation), `[@key "wireName"]`
(a wire key that isn't a keyword escape), `[@default v]` (a non-obvious default).

One writing site, zero duplication, and the record stays 100% vanilla OCaml: dot access, pattern
matching, merlin hover/completion all work natively (no synthetic types). The deriver — one small
framework-owned ppxlib rewriter (the same machinery the jsoo ppx already links; fennec already owns
syntax where it pays: mlx, [%%style]) — generates the `Fields` module of typed handles, the
GADT-backed codec, and the `Model.define`. **The combinators remain the truth; the ppx is only the
pen**: its expansion is documented, auditable, and hand-writable.

The hand-written fallback (also what the ppx targets) is the record-builder form — linear, one line
per field, no positional make/split tuples:

```ocaml
let model = Model.(
  record (fun id title done_ tags -> { id; title; done_; tags })
  |> field  "_id"   doc_id        (fun t -> t.id)
  |> field  "title" string        (fun t -> t.title)
  |> field  "done"  bool          (fun t -> t.done_)
  |> fieldd "tags"  (list string) ~default:[] (fun t -> t.tags)
  |> seal "tasks")
```

Use it when the deriver doesn't fit (computed fields, exotic codecs); it is the same checked
machinery, just written by hand. Meteor's SimpleSchema had one writing site and zero static
checking; we get one writing site WITH the compiler — the trade they wanted and couldn't have.

Everything downstream of `define` is then typed:

```ocaml
(* server *)
let tid = Model.insert model { id = ""; title; done_ = false; tags = [] }  (* id minted when "" *)
Model.publish model "tasks" (fun _ -> Model.cursor model ~where:Q.[ eq done_ false ] ())

(* methods: the model's codec IS an argument codec — no second declaration *)
let add_task = Method.define "addTask" ~args:(Codec.a1 (Model.codec model)) ~result:Codec.string ...

(* component — typed all the way into the view *)
let tasks = Model.find client model ~where:Q.[ eq done_ false ] () in
(each (Fur.get tasks) (fun task -> <li key=task.id>(node task.title)</li>))

(* writes through field handles — a renamed field is a compile error everywhere *)
Model.update model ~where:Q.[ eq id tid ] M.[ set done_ true; push tags "urgent" ]
```

`task.title` instead of `match Bson.get doc "title" with Some (Bson.String s) -> s | _ -> ...` —
that line is the whole pitch.

## The keystone: a GADT runtime-type core, invisible from outside

Today `Codec.t` is a pair of opaque functions — nothing can be derived from it. The redesign makes
field specs carry **structure**: internally, `'a Codec.t` wraps a GADT type representation

```ocaml
type _ ty = String : string ty | Int : int ty | Bool : bool ty | Float : float ty
          | List : 'a ty -> 'a list ty | Option : 'a ty -> 'a option ty
          | Obj : ... (* field list *) | Check : ('a -> bool) * string * 'a ty -> 'a ty | ...
```

Userland never sees it — they write `Codec.string`, `Codec.req`, `obj4`, exactly as today. But from
that one representation the framework derives, now and later:

1. **The codec** (encode/decode with field-named errors) — as today.
2. **The mongod `$jsonSchema` validator** — installed on the collection at boot (`Model.define` →
   `collMod`/create with validator). The DATABASE now rejects foreign writes that violate the
   declared structure; minimongo enforces the identical rule in-engine. This answers "Mongo can
   hold anything": shape is enforced at the write boundary of every path, including paths that
   aren't us. Nobody has this from one declaration — Prisma's schema doesn't validate foreign
   writes, Mongoose validates only its own.
3. **Typed field handles** (name + ty) powering `Q.`/`M.`/`Index.`/projections.
4. Later, for free: **OpenAPI emission** from method declarations, and the **admin UI** (a generic
   live CRUD screen needs exactly: field names, types, and codecs — the Django-admin primitives).

Predicates (`Codec.check`) split deliberately: structural facts go into `$jsonSchema`; arbitrary
checks run at the boundary on BOTH sides — which means **an optimistic stub validates with the
same check the server enforces**: instant, offline-capable form errors with zero duplicated
validation logic. Ecto changesets without the second language.

## Taste decisions (each one a Meteor scar avoided)

- **`Model` is the recommended path; `Collection` remains the dynamic substrate** and the escape
  hatch (aggregations, migrations-by-hand, weird documents). Two named layers, no wrapping of one
  by hidden monkey-patching (collection2's sin: it silently wrapped `insert` and broke composability).
- **Selectors/modifiers are functions, not a parser**: `Q.eq f v`, `Q.all [...]`, `Q.lt`, `M.set`,
  `M.push`, `M.inc` — typed against each handle's `ty`, compiling down to the same Bson the engine
  already executes. `Q.raw bson` keeps the full Mongo operator surface reachable. No string DSL, no
  magic comparison operators by default (an optional `Q.O` for taste-holders).
- **Projections are tuples, not phantom records**: `Model.project client model P.(f2 title done_)`
  → `(string * bool) array signal`. Composable, no new type declarations, no partial-record lies.
  Full `find` returns whole `t`s (the live cache holds whole docs anyway).
- **`_id` is the author's choice**: include `Codec.doc_id` in the record (typed `string`, ObjectId
  coerced — the RX6 lesson baked in) or omit it and use `(id, t)` pair reads. No forced wrapper
  record, no `.v` hop.
- **Malformed-doc policy is one deliberate default, not per-call-site choice**: typed reads SKIP
  documents that fail decode, count them, and warn once per doc — the UI never crashes on foreign
  garbage, the dev sees it immediately (and the agent fastlane surfaces the warning).
  `Model.find_results` returns `('a, Decode_error.t) result` per doc for code that must care.
  Writes never skip: an encode is total by construction.
- **Evolution without migrations**: `opt` fields + defaults are forward-tolerant readers — the
  Mongo-idiomatic discipline, documented with patterns (additive first, rename = add+backfill+drop,
  version field for hard breaks). No migration framework; the validator is updated by deploy.
- **The optimistic path is typed too**: `sim_writes` gains model-aware forms so a stub writes
  `{ title; done_ = false }`, not hand-built Bson — the stub and the handler share the model value
  the way they already share the method value.

## What we deliberately do NOT build

No ORM, no relations DSL (joins stay `$lookup` — already cross-collection on both sides), no
lazy-loading proxies, no identity map, no migration framework, no deriving ppx. Each is a tar pit
with a worse replacement already in the stack.

## Phases (bottom-up, each provable)

1. **Codec core rebuild on the GADT `ty`** — same public combinators, plus introspection;
   field-named errors throughout; `check`; `doc_id`; the record-builder form (`record |> field
   |> seal`). (Pure; heavy unit tests.)
2. **The `[@@fennec.model]` deriver** — a small framework-owned ppxlib rewriter targeting the
   record-builder; expansion golden-tested (ppx output = the hand-written form, byte-compared);
   convention rules (id → "_id", trailing-underscore strip, option/list absent-tolerance) tested
   alongside the deviation attributes ([@key] [@check] [@default]).
3. **`$jsonSchema` derivation** (pure generation, golden tests) + minimongo write-validation hook
   + driver `collMod` install at define-time.
4. **`Model.define` + typed reads/writes server-side** over the existing Collection, `Q`/`M`/
   `Index` handles, index ensure at boot.
5. **Client: typed live `find`/`project` + skip-count-warn policy + typed stub writes.**
6. **Example app converts to a model module; TERRAIN/METHODS/README updates; the `Bson.get`
   defensive dance deleted from the example.**

The acceptance bar: the example component contains zero `Bson.get` calls, a field rename is a
compile error in every file that touches it, and a foreign writer inserting garbage shows up as a
counted skip in dev — not a silent "(untitled)".
