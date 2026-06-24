# collections/ — your data, one concept per file

A top-level peer of `web/` and `workflows/`. Each file is **one persisted concept**: a plain record
with its validation catalog inline as attributes, plus the collection (name + indexes).

```ocaml
(* collections/ticket.ml *)
type t = {
  id : string;
  subject : string; [@trim] [@non_empty] [@max_len 120]
  status : string;  [@one_of [ "open"; "closed" ]]
  opened_at : string;
}
[@@deriving model]

let collection = Def.v ~indexes:Index.[ asc Fields.status ] "tickets" codec
```

`[@@deriving model]` turns the record into the codec + the typed `Fields` handles, and **the codec is
the validation** — an invalid ticket cannot be written. `Def.v` names the collection and declares its
indexes (reconciled at boot).

These models are **server-only** (the SSR binary links them; clients see the data through a publication,
not by linking the model), so they use `[@@deriving model]` + `Def.v` rather than `[@@deriving
collection]` — no client reactive cursor is generated. Group files into subfolders freely;
`(include_subdirs unqualified)` folds the whole tree into one flat-namespace library.

**You never write to a collection here.** Writes go through the named transitions in
[`../workflows/`](../workflows/) — so every change goes through an explicit function, and that is what
makes `@after` catch every change.
