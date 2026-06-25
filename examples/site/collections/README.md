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
[@@deriving collection ~name:"tickets"]

let () = [%index status]   (* declared indexes, reconciled at boot *)
```

`[@@deriving collection]` turns the record into the codec, the typed `Fields` handles, and the
collection (+ its `$jsonSchema` validator) — and **the codec is the validation** (an invalid ticket
cannot be written). `[%index status]` declares an index, co-located, reconciled at boot.

**Every collection is declared the same way** — there is no "server-only" variant. A ticket can become
client-relevant any time (the client reactive cursor `[@@deriving collection]` also generates is just
dead code until a jsoo bundle links the model — zero runtime cost otherwise). Less to learn, nothing to
migrate. Group files into subfolders freely; `(include_subdirs unqualified)` folds the whole tree into
one flat-namespace library.

**You never write to a collection here.** Writes go through the named transitions in
[`../workflows/`](../workflows/) — so every change goes through an explicit function, and that is what
makes `@after` catch every change.
