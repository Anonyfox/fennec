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

`[@@deriving collection]` turns the record into the codec, the typed `Fields` handles, the collection
(+ its `$jsonSchema` validator), and the **methods on the module** — `Ticket.create` / `save` / `delete`
/ `find_one` / `where` / `all` / `count` (the server write+read verbs) next to `Ticket.find` / `project`
(the client reactive readers). The codec **is the validation** (an invalid ticket cannot be written).
`[%index status]` declares an index, co-located, reconciled at boot.

**Every collection is declared the same way** — there is no "server-only" variant. A ticket can become
client-relevant any time (the client reactive cursor `[@@deriving collection]` also generates is just
dead code until a jsoo bundle links the model — zero runtime cost otherwise). Less to learn, nothing to
migrate. Group files into subfolders freely; `(include_subdirs unqualified)` folds the whole tree into
one flat-namespace library.

**You don't write data here, just declare it.** The writes happen in [`../workflows/`](../workflows/)
(or a method handler) via those generated methods — so every change goes through an explicit function,
which is what makes `@after` catch every change. The methods run server-side over an isomorphic seam the
framework installs at boot; a *client* changes data through a method, never the collection directly.
