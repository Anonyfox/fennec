# workflows/ — your business logic (the 2-minute teach)

A top-level peer of `web/` and `collections/`. Workflows are **ordinary OCaml functions**. You tag a
business function `[@workflow]` and write normal imperative code inside — the transaction and the
reactions are invisible. Data changes through the collection's own **methods** (`Ticket.create`,
`Ticket.save`, `Ticket.where …`), Meteor-style — no `db`, no free-floating verbs.

Read [`tickets.ml`](tickets.ml) top to bottom; it shows everything. It opens its primary collection so
the methods + fields are in scope, exactly like a component does:

```ocaml
open Ticket   (* brings Ticket's fields AND its methods (create/save/where/find_one) into scope *)
```

### 1. A workflow is a function. It's atomic.

```ocaml
let[@workflow] open_ticket subject =
  let t = create { id = ""; subject; status = "open"; opened_at = stamp () } in
  ignore (Ticket_event.create { Ticket_event.id = ""; ticket_id = t.id; action = "opened"; at = stamp () });
  t
```

`open_ticket` writes **two collections** (`create` is `Ticket.create`, brought in by the `open`; the
audit row is qualified). You call it like any function: `open_ticket "Printer on fire"`. They commit
together or not at all — if anything `raise`s, *neither* row is written. `[@workflow]` runs the body in
one transparent transaction. No transaction code, no `db`.

### 2. A transition is just a workflow that saves.

```ocaml
let[@workflow] close (t : t) =
  if t.status <> "open" then failwith "ticket is not open";   (* a raise vetoes the whole call *)
  save { t with status = "closed" }
```

No special "transition" type — `close` reads, changes, and `Ticket.save`s. Call it `close ticket`.

### 3. Reactions are annotations.

```ocaml
let[@after close] notify_closed (t : t) =
  Printf.printf "ticket %s closed — notifying the reporter\n%!" t.id
```

`[@after close]` fires *after* the close commits, isolated, with the workflow's **result**. `[@before
f]` is the mirror — a guard that runs *in* the transaction and may `raise` to veto. Reactions are real,
type-checked references: go-to-references on `close` lists everything that fires around it, rename is
safe, and a reaction graph that loops is a **compile error**.

### 4. Schedules are annotations too — and the queries are typed.

```ocaml
let[@cron "0 * * * *"] auto_close_stale () =
  where [%q status = "open"] |> List.iter (fun t -> try ignore (close t) with _ -> ())
```

`where [%q status = "open"]` is the typed Meteor query (`Ticket.where` over the model's `Fields`) — reads
like `db.tickets.find({status: "open"})`, but `[%q status = 5]` is a compile error, and dotted subpaths
(`assignee.email`) stay typed. `[@cron]` (or `[@every 60.]`) runs the job **at-most-once across all your
replicas** — no configuration, no duplicate runs.

---

The collection methods (`create` / `save` / `delete` / `find_one` / `where` / `all` / `count`) come from
`[@@deriving collection]` — the same generation that gives the client its reactive `find`. They run
server-side over an isomorphic seam the framework installs at boot; a *client* still changes data
through a method, never the collection. Full design: `docs/internal/CORE-LAYER.md`.
