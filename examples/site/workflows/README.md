# workflows/ — your business logic (the 2-minute teach)

A top-level peer of `web/` and `collections/`. Workflows are **ordinary OCaml functions**. You tag a
business function `[@workflow]` and write normal imperative code inside — the transaction and the
reactions are invisible. There is no `db` passed around, no wrappers, no ceremony.

Read [`tickets.ml`](tickets.ml) top to bottom; it shows everything.

### 1. A workflow is a function. It's atomic.

```ocaml
open Fennec_pulse_app   (* the data verbs: create / save / find, and the [@workflow] machinery *)

let[@workflow] open_ticket subject =
  let t = create Ticket.collection { Ticket.id = ""; subject; status = "open"; opened_at = stamp () } in
  ignore (create Ticket_event.collection { Ticket_event.id = ""; ticket_id = t.Ticket.id; action = "opened"; at = stamp () });
  t
```

`open_ticket` writes **two collections** and you call it like any function: `open_ticket "Printer on
fire"`. They commit together or not at all — if anything `raise`s (say the subject fails
`[@non_empty]`), *neither* row is written. You wrote no transaction code; `[@workflow]` runs the body in
one transparent transaction. `create` / `save` / `find` are plain calls.

### 2. A transition is just a workflow that saves.

```ocaml
let[@workflow] close (t : Ticket.t) =
  if t.Ticket.status <> "open" then failwith "ticket is not open";   (* a raise vetoes the whole call *)
  save Ticket.collection { t with Ticket.status = "closed" }
```

No special "transition" type — `close` reads, changes, and `save`s. Call it `close ticket`; it
validates, persists by `_id`, and rolls back if the body raises. Control flow is just `raise`.

### 3. Reactions are annotations.

```ocaml
let[@after close] notify_closed (t : Ticket.t) =
  Printf.printf "ticket %s closed — notifying the reporter\n%!" t.Ticket.id
```

`[@after close]` fires *after* the close commits, isolated (a failure here never rolls back the close),
and receives the workflow's **result**. `[@before f]` is the mirror — a guard that runs *in* the
transaction and may `raise` to veto. Reactions are real, type-checked references: go-to-references on
`close` lists everything that fires around it, rename is safe, and a reaction graph that loops is a
**compile error** (the ppx checks intra-module; a cross-module loop is a module dependency cycle).

### 4. Schedules are annotations too.

```ocaml
let[@cron "0 * * * *"] auto_close_stale () =
  find Ticket.collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "open") ]) ]
  |> List.iter (fun t -> try ignore (close t) with _ -> ())
```

`[@cron]` (or `[@every 60.]`) registers a job that runs **at-most-once across all your replicas** — no
configuration, no duplicate runs. The `unit -> unit` shape is forced by the annotation.

---

That's the whole non-web core. `open Fennec_pulse_app` brings the data verbs and the `[@workflow]`
machinery. The server wires it in `server.ml`'s `setup_realtime` (seed by *calling* the workflow,
`Pulse.publish` the collections). Full design: `docs/internal/CORE-LAYER.md`. Runtime reference:
`fennec/pulse/workflow/README.md`.
