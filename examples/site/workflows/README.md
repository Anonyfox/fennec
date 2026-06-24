# workflows/ — your business logic (the 2-minute teach)

A top-level peer of `web/` and `collections/`. Workflows are **ordinary OCaml functions** over your
data. What makes them special is invisible: each runs inside **one transaction**, and you attach
**reactions** to them with annotations. There is no `db` passed around — data just exists.

Read [`tickets.ml`](tickets.ml) top to bottom; it shows everything.

### 1. A workflow is a function. It's atomic.

```ocaml
let open_ticket =
  W.make "open_ticket" (fun subject ->
      let t = W.call (C.create Ticket.collection) { Ticket.id = ""; subject; status = "open"; opened_at = stamp () } in
      ignore (W.call (C.create Ticket_event.collection) { Ticket_event.id = ""; ticket_id = t.Ticket.id; action = "opened"; at = stamp () });
      t)
```

`open_ticket` writes **two collections**. They commit together or not at all: if anything `raise`s
(say the subject fails `[@non_empty]`), *neither* row is written. You wrote no transaction code —
`W.call` runs the body in the transparent transaction. Control flow is just `raise`.

### 2. A transition is the write API. It's guarded.

```ocaml
let close_rule (t : Ticket.t) =
  if t.Ticket.status <> "open" then failwith "ticket is not open";   (* a raise vetoes the whole call *)
  { t with Ticket.status = "closed" }

let close = C.transition Ticket.collection "close" close_rule
```

`close` is a named transition — the *only* way a ticket's status changes. Calling `W.call close t`
validates, persists by `_id`, and rolls back if the rule raises. The pure rule (`close_rule`) is a
plain function, so it unit-tests with no database (see the `let%test` at the bottom of the file).

### 3. Reactions are annotations.

```ocaml
let[@after close] notify_closed (t : Ticket.t) =
  Printf.printf "ticket %s closed — notifying the reporter\n%!" t.Ticket.id
```

`[@after close]` fires *after* the close commits, isolated (a failure here never rolls back the close),
and receives the workflow's **result**. `[@before f]` is the mirror: a guard that runs *in* the
transaction and may `raise` to veto. Reactions are real references — go-to-references on `close` lists
everything that fires around it, and a reaction graph that loops is a **compile error** (see the core
README).

### 4. Schedules are annotations too.

```ocaml
let[@cron "0 * * * *"] auto_close_stale () =
  C.find Ticket.collection ~where:[ Filter.raw (Bson.doc [ ("status", Bson.str "open") ]) ]
  |> List.iter (fun t -> try ignore (W.call close t) with _ -> ())
```

`[@cron]` (or `[@every 60.]`) registers a job that runs **at-most-once across all your replicas** — no
configuration, no duplicate runs. The `unit -> unit` shape is forced by the annotation.

---

That's the whole non-web core. `module C = Pulse.Collection` and `module W = Pulse.Workflow` come from
`open`ing `Fennec_pulse_app`. The server wires it in `server.ml`'s `setup_realtime` (seed via the
workflow, `Pulse.publish` the collections). Full design: `docs/internal/CORE-LAYER.md`. Runtime
reference: `fennec/pulse/workflow/README.md`.
