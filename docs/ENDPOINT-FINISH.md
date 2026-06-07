# Endpoint layer — finish list

Nine items, each independent, each making the developer's life better without asking them to
learn anything new. Ordered by dependency (earlier items unblock later ones). Every item gets
checked off only when it is FULLY done, airtight tested, proven correct, with the best
possible performance and DX.

The overarching goal: the Endpoint half of the Fennec library should match the quality of
the Conn/Paw half — maximum type-safety, zero unnecessary API surface, fast at scale,
and a DX that feels natural and delightful. Transparent excellence — the developer just
happens to get the very best.

---

## Checklist

- [x] **E1. `Fennec.mli` — lock the public API** (`dc5a33c`)
- [x] **E2. Collapse `Endpoint.use` / `Endpoint.add`** (`9fcfefa`)
- [x] **E3. Report all `Host_router.build` errors at once** (`475b086`)
- [x] **E4. `Server.run` returns a result, never calls `exit`** (`64f6aa7`)
- [x] **E5. Trie-based host router** (`233fba1`)
- [x] **E6. Unified error funnel** (`c4bfa6f`)
- [x] **E7. Route-matched middleware phase** (`bb9d713`)
- [x] **E8. Document the pipeline execution model** (`637a8e1`)
- [x] **E9. Collapse task list + final e2e proof** (all green: 63+147 unit, 6/6 e2e)

---

## E1. `Fennec.mli` — lock the public API

**What.** `fennec/app/fennec.ml` has no `.mli`. Every `let` and `module` in the file is
public — including `Dev_proto`, `Livereload`, the `started` Atomic, internal helpers. A user
writing `server.ml` sees all of it in autocomplete.

**Why.** The facade defines "what does a Fennec user need to know?" Every extra export is
cognitive load and a coupling surface. If a user accidentally depends on `Dev_proto`,
renaming an env var becomes a breaking change for them.

**How.** Write `fennec/app/fennec.mli` exporting exactly:
- `module Conn` (= `Fennec_paw.Conn`)
- `module Endpoint` (= `Fennec_server.Endpoint`)
- `module Paw` (the enriched one with batteries as submodules)
- `module Http` (= `Fennec_core.Http`)
- `module Cookie` (= `Fennec_core.Cookie`)
- `val is_dev : bool`
- `val static : ...` (the static-serving helper)
- `val web_source : ...` (the dev/prod web root source)
- `val parallel : (unit -> 'a) list -> 'a list`
- `val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b`
- `val serve : ?timeout:float -> ?max_conns:int -> Endpoint.t list -> unit`

Everything else (`Dev_proto`, `Livereload`, `started`, `dev_control`, `is_dev`'s
implementation) is internal. This is the single most impactful DX change — it defines what
Fennec IS from the outside.

**Verify.** `dune build` succeeds; no example or test references a now-hidden module
(`grep -rn "Fennec.Dev_proto\|Fennec.Livereload\|Fennec.started"` returns nothing); the
example's `server.ml` compiles unchanged (it uses only `Endpoint`, `Paw`, `Conn`, `serve`,
`static`). The `.mli` becomes the canonical API reference.

**Files.** New `fennec/app/fennec.mli`. Possibly remove re-export lines from `fennec.ml`
(keep the internal `module Dev_proto = ...` for use within the file, just don't expose it).

---

## E2. Collapse `Endpoint.use` / `Endpoint.add`

**What.** `endpoint.ml` defines `add` (internal) and `use` (public), both doing
`{ t with paws = t.paws @ [p] }`. They are literally the same function with two names.
Every verb shortcut (`get`, `post`, …) calls `add` internally.

**Why.** Two names for the same operation is a "which do I pick" question. `add` leaking
into the namespace alongside `use` confuses contributors reading the source.

**How.** Rename the internal helper to something obviously internal (or inline it — it's one
line). The `.mli` already only exposes `use` and `pipe`; confirm `add` is not in it. Define
`pipe` in terms of `use`: `let pipe paws t = List.fold_left (Fun.flip use) t paws`. One
implementation path, not two.

**Verify.** `dune build` + full unit suite green. No functional change — this is naming only.

**Files.** `fennec/server/endpoint.ml`.

---

## E3. Report all `Host_router.build` errors at once

**What.** `Host_router.build` stops at the first error and returns it. A config with three
problems requires three fix → run → fix → run cycles.

**Why.** DX: the developer should see every misconfiguration in one shot and fix them all in
one pass.

**How.** Change the return type: `(_, error) result` → `(_, error list) result`. The `build`
logic collects all errors (bad names, duplicate names, bad patterns, duplicate patterns,
multiple catch-alls) into a list. `describe_error` becomes `describe_errors` (joins them
with newlines). `fennec/app/fennec.ml`'s error print path uses the new list formatter.

**Verify.** Test: a config with TWO problems (e.g. a bad name AND a duplicate pattern)
returns BOTH errors. Existing single-error tests still pass (they now return a singleton
list). The `serve` error print shows all issues.

**Files.** `fennec/server/host_router.ml(+.mli)`, `fennec/app/fennec.ml`,
`fennec/server/test/test_host_router.ml`.

---

## E4. `Server.run` returns a result, never calls `exit`

**What.** `Server.run` calls `exit 1` on a bad `Port_plan` and `exit 98` on EADDRINUSE.
`Fennec.serve` calls `exit 1` on a bad router. These `exit` calls are scattered deep in the
stack, making the server un-embeddable (e.g. in a test that boots a server programmatically).

**Why.** `exit` is a global side effect. A library function should never call it. For the
EADDRINUSE case, the distinct exit code (98) is part of the CLI↔server wire
(`Dev_proto.port_in_use_exit`), so the *process* must exit 98 — but that should be done by
`Fennec.serve`, the one user-facing entry point, not by `Server.run`.

**How.** `Server.run` returns
`(unit, [> \`Port_in_use of int | \`Bad_plan of string ]) result`. `Fennec.serve` matches
the result: `Port_in_use p` → print `Dev_proto.port_busy_line p` + `exit 98`; `Bad_plan` →
print + `exit 1`. The `exit` happens in exactly ONE place (the facade), not in the server
internals.

**Verify.** Existing tests pass (the facade still exits the same way from the outside). A
new test can call `Server.run` directly, get back `Error (\`Port_in_use 4000)` without the
process dying. The CLI's port-reclaim path is unaffected (it detects exit code 98 from the
process status, not from the OCaml result type).

**Files.** `fennec/server/server.ml`, `fennec/app/fennec.ml`.

---

## E5. Trie-based host router

**What.** `Host_router.route` does a linear scan (`List.find_opt`) over the specific
patterns list. For 2–5 endpoints this is irrelevant. For a multi-tenant platform with 50+
wildcard patterns (one per whitelabel customer), it's O(n) per request on the hot path.

**Why.** The stated target is multi-tenant scale. A reversed-label trie makes exact matches
O(1) (hash lookup) and suffix matches O(label depth) regardless of pattern count. The data
structure lives inside the abstract `Host_router.t`, so the API doesn't change — transparent
performance.

**How.** New `fennec/server/host_trie.ml(+.mli)`:
- A reversed-label trie: labels split on `.`, stored root-last (`admin.acme.com` →
  `["com"; "acme"; "admin"]`).
- Internal nodes use a `Hashtbl` for O(1) child lookup by label.
- A node may carry a payload (the endpoint) = an exact match termination.
- A `*` child at any node is the wildcard = a suffix match.
- `build : (Host_pattern.t * 'ep) list -> 'ep t` (from the already-validated list — no
  conflict checks needed here, they ran in `Host_router.build`).
- `lookup : 'ep t -> host:string -> 'ep option` (normalize → split → walk).
- The default (`Any`) stays outside the trie (held separately in `Host_router`).

Then `Host_router.route` becomes:
```ocaml
let route t ~host =
  match Host_trie.lookup t.trie ~host with Some ep -> Some ep | None -> t.default
```

**Verify.** Exhaustive test table: exact match, single-level wildcard, deep wildcard, the
longer-suffix-wins case (important: `*.api.acme.com` must beat `*.acme.com` for
`x.api.acme.com`), no-match → None, case-insensitive, host with `:port`, empty host,
single-label host. All existing `test_host_router.ml` precedence tests must produce
identical results (the trie is a performance optimization, not a behavior change). Consider a
benchmark asserting 100-pattern lookup stays under a threshold.

**Files.** New `fennec/server/host_trie.ml(+.mli)`, modify `fennec/server/host_router.ml`,
new `fennec/server/test/test_host_trie.ml`, update `fennec/server/test/dune`.

---

## E6. Unified error funnel

**What.** Today errors scatter across three paths:
- Connection-level errors (reset, broken pipe, EOF) → `on_error` in `Server.run`, which
  silently drops benign ones and logs the rest.
- Handler exceptions → `run_handler`'s try/catch → 500 text response + `eprintf`.
- Unmatched routes → `None` from `resolve` → 404 text response (hardcoded).

There is no single place for the developer to customize error rendering (JSON vs HTML,
include a request ID, log to a structured logger, render a branded error page).

**Why.** A unified error funnel gives the developer ONE hook — `~on_error` on `serve` or on
the endpoint — with a sensible default (exception → 500 text, no match → 404 text). The 90%
who don't customize get the same behavior as today; the 10% who need JSON errors or branded
pages get one function to write. Dream (OCaml) proves this works.

**How.** Define an error type:
```ocaml
type server_error =
  | Handler_exception of exn * Http.request
  | No_route of Http.request
```
A default error handler renders text 500/404. `Fennec.serve ~on_error:(fun err -> ...)` lets
the developer override. `Server.run` takes the handler as a parameter; the `run_handler`
and `select_endpoint` paths feed into it instead of doing their own ad-hoc rendering.

The connection-level `on_error` (reset/broken pipe) stays separate — it's transport noise,
not application errors. The funnel is for request-scoped errors only.

**Verify.** Default behavior unchanged (existing e2e guards pass). A test that passes a
custom `on_error` asserting it receives `Handler_exception` when a handler raises, and
`No_route` when a path doesn't match. The `eprintf` in `run_handler` is gone (errors flow
through the funnel).

**Files.** `fennec/server/server.ml`, `fennec/app/fennec.ml(+.mli)`, new or updated tests.

---

## E7. Route-matched middleware phase

**What.** The paw pipeline is currently flat — every paw runs on every request, regardless of
whether a route matched. This means auth middleware runs on a request for `/nonexistent`,
turning what should be a 404 into a 401. The developer doesn't realize auth ran on a
non-existent path.

**Why.** This is a concrete bug class in every flat-pipeline framework. Axum solved it with
`route_layer` (middleware that only runs if a route matched). ASP.NET Core solved it with
endpoint filters (middleware scoped per route/group). The fix eliminates the bug class
without adding cognitive load for simple apps (one phase still works).

**How.** The endpoint pipeline gets two attachment points:
- `Endpoint.pipe` / `Endpoint.use` — **always** paws. Run on every request, matched or not.
  Logger, CORS, security headers, static file serving belong here.
- `Endpoint.pipe_matched` / `Endpoint.use_matched` — **matched** paws. Run only after a
  route in this endpoint's pipeline has matched. Auth, rate limiting, business middleware.

Implementation: `Endpoint.t` grows a second paw list. The server composes them:
`always_pipeline |> route_match_check |> matched_pipeline`. If no paw in the always phase
answered (including route paws), the matched phase never runs.

For the simple case (no `pipe_matched`), behavior is identical to today — the matched list
is empty and nothing changes.

**Verify.** Test: an endpoint with auth in `pipe_matched` and a route at `/api`. A request
to `/nonexistent` gets a 404 (from the error funnel), NOT a 401. A request to `/api` gets
auth + the route. The example's `server.ml` splits `common` naturally: logger + security →
`pipe`, auth (if any) → `pipe_matched`. All existing e2e guards pass unchanged.

**Files.** `fennec/server/endpoint.ml(+.mli)`, `fennec/server/server.ml` (the composition
in `handle_conn`), `fennec/server/test/test_endpoint.ml`, `examples/site/server.ml`.

---

## E8. Document the pipeline execution model

**What.** The paw pipeline's "passthrough + first-response-wins + post-processing" model
is the conceptual heart of Fennec and is nowhere in a doc. A user reading the code sees
`Paw.seq` and `Conn.t -> Conn.t` but has to reverse-engineer the semantics.

**Why.** Without this, users will:
- Think `app` is terminal (it's not — paws after it run for post-processing).
- Not realize they can mount multiple apps at different subpaths on one endpoint.
- Not understand why middleware order matters for `before_send` hooks.
- Not know the catch-all / 404 is a convention, not a framework requirement.

**How.** A standalone `docs/PIPELINE.md`:
1. A paw is `Conn.t -> Conn.t`. An unanswered conn has no response set; an answered conn
   has a response (the conn is "halted").
2. `Paw.seq [a; b; c]`: every paw runs in order. The first to set a response "wins." Later
   paws still run — they see the halted conn and (by convention) decline, but they CAN do
   post-processing (logging the response, stamping headers via `before_send`).
3. Route paws (`Paw.get "/path" handler`) only run their handler on a method+path match;
   otherwise they pass through unchanged.
4. `Endpoint.app ~at:"/seller" render` is a paw that answers GET/HEAD under `/seller` if
   the render function matches; otherwise it declines. Multiple apps on one endpoint compose
   naturally — first match wins. The app mount is NOT terminal.
5. `Endpoint.pipe` = always-phase paws (logger, CORS, static). `Endpoint.pipe_matched` =
   matched-phase paws (auth, rate limiting). The always phase runs on every request; the
   matched phase only after a route matched.
6. The order in `pipe`/`pipe_matched` is the precedence order. Earlier paws run first.
7. The catch-all / 404 is a convention (a `rest__` file-route in the Fur router), not a
   framework requirement. Without one, an unanswered conn falls to the error funnel's
   `No_route` handler.

Include a visual diagram of the request flow:
```
Request → Host_router (select endpoint)
        → always-phase pipeline (logger, CORS, static, routes…)
        → [if a route matched] matched-phase pipeline (auth, rate limit…)
        → Response (or error funnel if unanswered / exception)
```

**Verify.** Review by reading it cold — does a newcomer understand the model in one pass?

**Files.** New `docs/PIPELINE.md`.

---

## E9. Collapse task list + final e2e proof

**What.** After all items above are complete: run the full unit suite + all e2e guards + a
manual `fennec dev` smoke test. Confirm every change is committed, the tree is clean, the
docs are up to date, and the example's `server.ml` reads as the ideal reference.

**Why.** The proof that the work is done, not just "tests pass."

**How.** Full `dune runtest` + `fennec test all` (unit → http → browser → system → docs; the system
cut drives a real `fennec dev` and replaces the old `e2e/*.sh`) + a live `fennec dev` session
(content edit, CSS edit, error + recovery, the named banner). Review the final `server.ml`
line by line — every line should earn its place.

**Verify.** All green. The `ENDPOINT-FINISH.md` checklist is fully checked. The commit
history reads as a clean sequence of focused improvements.

---

## Background: the domain model (reference)

Fennec is a multi-tenant isomorphic OCaml web framework. A **single process** serves
arbitrary (sub)domains by routing incoming requests to **endpoints** by Host header.

An endpoint is a **named app** (identity: a name + host patterns) with a **paw pipeline**
(behavior: `Conn.t -> Conn.t` functions composed left-to-right via `Paw.seq`).

The key design property of the paw pipeline is **passthrough + short-circuit**: every paw
in the pipeline receives the connection. If no paw has answered it yet, the paw may
inspect/modify/answer it. If a paw answers (sets a response), the conn is "halted" — later
paws still run but see a halted conn and (by convention) decline, so the first answer wins.
Paws AFTER the answering one can still do post-processing (exotic encryption, response
logging, header stamping via `before_send` hooks).

This means:
- **Multiple apps can live on one endpoint**, mounted at different subpaths (`/seller`,
  `/buyer`, `/` public — a two-sided marketplace, all one domain).
- **The `app` mount is NOT terminal.** Paws after `app` run for post-processing.
- **Multi-tenancy layers ON TOP**: the Host router selects which endpoint handles a request;
  within that endpoint, multiple apps + middleware compose freely.

### What exists today

| Module | Location | Role |
|---|---|---|
| `Endpoint` | `fennec/server/endpoint.ml(+.mli)` | `make ~name ?hosts ()` then `\|> pipe/use/get/post/app` |
| `Host_pattern` | `fennec/server/host_pattern.ml(+.mli)` | Parsed host: `Exact \| Suffix \| Any`; `of_string`, `matches`, `specificity` |
| `Host_router` | `fennec/server/host_router.ml(+.mli)` | Validated routing table: `build` enforces invariants; `route` resolves host→endpoint |
| `Port_plan` | `fennec/server/port_plan.ml(+.mli)` | Deterministic dev port allocation |
| `Server.run` | `fennec/server/server.ml` | Binds ports, dispatches via `resolve ~host` |
| `Fennec.serve` | `fennec/app/fennec.ml` | Userland entry: builds router, wires dev plumbing, calls `Server.run` |
| `Paw` | `fennec/paw/paw.ml(+.mli)` | `type t = Conn.t -> Conn.t`; `seq`, `get/post/…`, `on` |
| `Conn` | `fennec/paw/conn.ml(+.mli)` | The request/response carrier |

### Research context

A 10-language, 60+ framework sweep (see `docs/ROUTER-RESEARCH.md`) confirmed:
- The Paw/Conn model is in the top tier (Ring/Plug/http4k/Giraffe family).
- The gap is not the primitive but the **operational polish**: a locked API surface (E1),
  a unified error funnel (E6), and route-matched middleware (E7) are the three moves that
  lift the framework from "correct" to "delightful."
- Everything else (verified routes, compile-time chain, layered inheritance, symmetric
  test type) is already done or naturally supported by the architecture.
