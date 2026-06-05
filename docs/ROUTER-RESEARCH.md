# Server/Router/Endpoint innovations across modern web stacks

A 10-language sweep (Rust, Go, Elixir/Ruby, TypeScript/JS, Python, Java/Kotlin, C#/.NET,
Swift/Zig/C++, Gleam/OCaml/Haskell/F#, PHP/Dart/Scala/Clojure) of 60+ frameworks, focused
on what's genuinely novel or well-received at the server/router/endpoint layer — and what
Fennec should steal, adapt, or consciously skip.

---

## The 12 ideas that matter (ranked by relevance to Fennec)

### 1. Typed context accumulation through the pipeline
**Source:** Hummingbird 2 (Swift), ElysiaJS (TypeScript), Crow (C++)

The pipeline isn't just `Conn -> Conn` — each middleware stage *adds* typed properties that
downstream stages can see at compile time. Hummingbird threads a generic `RequestContext`
protocol through `Router<Context>` and `RouterMiddleware<Context>`, so the compiler verifies
the whole chain before a request is processed. ElysiaJS does this with TypeScript generics:
each `.derive()` / `.resolve()` extends the `Elysia<NewContext>` type. Crow encodes the
middleware set as template parameters on the app type itself.

**For Fennec:** OCaml's module system + GADTs could encode this: a `Conn.t` parameterized by
a phantom type that middleware extends. The auth middleware returns a
`Conn.t<[`Authenticated]>` and the handler requires it. This is the single highest-impact
type-level improvement — it makes "middleware forgot to run" a compile error, not a runtime
crash. Explore whether ppx or first-class modules can keep the DX lightweight.

### 2. Route-matched middleware vs. global middleware
**Source:** Axum (`route_layer` — only runs if a route matched), ASP.NET Core (endpoint
filters scoped to a route or route group), Salvo (same-path routers with different
middleware trees), Chi (scoped `Route()` sub-routers)

The insight: a 404 should not trigger auth middleware and become a 401. Global middleware
runs on every request regardless of whether a route matched. Route-matched middleware runs
only on a matched route — so unmatched requests fall through cleanly.

**For Fennec:** the paw pipeline is currently flat (every paw runs). Adding a "route-matched"
phase (paws that only execute after the router found a match) would prevent auth-before-match
footguns. This maps to Ktor's named phases: `before_match` (logging, CORS) vs `after_match`
(auth, rate limiting).

### 3. Endpoint-as-value (describe once → derive server + client + docs)
**Source:** Tapir (Scala), Servant (Haskell), oRPC (TypeScript), Hono RPC, Poem-OpenAPI
(Rust), Huma (Go)

An endpoint is an immutable typed value: its path, method, input types, output types, and
error types. From that ONE description, you derive: the server handler, a type-safe client,
OpenAPI documentation, and property tests. The description IS the contract — no drift.

**For Fennec:** the `Fur` system already generates typed route paths (`Paths.products_id`).
Extending this so the route definition also describes the response shape (the Fur component's
props = the API contract) would give Tapir-style derive-everything from the existing
file-tree router. Not a v1 priority, but the architecture should leave room for it.

### 4. Typed route parameters from the route definition, not handler bodies
**Source:** Axum `TypedPath` (derive validates captures match struct fields), Ktor
`@Resource` (reverse routing from typed classes), Giraffe `routef` (printf-format typed
params), OCaml `ppx_deriving_router` (typed routes from variant types), `routes` library
(`s "sum" / int / int /? nil` with arity visible in the type)

**For Fennec:** `ppx_deriving_router` is the OCaml-native answer — derive type-safe routing
from a variant type. The `routes` library's structural path encoding is even cleaner. Either
could layer on top of the file-tree router for typed handler params.

### 5. Compile-time middleware chain (build once at startup, not per-request)
**Source:** Reitit (Clojure) `:compile` key, Pavex (Rust) transpile-time DI graph, ASP.NET
Core `AddEndpointFilterFactory` (inspect handler signature at build time, emit a live filter
or a no-op shim)

The middleware chain is resolved, pre-compiled, and flattened at server startup. Per-request
dispatch is a direct function call through a pre-built chain, not a linked list walk. Reitit
measured 3–10x speedup over Ring's runtime wrapping.

**For Fennec:** `Paw.seq paws` already builds the composed function at endpoint construction
time (not per-request), so Fennec is already doing this correctly. Worth verifying the
`List.fold_left` in `Paw.seq` produces a single closure chain, not a linked-list walk at
request time. If it does, document it as a deliberate design choice.

### 6. Unified error funnel (all errors → one handler)
**Source:** Dream (OCaml), Pavex (Rust) error observer/handler split, Echo (Go) central
`HTTPErrorHandler`

All errors — HTTP protocol errors, TLS errors, handler exceptions, middleware failures —
flow through a single error handler. No silent swallowing, no inconsistent error formats
across layers. Dream's unified error funnel is the cleanest implementation.

**For Fennec:** `Server.run`'s `on_error` currently handles only connection-level errors.
Handler exceptions become 500s via `run_handler`. A unified `error_handler` on the endpoint
or server that receives ALL errors (with the request context) and returns a response would
match Dream's model and give users one place to customize error rendering (JSON vs HTML,
with request IDs, etc.).

### 7. `Option`/`Result` route fallthrough (no exceptions for "no match")
**Source:** Giraffe (F#) `HttpContext option`, http4s (Scala) `OptionT`, Gleam/Wisp
`Result`

A route that doesn't match returns `None`/`Error` — a typed value, not an exception or a
sentinel. `choose [route_a; route_b]` composes by trying each until one returns `Some`.
The pipeline short-circuits on `None` without exceptions.

**For Fennec:** Paw already does this — an unanswered `Conn.t` flows through, and `Paw.seq`
is first-answer-wins. The model is correct. What Fennec could formalize: expose "did this
paw answer?" as a typed predicate on `Conn.t` (it's currently `conn.status <> 0` or similar
convention), so the short-circuit is structural, not a hidden field check.

### 8. Symmetric client/server type (handler = client interface)
**Source:** http4k (Kotlin), Hono RPC + `hc<AppType>()` (TypeScript)

An `HttpHandler = (Request) -> Response` is both a server handler and a client interface.
Testing a service graph = calling functions, no mock HTTP client, no server process, no port.
Hono's `hc<typeof routes>` generates a fully typed fetch client from the server's exported
type.

**For Fennec:** OCaml's module system makes this natural — a handler module with a
`val handle : Http.request -> Http.response` signature can be used as both a server paw and
a test client. The `Paw.run` function already does this for unit tests. Worth documenting
as a first-class testing pattern.

### 9. Verified routes (compile-time URL checking)
**Source:** Phoenix `~p` sigil (compile-time route verification), Ktor typed `@Resource`
reverse routing, Axum `TypedPath`

Broken routes become compile errors, not 500s in production. Phoenix's `~p"/users/#{user}"`
verifies against the router at compile time. Fennec's `route_gen` already generates
`Paths.products_id` — this is the same idea, already shipped.

**For Fennec:** already done via `route_gen --glue`. Ensure every generated path is
compile-checked and that a renamed/deleted route is a compile error (it should be, since the
generated module disappears). Document this as a feature.

### 10. Layered config inheritance (attach once, inherit down)
**Source:** Litestar (Python), Phoenix `pipe_through`, Saturn (F#) pipeline CE

Configuration (DTOs, guards, middleware, dependencies) flows down the router tree. A guard
attached at the router level applies to every handler under it. Handlers opt out, not opt in.

**For Fennec:** `Endpoint.pipe common` already does this — a shared pipeline applied to the
endpoint's entire handler chain. The architecture is correct. For nested sub-routers (if/when
Fennec adds them), ensure the inheritance model is explicit: inner scopes inherit outer
middleware, and can prepend/append/override.

### 11. Per-route typed result unions (the response contract)
**Source:** ASP.NET Core `Results<Ok<T>, NotFound, BadRequest>`, Poem (Rust) `ApiResponse`
derive, oRPC typed error variants

The handler's return type IS the response contract. `Results<Ok<Product>, NotFound>` means
the endpoint can return exactly those two shapes — and OpenAPI is generated from the type,
not from attributes. Returning an undeclared shape is a compile error.

**For Fennec:** OCaml's polymorphic variants could express this cleanly:
`[> \`Ok of product | \`Not_found] -> Conn.t`. The Conn's response builder could be
parameterized by the allowed response variants. Ambitious but powerful — explore after the
core router is solid.

### 12. `use`-as-middleware (CPS without monads)
**Source:** Gleam/Wisp

```gleam
use <- wisp.log_request(req)
use <- wisp.require_method(req, Post)
use json <- wisp.require_json(req)
handle_request(json)
```

Any last-arg-callback function is middleware. No wrapper type, no trait, no type class.

**For Fennec:** OCaml's `let*`/`let+` binding operators could provide a similar experience
within a handler: `let* user = require_auth conn in ...`. This would let a handler compose
"early return on failure" steps without nesting. Not a framework primitive but a pattern
worth documenting.

---

## Consciously skipped (not relevant for Fennec)

| Idea | Why skip |
|---|---|
| OpenAPI generation from schemas | Fennec is SSR/isomorphic, not API-first; the Fur component IS the contract |
| tRPC-style procedure definitions | Fennec's data model is `source : path -> string option`, not RPC |
| File-based routing for the server | Fennec already has file-based routing via `route_gen` for the frontend; the server's `server.ml` is explicit by design |
| FrankenPHP-style boot-once/reset-per-request | OCaml processes are already long-lived; no cold-start problem |
| io_uring / zero-allocation extreme | Eio already uses the best available backend per platform; the framework shouldn't special-case this |

---

## The one-line summary

Fennec's Paw/Conn model is already in the top tier (it's Ring/Plug/http4k/Giraffe — the
proven "handler as a function" family). The gap is not the primitive but the **type-level
leverage**: typed context accumulation (#1), route-matched middleware (#2), and a unified
error funnel (#6) would lift it from "correct" to "delightful." Everything else is either
already done (verified routes, compile-time chain, layered inheritance) or future-work that
the architecture naturally supports (endpoint-as-value, typed result unions).
