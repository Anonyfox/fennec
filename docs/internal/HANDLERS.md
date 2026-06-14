# HANDLERS — standalone full HTTP handlers (one .mlx, own bundle)

**What a handler is (settled).** A *handler* is ONE file — `frontend/handlers/<name>.mlx` — that is a
full HTTP handler: a server `load : conn -> outcome` fused with an isomorphic `view : payload -> vnode`,
over a `payload` type (`type t [@@deriving model]`). On `render payload` the framework seeds exactly
that codec-typed value, SSRs the view, and ships the handler's OWN jsoo bundle so it hydrates into a
tiny SPA; `load` can also `redirect`/`error`/`not_found` (it is a full handler, not just a page). You
**mount it yourself** in `server.ml` at any path — `Endpoint.get "/greet" Site_handlers.Greet.serve` —
with path params, **reusable** across paths/endpoints. Central-router DX; no auto route table.

```ocaml
(* frontend/handlers/greet.mlx — the whole handler. No key/bundle/boot/dune. *)
type t = { who : string; count : int } [@@deriving model]
let load conn =                                  (* server-only; stripped from the bundle *)
  match Conn.param conn "name" with Some n -> render { who = n; count = String.length n }
  | None -> render { who = "world"; count = 5 }
let view (p : t) = <main><h1>(node ("Hello, " ^ p.who))</h1><Counter start=(p.count) /><Task_list /></main>
```

## The model: cross-stage persistence (unchanged, still the science)

A handler is a TWO-STAGE computation: stage 1 = `load`; stage 2 = `view`/hydration. Only the
CROSS-STAGE-PERSISTENT values cross (multi-stage programming — MetaOCaml; Eliom's injections +
converters): the serializable values the view consumes. The CONVERTER is the **`Codec`**; behaviour
(Fur signals) + live data (Pulse subscriptions) cross as HANDLES, never serialized. We are **not
LiveView** (no server-held state) — autonomous Meteor-style clients. **Leak-proof by construction:**
only `codec.enc payload` crosses; the Conn/secrets/Pulse-handles have no codec; **`Server_only`** makes
a secret un-encodable, so a leak is a compile error. (See `fennec/fur/handler/handler.mli`.)

## How the ppx fuses one file (Eliom's section split, one flag)

`fur.ppx -handler` derives `key` (`handler:<name>`) and `bundle` (`/_handlers/<name>/main.js`) from the
filename and:
- **server** (`-handler`): wraps `load` with the facade opened locally (so `Conn`/`render`/`redirect`/
  `Server_only` resolve) and appends `serve` (run `load` → seed+SSR via `render_doc`, or the other
  responses) — every server-only reference stays inside the generated `serve`;
- **client** (`-handler -conn-client`): STRIPS `load`, appends `boot` (`Fur_csr.start_page` over the
  seeded `view`), so the jsoo bundle never sees `Conn`.

The two compilations are the server lib `site_handlers` and a `(subdir client …)` mirror (`copy_files`
+ `-conn-client`) — one folder, no parallel `pages_client` tree.

## The dune mechanism — N isolated bundles, zero hand-dune (the hard-won part)

Per-handler isolated bundles need a per-handler `(executable (modes js))`, and **dune cannot generate a
stanza it evaluates in the same pass** (every `dynamic_include` of a rule-generated `.inc` *in the same
dir* is a dependency cycle — verified four ways). The escape (from the official *Using Rule Generation*
howto) is **two sibling subdirs where the generator depends only on the EXTERNAL source dir**:

- `client/handlers/gen/` runs `route_gen --handler-bundles <frontend/handlers> . site_handlers_client`,
  globbing `frontend/handlers/*.mlx` (never `../run`), emitting `dune.inc` with, **per handler, a boot
  rule + a private jsoo `(executable)`**;
- `client/handlers/run/` is just `(dynamic_include ../gen/dune.inc)` + one fixed staging rule that
  copies the generated `*.bc.js` into a served tree `served/_handlers/<name>/main.js`.

The webroot `--public`s that tree (so `fennec build`'s `--public` became repeatable). **jsoo separate
compilation** (the dev-profile default) compiles each module to JS once and links per-executable, so the
N bundles share artifacts and stay incremental — touch one handler, only its bundle relinks.

Net userland footprint to add a handler: **drop a `.mlx` in `frontend/handlers/` + one
`Endpoint.get` line.** No per-handler dune, no boots, no webroot edits, no external CLI step — the only
generated/operational dirs are the `client/` route_gen folder.

## Banked for later

- Auto-`default` from `[@@deriving model]` (today the seed is always present, so `Handler.payload`
  reads it without a fallback; a derived default would let `resource`-style views drop the fallback).
- Migrating the *app* bundles to the same generated-stanza mechanism (kill the last hand-written
  `(executable)`s in `client/dune`).
- A richer `outcome` (`Html`/`Json`) so a handler can also be a static-HTML or JSON endpoint without a
  bundle — fully realizing "a handler is a full HTTP handler."
