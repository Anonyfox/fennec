# HANDLERS — standalone full HTTP handlers (one .mlx, own bundle)

**What a handler is (settled).** A *handler* is ONE file — `frontend/handlers/<name>.mlx` — that is a
full HTTP handler: a server `load : conn -> outcome` fused with an isomorphic `view : payload -> vnode`,
over a `payload` type (`type t [@@deriving model]`). `load` is a **full HTTP action** — the SAME
`view`/`payload` can be served as a hydrated SPA (`render p` — seed + SSR + own bundle), as plain
static no-JS HTML (`html p` — same view, no bundle/seed/`#app`), content-negotiated to `json codec v`,
or as `text`/`redirect`/`not_found`/`error`. You **mount it yourself** in `server.ml` at any path —
`Endpoint.get "/greet" Site_handlers.Greet.serve` — with path params, **reusable** across
paths/endpoints. Central-router DX; no auto route table.

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
`Endpoint.get` line.** No per-handler dune, no boots, no webroot edits, no external CLI step.

**Apps use the same mechanism.** `route_gen --app-bundles` emits, per `frontend/apps/<app>/`, its boot
rule + private jsoo `(executable)` + CSS rule into `client/apps/gen/dune.inc`; `client/apps/run`
consumes it and stages `served/_apps/<app>/{main.js,main.css}`. `client/dune` is gone. The webroot is
just three `--public`s (public + apps + handlers). Adding an app OR a handler is drop-a-folder with
**zero dune edits anywhere**; the only operational dirs are the generated `client/apps` and
`client/handlers` gen+run pairs.

## Full HTTP handler — the outcome set

`load : conn -> 'p outcome`, with smart-verb constructors (in scope inside `load`):
`render p` (hydrated SPA) · `html p` (same view, plain static HTML via `render_static`) ·
`json codec v` (JSON body, same codec) · `text s` · `redirect url` · `not_found` · `error n`. The
example `greet.mlx` content-negotiates one payload three ways — `/greet` (SPA), `?format=json`
(JSON, no seed/bundle), `?format=html` (static HTML, no `#app`/seed/bundle) — verified by the http
suite. So a handler is genuinely a full HTTP citizen, not just a page.

## Considered + rejected: auto-`default` from `[@@deriving model]`

The earlier "bank it" item — derive a zero-value `default` so a seed read could fall back — was
examined and **dropped**, for reasons worth recording:

- **It was solving a non-problem.** A handler `view` is `payload -> vnode`; the boot reads the seed as
  a *plain value* (`Handler.payload`), and the seed is ALWAYS present on a rendered handler (the server
  seeds before shipping the bundle). There was a vestigial signal-form `resource ~fallback` (from the
  old page model) that nothing used — it has been **removed**. With no fallback site, no default is
  needed. `Handler.payload` *raising* on a missing seed is correct: it's a "can't happen / corrupt page"
  signal, and a silent default would mask the bug.
- **A derived default is semantically wrong as often as right.** The zero value of a record
  (`{ who=""; count=0 }`) frequently **violates the model's own validation** (`who` is `[@non_empty]`),
  so "default value" and "valid value" are different concepts the deriver can't reconcile. And
  sum/abstract types have no canonical zero at all.
- **Where an empty value IS wanted — forms, new minimongo docs — it's a caller concern**, supplied
  explicitly (and validated), not silently synthesized by the model deriver.

Net: handlers stay fallback-free and the model deriver stays honest (a model is its *codec* +
`Fields`, nothing pretends to be a valid blank). Nothing structural remains banked.
