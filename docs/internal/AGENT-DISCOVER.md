# Agent Discover

Fennec fastlane compresses the post-edit loop. `fennec discover` should compress
the pre-edit orientation loop: before a human or agent writes code, it should
answer what Fennec already provides, which public path to use, which examples
prove it, and what to inspect next.

This is not symbol lookup. If the caller already knows the symbol, compiler
feedback, LSP, local search, and normal docs can take over. Discover targets the
earlier task-shaped question:

```sh
fennec discover "build login with signed cookies"
fennec discover "SSR page with client-side counter"
fennec discover "test middleware"
fennec discover "Fur state vs Pulse live data"
```

The output is a small evidence-backed orientation card, not a manual or search
result list.

## Why

Humans forget framework surfaces. LLM agents carry stale training data,
overfit on popular frameworks, and hallucinate APIs. Both fall back to expensive
blind exploration: broad `rg`, file reads, inferred conventions, wrong edits,
compiler feedback, repeat.

IDE/LSP tooling helps once a module or symbol is known. It does not answer:
"What is the Fennec way to do this task?"

Research points at the same failure mode:

- Aider repo maps validate "small map first", not whole-tree context:
  <https://aider.chat/docs/repomap.html>
- Cody/Copilot/Cursor-style workspace context still requires search/read loops:
  <https://sourcegraph.com/docs/cody/core-concepts/context>,
  <https://code.visualstudio.com/docs/agents/reference/workspace-context>
- `llms.txt` and API discovery flows help agents enter docs, but are still
  docs/catalogs rather than source-derived task plans:
  <https://mintlify.mintlify.app/ai/llmstxt>,
  <https://wso2.com/api-platform/docs/cloud/devportal/discover-apis/ai-agent-discovery/>
- Anthropic context-engineering guidance favors tight runtime context,
  lightweight references, and examples:
  <https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents>
- ContextBench shows agents over-retrieve, favor recall over precision, and lose
  efficiency when context is noisy:
  <https://arxiv.org/html/2602.05892v2>
- Repo Mind separates semantic retrieval, structural graph context, and
  architectural summaries, and improves consistency:
  <https://githubnext.com/projects/repo-mind/>
- Static agent instruction files can add cost and reduce success when they
  over-constrain agents:
  <https://arxiv.org/abs/2602.11988>

Fennec's advantage is source ownership: `.mli` docs, signatures, examples,
tests, generated skeletons, and Dune graph facts can become a native,
source-truthful discovery index generated with the framework packages.

## Product Contract

Core surface:

```sh
fennec discover "<task>"
fennec discover --json "<task>"
fennec discover --more "<task>"
fennec discover --why ID
fennec discover --browse MODULE
```

No other v1 modes. `--examples` is not needed: examples are evidence in the
normal card, and `--more` expands them.

The command returns one bounded card from a single structured
`Discover_result.t` model.

V1 is framework-snapshot first. It answers "what does the shipped Fennec stack
provide?" from an index generated at build/release time for published packages
such as `fennec`, `fennec-hunt`, `fennec-mongo`, and `fennec-cli`. It does not
need to crawl or annotate the user's app to be useful. A user-workspace overlay
can be added later with the same snapshot format and query engine.

### Plan Card

Used when there is a clear public path. It contains a short ordered sequence,
not only one winning symbol.

```text
Task: protected admin route

Recommended path:
  1. Define the admin endpoint with Fennec.Endpoint.
  2. Put auth in the matched phase, not the always phase.
  3. Use Fennec.Paw.Basic_auth.make.
  4. Copy the matched-route test shape.

Use:
  api:Fennec.Endpoint.pipe_matched
  api:Fennec.Paw.Basic_auth.make

Best examples:
  example:site:server:admin_basic_auth#91cc02  examples/site/server.ml:100
  test:site_system:domains:matched_auth#8fd3a1  examples/site/test/system/domains_test.ml:40

Avoid unless:
  Do not put auth in the always phase if unmatched routes must stay 404.

Confidence: high - public docs, facade APIs, example, and system test agree.
Next:
  fennec discover --why api:Fennec.Endpoint.pipe_matched
  fennec discover --more "pipe_matched basic auth"
```

### Compare Card

Used only for explicit comparison queries (`vs`, `when`, `choose`) or when two
high-confidence public candidates are close and a real decision axis is
evidence-backed. The card must say `Use X when` / `Use Y when`. If no axis is
legible, return a plan card with a secondary pointer or an insufficient card.

Decision axes are explicit classes, not open-ended prose:

- scope: local component state vs cross-client/server state
- persistence: one response cookie vs request session
- phase: always-phase pipeline vs matched-phase pipeline
- render timing: SSR seed vs browser-only fetch
- test layer: unit vs http vs browser vs system

### Browse Card

Only for `--browse MODULE`. It is a shallow public surface map, not a generic
explorer. It includes:

- short module summary
- immediate public submodules/re-exports
- key public values/types
- 1-2 canonical examples/tests
- source refs

It is screen-bounded; `--more` expands.

### Insufficient Card

Used when public evidence is weak. It must not pretend certainty.

```text
Task: ...

Confidence: insufficient - no public Fennec API matched strongly.
Try:
  fennec discover "..."
  fennec discover --browse Fennec.Paw
Inspect:
  ...
```

## Output Rules

Default terminal output should fit one screen: about 35-45 lines. It should
prefer one precise card over recall-heavy match dumps.

Human and JSON renderers derive from the same `Discover_result.t`. JSON is
schema-versioned and carries the same ids, source refs, confidence, evidence,
avoid/require notes, and next commands as the terminal card. Human output is the
primary UX; JSON exists so agents get stable machine-readable context.

Every claim must be grounded in a source path/line or generated graph fact.
Low evidence produces low/insufficient confidence, not a made-up plan.

## Source Truth

Primary v1 inputs:

- public `.mli` docs and signatures
- module declarations and re-exports, especially `fennec/app/fennec.mli`
- Dune package/library/test/executable facts
- framework examples and generated skeletons
- inline tests, doctests, http/browser/system tests
- route/app conventions such as `.mlx`, generated `Routes`, and `Paths`
- explicit hazards from `.mli` docs, precondition docs, and tests

Secondary inputs:

- public-facing implementation error strings and require-fail messages
- git recency or explicit canonical markers, later
- user-workspace overlay, later
- devserver hot state, later

Error strings are secondary evidence only. They may explain `--why`, but they
must not create a high-confidence `Avoid` by themselves.

Manual discover-only annotations are a non-goal. If a query needs better
vocabulary or a clearer hazard, improve the public `.mli`, example, or test name
so humans benefit too. Tiny tested aliases are allowed in the matcher; broad
`@topic` or `@discover` metadata would be brittle and should be treated as a
design smell.

Local substrate already exists:

- `cli/docscmd` parses `.mli`/`.ml` with `Ppxlib` and extracts public items,
  docs, line numbers, and missing-doc state.
- `cli/docscmd/doctest_gen.ml` already extracts executable `{@ocaml[ ... ]}`
  blocks from `.mli` docs; discover can reuse that extractor as example
  evidence instead of only generating tests.
- `fennec test docs fennec cli` currently sees hundreds of public exports with
  high doc coverage.
- `examples/site` is a canonical pattern corpus for SSR, Fur components,
  hydration, host routing, basic auth, realtime DDP, static assets, streaming,
  browser tests, and system tests.
- `fennec/fur/tools/route_gen.ml` already turns app file trees into route
  patterns, generated `Routes`, generated `Paths`, and mount values. Discover
  should index this same route convention rather than inventing route logic.
- `dune describe --format csexp` can provide authoritative graph facts, but
  must be distilled before users or agents see it.
- `dune-project` already describes multiple published packages. The discover
  generator should build package-scoped snapshots so standalone libraries such
  as `fennec-hunt` can be discovered alongside the main framework.

Naming caveat: `cli/dev/discover` already means "find the server executable for
`fennec dev`". The user command can still be `fennec discover`; implementation
modules should avoid a direct `Discover` collision, e.g. `Task_discover_*` or a
separate `fennec_discover` library.

## Implementation Shape

Build this as a small native discover library plus CLI wiring, not as logic
inside `cli/fennec.ml`.

Suggested library: `fennec_discover`, linked into `fennec-cli`.

Modules:

- `Source_ref`: path, line, optional digest, generated-fact marker.
- `Public_item`: normalized public API item extracted from `.mli` signatures:
  package, library, module path, kind, name, type/doc text, source ref.
- `Doc_extract`: parser-backed extraction. Reuse and extend the pure substrate
  in `cli/docscmd`; do not create a parallel parser.
- `Doctest_extract`: reusable wrapper around existing `{@ocaml[ ... ]}`
  extraction so doc examples become evidence.
- `Dune_index`: distilled package/library/test/executable facts. It may parse
  dune stanzas directly for the framework snapshot, with `dune describe` as a
  later/runtime authority when available.
- `Example_extract`: framework examples/tests as evidence nodes, including
  source refs, names, labels, and public API mentions.
- `Route_facts`: shared convention logic from `fennec/fur/tools/route_gen.ml`
  for `.mlx` routes and typed path builders.
- `Snapshot`: versioned, package-scoped graph format with interned strings,
  nodes, edges, postings, and public-interface digest.
- `Snapshot_gen`: build/release-time generator for framework package snapshots.
- `Query`: normalization, tiny tested aliases, lexical/BM25-style scoring,
  structural boosts, grouping, confidence, and card precedence.
- `Select`: final card composition. Retrieval returns many plausible APIs and
  evidence items; selection picks public representatives, preserves distinct
  API families, favors exact task-action leaves, and diversifies compare-card
  evidence so one source cannot crowd out the other side.
- `Result`: the single typed product model (`Plan`, `Compare`, `Browse`,
  `Insufficient`) used by both renderers.
- `Render_text` and `Render_json`: bounded terminal output and schema-versioned
  JSON from `Result.t`.
- `Golden`: golden task fixtures and assertions for accuracy, refs, confidence,
  card type, and output bounds.

The CLI layer only parses flags, loads the embedded or installed framework
snapshot, calls `Query`, and renders. It should not know extraction details.

V1 build path:

1. Extend `cli/docscmd` extraction into reusable pure modules or move the shared
   parser code into `fennec_discover`.
2. Generate framework snapshots from checked-in source during the release/build
   pipeline.
3. Embed snapshots into the `fennec` binary, or install them as small data files
   beside it. The query path is identical either way.
4. Add `fennec discover` command wiring with exactly the product-contract flags.
5. Add `fennec test docs --discover` or an equivalent test target that rebuilds
   snapshots, validates digests/refs, and runs golden tasks.

No implementation step requires discover-only annotations or user app crawling.
If a source file needs better discoverability, improve the public docs, examples,
or test names that already serve humans.

Current implementation status:

1. `Snapshot_gen` parses public `.mli` signatures, adds facade aliases, extracts
   example/test/route/doc evidence, and embeds the snapshot into the CLI.
2. `Retrieve` runs deterministic lexical retrieval across public APIs and
   evidence, propagates evidence to linked APIs, follows odoc advisory links,
   and keeps source-backed proof items.
3. `Select` composes the card-facing `Use:` list from retrieved APIs plus
   evidence-seeded APIs. This is intentionally separate from retrieval: broad
   APIs, helper leaves, and implementation internals may be useful candidates
   but should not automatically become the visible recommendation.
4. `Query` handles card choice, confidence, evidence selection, `--why`, and
   `--browse`.
5. `Golden` locks the practical UX contract with task-shaped assertions for
   auth phases, response cookies, signed sessions, Fur local state, HTTP tests,
   typed routes, and Fur-vs-Pulse comparison.

Next performance step: materialize a real postings index in the snapshot
instead of scanning tokenized strings at query time. The current code is already
bounded and deterministic, but a compact fielded inverted index with precomputed
IDF/doc lengths is the correct shape for sub-millisecond runtime and fast,
incremental snapshot rebuilds.

## Index Model

Build a compact interned graph, not a bag of string matches.

Nodes:

- public API item: module, val, type, exception, module type
- facade re-export
- example
- test
- hazard
- package/library/executable/test suite
- route/app convention

Edges:

- `defines`
- `reexports`
- `uses`
- `proves`
- `warns_about`
- `belongs_to`
- `adjacent_to`

Intern strings early. Query-time work should use ids, arrays, compact maps, and
precomputed token postings. Hash tables are fine in index construction; avoid
ad-hoc repeated scans in the hot path.

Framework package snapshots and any later local workspace overlay must use the
same normalized snapshot format and query engine. V1 only needs the framework
snapshots.

## Path Construction

Discover cards contain two kinds of "paths"; keep them separate.

### Recommendation Paths

The `Recommended path` section is not stored prose. It is synthesized from an
ordered evidence bundle at query time.

Construction:

1. Pick one public anchor, usually a facade API or public module.
2. Attach adjacent public APIs by graph edges (`reexports`, `uses`,
   `belongs_to`) and by source order when they live in the same documented
   surface.
3. Attach proof evidence: doc examples, canonical examples, and named tests
   that use the same public APIs or match the same concept tokens.
4. Attach hazards from docs/tests/preconditions.
5. Sort steps by workflow phase:
   - create/configure public surface
   - compose/mount/wire it
   - call it from app code
   - test the behavior
6. Render 2-4 imperative steps with evidence ids.

For example, "protected admin route" should be synthesized from framework
evidence:

- `Fennec.Endpoint.pipe_matched` docs: matched-phase middleware
- `Fennec.Paw.Basic_auth.make` facade API
- `examples/site/server.ml`: admin endpoint uses `pipe_matched`
- `examples/site/test/system/domains_test.ml`: unmatched stays default/404-ish,
  matched admin route becomes 401 without auth
- hazard: auth in always phase can turn unmatched routes into auth failures

The path is therefore:

1. Define endpoint.
2. Add normal routes/app.
3. Put auth in matched phase.
4. Copy the system/http proof shape.

This keeps discover from becoming a hand-written recipe database while still
returning a human-readable plan.

### URL And Typed Route Paths

Fur route paths are already dynamically derivable from app files. Discover
should reuse the same convention as `route_gen`:

- `index.mlx` maps to `/`
- `about.mlx` maps to `/about`
- `products/index.mlx` maps to `/products`
- `products/id_.mlx` maps to `/products/:id`
- `rest__.mlx` maps to a catch-all

`route_gen --glue` emits:

- `routes.ml`: `Router.page` calls and the app `mount`
- `paths.ml`: typed path builders using `Router.absolutize Main.base`

For v1, these route facts come from Fennec's own examples/skeletons. A future
workspace overlay can index the user's app route files the same way. If
generated files are missing or stale, the route facts can be recomputed directly
from the file-tree convention without writing anything.

## Stable Ids

API ids use canonical public paths when available:

```text
api:Fennec.Paw.Session.make
```

Example/test ids include kind, package/library/app, semantic label or module,
and a short content/path hash suffix for collision safety:

```text
test:site_browser:web_test:hydration_seed#8fd3a1
example:site:server:admin_basic_auth#91cc02
```

`--why ID` resolves current exact ids, accepts unique prefixes, and reports
nearest current matches for stale ids. It must not silently pretend renamed
anchors are the same.

## Matching And Ranking

Start deterministic and source-backed:

1. Normalize query terms: module paths, camel/snake case, plural variants,
   punctuation, and common web terms (`auth`/`authentication`,
   `csrf`/`form token`, `live`/`realtime`, `SSR`/`server render`).
2. Expand a tiny tested alias table plus terms mined from source docs/tests.
   Aliases live next to the discover code and are tested; they are not a second
   docs layer.
3. Score lexical/BM25-style field matches.
4. Add structural boosts from public facade/API status, examples, tests, and
   Dune graph facts.
5. Group hits into evidence bundles around public APIs and task concepts.
6. Compute confidence from score margin, evidence diversity, public/private
   status, and whether examples/tests prove the path.

No embeddings, network calls, MCP, or model-generated summaries in v1.
Embeddings may help later when users describe concepts absent from docs, but
only if every recommendation remains source-cited and measured against golden
tasks.

Canonicality is evidence-weighted:

```text
example/test rank =
  provenance weight
  * concept/token match
  * actual API-use edges
  * proof strength
```

Source class alone is not enough. Generated skeletons, doc examples,
`examples/site`, named tests, and incidental usages all have different priors,
but concept match and actual API-use edges decide the final anchor.

Public facade/API evidence is the anchor. Examples/tests boost confidence and
teach usage; they should not become the primary recommendation unless no public
API exists and confidence drops.

## Card Precedence

The decision is deliberately boring:

1. `--browse` always returns a bounded public browse card.
2. If no strong public evidence exists, return insufficient.
3. If the query is explicitly comparative, or two high-confidence public
   candidates are close and there is an evidence-backed decision axis, return
   compare.
4. Otherwise return a plan card.

Thresholds for "strong public evidence", "close candidates", and "decision
axis" must be covered by golden query tests. Card type should not oscillate
under small unrelated source changes.

## Index Lifecycle

`discover` works standalone. A running devserver is not required.

Framework package snapshots:

- generate versioned indexes from source at build/release time
- include package name, package version, and digest of the public interface
  corpus
- verify the digest when source, `.cmti`, or `.mli` files are available
- rebuild or mark stale on mismatch

Future local project overlay:

- materialize a content-hash snapshot under `_build`
- store per-file digests
- reparse changed files only
- tolerate missing/stale cache by rebuilding

The CLI can embed the framework snapshots or install them as small data
artifacts. Either way, framework-only discover should be effectively instant and
work without a running devserver. `fennec dev` may keep a future workspace
overlay hot later, but that is an optimization, not the product contract.

## Quality Gates

Golden tasks are product tests. Each has expected context, recommended path,
avoid/require notes, confidence, and acceptable follow-up commands.

Initial golden tasks:

- add a JSON health endpoint
- protect only matched admin routes with basic auth
- add signed cookie-backed sessions
- add CSRF to unsafe form posts
- set and delete a response cookie
- serve static assets with dev/prod cache behavior
- expose a WebSocket endpoint
- build an SSR page with a local counter
- add a controlled form and keyed list
- add a dynamic route and typed path link
- use browser-only data after hydration
- test component SSR output
- test browser hydration and local state
- render an SSR-seeded live task list
- subscribe from the browser and handle readiness
- choose Pulse live data vs local Fur state
- write an HTTP test
- write a browser test
- write a system test for devserver behavior
- interpret a fastlane compile/test failure

Required checks:

- top-1/top-3 accuracy on golden task queries
- no private/internal API recommended by default
- source refs and graph facts resolve
- terminal output stays bounded
- JSON schema is stable
- confidence is conservative
- compare cards have a real decision axis
- browse cards stay shallow and public
- `--why` ids resolve or report nearest current matches
- generated package index digests match the installed public interface corpora
- stale future workspace snapshots rebuild safely

Useful metrics:

- average terminal lines and JSON byte size
- cold index build latency and warm query latency
- memory growth on representative repo size
- follow-up source reads needed before first edit
- context precision/recall against hand-labeled golden contexts
- behavioral smoke: small agents use `fennec discover`, make fewer exploratory
  reads, choose public APIs, and avoid known wrong paths

`fennec test docs` can grow a `--discover` gate that builds the index, checks
dangling references, and asserts the golden tasks.

## Non-Goals

V1 must not:

- become generic semantic search
- dump broad `rg`-style match lists
- require devserver, editor plugin, MCP, network, or a model provider
- ship model-generated summaries
- recommend private/internal modules by default
- add many flags before the default card is excellent
- create a second stale documentation layer
- require discover-only annotations in source files
- let browse become symbol lookup

The goal is one native, deterministic, source-truthful orientation primitive:

```text
task phrase -> compact evidence graph -> bounded card -> guided drilldown
```
