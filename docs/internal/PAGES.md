# PAGES — standalone server-rendered pages (the cross-stage boundary)

**What a page is (settled).** A *page* is an app with **no sub-router**: a single isomorphic `view`
fused with a server-side `data` handler (the *conn block*). The conn block runs on the server with
the full `Conn` + Pulse + Accounts, decides the HTTP response, and on `Render payload` hands a
`Codec`-typed value to the view and **seeds it** for the client. The view SSRs, then the page's own
tiny jsoo bundle hydrates it (a true single-page SPA — interactivity + live data, no client router;
all links are real links). Contrast with an `apps/*` MPA: that's a small universe of routes with a
client router and a shared bundle. Pages reuse the *same* SSR engine, document templates, components,
signals, and `Fur.Data` seed — verbatim.

## The named model: cross-stage persistence (don't forget this — it's defensible)

A page is a **two-stage computation**. Stage 1 = the server conn block; stage 2 = the client
view/hydration. The values that must travel stage 1 → stage 2 are exactly what multi-stage
programming calls the **cross-stage-persistent (CSP) values**: the *serializable values the later
stage actually consumes*. This is not folklore — it is named PL science, and we should keep pointing
at it so the model stays principled:

- **Multi-stage programming / CSP** — MetaML, MetaOCaml (Walid Taha; Oleg Kiselyov, BER MetaOCaml).
  CSP is the precise term for "a value computed in an earlier stage used in a later stage." The rule:
  primitives/records **lift by value**; closures, refs, and abstract handles **cannot lift** (no
  external representation). There is literally a paper *"MetaOCaml server pages: web publishing as
  staged computation."*
- **Ocsigen / Eliom** (Radanne, Vouillon, Balat — "Eliom: A Language for Modular Tierless Web
  Programming") — the direct OCaml/js_of_ocaml prior art. Its `~%x` **injections** are CSP-by-value;
  its **converters** (`τ_s ⇝ τ_c`: serialize-server / deserialize-client, JSON so the server can
  re-validate) are the typed boundary; and crucially there is **no identity converter** + disjoint
  client/server type universes, so DB handles / secrets / closures simply *cannot* be injected — a
  compile error, not a runtime leak.
- **Ur/Web** (Chlipala) — leak-proofing from **abstract server-only types**: the client has no *name*
  for SQL/secrets, so they can't be in the payload. **Links** (Cooper/Lindley/Wadler) — the
  anti-pattern: serializing closures/continuations to the client → integrity attacks → forced crypto.
  Lesson: **send behavior as a handle, never serialize the function.**
- **The over-transfer cautionary tale** — Remix/Next loaders ship the *whole* loader return ("treat
  loaders as public API endpoints"); React Server Components ship whole prop objects and bolted on
  *taint APIs* that are opt-in and defeated by clone/concat. Astro got the unit right (only
  *interactive-island props* cross; the rest is static HTML that can't leak). The current
  `Page.serve` seeding a single shaped payload is the Astro-correct unit — keep the payload to *what
  the view renders*.

## Why we are deliberately NOT LiveView

Phoenix LiveView keeps the page state in a server process and ships **render diffs** over a socket.
That is genuinely minimal on the wire, but it pays a **permanent latency + server-state tax** (every
interaction is a round-trip; offline is a non-starter; scaling means sticky sockets). fennec's whole
bet is the opposite: **autonomous Meteor-style clients** (minimongo, optimistic writes,
offline-for-free, transparent reconnect). So the **resumable/CSP pole is the right one for us** — seed
the cross-stage values, hydrate, and let the client live on its own. (LiveView's static/dynamic
*change-tracking* is still a good idea done right — bank it as future inspiration for an *optional*
socket-connected page mode, since we already have the DDP socket. Document it; don't build it now.)

## Where it lives (realized)

The page is a **Fur concept**, not a fabricated "web" module. The isomorphic essence is
**`fennec.fur.page`** (`Page`: `resource`, `render`, `default_template`, `outcome`, `Server_only`) —
jsoo-safe, so a page's view links into the server SSR *and* its own bundle. The one Conn-aware bit,
`serve`, folds into the facade **`Fennec.Fur.Page`**. The client boot is `Fur_csr.start_page`. A page
is authored as **one `.mlx`** (`pages/*.mlx`): `key`/`codec`/`bundle` + the `page` view + a server
**`[%%conn fun conn -> outcome]`** block. The `fur.ppx` expands `[%%conn]` to `serve` for the server
build and **strips it** for the client build (driver flag `-conn-client`) — Eliom's server/client
section split, one flag, no hand-written second file. Demonstrated end-to-end by `examples/site`
`pages/greet_page.mlx` (http + browser e2e: SSR seed → hydrate → interactive Counter + live Task_list,
leak-proof).

## How fennec realizes CSP with three primitives it already has

1. **Converter = `Codec`.** The same codec that powers Mongo + DDP + forms encodes the payload
   server-side and decodes it client-side. A value crosses ONLY through a codec → **leak-proof by
   construction**: `Conn`, full records, and Pulse server handles have no codec, so they cannot be
   seeded. **Done:** `Server_only.t` — a wrapper with no codec, so even a *secret string* is unsendable
   by type (putting one in a payload is a compile error), closing the "a string has a codec" gap —
   Eliom's no-identity-converter.
2. **Minimal transfer = the shaped payload (and, later, Fur read-tracking).** Today: the dev shapes
   the payload to exactly what the view renders (the Astro-island unit). Future: `Fur.Data`/signals
   already record which seeded values a render *reads*, so we can seed only the consumed set
   automatically — finer than Astro's whole-prop, matching Qwik's capture-minimality, reusing the
   reactivity we already ship. Bank it; don't build it now.
3. **Behavior + live data = Pulse handles.** Interactivity is the view's own Fur signals; live data
   is a Pulse subscription the client **reconstructs** (re-subscribes) rather than something seeded.
   So: seed the cheap static-derived values, reconstruct the live ones — never serialize a closure.
   **Done:** the demo page seeds the static greeting/count and reconstructs live tasks via a Pulse
   subscription after hydration.

**One sentence:** the server→client handoff is cross-stage persistence — transfer only the
codec-typed values the view consumes, make server-only types un-encodable so leaks are compile
errors, and send behavior + live data as Pulse handles — all three falling out of primitives fennec
already has, which is why no other framework can do it this cleanly.
