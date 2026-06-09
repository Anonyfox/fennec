# Quickstart — zero to a running app

New to OCaml? Read [`OCAML-FOR-WEB-DEVS.md`](./OCAML-FOR-WEB-DEVS.md) first — a 5-minute orientation
for people coming from JS/TS, Rails, or Phoenix. You do **not** need to be an OCaml expert.

## 1. Prerequisites

- **OCaml 5.x + opam** (the package manager) — <https://ocaml.org/install>.
- Building the `fennec` CLI from source also needs **Go** and **Rust** toolchains — they're only the
  JS/CSS bundlers, linked in once at build time (prebuilt per-platform binaries are planned, so this
  step will go away).

## 2. Run the example — the fastest way to see fennec

The reference app (`examples/site`) is two isomorphic apps, live data, and tests — a working tour:

```sh
git clone https://github.com/Anonyfox/fennec && cd fennec
dune build                                      # builds the framework + the `fennec` CLI
# put the built CLI on your PATH (symlink or alias):
export PATH="$PWD/_build/default/cli:$PATH"      # fennec.exe lives here
cd examples/site && fennec dev                   # → http://localhost:4000
```

Open <http://localhost:4000>. Edit `frontend/apps/web/index.mlx` and save — the page **hot-reloads**
(SSR + client both rebuild; CSS swaps without a refresh). That loop is the whole point.

## 3. The shape of an app

Three things, no hidden wiring:

- **`server.ml`** — your endpoints + the [paw pipeline](./PAW.md) + `Fennec.serve`. No HTML strings.
- **`frontend/apps/<name>/`** — your pages as `.mlx` files; the **file name is the route**
  (`about.mlx` → `/about`, `products/id_.mlx` → `/products/:id`). Real Dune modules, so the editor
  (Merlin/LSP) works.
- **A component** is one `.mlx`: HTML-like markup + colocated `[%%style]`, rendered on the server for
  the first paint and hydrated in the browser — same source. Local state is a `signal`.

The full tour of the example (routing, global state, isomorphic data, live realtime) is in
[`examples/site/README.md`](../examples/site/README.md).

## 4. Test it

`fennec-hunt` + `fennec test` — authoring is a bare `let%http` / `let%browser` block, no `main`:

```sh
fennec test            # fast unit gate (inline let%test + doctests)
fennec test http       # HTTP assertions against a booted instance
fennec test browser    # real headless-Chrome e2e (needs Chrome)
fennec test all        # unit → http → browser → system → docs
```

See [`TEST-CLI.md`](./TEST-CLI.md).

## 5. Ship it

```sh
dune build --profile release examples/site/server.exe   # one self-contained native binary (assets embedded)
```

Add HTTPS with one line — `Fennec.serve ~acme:(Acme.auto ()) [ … ]` obtains + renews Let's Encrypt
certificates automatically ([`HTTPS.md`](./HTTPS.md)). Behind nginx / a PaaS, it just serves plain
HTTP on `$PORT` instead.

---

Next: the concept map in [`PIPELINE.md`](./PIPELINE.md), the middleware ladder in [`PAW.md`](./PAW.md),
or the UI runtime in [`../fennec/fur/README.md`](../fennec/fur/README.md).
