# `frontend/` — your code. All of it.

Every file here is a **real Dune module you wrote** — `.mlx` (markup) or `.ml` — so Merlin/LSP
works and editing one recompiles just that module. The build compiles this *same* tree twice (the
OCaml server for SSR, js_of_ocaml for the browser); the browser mirror lives in [`../_client/`](../_client/)
and regenerates itself, so **you only ever touch this folder.**

Five categories, each a plain word for what it holds:

| folder | what it holds |
|---|---|
| `apps/<name>/` | a self-contained app — a page tree (`index.mlx` → `/`, `products/id_.mlx` → `/products/:id`) compiled to its own isolated client bundle. Per-app asset escape hatches live here too: `main.css`/`main.scss` (the ordering manifest), `styles/` (drop a theme or vendor sheet), `scripts/` (drop a vendor `.ts`/`.js`). |
| `components/` | shared `<Components>` — markup + colocated `[%%style]` + inline `let%test`, one `.mlx` each. |
| `documents/` | the outer HTML **document shells** a page renders into (`Default`, `Admin_shell`) — the `<html><head><body>` wrapper. The Next.js `_document` / Remix `root` idea, **not** per-route view templates. |
| `handlers/` | standalone endpoints mounted in `server.ml` — an SPA handler (own bundle) or a server-rendered form. |
| `store/` | shared signals (`Store.todos …`) — global reactive state. |

**The one workflow rule: group into subfolders freely — no dune anywhere under a category.**
`mkdir marketing && mv hero.mlx marketing/` just works. `(include_subdirs unqualified)` folds the
whole tree into one library with a FLAT module namespace — a file's name is its module wherever it
sits (`<Hero/>` whether `hero.mlx` is at the root or in `marketing/`). The only constraint: no two
files share a basename across subfolders (dune says so plainly if you slip).

`apps/` is the one exception: it nests by folder→module **path** (`products/id_.mlx` → `Products.Id_`),
which routing needs — so its browser mirror keeps hand-written stanzas instead of auto-generating.

---
Full tour (routing, data, tests, build): [`../README.md`](../README.md) · The generated other half:
[`../_client/README.md`](../_client/README.md).
