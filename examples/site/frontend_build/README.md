# frontend_build/ — generated build machinery (ignore me)

Nothing to edit here. This folder only exists because the isomorphic `frontend/`
tree has to be compiled **twice**, and dune needs two stanzas to do it:

- `frontend/` itself is the **native (SSR)** library, compiled in place with
  `server-reason-react`.
- `mirror/` is the **client (CSR)** library: it `copy_files` the same `*.mlx`
  sources from `../frontend` (apps + components, never templates) and compiles
  them with `melange` + `reason-react`. It can't live inside `frontend/` because
  that directory's `(include_subdirs qualified)` would try to absorb it.
- `emit/` runs `melange.emit` over that mirror, producing one JS entry per app
  under `out/`. `fennec assemble` (driven by the top-level `dune`) then bundles
  each entry into `/_apps/<name>/main.js`.

All of this is build-only: the sources are copied/emitted into `_build`, so the
only things checked in here are these two tiny `dune` files. Add an app by
creating `frontend/apps/<name>/` — the only line you touch in here is one
`(subdir apps/<name> (copy_files …))` in `mirror/dune`.
