# Vendored mlx parser

This tree is the **mlx** OCaml-with-JSX parser, vendored into fennec so the `.mlx`
dialect toolchain builds with **only** `fennec-cli` — no external `mlx` opam package,
no separate dialect binary on `PATH`. It is the same model as the already-vendored Go
esbuild / Rust Lightning CSS archives in `fennec_buildkit`: a third-party engine pinned
and built from source, so a downstream `fennec` is self-contained.

## Provenance

- **Source:** [`ocaml-mlx/mlx`](https://github.com/ocaml-mlx/mlx), opam package `mlx.0.11`
  (`~/.opam/<switch>/.opam-switch/sources/mlx.0.11/mlx/`).
- **Pinned to:** OCaml **5.4.1** (the parser is OCaml's own `parser.mly` / `lexer.mll`
  fork — it tracks the compiler's AST, so it is version-coupled to the switch's OCaml).
- **License:** `LICENSE.mlx` (LGPL 2.1 with the OCaml linking exception, © Andrey Popp,
  derived from the OCaml Core System) — kept verbatim alongside the sources.

## Layout (a 3-library split — see each `dune` for why)

| dir       | library          | files                                                            |
| --------- | ---------------- | ---------------------------------------------------------------- |
| `shim/`   | `mlx_shim`       | `mlx_shim.ml` — compiler-libs bridge under the Astlib shadow     |
| `rt/`     | `fennec_mlx_rt`  | `warnings` `syntaxerr` `ast_helper` `docstrings` `jsx_helper`    |
| `parser/` | `fennec_mlx`     | `lexer.mll` `parser.mly` `parse.ml(.mli)` — the parser proper    |

`fennec_mlx.Parse.implementation : Lexing.lexbuf -> Parsetree.structure` is the entry
point `fennec mlx-pp` and the merlin reader call in-process.

The single-unwrapped-lib layout hits the compiler's "Warnings is dangling" shadow snag;
one wrapped lib hits a menhir "Parser depends on the wrapper" cycle. The shim-as-leaf +
rt + parser split sidesteps both.

## Re-syncing on an OCaml / menhir bump

The parser is **regenerated from `parser.mly`** via the `(menhir …)` stanza in
`parser/dune` (NOT mlx's promoted `parser.ml`, which is locked to menhir 20201216).
When the switch's OCaml version changes:

1. `opam reinstall mlx` (or fetch the mlx release matching the new OCaml).
2. Re-copy the files in the table above from `…/sources/mlx.<v>/mlx/`.
3. Re-copy `LICENSE.mlx` from `…/sources/mlx.<v>/`.
4. `dune build @all @check && dune runtest` — the AST-equivalence / bundle-byte tests
   guard that the marshalled output is unchanged.

Do **not** hand-edit these files; treat the tree as a vendored drop.
