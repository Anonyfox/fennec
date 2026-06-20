# Vendored mlx parser

This tree is the **mlx** OCaml-with-JSX parser, vendored into fennec so the `.mlx`
dialect toolchain builds with **only** `fennec-cli` — no external `mlx` opam package,
no separate dialect binary on `PATH`. It is the same model as the already-vendored Go
esbuild / Rust Lightning CSS archives in `fennec_buildkit`: a third-party engine pinned
and built from source, so a downstream `fennec` is self-contained.

## Provenance

- **Source:** [`ocaml-mlx/mlx`](https://github.com/ocaml-mlx/mlx), opam package `mlx.0.11`
  (`~/.opam/<switch>/.opam-switch/sources/mlx.0.11/mlx/`).
- **Targets:** the *frozen* `Astlib.Ast_501` parsetree — it does **not** track the live
  compiler (`fennec mlx-pp` migrates Ast_501 → the running compiler's AST via ppxlib).
  First vendored on OCaml **5.4.1** / mlx 0.11. See **Toolchain coupling** below: most
  OCaml bumps need no action here.
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

## Toolchain coupling — why most OCaml bumps need NOTHING

This vendor is far more version-stable than a "tracks the compiler" parser, by design.
Three things insulate it, so a minor (and usually a major) OCaml bump just **rebuilds and
works** — you do **not** re-vendor on version bumps:

- **The AST is bridged, not pinned.** The parser emits the *frozen* `Astlib.Ast_501`;
  `fennec mlx-pp` migrates it to whatever compiler is live via ppxlib's
  `Convert(OCaml_501)(Compiler_version)`. ppxlib keeps every historical AST migration, so a
  new OCaml → new ppxlib → still migrates Ast_501. The vendored sources don't change.
- **menhir / ocamllex regenerate at build time** from `parser.mly` / `lexer.mll` (we do NOT
  ship mlx's promoted `parser.ml`, which is locked to menhir 20201216), so they self-heal
  across menhir versions.
- **The only live-compiler touch** is `Ocaml_common.Location` + `Ocaml_common.Warnings` —
  two of the most stable compiler-libs interfaces, unchanged for years.

## When to re-vendor (rarely — and the build tells you)

The guard is automatic and loud. If a new toolchain ever breaks this parser, the vendored
libs fail to **compile**, or the parse tests go **red**: `vendor/proof_parse.ml`,
`test/parses_through_mlx.ml`, and the 31-file AST-equivalence oracle all run under
`dune build @all @check && dune runtest`. **Green = nothing to do, on any toolchain you
build on.**

Re-vendor ONLY when one of these actually happens:

1. **The guard above goes red** on a new toolchain (a genuine compiler-libs / ppxlib break —
   major-version territory), or
2. **You want NEW OCaml syntax usable inside `.mlx`** files (the grammar would need mlx's
   newer `parser.mly`).

The procedure is a ~10-minute mechanical drop, not a research task:

1. `opam reinstall mlx` (or fetch the mlx release matching your OCaml).
2. Re-copy the files in the layout table from `…/sources/mlx.<v>/mlx/` + `LICENSE.mlx`.
3. `dune build @all @check && dune runtest` — the guard confirms the marshalled AST is
   unchanged.

Do **not** hand-edit these files; treat the tree as a vendored drop.
