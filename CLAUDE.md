# Fennec monorepo

The Fennec OCaml fullstack framework and its standalone packages: `paw/` (HTTP micro-framework),
`mongo/` (the MongoDB data plane — minimongo / the embedded Burrow engine / the libmongoc driver),
`hunt/` (the testing package), `cli/` (the `fennec` binary), `fennec/` (the framework proper),
`examples/site/` (the reference app). OCaml 5 + Eio + dune, one workspace.

## Build & verify

```sh
dune build              # whole workspace
dune build @runtest     # the full test gate — green before every commit
```

The example app is driven by the `fennec` CLI (`./_build/default/cli/fennec.exe`); `fennec skill`
prints the agent guide for app-level work (dev server, agent fastlane, edit→verdict loop).

## The testing ladder — where a test goes

Testing runs on `hunt/` (fennec-hunt) at every caliber. The rule optimizes for colocated context —
the test lives as close to the code as its caliber allows:

1. **Inline `let%test` / `let%prop` in the source file — the DEFAULT.** Any localized logic
   (parsing, encoding, matching, pure transforms) is tested at the bottom of the file that
   implements it. One dune stanza wires a library: `(inline_tests (backend fennec-hunt.runner))`
   + `(preprocess (pps fennec-hunt.ppx))` (or `fennec.fur.ppx` for `.mlx`). Do NOT create a
   separate test file for unit-caliber assertions — sprawl across files is the anti-pattern.

2. **A `test/` folder next to the lib — integration caliber.** Real IO, subprocesses, tmp dirs,
   differential suites, spec-vector conformance (e.g. `mongo/burrow/engine/test/`,
   `mongo/wire/test/`): plain self-asserting executables under a `(tests …)` stanza, one concept
   per file, named `test_<concept>.ml`.

3. **App-level cuts `test/{http,browser,system}/` — hunt's Http / Live / System.** Drop a
   `*_test.ml` with a `let%http` / `let%browser` / `let%system` block; `fennec test <cut>` runs
   each suite against its own isolated instance. Browser tests drive real Chromium over CDP.

**The one exception (js physics):** inline registrations compile INTO the library, and some pure
libs ship to the browser via js_of_ocaml (`mongo/bson`, `mongo/query`, `mongo/mem`, `bson_json`,
sift). Inlining there would carry test closures into every app bundle — so js-shipping libs keep
their tests in a `test/` folder (`mongo/test/test_mongo.ml` states the rule). `.mlx` components
are the exception to the exception: their client mirrors are compiled with `-fennec-drop-tests`,
so components DO test inline.

## Conventions

- One concept per file; avoid file sprawl. Every public module carries its `.mli` (a file of raw
  externals is its own interface). Internal modules are hidden via `private_modules` / wrapping.
- No hot-path taxes: features must not slow the per-request/per-write path (see
  `mongo/burrow/PRODUCTION.md` §2 for the covenant on the engine).
- Commit each verified unit separately; `dune build @runtest` green first.
