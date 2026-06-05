(lang dune 3.16)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

; Directory targets: the assembled web root (bundles + public/) is a single
; (dir ...) rule target served as one tree.
(using directory-targets 0.1)

; The runtime framework shipped to opam: portable HTTP core + Eio server with
; livereload, and Fennec.Fur — the isomorphic UI runtime (signals + SSR + jsoo
; hydration; no Melange, no React). The MLX dialect is declared here once so .mlx
; components compile in any package.
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(package
 (name fennec)
 (synopsis "A lean isomorphic web framework for OCaml — native Eio server, Fur UI runtime")
 (description
  "Fennec is the runtime of the fennec fullstack framework: a portable HTTP core\n(Plug-style middleware, a shared route table), a compact Eio HTTP + WebSocket\nserver with built-in dev livereload, and Fennec.Fur — an isomorphic UI runtime\n(signals + SSR + js_of_ocaml hydration; no Melange, no React). No cohttp, no Lwt —\nEio only. Pairs with the `fennec` CLI for the dev/build lifecycle, but a project\nstays a plain dune project.")
 (tags
  ("web" "framework" "eio" "isomorphic" "ssr" "websocket"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  (eio
   (>= 1.0))
  eio_main
  digestif
  base64
  zlib
  bigstringaf
  (ppxlib
   (>= 0.28))
  js_of_ocaml
  js_of_ocaml-ppx))

; The real-browser e2e testing driver (Fennec_hunt). A DEV/TEST tool, kept in its OWN
; package so the runtime `fennec` — and therefore every production server that links it —
; never carries the e2e/CDP machinery or its yojson dependency. It is a standalone Eio
; library (no dependency on the framework): a hand-written WebSocket + Chrome DevTools
; Protocol client, an auto-waiting page DSL, an in-memory fake backend for unit tests, and
; a cross-platform reporter. No npm, no Lwt, no chromedriver — only a Chromium-family
; browser at runtime. (The prod-lean guarantee is enforced by a runtest tripwire in the
; site example: examples/site/check_lean.ml.)
(package
 (name fennec-hunt)
 (synopsis "Real-browser end-to-end testing for Fennec — pure OCaml/Eio over the Chrome DevTools Protocol")
 (description
  "A self-contained headless-browser e2e driver for Fennec apps: a hand-written WebSocket\nand Chrome DevTools Protocol client on Eio, an auto-waiting page DSL with rich failure\ndiagnostics, a deterministic in-memory fake backend (so the DSL/runner are unit-tested\nwithout a browser), and a terminal reporter that adapts from a TTY to a CI log. No Node,\nno chromedriver, no Selenium, no Lwt — only a Chromium-family browser at runtime. Shipped\nas a separate package so production servers never link it.")
 (tags
  ("testing" "e2e" "browser" "chrome" "devtools-protocol" "eio"))
 (depends
  ; Eio is direct-style effects, so OCaml 5+ is required (not 4.14).
  (ocaml
   (>= 5.0))
  (dune
   (>= 3.16))
  (eio
   (>= 1.0))
  eio_main
  (yojson
   (>= 2.0))
  (base64
   (>= 3.0))
  (re
   (>= 1.10))))

; The CLI is the distributable: a self-contained `fennec` binary that statically
; links the native bundlers (esbuild, Lightning CSS, grass). It is released as a
; prebuilt binary per platform via GitHub Releases, so end users never compile
; Go/Rust. fennec_buildkit is an internal, unpublished library (the CLI's engine).

(package
 (name fennec-cli)
 (synopsis "Native JavaScript and CSS build tooling in a single self-contained binary")
 (description
  "The `fennec` command-line tool bundles JavaScript (esbuild) and compiles and\noptimizes CSS and SCSS (Lightning CSS + grass) from one statically-linked binary.\nNo Node, no separate toolchain: download the binary and build. `fennec build`\ndrives both engines in a single pass, routing each input to the right tool by its\nfile extension.")
 (tags
  ("build" "javascript" "css" "esbuild" "bundler" "cli"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  (cmdliner
   (>= 1.1.0))))

; Non-published: the userland examples. This package exists only to declare the
; example's extra build deps (the mlx dialect) so CI can build them, WITHOUT those
; leaking into the published `fennec` framework. It is never released to opam.
(package
 (name fennec-examples)
 (allow_empty) ; examples are built, not installed — no stanzas attach here
 (synopsis "Fennec example applications (not published)")
 (description
  "The userland examples that exercise the fennec framework end to end — the\nliving DX benchmark. Not published to opam.")
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  fennec
  fennec-cli
  ; the site example's real-browser e2e suite (examples/site/e2e) uses the standalone
  ; e2e driver; it brings yojson/base64 transitively. No npm, no Lwt, no chromedriver.
  fennec-hunt
  (mlx
   (>= 0.11))))
