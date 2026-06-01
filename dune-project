(lang dune 3.16)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

; Melange (OCaml -> JS) for the isomorphic client: the same .mlx components that
; render natively on the server compile to JS here.
(using melange 0.1)

; Directory targets: the assembled web root (bundles + public/) is a single
; (dir ...) rule target served as one tree.
(using directory-targets 0.1)

; The runtime framework shipped to opam: portable HTTP core + Eio server with
; livereload. The MLX dialect is declared here once so .mlx components compile in
; any package (the isomorphic client/SSR components land here later).
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

(package
 (name fennec)
 (synopsis "A lean isomorphic web framework for OCaml — native server, Melange client")
 (description
  "Fennec is the runtime of the fennec fullstack framework: a portable HTTP core\n(Plug-style middleware, a shared route table) and a compact Eio HTTP + WebSocket\nserver with built-in dev livereload. No cohttp, no Lwt — Eio only. Pairs with the\n`fennec` CLI for the dev/build lifecycle, but a project stays a plain dune project.")
 (tags
  ("web" "framework" "eio" "isomorphic" "melange" "websocket"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  (routes
   (>= 2.0.0))
  (eio
   (>= 1.0))
  eio_main
  digestif
  base64
  zlib
  bigstringaf
  (ppxlib
   (>= 0.28))))

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
  ("build" "javascript" "css" "esbuild" "bundler" "cli" "melange"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  (cmdliner
   (>= 1.1.0))))

; Non-published: the userland examples (e.g. examples/helloworld). This package
; exists only to declare the example's Melange/isomorphic deps so CI can build
; them, WITHOUT those deps leaking into the published `fennec` framework. It is
; never released to opam.
(package
 (name fennec-examples)
 (allow_empty) ; examples are built, not installed — no stanzas attach here
 (synopsis "Fennec example applications (not published)")
 (description
  "The userland examples that exercise the fennec framework end to end — the\nliving DX benchmark. Not published to opam; depends on the Melange toolchain the\nexamples need (the framework itself does not).")
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))
  fennec
  fennec-cli
  (mlx
   (>= 0.11))
  (melange
   (>= 3.0.0))
  (reason-react
   (>= 0.14.0))
  server-reason-react))
