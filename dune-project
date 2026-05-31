(lang dune 3.16)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

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
  base64))

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
