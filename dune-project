(lang dune 3.16)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

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
