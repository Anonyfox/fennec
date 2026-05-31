(lang dune 3.16)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

; NOTE: no project-level (name ...) yet — dune requires that a project with a
; single package share its name, and the core `fennec` package does not exist
; yet. It will be added when the framework packages land.

(package
 (name fennec-buildkit)
 (synopsis "In-process JavaScript bundler (esbuild) and CSS/SCSS engine for OCaml")
 (description
  "Fennec Buildkit statically links esbuild (Go) and Lightning CSS + grass (Rust)\ninto your OCaml binary as native libraries, so bundling JavaScript and compiling\nSCSS are plain function calls -- no node, no subprocess, no second-language\nruntime at run time. The native archives are compiled for the host platform\nautomatically at build time; library consumers need no manual steps beyond a Go\nand a Rust toolchain being available when the package is built.")
 (tags
  ("build" "javascript" "css" "esbuild" "bundler" "melange"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))))
