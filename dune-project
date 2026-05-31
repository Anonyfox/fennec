(lang dune 3.16)

(name fennec)

(generate_opam_files true)

(source
 (github Anonyfox/fennec))

(authors "Anonyfox <maximilian.stroh@gastrorocket.com>")

(maintainers "Anonyfox <maximilian.stroh@gastrorocket.com>")

(license MIT)

(package
 (name fennec-buildkit)
 (synopsis "In-process JavaScript bundler (esbuild) and CSS/SCSS engine for OCaml")
 (description
  "Fennec Buildkit statically links esbuild (Go) and Lightning CSS + grass (Rust)\ninto your OCaml binary as native libraries, so bundling JavaScript and compiling\nSCSS are plain function calls -- no node, no subprocess, no second-language\nruntime at run time. The native archives are compiled for the host platform\nautomatically at build time; library consumers need no manual steps.")
 (tags
  ("build" "javascript" "css" "esbuild" "bundler" "melange"))
 (depends
  (ocaml
   (>= 4.14))
  (dune
   (>= 3.16))))

; Build-time prerequisites that are NOT opam packages: a Go toolchain and a Rust
; toolchain. CI provisions them via setup-go / setup-rust; the dune rule then
; compiles the esbuild and CSS static archives from the pinned sources under
; buildkit/native (go.sum + Cargo.lock). A fully toolchain-free install path
; (prebuilt archives fetched from a release, checksum-verified) is the next step.
