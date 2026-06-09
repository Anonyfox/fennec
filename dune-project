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
   (>= 5.2))
  (dune
   (>= 3.16))
  (eio
   (>= 1.0))
  eio_main
  digestif
  base64
  zlib
  bigstringaf
  fennec-mongo
  fennec-hunt
  (ppxlib
   (>= 0.28))
  js_of_ocaml
  js_of_ocaml-ppx))

; The testing package (Fennec_hunt). It is kept in its OWN package and stays standalone
; (no dependency on the framework), but the framework intentionally depends on its tiny inline-test
; runtime/ppx pieces so production modules can keep colocated let%test / let%prop checks. DX wins:
; the same syntax works everywhere, and Dune's package metadata is honest about it. Two independent layers:
; an Http layer (a typed HTTP/1.1 client + deterministic assertions over any URL) and a
; Browser layer (a hand-written WebSocket + Chrome DevTools Protocol client, an auto-waiting
; page DSL, an in-memory fake backend for unit tests, a cross-platform reporter). No npm, no
; Lwt, no chromedriver, no cohttp — only a Chromium-family browser at runtime, for the Browser
; layer. The prod-lean guarantee is now "no full browser/http machinery unless asked for";
; fennec-hunt.unit/.prop/.ppx.rules are part of the developer platform.
(package
 (name fennec-hunt)
 (synopsis "HTTP + real-browser testing for OCaml web apps — pure OCaml/Eio, no Node or Selenium")
 (description
  "A two-layer testing library for OCaml web apps, with no dependency on any framework. The\nHttp layer is a typed HTTP/1.1 client (hand-written on Eio sockets, no cohttp) with\ndeterministic assertions over any URL — status, body, headers, JSON-by-path, cookies,\nredirects, timing — plus an automatic per-test cookie jar. http and https targets both work\n(TLS via tls-eio, accept-any-cert by default for self-signed/staging servers). The Browser\nlayer drives a real headless Chromium via a hand-written Chrome DevTools Protocol client: an\nauto-waiting page DSL, rich self-explaining failures, a deterministic in-memory fake backend\n(so the DSL is unit-tested without a browser), and a terminal reporter that adapts from a TTY\nto a CI log. No Node, no chromedriver, no Selenium, no Lwt. The small inline-test runtime and\nppx rules are intentionally reusable by the framework and CLI; the browser/http machinery stays\nseparate unless a user asks for it.")
 (tags
  ("testing" "http" "integration" "e2e" "browser" "chrome" "devtools-protocol" "tls" "eio"))
 (depends
  ; Eio is direct-style effects, so OCaml 5+ is required (not 4.14).
  (ocaml
   (>= 5.2))
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
   (>= 1.10))
  ; TLS transport for https targets (accept-any-cert default — testing your own server)
  (tls-eio
   (>= 2.0))
  (x509
   (>= 1.0))
  domain-name
  mirage-crypto-rng
  ; property testing (let%prop / Fennec_hunt.Prop) — lean, pure-OCaml, test-only weight
  (qcheck-core
   (>= 0.20))))

; The CLI is the distributable: a self-contained `fennec` binary that statically
; links the native bundlers (esbuild, Lightning CSS, grass). It is released as a
; prebuilt binary per platform via GitHub Releases, so end users never compile
; Go/Rust. fennec_buildkit is an internal, unpublished library (the CLI's engine).

(package
 (name fennec-cli)
 (synopsis "Native JavaScript and CSS build tooling in a single self-contained binary")
 (description
  "The `fennec` command-line tool bundles JavaScript (esbuild) and compiles and\noptimizes CSS and SCSS (Lightning CSS + grass) from one statically-linked binary.\nNo Node, no separate toolchain: download the binary and build. `fennec build`\ndrives both engines in a single pass, routing each input to the right tool by its\nfile extension. Distributed as a prebuilt per-platform binary (GitHub Releases, and\nHomebrew later) — its build links Go + Rust archives, so it is not an opam-install-\nfrom-source package; the .opam exists only so dune knows the package locally.")
 (tags
  ("build" "javascript" "css" "esbuild" "bundler" "cli"))
 (depends
  (ocaml
   (>= 5.2))
  (dune
   (>= 3.16))
  (cmdliner
   (>= 1.1.0))
  fennec
  fennec-mongo
  fennec-hunt
  (eio
   (>= 1.0))
  eio_main
  (ppxlib
   (>= 0.28))))

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
   (>= 5.2))
  (dune
   (>= 3.16))
  fennec
  fennec-cli
  ; the site example's real-browser e2e suite (examples/site/e2e) uses the standalone
  ; e2e driver; it brings yojson/base64 transitively. No npm, no Lwt, no chromedriver.
  fennec-hunt
  (mlx
   (>= 0.11))))

; The data layer: a standalone MongoDB story — a pure BSON value + query/update
; engine + in-memory Minimongo (this trio also compiles to JS for an npm bundle),
; and (later) a native driver statically linking libmongoc. Useful on its own; the
; `fennec` framework depends on it and bolts the Meteor-style reactive API on top.
; See DATAFLOW.md. This pass ships the PURE trio only; the native driver follows.
(package
 (name fennec-mongo)
 (synopsis "Pure-OCaml BSON, a MongoDB query/update engine, and in-memory Minimongo")
 (description
  "A standalone data layer: a BSON value type, a pure MongoDB query/update/projection/sort engine\nwith geospatial and aggregation support, an in-memory Minimongo collection with a reactive observe\nengine, and an extended-JSON codec. The pure core is OCaml-only (just `re` beyond Stdlib), so the\nsame source runs native and in the browser via js_of_ocaml. It also ships an optional native driver\n(fennec-mongo.ffi) that statically vendors libmongoc — built from source as static archives with a\nnative TLS backend, so a downstream binary is self-contained — plus a mongod lifecycle helper\n(fennec-mongo.mongod). The native driver degrades gracefully (in-memory only) where it cannot be\nbuilt, so this package builds across the matrix.")
 (tags
  ("mongodb" "bson" "minimongo" "database" "query" "javascript" "jsoo"))
 (depends
  (ocaml
   (>= 5.2))
  (dune
   (>= 3.16))
  re
  (eio
   (>= 1.0))
  dune-configurator
  (conf-pkg-config :build)))
