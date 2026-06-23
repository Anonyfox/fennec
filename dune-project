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

; The vendored mlx parser (fennec/fur/prepass/vendor/) regenerates its menhir
; grammar from parser.mly via a (menhir …) stanza, so the `.mlx` toolchain needs
; no external `mlx` opam package — the parser is fennec-owned, dune-resolved.
(using menhir 2.1)

; The runtime framework shipped to opam: portable HTTP core + Eio server with
; livereload, and Fennec.Fur — the isomorphic UI runtime (signals + SSR + jsoo
; hydration; no Melange, no React). The MLX dialect is declared here once so .mlx
; components compile in any package.
;
; The preprocessor is the `fennec` binary ITSELF — `fennec mlx-pp` — resolved by dune from the
; project's deps via %{bin:fennec}, exactly like the asset bundler (`fennec build`). It runs a
; fennec-owned PRE-PASS that turns the fennec-flavoured .mlx surface (bare JSX text children +
; {expr} interpolation, JSX-identical) into the quoted-text + (expr) form, then parses it with the
; VENDORED mlx parser (fennec/fur/prepass/vendor/) — IN-PROCESS, no external `mlx` opam package and
; no separate dialect binary. The marshalled AST is byte-identical to the old fennec-mlx-pp (proven
; over every example). So `opam install fennec-cli` alone is the whole `.mlx` build toolchain.
;
; The merlin_reader is fennec-mlx: Merlin spawns `ocamlmerlin-fennec-mlx` as the external reader for
; .mlx buffers in the EDITOR. It runs the SAME pre-pass, then — if the stock `ocamlmerlin-mlx` is
; installed — DELEGATES to it for full error-recovery; if absent, it parses in-process with the SAME
; vendored parser, so the editor still works off fennec-cli alone (valid buffers + bare text parse,
; no false errors). It is an EDITOR-only tool: its `merlin-extend` dependency ships with
; merlin/ocaml-lsp, and the executable is built via a (select) — absent that dependency it falls back
; to a build-only stub and the BUILD stays green (the build never depends on merlin). See
; fennec/fur/prepass/bin/ocamlmerlin_fennec_mlx.ml.
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader fennec-mlx)
  (preprocess
   (run %{bin:fennec} mlx-pp %{input-file}))))

(package
 (name fennec)
 (synopsis "A lean isomorphic web framework for OCaml — Eio server, Fur UI, Pulse reactive data")
 (description
  "Fennec is the runtime of the fennec fullstack framework: a portable HTTP core\n(Plug-style middleware, a shared route table), a compact Eio HTTP + WebSocket\nserver with built-in dev livereload, Fennec.Fur — an isomorphic UI runtime\n(signals + SSR + js_of_ocaml hydration; no Melange, no React), and Fennec.Pulse —\na Meteor-style reactive data layer (publish/subscribe over DDP, live queries,\nSSR-with-live-data). No cohttp, no Lwt — Eio only. Pairs with the `fennec` CLI\nfor the dev/build lifecycle, but a project stays a plain dune project.")
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
  tls
  tls-eio
  x509
  ca-certs
  domain-name
  zarith
  ptime
  mirage-crypto-rng
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
  (ppxlib
   (>= 0.28))
  (yojson
   (>= 2.0))
  (base64
   (>= 3.0))
  (re
   (>= 1.10))
  ; TLS transport for https targets (accept-any-cert default — testing your own server)
  tls
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
;
; fennec-cli owns the WHOLE `.mlx` dialect toolchain, with ZERO external dependency: the build
; preprocessor is `fennec mlx-pp` (the `fennec` binary itself, resolved by dune via %{bin:fennec}
; in the `(dialect …)` stanza — like the asset bundler `fennec build`), and the editor reader is
; `ocamlmerlin-fennec-mlx`. Both parse `.mlx` IN-PROCESS with mlx's parser VENDORED into the binary
; (fennec/fur/prepass/vendor/, same model as the vendored Go esbuild / Rust Lightning CSS) — no
; `mlx` opam package, no separate dialect binary. So `opam install fennec-cli` is the entire `.mlx`
; toolchain in one step; a frontend `.mlx` app builds + the editor lights up with nothing else to
; install. (Run `fennec doctor` to verify.) The vendored parser is regenerated by `menhir` at build
; time (a build-only dep); `menhirLib` is linked into the binary transitively.

(package
 (name fennec-cli)
 (synopsis "Native JavaScript and CSS build tooling in a single self-contained binary")
 (description
  "The `fennec` command-line tool bundles JavaScript (esbuild) and compiles and\noptimizes CSS and SCSS (Lightning CSS + grass) from one statically-linked binary.\nNo Node, no separate toolchain: download the binary and build. `fennec build`\ndrives both engines in a single pass, routing each input to the right tool by its\nfile extension. It also IS the `.mlx` dialect toolchain — `fennec mlx-pp` (the build\npreprocessor dune runs on every `.mlx`) and `ocamlmerlin-fennec-mlx` (the editor reader)\n— parsing `.mlx` in-process with a VENDORED copy of the mlx parser, so installing this\npackage pulls everything a frontend Fennec app needs with NO external `mlx` dependency.\nDistributed as a prebuilt per-platform binary (GitHub Releases, and Homebrew\nlater) — its build links Go + Rust archives, so it is not an opam-install-from-source\npackage; the .opam exists only so dune knows the package locally.")
 (tags
  ("build" "javascript" "css" "esbuild" "bundler" "cli"))
 (depends
  (ocaml
   (>= 5.2))
  (dune
   (>= 3.16))
  (cmdliner
   (>= 1.1.0))
  (yojson
   (>= 2.0))
  fennec
  fennec-mongo
  fennec-hunt
  ; the `.mlx` parser is VENDORED (fennec/fur/prepass/vendor/) and regenerated from parser.mly at
  ; build time, so `menhir` is a build-only dependency (its `menhirLib` is linked into the binary).
  ; No `mlx` opam package is needed any more — the parser is fennec-owned.
  (menhir
   (>= 20201216))
  (eio
   (>= 1.0))
  eio_main
  (ppxlib
   (>= 0.28))))

; Non-published: the userland examples. This package exists only to declare the
; example's extra build deps so CI can build them, WITHOUT those leaking into the
; published `fennec` framework. It is never released to opam. The `.mlx` dialect needs
; no extra dep — its preprocessor IS `fennec mlx-pp` (from fennec-cli), parser vendored.
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
  ; the site example's test suites (examples/site/test/{http,system,browser}) use the standalone
  ; hunt driver; it brings yojson/base64 transitively. No npm, no Lwt, no chromedriver.
  fennec-hunt))

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

; A standalone HTTP micro-framework carved out of the runtime: the HTTP vocabulary
; (request/response, headers, cookies, multipart, MIME, dates, semantics), the
; connection carrier with typed assigns (Conn), the paw pipeline algebra + routing,
; the websocket channel, and the dev/livereload wire. Depends on no other fennec.*
; library, so it stands on its own; the framework builds on it.
(package
 (name fennec-paw)
 (synopsis "A standalone HTTP micro-framework for OCaml/Eio — routing, middleware, the Conn pipeline")
 (description
  "fennec-paw is a compact HTTP/1.1 + WebSocket server and toolkit for OCaml on Eio\n(no cohttp, no Lwt): a routing + middleware pipeline around one primitive (a paw,\n[Conn.t -> Conn.t]), a prebuilt middleware battery (CORS, CSRF, sessions, static,\nrate limiting, logging, security headers, …), in-process TLS termination with\nautomatic HTTPS via ACME/Let's Encrypt, and virtual-host routing. One curated\n[Paw.*] namespace; no dependency on any other fennec library.")
 (depends
  (ocaml
   (>= 5.2))
  dune
  eio
  eio_main
  tls
  tls-eio
  x509
  ca-certs
  digestif
  base64
  zarith
  ptime
  mirage-crypto-rng
  domain-name
  bigstringaf
  zlib
  yojson))
