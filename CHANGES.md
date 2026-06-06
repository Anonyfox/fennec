# Changelog

## Unreleased

### fennec-hunt

- Initial release: a two-layer testing library for OCaml web apps, with no dependency on any
  framework (works against any server). Packaged separately from `fennec` so production
  servers never link it.
  - **Http tests** (`Fennec_hunt.Http`) — a typed HTTP/1.1 client hand-written on Eio sockets
    (no cohttp) with deterministic, no-retry assertions over any URL: status families, body
    (substring / exact / regex / emptiness), headers, JSON-by-dotted-path (value, type, UUID,
    datetime, array length), cookies (automatic per-test jar), redirects, and timing. `http`
    and `https` both work (TLS via tls-eio, accept-any-cert by default for self-signed /
    staging servers). Bare-function DSL: `hunt`/`check` blocks, `get`/`post`/…, `~query` /
    `~form` / `~json` request bodies. Spawn a server (killed on exit via the Eio switch) or
    test one already running; multiple suites per process all run, exit non-zero once at the end.
  - **Browser tests** (`Fennec_hunt.Live` + `Run`) — a self-contained real-browser driver: a
    hand-written WebSocket + Chrome DevTools Protocol client on Eio, an auto-waiting page DSL,
    a deterministic in-memory fake backend for unit tests, rich self-explaining failure
    reports, and a cross-platform terminal reporter (TTY vs CI). No Node, no chromedriver, no
    Selenium, no Lwt; only a Chromium-family browser at runtime.
  - (Formerly `fennec-e2e`; renamed to `fennec-hunt` when the Http layer was added.)
