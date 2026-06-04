# Changelog

## Unreleased

### fennec-e2e

- Initial release: a self-contained real-browser end-to-end testing library — a hand-written
  WebSocket + Chrome DevTools Protocol client on Eio, an auto-waiting page DSL, a
  deterministic in-memory fake backend for unit tests, rich self-explaining failure reports,
  and a cross-platform terminal reporter (TTY vs CI). No Node, no chromedriver, no Selenium,
  no Lwt; only a Chromium-family browser at runtime. Packaged separately from the `fennec`
  framework so production servers never link it.
