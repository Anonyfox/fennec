#!/bin/sh
# Real-browser e2e for the site example — pure OCaml/Eio, ZERO npm. Builds the server +
# web root + the e2e driver, then runs it: it boots the server, launches a headless
# Chrome, and drives it over the DevTools Protocol (see cdp.ml). Needs a Chrome install
# (override with CHROME=/path/to/chrome). No chromedriver, no Lwt, no node_modules.
#
# Usage:  sh examples/site/e2e/run.sh
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
dune build examples/site/server.exe examples/site/webroot examples/site/e2e/e2e.exe
exec _build/default/examples/site/e2e/e2e.exe _build/default/examples/site/server.exe
