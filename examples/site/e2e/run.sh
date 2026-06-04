#!/bin/sh
# Real-browser e2e for the site example, on the fennec.e2e driver — pure OCaml/Eio, ZERO
# npm. Builds the server + web root + the runner, then runs it: it boots the server,
# launches a headless Chrome, and drives it over the DevTools Protocol. Needs a Chromium
# install (override with CHROME=/path/to/chrome). No chromedriver, no Lwt, no node_modules.
#
# Usage:  sh examples/site/e2e/run.sh
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
dune build examples/site/server.exe examples/site/webroot examples/site/e2e/run.exe
# The prefix a failure report prints in its copy-pasteable "rerun" line, so it points back
# at THIS wrapper rather than the _build/ binary.
export FENNEC_E2E_RERUN="sh examples/site/e2e/run.sh"
# No retries: navigation is synchronised by loaderId and evals are pinned to the live
# execution context, so the run is deterministic. Flags: --grep <name>, --bail, --jobs N,
# --headed, --timeout S, --browsers M.
exec _build/default/examples/site/e2e/run.exe "$ROOT/_build/default/examples/site/server.exe" "$@"
