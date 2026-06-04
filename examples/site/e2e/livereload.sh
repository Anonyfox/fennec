#!/bin/sh
# Livereload regression test (dev-only). Runs the REAL `fennec dev` against the site example
# and proves that editing a frontend source file makes the change reach the browser — the two
# ways this has regressed before, both caught here, deterministically and without a browser:
#
#   1. the served CLIENT BUNDLE must be freshly REBUILT (a watch-target miss left it stale, so
#      the page reloaded but hydrated old code);
#   2. the bundle + page must be served NO-CACHE in dev (a prod max-age default let the browser
#      hydrate a cached old bundle).
#
# Fresh bundle + no-cache ⟹ a reloaded browser necessarily runs the new code. That is exactly
# what we assert here (via curl), so there is no rebuild/reload race to make this flaky.
#
# Usage:  sh examples/site/e2e/livereload.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
PAGE=http://localhost:8200/
BUNDLE=http://localhost:8200/_apps/web/main.js
SRC=examples/site/frontend/apps/web/index.mlx
MARK="LIVERELOAD_$(date +%s)"
fail() { echo "FAIL: $1"; cleanup; exit 1; }
cleanup() {
  [ -n "${DEV:-}" ] && kill -INT "$DEV" 2>/dev/null || true
  sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true
  git checkout "$SRC" 2>/dev/null || true
}
trap cleanup EXIT

echo "warming the dev build…"
dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

echo "starting fennec dev…"
( cd examples/site && fennec dev ) >/tmp/fennec_lr_test.log 2>&1 &
DEV=$!
i=0; while ! grep -q "serving 2 endpoint" /tmp/fennec_lr_test.log 2>/dev/null; do
  i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up in 40s"; sleep 0.5
done

# 1. dev must serve no-cache (else a cached bundle hydrates stale)
echo "checking dev cache headers…"
curl -sI "$PAGE"   | grep -qi 'cache-control: *no-cache' || fail "page is not served no-cache in dev"
curl -sI "$BUNDLE" | grep -qi 'cache-control: *no-cache' || fail "client bundle is not served no-cache in dev"

# 2. edit a frontend label, then the SSR *and* the client bundle must both reflect it
echo "editing $SRC (marker $MARK)…"
sed -i.bak "s/Welcome to the Fennec site/$MARK/" "$SRC" && rm -f "$SRC.bak"
i=0; while ! curl -s "$BUNDLE" | grep -q "$MARK"; do
  i=$((i+1)); [ $i -gt 40 ] && fail "client bundle never picked up the edit (stale hydration)"; sleep 0.5
done
curl -s "$PAGE" | grep -q "$MARK" || fail "SSR never picked up the edit"

echo "PASS: edit reached both the SSR and the freshly-rebuilt, uncached client bundle."
