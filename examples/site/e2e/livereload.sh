#!/bin/sh
# Livereload regression test (dev-only). Runs the REAL `fennec dev` against the site example
# and guards the ways livereload has actually broken before — deterministically, no browser:
#
#   1. SINGLE INSTANCE: a second `fennec dev` must reap the first, so a stale supervisor (and
#      the server it keeps respawning) can never win the port and serve an old build.
#   2. STARTUP FRESHNESS: on start, the server must serve the CURRENT on-disk source, not a
#      stale build left in _build.
#   3. NO-CACHE: the page + client bundle must be served no-cache in dev (a prod max-age
#      default let the browser hydrate a cached old bundle).
#   4. EDIT PROPAGATION: editing a frontend source must reach BOTH the SSR and the freshly
#      REBUILT client bundle (a watch-target miss left the bundle stale -> old hydration).
#
# Fresh + uncached bundle ⟹ a reloaded browser necessarily runs the new code, so these cover
# the browser-visible behaviour without a rebuild/reload race to make the test flaky.
#
# Usage:  sh examples/site/e2e/livereload.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
PAGE=http://localhost:8200/
BUNDLE=http://localhost:8200/_apps/web/main.js
SRC=examples/site/frontend/apps/web/index.mlx
MARK="LIVERELOAD_$(date +%s)"
D1="" ; D2=""
h1() { curl -s "$PAGE" 2>/dev/null | grep -oE '<h1>[^<]*</h1>'; }
fail() { echo "FAIL: $1"; exit 1; }
cleanup() {
  [ -n "$D1" ] && kill -9 "$D1" 2>/dev/null || true
  [ -n "$D2" ] && kill -INT "$D2" 2>/dev/null || true
  sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true
  git checkout "$SRC" 2>/dev/null || true
}
trap cleanup EXIT

echo "warming the dev build…"
dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

up() { i=0; while ! grep -q "serving 2 endpoint" "$1" 2>/dev/null; do i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up"; sleep 0.5; done; }

echo "starting instance 1…"
( cd examples/site && exec fennec dev ) >/tmp/fennec_lr_1.log 2>&1 & D1=$!
up /tmp/fennec_lr_1.log

echo "1) single instance — a second start must reap the first…"
( cd examples/site && exec fennec dev ) >/tmp/fennec_lr_2.log 2>&1 & D2=$!
up /tmp/fennec_lr_2.log
sleep 2
kill -0 "$D1" 2>/dev/null && fail "first instance still alive after a second start (orphans accumulate)"
D1=""

echo "2) startup freshness — served h1 must match the on-disk source (clean checkout)…"
DISK="Welcome to the Fennec site"
grep -q "$DISK" "$SRC" || fail "test precondition: $SRC is not at a clean checkout"
h1 | grep -q "$DISK" || fail "server served a STALE build on startup (expected '$DISK', got '$(h1)')"

echo "3) dev cache headers must be no-cache…"
curl -sI "$PAGE"   | grep -qi 'cache-control: *no-cache' || fail "page is not served no-cache in dev"
curl -sI "$BUNDLE" | grep -qi 'cache-control: *no-cache' || fail "client bundle is not served no-cache in dev"

echo "4) edit propagation — SSR and the client bundle must both pick up an edit…"
sed -i.bak "s/Welcome to the Fennec site/$MARK/" "$SRC" && rm -f "$SRC.bak"
i=0; while ! curl -s "$BUNDLE" | grep -q "$MARK"; do i=$((i+1)); [ $i -gt 40 ] && fail "client bundle never picked up the edit (stale hydration)"; sleep 0.5; done
curl -s "$PAGE" | grep -q "$MARK" || fail "SSR never picked up the edit"

# revert WHILE fennec dev is alive, so the running dune --watch re-syncs and leaves no stale
# _build (reverting after kill, e.g. via `git checkout`, is NOT seen by dune and freezes the
# build at the marker). Asserting the revert propagates also guards that very failure.
echo "5) revert propagation — undo the edit so no stale build is left behind…"
sed -i.bak "s/$MARK/Welcome to the Fennec site/" "$SRC" && rm -f "$SRC.bak"
i=0; while ! curl -s "$PAGE" | grep -q "Welcome to the Fennec site"; do i=$((i+1)); [ $i -gt 40 ] && fail "revert not picked up — dune left a stale build"; sleep 0.5; done

echo "PASS: single instance, fresh on startup, no-cache, edit+revert both propagate cleanly."
