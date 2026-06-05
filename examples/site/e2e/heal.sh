#!/bin/sh
# `fennec dev --clean` regression test (dev-only). The opt-in nuclear heal: --clean runs a full
# `dune clean` before starting (for the rare corrupt-_build case a normal restart can't fix),
# while a plain `fennec dev` leaves _build alone. We prove both with a sentinel planted at the
# root of _build:
#
#   - plain `fennec dev`      -> the sentinel SURVIVES (no clean);
#   - `fennec dev --clean`    -> the sentinel is GONE (dune clean ran).
#
# Fast on purpose: for --clean we only wait for the clean to land, then stop — we do NOT sit
# through the from-scratch rebuild. NOTE: this leaves _build cleaned; your next build rebuilds it.
#
# Usage:  sh examples/site/e2e/heal.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
SENT=_build/CLEAN_SENTINEL
DEV=""
fail() { echo "FAIL: $1"; cleanup; exit 1; }
cleanup() { [ -n "$DEV" ] && kill -INT "$DEV" 2>/dev/null || true; sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; }
trap cleanup EXIT

echo "warming…"
dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

echo "1) a plain start leaves _build alone…"
touch "$SENT"
( cd examples/site && exec fennec dev ) >/tmp/heal_1.log 2>&1 & DEV=$!
i=0; while ! grep -q "ready" /tmp/heal_1.log 2>/dev/null; do i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up"; sleep 0.5; done
[ -f "$SENT" ] || fail "a plain start wrongly cleaned _build"
curl -s http://localhost:4001/ | grep -q "Welcome to the Fennec site" || fail "plain start didn't serve"
kill -INT "$DEV" 2>/dev/null || true; DEV=""; sleep 2; pkill -9 -f "dune build --watch" 2>/dev/null || true

echo "2) --clean runs a full dune clean before starting…"
touch "$SENT"
( cd examples/site && exec fennec dev --clean ) >/tmp/heal_2.log 2>&1 & DEV=$!
# wait only for the clean to land (the from-scratch rebuild that follows is slow; we don't wait)
i=0; while [ -f "$SENT" ]; do i=$((i+1)); [ $i -gt 40 ] && fail "--clean did not run dune clean (sentinel still present)"; sleep 0.5; done
grep -q "dune clean" /tmp/heal_2.log || fail "--clean did not announce the clean"
kill -INT "$DEV" 2>/dev/null || true; DEV=""

echo "PASS: plain start preserves _build; --clean wipes it. (NOTE: _build is now clean — your next build rebuilds from scratch.)"
