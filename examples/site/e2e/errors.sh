#!/bin/sh
# Error-panel regression test (dev-only). Provokes a build error through the REAL `fennec dev` and
# asserts the terminal does the right thing — the exact bugs that bit us:
#
#   1. the panel shows the RIGHT count (a syntax error's "might be unmatched" hint is the SAME
#      error, not a second one) and an actual message;
#   2. FIXING it clears the panel — even when the fix is a revert to byte-identical output, which
#      dune rebuilds WITHOUT bumping the artifact mtime (the "stuck panel after a fix" bug). The
#      supervisor must still notice the green build and print "resolved".
#
# Usage:  sh examples/site/e2e/errors.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
L=examples/site/frontend/apps/web/layout.mlx
LOG=/tmp/fennec_err_test.log
DEV=""
log_plain() { sed 's/\x1b\[[0-9;]*m//g' "$LOG"; }
fail() { echo "FAIL: $1"; echo "--- log ---"; log_plain | grep -vE '^\s*$' | tail -20; exit 1; }
cleanup() { [ -n "$DEV" ] && kill -INT "$DEV" 2>/dev/null || true; sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; git checkout "$L" 2>/dev/null || true; }
trap cleanup EXIT
wait_for() { i=0; while ! log_plain | grep -q "$1"; do i=$((i+1)); [ $i -gt 40 ] && fail "timed out waiting for: $1"; sleep 0.5; done; }

echo "warming…"; dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

( cd examples/site && exec fennec dev ) >"$LOG" 2>&1 & DEV=$!
i=0; while ! grep -q "localhost:8020" "$LOG" 2>/dev/null; do i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up"; sleep 0.5; done
sleep 1

echo "1) provoke a syntax error (remove a delimiter)…"
sed -i.bak 's|    ] />|     />|' "$L" && rm -f "$L.bak"
wait_for "build failed"
log_plain | grep -q "build failed · 1 error" || fail "wrong error count (a multi-location syntax error must read as 1)"
log_plain | grep -q "Syntax error" || fail "no error message shown for the syntax error"

echo "2) fix it (revert to identical bytes) — the panel must clear…"
sed -i.bak 's|     />|    ] />|' "$L" && rm -f "$L.bak"
wait_for "resolved"   # the supervisor noticed the green build and cleared the stuck panel

echo "PASS: error panel counts correctly + carries a message, and a revert-to-identical fix clears it."
