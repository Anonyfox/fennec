#!/bin/sh
# Process-hygiene regression test (dev-only): enforces that `fennec dev` can NEVER leave a
# leftover process holding the dev port, no matter how it dies. This has bitten us twice, so it
# is locked in here as a red/green test.
#
# It asserts:
#   A. SIGKILL the supervisor (worst case — no cleanup runs) -> the dev port frees on its own
#      within a couple of seconds (the server self-exits when it sees it's been orphaned).
#   B. a fresh `fennec dev` then binds the port cleanly (it was actually free).
#   C. a SECOND `fennec dev` reaps the first -> exactly one supervisor, no port fight.
#   D. a clean shutdown (SIGINT) leaves NOTHING listening on the port.
#
# Usage:  sh examples/site/e2e/no_leftovers.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
PORT=4000
fail() { echo "FAIL: $1"; cleanup; exit 1; }
cleanup() { pkill -9 -f "fennec dev" 2>/dev/null || true; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; }
trap cleanup EXIT
port_held() { lsof -nP -iTCP:$PORT -sTCP:LISTEN >/dev/null 2>&1; }
n_supervisors() { pgrep -f "fennec dev" 2>/dev/null | wc -l | tr -d ' '; }
wait_serving() { i=0; while ! grep -q "ready" "$1" 2>/dev/null; do i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up"; sleep 0.5; done; }

echo "clean slate…"; cleanup; sleep 2; port_held && fail "port $PORT held before we even start" || true
dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1; pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

echo "A) SIGKILL the supervisor — the port must free itself…"
( cd examples/site && exec fennec dev ) >/tmp/nolf_1.log 2>&1 & S1=$!
wait_serving /tmp/nolf_1.log
port_held || fail "server never bound the port"
kill -9 $S1   # worst case: no cleanup handler runs
wait $S1 2>/dev/null || true   # reap the killed supervisor so it isn't counted as a zombie
i=0; while port_held; do i=$((i+1)); [ $i -gt 20 ] && fail "port $PORT STILL held 10s after SIGKILL — a leftover server survived"; sleep 0.5; done
echo "   port freed itself after SIGKILL."

echo "B) a fresh supervisor binds the freed port…"
( cd examples/site && exec fennec dev ) >/tmp/nolf_2.log 2>&1 & S2=$!
wait_serving /tmp/nolf_2.log
port_held || fail "fresh supervisor failed to bind the port"

echo "C) a second supervisor reaps the first (single instance)…"
( cd examples/site && exec fennec dev ) >/tmp/nolf_3.log 2>&1 & S3=$!
wait_serving /tmp/nolf_3.log
sleep 2
kill -0 $S2 2>/dev/null && fail "the previous supervisor survived a new start (instances accumulate)"
wait $S2 2>/dev/null || true   # reap the reaped supervisor so it isn't counted as a zombie
[ "$(n_supervisors)" = "1" ] || fail "expected exactly 1 fennec dev, found $(n_supervisors)"

echo "D) a clean shutdown leaves nothing on the port…"
kill -INT $S3
wait $S3 2>/dev/null || true   # block until the supervisor actually exits (and reap it)
i=0; while port_held; do i=$((i+1)); [ $i -gt 20 ] && fail "port $PORT still held 10s after clean shutdown"; sleep 0.5; done
[ "$(n_supervisors)" = "0" ] || fail "a fennec dev survived a clean shutdown"

echo "PASS: SIGKILL frees the port, restarts bind cleanly, single instance enforced, clean exit leaves nothing."
