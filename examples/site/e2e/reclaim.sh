#!/bin/sh
# Port-reclaim regression test (dev-only): `fennec dev` is resilient to a stray process holding the
# dev port — no matter where the leftover came from.
#
#   A) a LEFTOVER of OUR OWN server holding :8200  -> fennec dev SIGKILLs it and binds (transparent);
#   B) a FOREIGN process holding :8200             -> fennec dev does NOT kill it, and prints a
#                                                     one-command fix ("kill <pid>") naming the culprit.
#
# Usage:  sh examples/site/e2e/reclaim.sh   (needs `fennec` on PATH; python3 for case B)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
SERVER=_build/default/examples/site/server.bc
STUB="$(opam var lib 2>/dev/null)/stublibs"
LEFT=""; FOREIGN=""; DEV=""
fail() { echo "FAIL: $1"; cleanup; exit 1; }
cleanup() {
  [ -n "$DEV" ] && kill -INT "$DEV" 2>/dev/null || true
  [ -n "$LEFT" ] && kill -9 "$LEFT" 2>/dev/null || true
  [ -n "$FOREIGN" ] && kill -9 "$FOREIGN" 2>/dev/null || true
  sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true
}
trap cleanup EXIT
held() { lsof -nP -iTCP:8200 -sTCP:LISTEN >/dev/null 2>&1; }
logged() { i=0; while ! sed 's/\x1b\[[0-9;]*m//g' "$1" 2>/dev/null | grep -q "$2"; do i=$((i+1)); [ $i -gt 40 ] && return 1; sleep 0.5; done; }

echo "warming…"; dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; sleep 1

echo "A) a leftover of our own server is auto-reclaimed…"
CAML_LD_LIBRARY_PATH="$STUB" FENNEC_ENV=development "$SERVER" >/dev/null 2>&1 & LEFT=$!
i=0; while ! held; do i=$((i+1)); [ $i -gt 20 ] && fail "leftover server never bound :8200"; sleep 0.5; done
( cd examples/site && exec fennec dev ) >/tmp/reclaim_a.log 2>&1 & DEV=$!
# the URL banner is printed only AFTER a successful bind, so seeing it means fennec dev took the
# port — which it could only do by reclaiming the leftover that was holding it
logged /tmp/reclaim_a.log "localhost:8200" || fail "fennec dev never bound the port (didn't reclaim the leftover)"
holder=$(lsof -nP -iTCP:8200 -sTCP:LISTEN -t 2>/dev/null | head -1)
[ -n "$holder" ] && [ "$holder" != "$LEFT" ] || fail "the leftover ($LEFT) still holds :8200 — not reclaimed"
LEFT=""  # reclaimed (the live holder is fennec dev's server, not the leftover)
kill -INT "$DEV" 2>/dev/null || true; DEV=""; sleep 2; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true

echo "B) a FOREIGN holder is named, not killed…"
# bind the SAME loopback address fennec uses (127.0.0.1), or SO_REUSEADDR + address specificity
# lets fennec's 127.0.0.1:8200 coexist with a 0.0.0.0:8200 holder and there's no conflict
python3 -m http.server 8200 --bind 127.0.0.1 >/dev/null 2>&1 & FOREIGN=$!
i=0; while ! held; do i=$((i+1)); [ $i -gt 20 ] && fail "foreign holder never bound :8200"; sleep 0.5; done
( cd examples/site && exec fennec dev ) >/tmp/reclaim_b.log 2>&1 & DEV=$!
logged /tmp/reclaim_b.log "held by another process" || fail "fennec dev didn't report the foreign holder"
kill -0 "$FOREIGN" 2>/dev/null || fail "the FOREIGN process was wrongly killed"
sed 's/\x1b\[[0-9;]*m//g' /tmp/reclaim_b.log | grep -q "kill $FOREIGN" || fail "no one-command fix (kill <pid>) shown for the foreign holder"

echo "PASS: our own leftover is auto-reclaimed; a foreign holder is named with a one-command fix."
