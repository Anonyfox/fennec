#!/bin/sh
# Host-routing + port-override regression test (dev-only). Endpoints are identified by NAME + host
# pattern; ports are allocated by the runtime. This guards:
#
#   1. FIDELITY: the dev GATEWAY (:4000) routes by Host EXACTLY as prod will — a specific host
#      (admin.localhost) wins, the "*" endpoint (web) is the default, and an unknown host still
#      falls to that default. So `curl -H Host:` against the gateway exercises real prod selection
#      with no /etc/hosts.
#   2. PORT OVERRIDE: `fennec dev --port N` shifts the WHOLE port block. A different worktree (its
#      own _build, dune lock and pidfile) can then run a second instance with no port clash.
#      (Two instances in the SAME root can't coexist — dune --watch is single-per-root — so this
#      runs them sequentially; cross-worktree coexistence follows from per-root isolation + --port.)
#
# Usage:  sh examples/site/e2e/domains.sh   (needs `fennec` on PATH)
set -eu
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
cd "$ROOT"
DEV=""
fail() { echo "FAIL: $1"; cleanup; exit 1; }
cleanup() { [ -n "$DEV" ] && kill -INT "$DEV" 2>/dev/null || true; sleep 1; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; }
trap cleanup EXIT
up() { i=0; while ! grep -q "$2" "$1" 2>/dev/null; do i=$((i+1)); [ $i -gt 80 ] && fail "server did not come up ($2)"; sleep 0.5; done; }
stop() { [ -n "$DEV" ] && kill -INT "$DEV" 2>/dev/null || true; DEV=""; sleep 2; pkill -9 -f "dune build --watch" 2>/dev/null || true; pkill -9 -f "ocamlrun.*server" 2>/dev/null || true; sleep 1; }

echo "warming…"; dune build examples/site/server.bc examples/site/webroot >/dev/null 2>&1
pkill -9 -f "dune build --watch" 2>/dev/null || true; sleep 1

echo "1) host-routing fidelity on the gateway (:4000)…"
( cd examples/site && exec fennec dev ) >/tmp/fennec_dom_1.log 2>&1 & DEV=$!
up /tmp/fennec_dom_1.log "ready"
curl -s http://localhost:4000/ | grep -q "Welcome to the Fennec site" || fail "gateway plain visit (Host: localhost) did not route to the web '*' default"
curl -s -H "Host: admin.localhost" http://localhost:4000/ | grep -q "Admin Dashboard" || fail "gateway did not route Host admin.localhost to the admin app (prod fidelity)"
curl -s -H "Host: random.example.com" http://localhost:4000/ | grep -q "Welcome to the Fennec site" || fail "an unknown host did not fall to the '*' default"
curl -s http://localhost:4000/api/health | grep -q '"app":"web"' || fail "web's own route missing on the gateway"
echo "   specific host -> admin; default + unknown -> web."
stop

echo "2) --port override — the whole block shifts to a custom base…"
( cd examples/site && exec fennec dev --port 9000 ) >/tmp/fennec_dom_2.log 2>&1 & DEV=$!
up /tmp/fennec_dom_2.log "ready"
curl -s http://localhost:9000/ | grep -q "Welcome to the Fennec site" || fail "--port 9000 instance did not serve on :9000"
curl -s -H "Host: admin.localhost" http://localhost:9000/ | grep -q "Admin Dashboard" || fail "--port 9000 gateway did not route to admin"
echo "   --port 9000 serves + routes; the block moved off the default base."

echo "PASS: the dev gateway routes by Host like prod; --port shifts the whole block for isolated instances."
