#!/usr/bin/env bash
# Cross-stack HTTP benchmark. Builds each server (cached after the first run), then drives
# wrk identically against all three routes, ONE server at a time on 127.0.0.1:PORT, and
# prints a comparison table. See README.md for the contract + fairness rules.
# Portable to stock macOS bash 3.2 (no associative arrays — results go through a temp file).
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"

PORT="${PORT:-8080}"
THREADS="${THREADS:-4}"   # wrk client threads (leaves cores for the server; wrk far outpaces these servers)
CONNS="${CONNS:-256}"     # keep-alive connections
DUR="${DUR:-10}"          # measured seconds per route
WARMUP="${WARMUP:-3}"     # discarded warmup seconds
ROUTES="plaintext json user/42"
RESULTS="$(mktemp)"

free_port() {
  lsof -ti tcp:"$PORT" 2>/dev/null | xargs kill -9 2>/dev/null
  pkill -f "benchmarks/go/server" 2>/dev/null
  pkill -f "benchmarks/paw/server.exe" 2>/dev/null
  pkill -f "paw-bench-rust" 2>/dev/null
  pkill -f "node server.js" 2>/dev/null
  pkill -f "mix run --no-halt" 2>/dev/null
  pkill -f "beam.smp" 2>/dev/null
  sleep 1
}

wait_ready() {
  i=0
  while [ "$i" -lt 80 ]; do
    curl -fs -o /dev/null "http://127.0.0.1:$PORT/plaintext" 2>/dev/null && return 0
    sleep 0.25; i=$((i + 1))
  done
  return 1
}

# per-stack start commands (own cwd/env; exec so the backgrounded job IS the server)
start_go()     { cd "$ROOT/benchmarks/go"     && PORT="$PORT" exec ./server; }
start_rust()   { cd "$ROOT/benchmarks/rust"   && PORT="$PORT" exec ./target/release/paw-bench-rust; }
start_node()   { cd "$ROOT/benchmarks/node"   && PORT="$PORT" exec node server.js; }
start_elixir() { cd "$ROOT/benchmarks/elixir" && MIX_ENV=prod PORT="$PORT" exec mix run --no-halt; }
start_paw()    { cd "$ROOT" && FENNEC_ENV=production FENNEC_PORT="$PORT" exec ./_build/default/benchmarks/paw/server.exe; }

measure() { wrk -t"$THREADS" -c"$CONNS" -d"${DUR}s" "$@" 2>/dev/null | awk '/Requests\/sec/{print $2}'; }

bench_stack() {
  name="$1"; startfn="$2"
  printf '\n──── %s ────\n' "$name"
  free_port
  ( "$startfn" ) >"/tmp/bench_$name.log" 2>&1 &
  disown 2>/dev/null               # detach so the later kill is silent
  if ! wait_ready; then echo "  $name FAILED to start:"; tail -5 "/tmp/bench_$name.log"; free_port; return; fi
  # per-request (no pipelining) — the realistic request/response pattern
  for route in $ROUTES; do
    wrk -t"$THREADS" -c"$CONNS" -d"${WARMUP}s" "http://127.0.0.1:$PORT/$route" >/dev/null 2>&1   # warmup
    rps=$(measure "http://127.0.0.1:$PORT/$route")
    echo "$name $route $rps" >>"$RESULTS"
    printf '  %-14s  %12s req/s\n' "$route" "$rps"
  done
  # pipelined ×16 on /plaintext — amortizes syscalls, exposing the framework's own ceiling
  wrk -t"$THREADS" -c"$CONNS" -d"${WARMUP}s" -s "$ROOT/benchmarks/pipeline.lua" "http://127.0.0.1:$PORT/plaintext" >/dev/null 2>&1
  ppl=$(measure -s "$ROOT/benchmarks/pipeline.lua" "http://127.0.0.1:$PORT/plaintext")
  echo "$name plaintext_x16 $ppl" >>"$RESULTS"
  printf '  %-14s  %12s req/s\n' "plaintext×16" "$ppl"
  free_port
}

build_all() {
  echo "building (cached after first run)…"
  ( cd "$ROOT/benchmarks/go" && go build -o server . ) || echo "  go build failed"
  ( cd "$ROOT/benchmarks/rust" && cargo build --release -q ) || echo "  rust build failed"
  ( cd "$ROOT/benchmarks/node" && npm install --silent --no-audit --no-fund >/dev/null ) || echo "  npm install failed"
  ( cd "$ROOT/benchmarks/elixir" && mix deps.get >/dev/null 2>&1 && MIX_ENV=prod mix compile >/dev/null 2>&1 ) || echo "  elixir build failed"
  ( cd "$ROOT" && dune build --profile release benchmarks/paw/server.exe ) || echo "  paw build failed"
}

echo "paw cross-stack benchmark — wrk -t$THREADS -c$CONNS -d${DUR}s, port $PORT, $(sysctl -n hw.ncpu 2>/dev/null || nproc) cores"
build_all
bench_stack go     start_go
bench_stack rust   start_rust
bench_stack node   start_node
bench_stack elixir start_elixir
bench_stack paw    start_paw

printf '\n================ req/s (higher is better) ================\n'
awk '
  { r[$1","$2]=$3; if(!seen[$1]++){order[++n]=$1} }
  END {
    printf "%-10s %13s %13s %13s %18s\n", "stack", "/plaintext", "/json", "/user/:id", "plaintext×16(pl)"
    for (i=1;i<=n;i++){ s=order[i]; printf "%-10s %13s %13s %13s %18s\n", s, r[s",plaintext"], r[s",json"], r[s",user/42"], r[s",plaintext_x16"] }
  }' "$RESULTS"
echo "(no-pipeline = realistic request/response; ×16 = pipelined, isolates framework cost)"
rm -f "$RESULTS"
