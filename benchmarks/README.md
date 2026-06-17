# paw — cross-stack HTTP benchmark

An honest, apples-to-apples comparison of fennec-**paw** against the idiomatic
"return a small response" path of four other stacks. Each server is a small,
self-contained subrepo using that stack's *typical real-world* choice:

| stack | choice | why |
|---|---|---|
| **Go** | `net/http` + `http.ServeMux` (1.22 method patterns) | compiled + GC + multicore, stdlib — the fair "capped" baseline |
| **Rust** | `actix-web` | the extreme-speed case |
| **Elixir** | `Plug` + `Bandit` | the small composable building block paw's model came from |
| **Node** | `Fastify` + `cluster` | the popular lean choice, clustered to use all cores |
| **paw** | `fennec-paw` (`Paw.serve`) | this repo |

## The shared contract (every server implements exactly this)

| route | status | content-type | body |
|---|---|---|---|
| `GET /plaintext` | 200 | `text/plain` | `Hello, World!` |
| `GET /json` | 200 | `application/json` | `{"message":"Hello, World!"}` |
| `GET /user/{id}` | 200 | `text/plain` | the `id` path segment |

`/json` returns a *fixed* string (this measures the framework's dispatch +
response path, not JSON-serialization-library speed, which varies wildly and
isn't what we're comparing). `/user/{id}` exercises routing + path-param capture.

## Fairness rules

- **Same machine, same load client, same parameters, one server at a time** on
  `127.0.0.1:8080`. The client (`wrk`) and server share the 10 cores, identically
  for every stack — so the **relative** ranking is fair (absolute numbers would be
  higher with a dedicated load box).
- **Production builds, all cores:** Go (`go build`, GOMAXPROCS=NCPU), Rust
  (`--release`, workers=NCPU), Elixir (`MIX_ENV=prod`, BEAM schedulers=NCPU),
  Node (`cluster`, workers=NCPU), paw (`--profile release`, `FENNEC_ENV=production`).
- **No request logging** anywhere (it dominates throughput).
- **Keep-alive on** (wrk default).
- **Caveat — paw does more per response.** paw's pipeline computes an ETag and a
  `Vary` and negotiates compression on every response; the others do not by
  default. So paw is doing strictly more work for the same bytes — a conservative
  comparison. (This may itself argue for making the ETag opt-in.)

## Run

```sh
./run.sh           # builds each, benchmarks all three routes, prints a table
```

Build artifacts (`target/`, `node_modules/`, `deps/`, `_build/`, the Go binary)
are git-ignored.

## Results

Measured on a 10-core arm64 Mac (Apple Silicon), `wrk -t4 -c256`. The load client
and the servers share the same 10 cores, so **absolute numbers are machine-capped
and bounce ±~15% run-to-run** (thermal + co-resident client). Read this as *tiers*,
not precise figures — the rankings are stable across runs.

| stack | /plaintext | /json | 11-header¹ | /plaintext ×16 (pipelined) |
|---|--:|--:|--:|--:|
| rust (actix) | ~180k | ~179k | ~177k | **~2.4M** |
| go (net/http) | ~178k | ~175k | ~169k | ~410k |
| node (Fastify×cluster) | ~170k | ~160k | ~165k | ~428k |
| elixir (Plug/Bandit) | ~148k | ~150k | ~129k | ~260k |
| **paw (fennec-paw)** | **~144–164k** | **~150k** | **~148–165k** | **~1.33–1.45M** |

¹ a realistic browser-shaped request (~11 headers — cookies, user-agent, accept, …),
where real per-header parse cost lives; the 1-header `/plaintext` hides it.

(paw is benchmarked last, when the machine is hottest, so its per-request figures
read ~10–15% low in-suite vs isolated — e.g. 11-header is ~148k in-suite, ~165k
isolated. The pipelined figure is large enough to be robust to that.)

**Two things are being measured.** *Per-request* (no pipelining) is the realistic
request/response pattern; on loopback it is dominated by kernel syscalls + the
co-resident client, so every modern stack clusters together. *Pipelined ×16*
amortizes the syscalls and exposes each framework's own ceiling.

**Reading it honestly:**

- **Pipelined — paw is between Go/Node and Rust, ~3× the others.** This is the
  framework's own ceiling (syscalls amortized). paw reaches ~1.3–1.44M req/s vs
  Go/Node's ~380–430k — **3× faster** — with only Rust ahead. This is the real
  proof: as a *framework*, paw's routing + response path is far faster than Go's or
  Node's; it was the per-request Eio timers (now off the hot path) that hid it.
- **On realistic (11-header) traffic, paw is ≈ Go/Node and clearly above Elixir**
  — *while doing strictly more per response* (ETag + Vary + compression
  negotiation the others skip). The hand-crafted zero-copy C parser
  (`http/http_parse_stubs.c`) is why: it scans the head with no per-header
  allocation, so paw doesn't degrade as headers pile up (the OCaml line-by-line
  parser dropped to ~153k on the same request; the C parser holds ~165k).
- **`/plaintext` (1 header) is OS-syscall-floored** — 1 read + 1 write/request on
  loopback — so every stack clusters there; that synthetic number isn't the
  framework. Measured: a *bare* Eio server (no paw — just read-head + fixed reply)
  hits the **same** ~160k floor as full paw, so paw adds ≈ zero overhead to Eio's
  per-request IO; the floor is Eio's effect-based IO on the posix/kqueue backend
  (~90% of Go's netpoller). A synchronous write path was tried vs Eio's background
  flusher and is a wash — Eio's fibers are cheap; we are not holding it wrong. On
  Linux, Eio's io_uring backend batches submissions and should lift this floor for
  free — a runtime-level win, not a framework change.
- **Pure framework cost** (`examples/paw_bench`, in-process, no socket): paw
  finalizes a full small request in ~290 ns ≈ 3.4M req/s of framework work.

Bottom line: as a framework, paw **surpasses Go and Node** (3× pipelined), matches
them on realistic traffic, and trails only Rust — while carrying more batteries.
The one C hot path (the request-head parser) is the deliberate "close to metal"
cheat that keeps the per-header cost off the table; everything else is idiomatic OCaml.
