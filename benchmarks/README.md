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

| stack | /plaintext | /json | /user/:id | /plaintext ×16 (pipelined) |
|---|--:|--:|--:|--:|
| rust (actix) | ~180k | ~178k | ~178k | **~2.4M** |
| go (net/http) | ~177k | ~174k | ~172k | ~380k |
| node (Fastify×cluster) | ~170k | ~166k | ~165k | ~430k |
| elixir (Plug/Bandit) | ~145k | ~152k | ~146k | ~253k |
| **paw (fennec-paw)** | **~150–164k** | **~150k** | **~149–164k** | **~1.3–1.44M** |

(paw is benchmarked last, when the machine is hottest, so its per-request figure
reads low in-suite — ~164k isolated, ~142k in-suite; the pipelined figure is large
enough to be robust to that.)

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
- **Per-request, paw is in the pack** — ≈ Node, ~85–95% of Go — *while doing
  strictly more per response* (ETag + Vary + compression negotiation the others
  skip). This number is OS-syscall-floored (1 read + 1 write/request on loopback),
  so every stack clusters; the small gap to Go is paw's extra work + allocation
  pressure (the stop-the-world minor GC across 10 domains), not its logic.
- **Pure framework cost** (`examples/paw_bench`, in-process, no socket): paw
  finalizes a full small request in ~290 ns ≈ 3.4M req/s of framework work.

Bottom line: as a framework, paw **surpasses Go and Node** (3× pipelined) and trails
only Rust; on raw loopback per-request it sits at the OS floor with everyone else,
≈ Node, while carrying more batteries. The next lever for the per-request floor is
cutting parse allocation (less GC / fewer stop-the-world pauses) — a zero-copy parser.
