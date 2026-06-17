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
| rust (actix) | ~174k | ~170k | ~164k | **~2.3M** |
| go (net/http) | ~173k | ~165k | ~157k | ~400k |
| node (Fastify×cluster) | ~165k | ~162k | ~159k | ~400k |
| elixir (Plug/Bandit) | ~139k | ~138k | ~136k | ~205k |
| **paw (fennec-paw)** | **~140k** | **~140k** | **~132k** | **~250k** |

**Two things are being measured.** *Per-request* (no pipelining) is the realistic
request/response pattern; on loopback it is dominated by kernel syscalls + the
co-resident client, so every modern stack clusters together. *Pipelined ×16*
amortizes the syscalls and exposes each framework's own ceiling.

**Reading it honestly:**

- **Per-request, paw is in the pack** — ~80% of the compiled/optimized tier
  (Go/Rust/Node), on par with Elixir — *while doing strictly more per response*
  (ETag + Vary + compression negotiation that the others skip by default). For
  the request rates real apps see, the framework is not the bottleneck; the OS is.
- **Pipelined, Rust is in a class of its own** (actix + tokio), as expected for
  "the extreme case." Go and Node sit ~400k; paw, after the flush-batching fix
  (`flush only when the read buffer is drained`), reaches ~250k — now past
  Elixir/Bandit. The remaining gap to Go/Node is the **Eio IO layer + paw's extra
  per-response work**, not paw's routing/response logic.
- **Pure framework cost** (see `examples/paw_bench`, in-process, no socket): paw
  finalizes a full small request in ~290 ns ≈ 3.4M req/s of framework work — far
  above any socket number here, confirming the wall is the OS/IO, not paw's code.

Bottom line: paw is genuinely competitive — a managed-runtime framework holding
~80% of the compiled leaders on realistic load while carrying more batteries.
