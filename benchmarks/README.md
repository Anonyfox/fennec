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
