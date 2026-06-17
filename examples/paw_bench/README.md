# paw_bench — fennec-paw micro-benchmarks

The per-request hot path, measured apples-to-apples. Pure in-memory (no sockets), so
numbers are stable run-to-run and isolate the framework's own cost.

```sh
dune exec examples/paw_bench/bench.exe
```

Each line reports:

- **ns/op** — wall-clock nanoseconds: single-request latency.
- **B/op** — bytes allocated per op. This is the metric to watch: OCaml 5's GC is a
  cross-domain bottleneck, so fewer allocations means both faster single requests
  *and* better multicore throughput (less GC, less contention across worker domains).

The suite covers request parsing, host + route dispatch, response finalization
(ETag / compression / Date / Content-Length), and the end-to-end logical request
(route → handler → finalize, minus the socket write).

Keep `bench.ml` stable across runs so before/after diffs stay comparable.
