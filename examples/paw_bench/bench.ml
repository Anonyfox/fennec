(* fennec-paw micro-benchmark suite — the per-request hot path, measured apples-to-apples.

   Run:  dune exec examples/paw_bench/bench.exe

   Reports, per operation:
     - ns/op : wall-clock nanoseconds (single-request latency)
     - B/op  : bytes allocated (GC pressure — the metric that caps BOTH single-request
               speed and multicore throughput, since OCaml 5's GC is a cross-domain
               bottleneck: fewer allocations ⇒ less GC ⇒ better parallel scaling)

   Pure in-memory (no sockets), so numbers are stable run-to-run and isolate the
   framework's own cost. Keep this file stable so before/after diffs are comparable. *)

module H = Paw.Http

(* run [f] [iters] times after a warm-up; report ns/op + bytes-allocated/op *)
let bench name ~iters (f : unit -> unit) =
  for _ = 1 to max 1 (iters / 10) do f () done;
  (* warm code + caches *)
  Gc.full_major ();
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  for _ = 1 to iters do f () done;
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  let ns = (t1 -. t0) *. 1e9 /. float_of_int iters in
  let bytes = (a1 -. a0) /. float_of_int iters in
  Printf.printf "  %-40s %9.1f ns/op  %9.0f B/op\n%!" name ns bytes

let keep x = ignore (Sys.opaque_identity x)
let section s = Printf.printf "\n%s\n%!" s

(* ── fixtures ─────────────────────────────────────────────────────────────── *)

let raw_small = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

(* a realistic browser GET: ~9 headers, query string *)
let raw_real =
  "GET /api/users/42?q=hello&page=2 HTTP/1.1\r\n\
   Host: app.example.com\r\n\
   User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36\r\n\
   Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n\
   Accept-Encoding: gzip, deflate, br\r\n\
   Accept-Language: en-US,en;q=0.9\r\n\
   Cookie: session=abc123def456; theme=dark; lang=en\r\n\
   Referer: https://app.example.com/dashboard\r\n\
   Connection: keep-alive\r\n\r\n"

let req_get = H.make_request ~meth:H.GET ~path:"/" ~headers:[ ("accept-encoding", "gzip") ] ()

let resp_small = { H.status = 200; headers = [ ("content-type", "text/plain; charset=utf-8") ]; body = String.make 200 'x' }
let resp_html = { H.status = 200; headers = [ ("content-type", "text/html; charset=utf-8") ]; body = String.make 8192 'h' }

(* an endpoint with [n] static routes; the request hits the LAST one (worst-case linear scan) *)
let endpoint_n n =
  let e = ref (Paw.endpoint ()) in
  for i = 1 to n do
    e := Paw.get (Printf.sprintf "/route/%d" i) (fun c -> Paw.text "ok" c) !e
  done;
  Paw.Endpoint.handler !e

let h1 = endpoint_n 1
let h10 = endpoint_n 10
let h50 = endpoint_n 50
let req_root = H.make_request ~meth:H.GET ~path:"/route/1" ~headers:[ ("host", "localhost"); ("accept-encoding", "gzip") ] ()
let req_last10 = H.make_request ~meth:H.GET ~path:"/route/10" ()
let req_last50 = H.make_request ~meth:H.GET ~path:"/route/50" ()

let router =
  match Paw.Host_router.build [ ("web", [ "*" ], ()); ("api", [ "api.example.com" ], ()); ("sub", [ "*.example.com" ], ()) ] with
  | Ok r -> r
  | Error _ -> failwith "bench: router build failed"

(* one full logical request: route → handler → materialize response → finalize *)
let full_request handler req () =
  let c = Paw.run_conn handler req in
  let resp = match Paw.Conn.resp c with Some r -> r | None -> H.text ~status:404 "" in
  keep (Paw.Responder.finalize ~now:1_700_000_000.0 ~req resp)

(* ── the suite ────────────────────────────────────────────────────────────── *)

let () =
  Printf.printf "fennec-paw micro-benchmarks  (ns/op = latency, B/op = GC pressure)\n%!";

  section "request parsing";
  bench "parse: minimal GET (1 header)" ~iters:1_000_000 (fun () ->
      keep (Paw.Server.read_request ~continue:(fun () -> ()) (Eio.Buf_read.of_string raw_small)));
  bench "parse: realistic GET (9 headers + query)" ~iters:1_000_000 (fun () ->
      keep (Paw.Server.read_request ~continue:(fun () -> ()) (Eio.Buf_read.of_string raw_real)));

  section "routing";
  bench "host route_all (exact, 3 endpoints)" ~iters:2_000_000 (fun () -> keep (Paw.Host_router.route_all router ~host:"api.example.com"));
  bench "route dispatch (1 route)" ~iters:1_000_000 (fun () -> keep (Paw.run_conn h1 req_root));
  bench "route dispatch (10 routes, hit last)" ~iters:1_000_000 (fun () -> keep (Paw.run_conn h10 req_last10));
  bench "route dispatch (50 routes, hit last)" ~iters:500_000 (fun () -> keep (Paw.run_conn h50 req_last50));

  section "response finalization (ETag/compress/Date/Content-Length)";
  bench "finalize: 200B text" ~iters:500_000 (fun () -> keep (Paw.Responder.finalize ~now:1_700_000_000.0 ~req:req_get resp_small));
  bench "finalize: 8KB html (gzip)" ~iters:100_000 (fun () -> keep (Paw.Responder.finalize ~now:1_700_000_000.0 ~req:req_get resp_html));

  section "end to end (route + handler + finalize, no socket)";
  bench "full request: 1 route, small body" ~iters:500_000 (full_request h1 req_root);
  bench "full request: 10 routes, hit last" ~iters:500_000 (full_request h10 req_last10);

  Printf.printf "\n%!"
