(* A second Http suite — the JSON API + streaming surface. It runs against its OWN dedicated
   isolated instance (own port, own server), concurrently with the other suites: the per-suite
   isolation is exactly what makes that safe and deterministic. No ~url — the harness sets
   FENNEC_TEST_URL. *)
open Fennec_hunt.Http

let%http "site api (http)" = fun () ->
  check "health endpoint: JSON shape + timing" (fun () ->
    get "/api/health" ~expect:[ status 200; is_json; json_path_is "ok" "true"; json_path_is "app" "web"; max_elapsed 500.0 ]);

  check "send_chunked stream reassembles in order" (fun () ->
    get "/api/stream" ~expect:[ status 200; body_contains "chunk-1"; body_contains "chunk-2"; body_contains "chunk-3" ]);

  check "send_file streams the file body" (fun () ->
    get "/api/download" ~expect:[ status 200; body_contains "hello from send_file" ]);

  check "extract + assert a JSON field" (fun () ->
    get "/api/health";
    assert (json_field "app" = "web"))
