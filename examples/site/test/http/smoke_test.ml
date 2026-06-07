(* An Http suite for `fennec test http`. No ~url — the harness boots a dedicated isolated
   instance and hands us its URL via FENNEC_TEST_URL. We target the gateway and route
   sub-apps by Host (prod fidelity). *)
open Fennec_hunt.Http

let%http "site (http)" = fun () ->
  check "web health endpoint" (fun () ->
    get "/api/health" ~expect:[status 200; is_json; json_path_is "ok" "true"; json_path_is "app" "web"]);

  check "web home page" (fun () ->
    get "/" ~expect:[status 200; is_html; body_contains "Welcome to the Fennec site"]);

  check "admin without auth → 401 (matched-phase)" (fun () ->
    get "/" ~host:"admin.localhost" ~expect:[status 401]);

  check "admin with basic auth → 200" (fun () ->
    get "/" ~host:"admin.localhost" ~headers:[basic_auth "admin" "admin"]
      ~expect:[status 200; body_contains "Admin Dashboard"])
