(* Host-routing + port-override system test (ported from domains.sh). Typed, deterministic
   (condition waits, never sleep-and-hope), contained (the whole process group is reaped on
   teardown — no orphans between scenarios).

   Guards two properties:
     1. FIDELITY: the dev GATEWAY (:4000) routes by Host EXACTLY as prod will — a specific host
        (admin.localhost) wins, the "*" endpoint (web) is the default, and an unknown host still
        falls to that default. So a routing Host header against the gateway exercises real prod
        selection with no /etc/hosts.
     2. PORT OVERRIDE: `fennec dev --port N` shifts the WHOLE port block, so a different worktree
        can run a second instance with no port clash. (Two instances in the SAME root can't
        coexist — dune --watch is single-per-root — so the cases run sequentially.) *)

module S = Fennec_hunt.System
let contains = Fennec_hunt.Unit.str_contains

let%system "host-routing fidelity on the gateway (:4000): specific host -> admin, default + unknown -> web" = fun sb ->
  let dev = S.dev sb in
  S.wait_ready dev ~port:4000 ();

  (* a plain visit (Host: localhost) routes to the web "*" default *)
  S.check "gateway plain visit routes to the web '*' default"
    (contains (S.request 4000 "/").S.body "Welcome to the Fennec site");

  (* a specific host (admin.localhost) wins — with auth it reaches the admin app *)
  S.check "gateway routes Host admin.localhost to the admin app (prod fidelity)"
    (contains
       (S.request ~host:"admin.localhost"
          ~headers:[ Fennec_hunt.Http.basic_auth "admin" "admin" ] 4000 "/").S.body
       "Admin Dashboard");

  (* an unknown host still falls to the "*" default *)
  S.check "an unknown host falls to the '*' default"
    (contains (S.request ~host:"random.example.com" 4000 "/").S.body "Welcome to the Fennec site");

  (* web's own route is present on the gateway *)
  S.check "web's own route present on the gateway"
    (contains (S.request 4000 "/api/health").S.body {|"app":"web"|});

  (* the matched-phase property: admin has basic auth in pipe_matched — no auth is 401 *)
  S.check "admin without auth is 401 (matched-phase auth)"
    ((S.request ~host:"admin.localhost" 4000 "/").S.status = 401)

let%system "--port override — the whole block shifts to a custom base" = fun sb ->
  let dev = S.dev sb ~args:[ "--port"; "9000" ] in
  S.wait_ready dev ~port:9000 ();

  S.check "--port 9000 instance serves on :9000"
    (contains (S.request 9000 "/").S.body "Welcome to the Fennec site");

  S.check "--port 9000 gateway routes to admin"
    (contains
       (S.request ~host:"admin.localhost"
          ~headers:[ Fennec_hunt.Http.basic_auth "admin" "admin" ] 9000 "/").S.body
       "Admin Dashboard")
