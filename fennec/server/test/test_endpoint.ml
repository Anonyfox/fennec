(* Unit tests for Endpoint composition: the two-phase pipeline (always + matched), the verb
   shortcuts, and the 404-stays-404 property (matched-phase paws must NOT fire on unmatched). *)

module Endpoint = Fennec_server.Endpoint
module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let fails = ref 0
let check name cond = if cond then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let req ?(meth = H.GET) path = H.make_request ~meth ~path ()

let () =
  print_endline "Endpoint (always-phase):";
  let e =
    Endpoint.make ~name:"app" ~hosts:[ "app.example.com" ] ()
    |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
    |> Endpoint.get "/" (fun c -> Conn.html c "<h1>home</h1>")
  in
  let run r = Paw.run (Endpoint.handler e) r in
  eq "api route" (run (req "/api/health")).H.body {|{"ok":true}|};
  eq "home route" (run (req "/")).H.body "<h1>home</h1>";
  eq "unmatched 404" (run (req "/nope")).H.status 404;
  check "name is carried" (Endpoint.name e = "app");
  check "hosts are carried" (Endpoint.hosts e = [ "app.example.com" ]);

  (* pipelines compose: a filter paw that halts before the routes *)
  let guard : Paw.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 =
    Endpoint.make ~name:"guarded" ()
    |> Endpoint.use guard
    |> Endpoint.get "/blocked" (fun c -> Conn.text c "should-not-reach")
    |> Endpoint.get "/ok" (fun c -> Conn.text c "ok")
  in
  eq "guard halts" (Paw.run (Endpoint.handler e2) (req "/blocked")).H.status 403;
  eq "guard passes others" (Paw.run (Endpoint.handler e2) (req "/ok")).H.body "ok";
  check "hosts default to the catch-all" (Endpoint.hosts e2 = [ "*" ]);

  (* ---- the 404-stays-404 property: matched-phase paws must NOT fire on unmatched ---- *)
  print_endline "Endpoint (matched-phase — the 404-stays-404 property):";
  let auth_ran = ref false in
  let auth_paw : Paw.t = fun c -> auth_ran := true; Conn.text ~status:401 c "unauthorized" in
  let e3 =
    Endpoint.make ~name:"secured" ()
    |> Endpoint.get "/api/secret" (fun c -> Conn.text c "top secret")
    |> Endpoint.pipe_matched [ auth_paw ] (* auth only fires after a route matched *)
  in
  let run3 r = auth_ran := false; Paw.run (Endpoint.handler e3) r in
  eq "matched route → auth runs, gets 401" (run3 (req "/api/secret")).H.status 401;
  check "auth DID run on a matched route" !auth_ran;
  eq "unmatched → 404 (not 401 from auth)" (run3 (req "/nonexistent")).H.status 404;
  check "auth did NOT run on an unmatched route" (not !auth_ran);

  (* matched-phase paw can POST-PROCESS a matched response (e.g. stamp a header) *)
  let stamp : Paw.t = fun c -> Conn.before_send c (fun r -> { r with H.headers = ("X-Auth", "ok") :: r.H.headers }) in
  let e4 =
    Endpoint.make ~name:"stamped" ()
    |> Endpoint.get "/api/data" (fun c -> Conn.text c "data")
    |> Endpoint.pipe_matched [ stamp ]
  in
  (* before_send hooks are applied by the server, so we simulate: run_conn, then apply_before_send *)
  let conn4 = Paw.run_conn (Endpoint.handler e4) (req "/api/data") in
  let resp4 = Conn.apply_before_send conn4 (Option.get (Conn.resp conn4)) in
  eq "matched route gets the header stamp" (List.assoc_opt "X-Auth" resp4.H.headers) (Some "ok");

  (* no matched paws → flat pipeline (backward compatible) *)
  let e5 =
    Endpoint.make ~name:"flat" ()
    |> Endpoint.get "/ok" (fun c -> Conn.text c "ok")
  in
  eq "flat (no pipe_matched) still works" (Paw.run (Endpoint.handler e5) (req "/ok")).H.body "ok";
  eq "flat unmatched → 404" (Paw.run (Endpoint.handler e5) (req "/nope")).H.status 404;

  if !fails = 0 then print_endline "all Endpoint tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
