(* Unit tests for Endpoint composition — the paw pipeline within one endpoint. Host-pattern
   matching and host->endpoint routing now live in Host_pattern / Host_router (their own tests). *)

module Endpoint = Fennec_server.Endpoint
module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let fails = ref 0
let check name cond = if cond then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let req ?(meth = H.GET) path = H.make_request ~meth ~path ()

let () =
  print_endline "Endpoint composition:";
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

  if !fails = 0 then print_endline "all Endpoint tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
