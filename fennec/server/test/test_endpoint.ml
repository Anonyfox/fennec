(* Unit tests for Endpoint composition + Host pattern matching. The Eio server
   loop itself is exercised by the example's live/isomorphic tests; here we test
   the pure routing/selection logic. *)

module Endpoint = Fennec_server.Endpoint
module Host = Fennec_server.Host
module Paw = Fennec_paw.Paw
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)
let req ?(meth = H.GET) path = { H.meth; path; query = []; headers = []; body = "" }

let () =
  print_endline "Host.matches:";
  check "exact" (Host.matches ~pattern:"example.com" "example.com");
  check "exact w/ port" (Host.matches ~pattern:"example.com" "example.com:8200");
  check "exact case-insensitive" (Host.matches ~pattern:"Example.COM" "example.com");
  check "exact mismatch" (not (Host.matches ~pattern:"example.com" "other.com"));
  check "wildcard one label" (Host.matches ~pattern:"*.example.com" "api.example.com");
  check "wildcard deep" (Host.matches ~pattern:"*.example.com" "a.b.example.com");
  check "wildcard needs a label" (not (Host.matches ~pattern:"*.example.com" "example.com"));
  check "wildcard mismatch base" (not (Host.matches ~pattern:"*.example.com" "api.other.com"));
  check "catch-all" (Host.matches ~pattern:"*" "anything.com");
  check "catch-all empty" (Host.matches ~pattern:"*" "")

let () =
  print_endline "Endpoint composition:";
  let e =
    Endpoint.make ~host:"app.example.com" ~port:443 ~dev_port:8200 ()
    |> Endpoint.get "/api/health" (fun c -> Conn.json c {|{"ok":true}|})
    |> Endpoint.get "/" (fun c -> Conn.html c "<h1>home</h1>")
  in
  let run r = Paw.run (Endpoint.handler e) r in
  eq "api route" (run (req "/api/health")).H.body {|{"ok":true}|};
  eq "home route" (run (req "/")).H.body "<h1>home</h1>";
  eq "unmatched 404" (run (req "/nope")).H.status 404;
  (* dev vs prod port *)
  eq "dev port" (Endpoint.listen_port ~dev:true e) 8200;
  eq "prod port" (Endpoint.listen_port ~dev:false e) 443;
  check "host matches" (Endpoint.host_matches e "app.example.com");
  check "host rejects other" (not (Endpoint.host_matches e "evil.com"));

  (* pipelines compose: a filter paw that halts before routes *)
  let guard : Paw.t = fun c -> if Conn.path c = "/blocked" then Conn.text ~status:403 c "no" else c in
  let e2 =
    Endpoint.make ~host:"*" ()
    |> Endpoint.plug guard
    |> Endpoint.get "/blocked" (fun c -> Conn.text c "should-not-reach")
    |> Endpoint.get "/ok" (fun c -> Conn.text c "ok")
  in
  eq "guard halts" (Paw.run (Endpoint.handler e2) (req "/blocked")).H.status 403;
  eq "guard passes others" (Paw.run (Endpoint.handler e2) (req "/ok")).H.body "ok"

let () =
  if !fails = 0 then print_endline "all Endpoint tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
