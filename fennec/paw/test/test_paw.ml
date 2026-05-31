(* Unit tests for the Paw core: typed assigns (incl. type-safety across distinct
   keys), conn responders, the pipeline short-circuit, and route matching. *)

module H = Fennec_core.Http
module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Route = Fennec_paw.Route
module Assigns = Fennec_paw.Assigns

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)
let req ?(meth = H.GET) path = { H.meth; path; query = []; headers = []; body = "" }

let () =
  print_endline "Assigns (typed):";
  let user : string Assigns.key = Assigns.key "user" in
  let count : int Assigns.key = Assigns.key "count" in
  let a = Assigns.empty in
  let a = Assigns.set a user "ada" in
  let a = Assigns.set a count 42 in
  eq "get user" (Assigns.get a user) (Some "ada");
  eq "get count" (Assigns.get a count) (Some 42);
  eq "absent key" (Assigns.get a (Assigns.key "missing" : float Assigns.key)) None;
  (* two keys of the SAME type are still distinct identities *)
  let user2 : string Assigns.key = Assigns.key "user" in
  eq "distinct keys same name/type" (Assigns.get a user2) None;
  (* set replaces *)
  let a = Assigns.set a count 7 in
  eq "set replaces" (Assigns.get a count) (Some 7);
  check "mem true" (Assigns.mem a user);
  check "mem false" (not (Assigns.mem a user2));
  eq "get_exn" (Assigns.get_exn a user) "ada"

let () =
  print_endline "Conn:";
  let c = Conn.make (req "/x") in
  check "fresh not answered" (not (Conn.answered c));
  let c = Conn.text c "hi" in
  check "text answers" (Conn.answered c);
  eq "text body" (match Conn.resp c with Some r -> r.H.body | None -> "") "hi";
  let c2 = Conn.make (req "/y") in
  let c2 = Conn.json ~status:201 c2 "{}" in
  eq "json status" (match Conn.resp c2 with Some r -> r.H.status | None -> 0) 201;
  (* halt without resp *)
  let c3 = Conn.halt (Conn.make (req "/z")) in
  check "explicit halt answers" (Conn.answered c3);
  (* typed assigns on conn *)
  let k : int Assigns.key = Assigns.key "k" in
  let c4 = Conn.assign (Conn.make (req "/")) k 5 in
  eq "conn assign/get" (Conn.get c4 k) (Some 5);
  (* req header lookup (case-insensitive) *)
  let c5 = Conn.make { (req "/") with H.headers = [ ("X-Foo", "bar") ] } in
  eq "req_header ci" (Conn.req_header c5 "x-foo") (Some "bar")

let () =
  print_endline "Paw pipeline (short-circuit):";
  let hits = ref [] in
  let tap name : Paw.t = fun c -> hits := name :: !hits; c in
  let answer : Paw.t = fun c -> Conn.text c "answered" in
  let p = Paw.seq [ tap "a"; answer; tap "b" (* must NOT run *) ] in
  let r = Paw.run p (req "/") in
  eq "answered body" r.H.body "answered";
  eq "downstream skipped after answer" (List.rev !hits) [ "a" ];
  (* unanswered pipeline -> 404 *)
  eq "empty pipeline 404" (Paw.run (Paw.seq []) (req "/")).H.status 404;
  eq "all-decline 404" (Paw.run (Paw.seq [ tap "x"; tap "y" ]) (req "/")).H.status 404

let () =
  print_endline "Route matching:";
  let app =
    Paw.seq
      [ Route.get "/api/ping" (fun c -> Conn.json c {|{"pong":true}|});
        Route.post "/api/ping" (fun c -> Conn.text c "posted");
        Route.get "/" (fun c -> Conn.html c "<h1>home</h1>") ]
  in
  eq "GET route" (Paw.run app (req "/api/ping")).H.body {|{"pong":true}|};
  eq "POST route (same path, diff method)" (Paw.run app (req ~meth:H.POST "/api/ping")).H.body "posted";
  eq "HEAD matches GET" (Paw.run app (req ~meth:H.HEAD "/")).H.body "<h1>home</h1>";
  eq "no match -> 404" (Paw.run app (req "/nope")).H.status 404;
  eq "wrong method -> 404" (Paw.run app (req ~meth:H.DELETE "/api/ping")).H.status 404;
  (* fallthrough answers only on Some *)
  let ft = Route.fallthrough (fun r -> if r.H.path = "/f" then Some (H.text "F") else None) in
  eq "fallthrough hit" (Paw.run (Paw.seq [ ft ]) (req "/f")).H.body "F";
  eq "fallthrough miss -> 404" (Paw.run (Paw.seq [ ft ]) (req "/g")).H.status 404

let () =
  if !fails = 0 then print_endline "all Paw tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
