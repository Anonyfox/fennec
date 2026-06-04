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
let req ?(meth = H.GET) path = H.make_request ~meth ~path ()

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
  eq "req_header ci" (Conn.req_header c5 "x-foo") (Some "bar");
  (* lazy, percent-decoded query params via the conn *)
  let cq = Conn.make (H.make_request ~meth:H.GET ~path:"/s" ~query_string:"q=a+b&n=2" ()) in
  eq "conn query decoded" (Conn.query cq "q") (Some "a b");
  eq "conn query other" (Conn.query cq "n") (Some "2");
  eq "conn query missing" (Conn.query cq "z") None;
  (* request metadata *)
  let cm =
    Conn.make
      (H.make_request ~meth:H.GET ~path:"/" ~host:"example.com" ~scheme:"https"
         ~remote_ip:(Some "1.2.3.4") ())
  in
  eq "conn host" (Conn.host cm) "example.com";
  eq "conn scheme" (Conn.scheme cm) "https";
  eq "conn remote_ip" (Conn.remote_ip cm) (Some "1.2.3.4");
  (* request cookies (lazy) *)
  let cc = Conn.make { (req "/") with H.headers = [ ("Cookie", "sid=abc; theme=dark") ] } in
  eq "conn cookie read" (Conn.cookie cc "sid") (Some "abc");
  eq "conn cookie other" (Conn.cookie cc "theme") (Some "dark");
  eq "conn cookie missing" (Conn.cookie cc "nope") None;
  (* response cookie: set, does not answer, survives a later answer *)
  let cw = Conn.set_cookie (Conn.make (req "/")) "sid" "xyz" in
  check "set_cookie does not answer" (not (Conn.answered cw));
  let cw = Conn.json cw "{}" in
  let setcs =
    match Conn.resp cw with
    | Some r -> Fennec_core.Headers.get_all r.H.headers "set-cookie"
    | None -> []
  in
  check "one Set-Cookie emitted" (List.length setcs = 1);
  check "Set-Cookie carries the value"
    (match setcs with
     | [ s ] ->
       let n = String.length s and sub = "sid=xyz" in
       let m = String.length sub in
       let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
       go 0
     | _ -> false);
  (* form body params (urlencoded), percent-decoded *)
  let cf =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/"
         ~headers:[ ("content-type", "application/x-www-form-urlencoded") ]
         ~body:"a=1&b=hello+world" ())
  in
  eq "body_param decoded" (Conn.body_param cf "b") (Some "hello world");
  eq "param falls back to the body" (Conn.param cf "a") (Some "1");
  (* query wins over body in [param] *)
  let cf2 =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/" ~query_string:"a=q"
         ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"a=b" ())
  in
  eq "param prefers the query string" (Conn.param cf2 "a") (Some "q");
  (* multipart file upload *)
  let mp =
    "--B\r\nContent-Disposition: form-data; name=\"f\"; filename=\"x.txt\"\r\n\
     Content-Type: text/plain\r\n\r\nDATA\r\n--B--\r\n"
  in
  let cu =
    Conn.make
      (H.make_request ~meth:H.POST ~path:"/"
         ~headers:[ ("content-type", "multipart/form-data; boundary=B") ] ~body:mp ())
  in
  (match Conn.file cu "f" with
   | Some p ->
     eq "uploaded filename" p.Fennec_core.Multipart.filename (Some "x.txt");
     eq "uploaded data" p.Fennec_core.Multipart.data "DATA"
   | None -> check "uploaded file present" false)

let () =
  print_endline "Conn (build-vs-answer + state):";
  (* set_header accumulates WITHOUT answering — the pipeline keeps running *)
  let c = Conn.set_header (Conn.make (req "/")) "X-A" "1" in
  check "set_header does not answer" (not (Conn.answered c));
  (* ...and the pre-set header survives a later answering paw *)
  let c = Conn.json c "{}" in
  check "answered after json" (Conn.answered c);
  let hdrs = match Conn.resp c with Some r -> r.H.headers | None -> [] in
  check "pre-set header preserved through the answer" (List.mem ("X-A", "1") hdrs);
  check "answer's content-type merged in too"
    (List.exists (fun (k, _) -> String.lowercase_ascii k = "content-type") hdrs);
  (* before_send hooks run in registration order (FIFO) *)
  let order = ref [] in
  let c = Conn.make (req "/") in
  let c = Conn.before_send c (fun r -> order := "a" :: !order; r) in
  let c = Conn.before_send c (fun r -> order := "b" :: !order; r) in
  let c = Conn.text c "x" in
  let _ = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "")) in
  eq "before_send FIFO order" (List.rev !order) [ "a"; "b" ];
  (* status alone answers with that code and an empty body *)
  let cs = Conn.set_status 204 (Conn.make (req "/")) in
  check "status answers" (Conn.answered cs);
  eq "status code" (match Conn.resp cs with Some r -> r.H.status | None -> 0) 204;
  (* halt answers but yields no response (the server turns this into a 404) *)
  let ch = Conn.halt (Conn.make (req "/")) in
  check "halt answers" (Conn.answered ch);
  check "halt has no response" (Conn.resp ch = None);
  (* answering short-circuits, so a header set AFTER the answer still applies (same conn) *)
  let c = Conn.text (Conn.make (req "/")) "body" in
  let c = Conn.set_header c "X-Late" "y" in
  check "header added post-answer is present"
    (match Conn.resp c with Some r -> List.mem ("X-Late", "y") r.H.headers | None -> false);
  (* a content-type pre-set via set_header is replaced by the answerer's — exactly one ships *)
  let c = Conn.set_header (Conn.make (req "/")) "content-type" "text/plain" in
  let c = Conn.json c "{}" in
  let cts =
    match Conn.resp c with
    | Some r -> List.filter (fun (k, _) -> String.lowercase_ascii k = "content-type") r.H.headers
    | None -> []
  in
  check "exactly one content-type after answer" (List.length cts = 1);
  check "the answerer's content-type wins" (List.assoc_opt "content-type" cts = Some "application/json")

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
  print_endline "Route path params:";
  let app =
    Paw.seq
      [ Route.get "/users/:id" (fun c -> Conn.text c (Option.value (Conn.path_param c "id") ~default:"?"));
        Route.get "/files/*rest" (fun c -> Conn.text c (Option.value (Conn.path_param c "rest") ~default:"?"));
        Route.get "/a/:x/b/:y" (fun c ->
            Conn.text c (Option.value (Conn.param c "x") ~default:"?" ^ "-"
                        ^ Option.value (Conn.param c "y") ~default:"?")) ]
  in
  eq "captures :id" (Paw.run app (req "/users/42")).H.body "42";
  eq "splat captures the rest" (Paw.run app (req "/files/a/b/c.txt")).H.body "a/b/c.txt";
  eq "two params" (Paw.run app (req "/a/1/b/2")).H.body "1-2";
  eq "param count mismatch -> 404" (Paw.run app (req "/users/42/extra")).H.status 404;
  eq "no match -> 404" (Paw.run app (req "/nope")).H.status 404;
  (* path param takes precedence over a query param of the same name in [param] *)
  let one = Paw.seq [ Route.get "/p/:id" (fun c -> Conn.text c (Option.value (Conn.param c "id") ~default:"?")) ] in
  eq "path param beats query in param"
    (Paw.run one (H.make_request ~meth:H.GET ~path:"/p/path" ~query_string:"id=query" ())).H.body "path"

let () =
  if !fails = 0 then print_endline "all Paw tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
