(* Unit tests for Fennec_core.App — the unified plug pipeline. Verifies that
   order-in-the-pipe is precedence, short-circuiting (first answer wins), the verb
   plugs (get/post/filter/fallthrough/pages/always), HEAD-as-GET, and the pure Dev
   livereload injection. *)

module H = Fennec_core.Http
module App = Fennec_core.App
module Dev = Fennec_core.Dev

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let req ?(meth = H.GET) path = { H.meth; path; query = []; headers = []; body = "" }

let () =
  print_endline "App pipeline precedence:";
  let app =
    App.empty
    (* a filter that halts on /blocked *)
    |> App.use (App.filter (fun r -> if r.H.path = "/blocked" then Some (H.text ~status:403 "blocked") else None))
    |> App.use (App.get "/api/ping" (fun _ -> H.json {|{"pong":true}|}))
    |> App.use (App.fallthrough (fun r -> if r.H.path = "/static.txt" then Some (H.text "static") else None))
    |> App.use
         (App.pages
            [ App.page Routes.nil (fun _ -> H.html "<h1>home</h1>");
              App.page Routes.(s "tasks" / str /? nil) (fun id _ -> H.html ("task " ^ id)) ])
    |> App.use (App.always (fun _ -> H.text ~status:404 "custom 404"))
  in
  let d r = App.run app r in
  eq "filter halts first" (d (req "/blocked")).H.status 403;
  eq "route" (d (req "/api/ping")).H.body {|{"pong":true}|};
  eq "fallthrough" (d (req "/static.txt")).H.body "static";
  check "page nil" (contains (d (req "/")).H.body "home");
  eq "page param" (d (req "/tasks/42")).H.body "task 42";
  eq "terminal 404 status" (d (req "/missing")).H.status 404;
  eq "terminal 404 body" (d (req "/missing")).H.body "custom 404";
  eq "HEAD routes as GET" (d (req ~meth:H.HEAD "/api/ping")).H.body {|{"pong":true}|};

  print_endline "App short-circuit (first wins):";
  let app2 =
    App.empty
    |> App.use (App.get "/x" (fun _ -> H.text "first"))
    |> App.use (App.get "/x" (fun _ -> H.text "second"))
  in
  eq "first plug wins" (App.run app2 (req "/x")).H.body "first";
  (* an unanswered pipeline -> default 404 *)
  eq "empty pipeline -> 404" (App.run App.empty (req "/")).H.status 404;

  print_endline "App method matching:";
  let app3 =
    App.empty
    |> App.use (App.post "/p" (fun _ -> H.text "posted"))
    |> App.use (App.get "/p" (fun _ -> H.text "got"))
  in
  eq "POST routes to post" (App.run app3 (req ~meth:H.POST "/p")).H.body "posted";
  eq "GET routes to get" (App.run app3 (req "/p")).H.body "got";
  eq "wrong method falls through to 404" (App.run app3 (req ~meth:H.DELETE "/p")).H.status 404;

  print_endline "Dev injection:";
  let out = Dev.inject_html "<html><body><h1>x</h1></body></html>" in
  check "injects script" (contains out "<script>");
  check "references endpoint" (contains out Dev.endpoint);
  check "before </body>" (contains out "</script></body>");
  check "appends w/o </body>" (contains (Dev.inject_html "<p>x</p>") "<script>");
  let out3 = Dev.inject_html "<body>a</body><body>b</body>" in
  let idx h n =
    let nl = String.length n and hl = String.length h in
    let rec go i = if i + nl > hl then -1 else if String.sub h i nl = n then i else go (i + 1) in
    go 0
  in
  check "injects before LAST </body>" (idx out3 "<script>" > idx out3 "a</body>");

  if !fails = 0 then print_endline "all App/Dev tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
