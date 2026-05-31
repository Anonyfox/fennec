(* Unit tests for Fennec_core.App dispatch precedence + Middleware, and the pure
   Dev livereload injection. Edge cases: precedence order, fallthrough chain,
   HEAD-as-GET, injection with/without </body>, multiple </body>. *)

module H = Fennec_core.Http
module MW = Fennec_core.Middleware
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
  print_endline "App dispatch precedence:";
  let app =
    App.create ()
    |> App.use (fun conn ->
           if conn.MW.req.H.path = "/blocked" then MW.halt conn (H.text ~status:403 "blocked"))
    |> App.get "/api/ping" (fun _ -> H.json {|{"pong":true}|})
    |> App.use_fallthrough (fun r ->
           if r.H.path = "/static.txt" then Some (H.text "static") else None)
    |> App.pages
         [ App.page Routes.nil (fun _ -> H.html "<h1>home</h1>");
           App.page Routes.(s "tasks" / str /? nil) (fun id _ -> H.html ("task " ^ id)) ]
    |> App.not_found (fun _ -> H.text ~status:404 "custom 404")
  in
  let d r = App.dispatch app r in
  eq "middleware halt wins" (d (req "/blocked")).H.status 403;
  eq "server route" (d (req "/api/ping")).H.body {|{"pong":true}|};
  eq "fallthrough (static)" (d (req "/static.txt")).H.body "static";
  check "page nil" (contains (d (req "/")).H.body "home");
  eq "page param" (d (req "/tasks/42")).H.body "task 42";
  eq "404 status" (d (req "/missing")).H.status 404;
  eq "404 body" (d (req "/missing")).H.body "custom 404";
  (* HEAD is dispatched as GET so static/pages answer it *)
  eq "HEAD routes as GET" (d (req ~meth:H.HEAD "/api/ping")).H.body {|{"pong":true}|};

  (* fallthrough order: first Some wins; tried before pages *)
  let app2 =
    App.create ()
    |> App.use_fallthrough (fun r -> if r.H.path = "/x" then Some (H.text "first") else None)
    |> App.use_fallthrough (fun r -> if r.H.path = "/x" then Some (H.text "second") else None)
    |> App.pages [ App.page Routes.(s "x" /? nil) (fun _ -> H.text "page") ]
  in
  eq "first fallthrough wins" (App.dispatch app2 (req "/x")).H.body "first";

  (* server route beats a page that could also match *)
  let app3 =
    App.create ()
    |> App.get "/y" (fun _ -> H.text "route")
    |> App.pages [ App.page Routes.(s "y" /? nil) (fun _ -> H.text "page") ]
  in
  eq "server route beats page" (App.dispatch app3 (req "/y")).H.body "route";

  print_endline "Dev injection:";
  let out = Dev.inject_html "<html><body><h1>x</h1></body></html>" in
  check "injects script" (contains out "<script>");
  check "references endpoint" (contains out Dev.endpoint);
  check "before </body>" (contains out "</script></body>");
  let out2 = Dev.inject_html "<p>no body</p>" in
  check "appends w/o </body>" (contains out2 "<p>no body</p>" && contains out2 "<script>");
  let out3 = Dev.inject_html "<body>a</body><body>b</body>" in
  let idx h n =
    let nl = String.length n and hl = String.length h in
    let rec go i = if i + nl > hl then -1 else if String.sub h i nl = n then i else go (i + 1) in
    go 0
  in
  check "injects before LAST </body>" (idx out3 "<script>" > idx out3 "a</body>");
  check "empty input -> just script" (contains (Dev.inject_html "") "<script>");

  if !fails = 0 then print_endline "all App/Dev tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
