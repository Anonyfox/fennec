(* Unit tests for the pure framework core: Http, Middleware, App dispatch, Dev
   livereload injection, and Page document assembly. No socket, no Eio — these are
   all pure functions, so the whole framework's request path is testable here. *)

module H = Fennec_core.Http
module MW = Fennec_core.Middleware
module App = Fennec_core.App
module Dev = Fennec_core.Dev
module Page = Fennec_ui.Page
module Mime = Fennec_core.Mime
module Sem = Fennec_core.Http_semantics
module Date = Fennec_core.Http_date

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n" name)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let req ?(meth = H.GET) ?(query = []) ?(headers = []) ?(body = "") path =
  { H.meth; path; query; headers; body }

(* ---- Http ---- *)

let test_http () =
  print_endline "Http:";
  check "meth_of_string GET" (H.meth_of_string "GET" = H.GET);
  check "meth_of_string unknown" (H.meth_of_string "FOO" = H.Other "FOO");
  check "split_target path+query"
    (H.split_target "/a/b?x=1&y=two" = ("/a/b", [ ("x", "1"); ("y", "two") ]));
  check "split_target no query" (H.split_target "/a" = ("/a", []));
  check "parse_query empty" (H.parse_query "" = []);
  check "parse_query flag" (H.parse_query "a&b=2" = [ ("a", ""); ("b", "2") ]);
  let r = req ~query:[ ("k", "v") ] "/" in
  check "query helper" (H.query r "k" = Some "v");
  check "query missing" (H.query r "nope" = None);
  let resp = H.json ~status:201 "{}" in
  check "json status" (resp.H.status = 201);
  check "json content-type" (List.assoc "content-type" resp.H.headers = "application/json")

(* ---- Middleware + App dispatch ---- *)

let test_dispatch () =
  print_endline "App dispatch:";
  (* precedence: middleware halt > server route > pages > 404 *)
  let app =
    App.create ()
    |> App.use (fun conn ->
           if conn.MW.req.H.path = "/blocked" then MW.halt conn (H.text ~status:403 "blocked"))
    |> App.get "/api/ping" (fun _ -> H.json {|{"pong":true}|})
    |> App.pages
         [ App.page Routes.nil (fun _ -> H.html "<h1>home</h1>");
           App.page Routes.(s "tasks" / str /? nil) (fun id _ -> H.html ("task " ^ id)) ]
    |> App.not_found (fun _ -> H.text ~status:404 "custom 404")
  in
  let dispatch r = App.dispatch app r in
  check "middleware halt wins" ((dispatch (req "/blocked")).H.status = 403);
  check "server route" ((dispatch (req "/api/ping")).H.body = {|{"pong":true}|});
  check "page nil" (contains (dispatch (req "/")).H.body "home");
  check "page param" ((dispatch (req "/tasks/42")).H.body = "task 42");
  check "custom 404" ((dispatch (req "/missing")).H.status = 404);
  check "404 body" ((dispatch (req "/missing")).H.body = "custom 404");
  (* server route beats a page that could also match *)
  let app2 =
    App.create ()
    |> App.get "/x" (fun _ -> H.text "route")
    |> App.pages [ App.page Routes.(s "x" /? nil) (fun _ -> H.text "page") ]
  in
  check "server route beats page" ((App.dispatch app2 (req "/x")).H.body = "route")

(* ---- Dev livereload injection ---- *)

let test_dev () =
  print_endline "Dev injection:";
  let out = Dev.inject_html "<html><body><h1>x</h1></body></html>" in
  check "injects script" (contains out "<script>");
  check "references endpoint" (contains out Dev.endpoint);
  check "script before </body>" (contains out ("</script></body>"));
  (* no body tag: appended *)
  let out2 = Dev.inject_html "<p>no body</p>" in
  check "appends when no </body>" (contains out2 "<p>no body</p>" && contains out2 "<script>");
  (* injects before the LAST </body> only *)
  let out3 = Dev.inject_html "<body>a</body><body>b</body>" in
  let idx h n =
    let nl = String.length n and hl = String.length h in
    let rec go i = if i + nl > hl then -1 else if String.sub h i nl = n then i else go (i + 1) in
    go 0
  in
  check "injects before last </body>" (idx out3 "<script>" > idx out3 "a</body>")

(* ---- Page document ---- *)

let test_page () =
  print_endline "Page document:";
  let doc =
    Page.document ~title:"T" ~description:"D" ~css_href:"/s.css"
      ~scripts:[ "/r.js"; "/a.js" ]
      ~props_json:{|{"name":"world"}|} ~body_html:"<h1>hi</h1>" ()
  in
  check "has doctype" (contains doc "<!DOCTYPE html>");
  check "title" (contains doc "<title>T</title>");
  check "description" (contains doc {|content="D"|});
  check "css link" (contains doc {|href="/s.css"|});
  check "scripts in order"
    (let i s =
       let nl = String.length s and hl = String.length doc in
       let rec go i = if i + nl > hl then -1 else if String.sub doc i nl = s then i else go (i + 1) in
       go 0
     in
     i "/r.js" < i "/a.js");
  check "body in #root" (contains doc {|<div id="root"><h1>hi</h1></div>|});
  check "props inlined" (contains doc "fennec-props");
  (* props JSON is escaped so it can't break out of the script element *)
  let evil = Page.document ~props_json:{|{"x":"</script><script>alert(1)"}|} ~body_html:"" () in
  check "escapes </script> in props" (not (contains evil "</script><script>alert"));
  check "escapes < as unicode" (contains evil "\\u003c");
  (* dev flag injects livereload; default does not *)
  check "dev=false no livereload" (not (contains doc Dev.endpoint));
  let devdoc = Page.document ~dev:true ~body_html:"" () in
  check "dev=true injects livereload" (contains devdoc Dev.endpoint)

(* ---- MIME ---- *)

let test_mime () =
  print_endline "Mime:";
  check "html" (Mime.of_path "x.html" = "text/html; charset=utf-8");
  check "css" (Mime.of_path "/a/b/app.css" = "text/css; charset=utf-8");
  check "js" (Mime.of_path "main.js" = "text/javascript; charset=utf-8");
  check "svg" (Mime.of_path "logo.svg" = "image/svg+xml");
  check "woff2" (Mime.of_path "f.woff2" = "font/woff2");
  check "uppercase ext" (Mime.of_path "IMG.PNG" = "image/png");
  check "unknown -> octet-stream" (Mime.of_path "x.qqq" = "application/octet-stream");
  check "no ext -> octet-stream" (Mime.of_path "README" = "application/octet-stream");
  check "compressible html" (Mime.compressible "text/html; charset=utf-8");
  check "compressible json" (Mime.compressible "application/json");
  check "compressible svg" (Mime.compressible "image/svg+xml");
  check "NOT compressible png" (not (Mime.compressible "image/png"));
  check "NOT compressible woff2" (not (Mime.compressible "font/woff2"));
  check "NOT compressible octet" (not (Mime.compressible "application/octet-stream"))

(* ---- content negotiation ---- *)

let test_negotiation () =
  print_endline "Accept-Encoding negotiation:";
  let neg s = Sem.negotiate_encoding ~accept:(Some s) () in
  check "absent -> identity" (Sem.negotiate_encoding ~accept:None () = Sem.Identity);
  check "empty -> identity" (neg "" = Sem.Identity);
  check "gzip" (neg "gzip" = Sem.Gzip);
  check "gzip, deflate -> gzip" (neg "gzip, deflate" = Sem.Gzip);
  check "deflate only" (neg "deflate" = Sem.Deflate);
  check "gzip;q=0 forbids gzip" (neg "gzip;q=0, deflate" = Sem.Deflate);
  check "br only (unsupported) -> identity" (neg "br" = Sem.Identity);
  check "prefers gzip over equal deflate" (neg "deflate;q=1.0, gzip;q=1.0" = Sem.Gzip);
  check "higher-q deflate beats lower-q gzip" (neg "gzip;q=0.5, deflate;q=0.9" = Sem.Deflate);
  check "* enables gzip" (neg "*" = Sem.Gzip);
  check "gzip;q=0,* -> deflate via star" (neg "gzip;q=0, *" = Sem.Deflate)

(* ---- conditional requests ---- *)

let test_conditional () =
  print_endline "Conditional requests:";
  let etag = Sem.make_etag "abc123" in
  check "etag quoted" (etag = "\"abc123\"");
  let h v = [ ("If-None-Match", v) ] in
  check "INM exact match" (Sem.if_none_match_satisfied ~etag (h "\"abc123\""));
  check "INM star" (Sem.if_none_match_satisfied ~etag (h "*"));
  check "INM weak matches strong" (Sem.if_none_match_satisfied ~etag (h "W/\"abc123\""));
  check "INM list contains" (Sem.if_none_match_satisfied ~etag (h "\"x\", \"abc123\""));
  check "INM no match" (not (Sem.if_none_match_satisfied ~etag (h "\"other\"")));
  check "INM absent" (not (Sem.if_none_match_satisfied ~etag []));
  (* If-Modified-Since: resource mtime <= client date => 304 *)
  let date = Date.format 1_000_000.0 in
  check "IMS not modified (older resource)"
    (Sem.if_modified_since_satisfied ~mtime:999_000.0 [ ("If-Modified-Since", date) ]);
  check "IMS modified (newer resource)"
    (not (Sem.if_modified_since_satisfied ~mtime:1_000_001.0 [ ("If-Modified-Since", date) ]))

(* ---- Range parsing ---- *)

let test_range () =
  print_endline "Range parsing:";
  let r v = Sem.parse_range ~len:100 [ ("Range", v) ] in
  check "no range header" (Sem.parse_range ~len:100 [] = `None);
  check "bytes=0-49" (r "bytes=0-49" = `Range { first = 0; last = 49 });
  check "bytes=50- (open end)" (r "bytes=50-" = `Range { first = 50; last = 99 });
  check "bytes=-20 (suffix)" (r "bytes=-20" = `Range { first = 80; last = 99 });
  check "clamps last to len-1" (r "bytes=90-200" = `Range { first = 90; last = 99 });
  check "start past end -> unsatisfiable" (r "bytes=100-200" = `Unsatisfiable);
  check "suffix 0 -> unsatisfiable" (r "bytes=-0" = `Unsatisfiable);
  check "multiple ranges -> None (decline)" (r "bytes=0-1,5-6" = `None);
  check "non-bytes unit -> None" (r "items=0-1" = `None)

(* ---- HTTP-date ---- *)

let test_http_date () =
  print_endline "HTTP-date:";
  (* round-trip: format then parse is identity at second resolution *)
  let t = 784_111_777.0 in
  let s = Date.format t in
  check "format is IMF-fixdate"
    (contains s "GMT" && String.length s = 29);
  check "round-trips" (Date.parse s = Some t);
  (* the canonical RFC example *)
  check "parse IMF example"
    (Date.parse "Sun, 06 Nov 1994 08:49:37 GMT" = Some 784_111_777.0);
  check "parse asctime form"
    (Date.parse "Sun Nov  6 08:49:37 1994" = Some 784_111_777.0);
  check "parse rfc850 form"
    (Date.parse "Sunday, 06-Nov-94 08:49:37 GMT" = Some 784_111_777.0);
  check "parse garbage -> None" (Date.parse "not a date" = None)

let () =
  test_http ();
  test_dispatch ();
  test_dev ();
  test_page ();
  test_mime ();
  test_negotiation ();
  test_conditional ();
  test_range ();
  test_http_date ();
  if !failures = 0 then print_endline "all core tests passed."
  else (
    Printf.printf "%d test(s) failed.\n" !failures;
    exit 1)
