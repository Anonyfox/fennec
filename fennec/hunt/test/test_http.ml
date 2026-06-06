(* Unit tests for the Http layer's pure cores (Fennec_hunt.Http.For_test) — deterministic,
   hermetic, no server: effects (clock, sleep) are injected. Plus the public assertion surface
   tested against constructed responses. *)

module H = Fennec_hunt.Http
module FT = Fennec_hunt.Http.For_test

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let raises f = match f () with () -> false | exception _ -> true
let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec at i j = j = ln || (i + j < lh && hay.[i + j] = needle.[j] && at i (j + 1)) in
  let rec scan i = i + ln <= lh && (at i 0 || scan (i + 1)) in
  ln = 0 || scan 0

(* a fake clock: time advances only when [sleep] is called (fully deterministic) *)
let fake_clock () =
  let t = ref 0.0 in
  let now () = !t in
  let sleep dt = t := !t +. dt in
  (now, sleep)

let () =
  print_endline "Http.For_test.poll (eventually policy):";

  (* succeeds immediately → body called once, no sleep *)
  (let now, sleep = fake_clock () in
   let calls = ref 0 in
   FT.poll ~now ~sleep ~within:5.0 ~interval:0.2 (fun () -> incr calls);
   check "passes on first try → 1 call" (!calls = 1));

  (* fails twice then passes → body retried, succeeds before deadline *)
  (let now, sleep = fake_clock () in
   let calls = ref 0 in
   FT.poll ~now ~sleep ~within:5.0 ~interval:0.2 (fun () ->
       incr calls;
       if !calls < 3 then failwith "not yet");
   check "passes on the 3rd try → 3 calls" (!calls = 3));

  (* always fails → raises after the deadline, message carries the last failure *)
  (let now, sleep = fake_clock () in
   let calls = ref 0 in
   let raised =
     try FT.poll ~now ~sleep ~within:1.0 ~interval:0.2 (fun () -> incr calls; failwith "still pending"); false
     with Failure m -> contains m "still pending" && contains m "within 1.0s"
   in
   check "always-failing → raises after deadline with last failure" raised;
   check "always-failing → polled multiple times before giving up" (!calls >= 2));

  (* the deadline is honoured: with within=1.0 and interval=0.2, it stops ~5-6 tries in *)
  (let now, sleep = fake_clock () in
   let calls = ref 0 in
   (try FT.poll ~now ~sleep ~within:1.0 ~interval:0.2 (fun () -> incr calls; failwith "no") with Failure _ -> ());
   check "bounded by the deadline (~6 tries for within/interval = 1.0/0.2)" (!calls >= 5 && !calls <= 7));

  print_endline "Http.For_test.decode_chunked:";
  check "two chunks → joined" (FT.decode_chunked "2\r\nab\r\n2\r\ncd\r\n0\r\n\r\n" = "abcd");
  check "three chunks" (FT.decode_chunked "2\r\nab\r\n2\r\ncd\r\n2\r\nef\r\n0\r\n\r\n" = "abcdef");
  check "single chunk" (FT.decode_chunked "5\r\nhello\r\n0\r\n\r\n" = "hello");
  check "hex size > 9 (0x10 = 16 bytes)" (FT.decode_chunked "10\r\n0123456789abcdef\r\n0\r\n\r\n" = "0123456789abcdef");
  check "chunk extension ignored" (FT.decode_chunked "2;foo=bar\r\nok\r\n0\r\n\r\n" = "ok");
  check "empty (immediate 0 chunk)" (FT.decode_chunked "0\r\n\r\n" = "");
  check "malformed → best-effort, no raise" (FT.decode_chunked "2\r\nab\r\nGARBAGE" = "ab");

  print_endline "Http.For_test.encode_multipart:";
  (let mp = FT.encode_multipart ~boundary:"BND"
       [ H.field "title" "hi"; H.file ~name:"f" ~filename:"a.txt" ~content_type:"text/plain" "DATA" ] in
   check "field part" (contains mp "--BND\r\nContent-Disposition: form-data; name=\"title\"\r\n\r\nhi\r\n");
   check "file part w/ filename + content-type" (contains mp "Content-Disposition: form-data; name=\"f\"; filename=\"a.txt\"\r\nContent-Type: text/plain\r\n\r\nDATA\r\n");
   check "closing boundary" (contains mp "--BND--\r\n"));
  (let mp = FT.encode_multipart ~boundary:"B" [ H.file ~name:"f" ~filename:"x" "y" ] in
   check "file without content-type omits the Content-Type line" (not (contains mp "Content-Type:")));

  print_endline "Http.For_test.follow_redirects (pure policy, fake fetch):";
  (* a fake response is just a (status, location-target option); fetch returns a scripted chain *)
  let chain = [ "/b", (302, Some "/c"); "/c", (302, Some "/d"); "/d", (200, None) ] in
  let location (st, loc) = if st >= 300 && st < 400 then loc else None in
  let fetch loc = (try List.assoc loc chain with Not_found -> (599, None)) in
  check "follows the chain to the final 200" (FT.follow_redirects ~max:10 ~location ~fetch (302, Some "/b") = (200, None));
  check "no redirect → returns first unchanged" (FT.follow_redirects ~max:10 ~location ~fetch (200, None) = (200, None));
  (* a cycle is bounded by max (does not loop forever) *)
  let cyclic _ = (302, Some "/loop") in
  check "cyclic redirects are bounded by max hops" (FT.follow_redirects ~max:3 ~location ~fetch:cyclic (302, Some "/loop") = (302, Some "/loop"));

  print_endline "Http.For_test.redirect_path:";
  check "absolute path kept" (FT.redirect_path "/dash?x=1" = "/dash?x=1");
  check "absolute URL → its path" (FT.redirect_path "http://host:8080/dash?x=1" = "/dash?x=1");
  check "absolute URL no path → /" (FT.redirect_path "http://host" = "/");
  check "relative → prefixed with /" (FT.redirect_path "dash" = "/dash");

  print_endline "Http assertions (against constructed responses):";
  let resp ?(status = 200) ?(headers = []) ?(body = "") () : H.response = { status; headers; body } in
  let ok name a r = check name (not (raises (fun () -> a r))) in
  let bad name a r = check name (raises (fun () -> a r)) in

  ok "status 200 passes" (H.status 200) (resp ());
  bad "status 200 fails on 404" (H.status 200) (resp ~status:404 ());
  ok "status_2xx passes on 201" H.status_2xx (resp ~status:201 ());
  bad "status_2xx fails on 500" H.status_2xx (resp ~status:500 ());
  ok "status_not 500 passes on 200" (H.status_not 500) (resp ());
  bad "status_not 200 fails on 200" (H.status_not 200) (resp ());

  ok "body_contains passes" (H.body_contains "ell") (resp ~body:"hello" ());
  bad "body_contains fails" (H.body_contains "xyz") (resp ~body:"hello" ());
  ok "body_is exact" (H.body_is "hi") (resp ~body:"hi" ());
  ok "body_not_contains passes" (H.body_not_contains "z") (resp ~body:"hi" ());
  ok "body_empty passes" H.body_empty (resp ~body:"" ());
  bad "body_empty fails on content" H.body_empty (resp ~body:"x" ());
  ok "body_matches regex" (H.body_matches "^[0-9]+$") (resp ~body:"42" ());
  bad "body_matches regex fails" (H.body_matches "^[0-9]+$") (resp ~body:"4a2" ());

  ok "header_is passes" (H.header_is "X-A" "1") (resp ~headers:[("X-A", "1")] ());
  bad "header_is fails on wrong value" (H.header_is "X-A" "2") (resp ~headers:[("X-A", "1")] ());
  ok "header_contains (case-insensitive name)" (H.header_contains "content-type" "json") (resp ~headers:[("Content-Type", "application/json")] ());
  ok "has_header passes" (H.has_header "X-A") (resp ~headers:[("X-A", "1")] ());
  bad "no_header fails when present" (H.no_header "X-A") (resp ~headers:[("X-A", "1")] ());
  ok "is_json passes" H.is_json (resp ~headers:[("Content-Type", "application/json")] ());

  let jbody = {|{"name":"alice","id":"550e8400-e29b-41d4-a716-446655440000","age":30,"tags":["a","b"],"when":"2026-01-02T03:04:05Z","ok":true}|} in
  ok "json_path_is string" (H.json_path_is "name" "alice") (resp ~body:jbody ());
  ok "json_path_is number-as-string" (H.json_path_is "age" "30") (resp ~body:jbody ());
  ok "json_path_is bool-as-string" (H.json_path_is "ok" "true") (resp ~body:jbody ());
  bad "json_path_is wrong" (H.json_path_is "name" "bob") (resp ~body:jbody ());
  ok "json_has present" (H.json_has "id") (resp ~body:jbody ());
  bad "json_has absent" (H.json_has "nope") (resp ~body:jbody ());
  ok "json_length array" (H.json_length "tags" 2) (resp ~body:jbody ());
  ok "json_is_uuid" (H.json_is_uuid "id") (resp ~body:jbody ());
  bad "json_is_uuid fails on non-uuid" (H.json_is_uuid "name") (resp ~body:jbody ());
  ok "json_is_datetime" (H.json_is_datetime "when") (resp ~body:jbody ());
  ok "json_is_number" (H.json_is_number "age") (resp ~body:jbody ());
  ok "json_is_array" (H.json_is_array "tags") (resp ~body:jbody ());
  ok "json_path_matches" (H.json_path_matches "id" "^[0-9a-f-]+$") (resp ~body:jbody ());

  ok "redirect_to passes" (H.redirect_to "/home") (resp ~status:302 ~headers:[("Location", "/home")] ());
  bad "redirect_to fails on 200" (H.redirect_to "/home") (resp ());

  print_endline "Http assertions (coverage completion):";
  ok "status_3xx passes on 302" H.status_3xx (resp ~status:302 ());
  bad "status_3xx fails on 200" H.status_3xx (resp ());
  ok "status_4xx passes on 404" H.status_4xx (resp ~status:404 ());
  ok "status_5xx passes on 503" H.status_5xx (resp ~status:503 ());
  ok "body_not_empty passes" H.body_not_empty (resp ~body:"x" ());
  bad "body_not_empty fails on empty" H.body_not_empty (resp ~body:"" ());
  ok "body_length exact" (H.body_length 5) (resp ~body:"hello" ());
  bad "body_length wrong" (H.body_length 4) (resp ~body:"hello" ());
  ok "min_body_length passes" (H.min_body_length 3) (resp ~body:"hello" ());
  bad "min_body_length fails when short" (H.min_body_length 10) (resp ~body:"hi" ());
  ok "content_type passes" (H.content_type "json") (resp ~headers:[("Content-Type", "application/json")] ());
  ok "is_html passes" H.is_html (resp ~headers:[("Content-Type", "text/html; charset=utf-8")] ());
  bad "is_html fails on json" H.is_html (resp ~headers:[("Content-Type", "application/json")] ());
  ok "no_header passes when absent" (H.no_header "X-Z") (resp ());
  bad "has_header fails when absent" (H.has_header "X-Z") (resp ());
  ok "json_is_string" (H.json_is_string "name") (resp ~body:jbody ());
  ok "json_is_bool" (H.json_is_bool "ok") (resp ~body:jbody ());
  bad "json_is_bool fails on string" (H.json_is_bool "name") (resp ~body:jbody ());
  ok "json_is_null passes" (H.json_is_null "maybe") (resp ~body:{|{"maybe":null}|} ());
  ok "json_path_contains substring" (H.json_path_contains "name" "lic") (resp ~body:jbody ());
  bad "json_path_contains fails" (H.json_path_contains "name" "zzz") (resp ~body:jbody ());
  ok "expect custom passes" (H.expect (fun r -> if r.H.status <> 200 then failwith "no")) (resp ());
  bad "expect custom fails" (H.expect (fun r -> if r.H.status <> 999 then failwith "no")) (resp ());
  ok "has_cookie passes" (H.has_cookie "sid") (resp ~headers:[("Set-Cookie", "sid=abc; Path=/")] ());
  bad "has_cookie fails when absent" (H.has_cookie "sid") (resp ());
  ok "no_cookie passes when absent" (H.no_cookie "sid") (resp ());

  print_endline "Http.For_test.parse_url:";
  check "scheme+host+port+path" (FT.parse_url "http://localhost:4000/api/x" = ("http", "localhost", 4000, "/api/x"));
  check "no scheme defaults http" (FT.parse_url "example.com:8080" = ("http", "example.com", 8080, ""));
  check "https default port 443" (FT.parse_url "https://acme.com" = ("https", "acme.com", 443, ""));
  check "http default port 80" (FT.parse_url "http://acme.com/p" = ("http", "acme.com", 80, "/p"));
  check "bare host" (FT.parse_url "localhost" = ("http", "localhost", 80, ""));

  print_endline "Http.For_test encoders:";
  check "encode_query escapes" (FT.encode_query [("q", "a b&c"); ("n", "1")] = "q=a%20b%26c&n=1");
  check "encode_form sets content-type" (snd (FT.encode_form [("a", "1")]) = "application/x-www-form-urlencoded");
  check "encode_form body" (fst (FT.encode_form [("a", "x y")]) = "a=x%20y");

  print_endline "Http.For_test cookie jar:";
  check "parse one Set-Cookie (strips attributes)" (FT.parse_set_cookies [("Set-Cookie", "sid=abc; Path=/; HttpOnly")] = [("sid", "abc")]);
  check "parse ignores non-Set-Cookie" (FT.parse_set_cookies [("X", "y"); ("set-cookie", "a=1")] = [("a", "1")]);
  check "update_jar adds" (List.sort compare (FT.update_jar [("a", "1")] [("b", "2")]) = [("a", "1"); ("b", "2")]);
  check "update_jar overwrites by name" (FT.update_jar [("a", "1")] [("a", "2")] = [("a", "2")]);

  print_endline "Test_proto.resolve (target resolution):";
  let module TP = Fennec_hunt.Test_proto in
  check "explicit wins over env" (TP.resolve ~explicit:(Some "http://a") ~from_env:(Some "http://b") = Ok "http://a");
  check "env used when no explicit" (TP.resolve ~explicit:None ~from_env:(Some "http://b") = Ok "http://b");
  check "explicit used when no env" (TP.resolve ~explicit:(Some "http://a") ~from_env:None = Ok "http://a");
  check "neither → clear error" (match TP.resolve ~explicit:None ~from_env:None with Error m -> contains m "fennec test" | Ok _ -> false);
  check "url_for builds localhost url" (TP.url_for ~port:7001 = "http://localhost:7001");

  print_endline "Http helpers:";
  check "basic_auth header" (H.basic_auth "user" "pass" = ("Authorization", "Basic dXNlcjpwYXNz"));
  check "bearer header" (H.bearer "tok" = ("Authorization", "Bearer tok"));
  check "json_content_type" (H.json_content_type = ("Content-Type", "application/json"));

  if !fails = 0 then print_endline "all Http tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
