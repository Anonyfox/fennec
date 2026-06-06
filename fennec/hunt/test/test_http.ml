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

  print_endline "Http helpers:";
  check "basic_auth header" (H.basic_auth "user" "pass" = ("Authorization", "Basic dXNlcjpwYXNz"));
  check "bearer header" (H.bearer "tok" = ("Authorization", "Bearer tok"));

  if !fails = 0 then print_endline "all Http tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
