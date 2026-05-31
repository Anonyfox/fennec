(* Unit tests for Fennec_core.Http — request/response types, query parsing,
   target splitting. Edge cases: empty/malformed query, multiple '?', missing
   values, percent-looking input (no decoding yet — documented). *)

module H = Fennec_core.Http

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let () =
  print_endline "Http.meth_of_string:";
  eq "GET" (H.meth_of_string "GET") H.GET;
  eq "POST" (H.meth_of_string "POST") H.POST;
  eq "PATCH" (H.meth_of_string "PATCH") H.PATCH;
  eq "unknown -> Other" (H.meth_of_string "PURGE") (H.Other "PURGE");
  eq "empty -> Other" (H.meth_of_string "") (H.Other "");
  eq "case-sensitive (get != GET)" (H.meth_of_string "get") (H.Other "get");

  print_endline "Http.parse_query:";
  eq "empty" (H.parse_query "") [];
  eq "single" (H.parse_query "a=1") [ ("a", "1") ];
  eq "multiple" (H.parse_query "a=1&b=2") [ ("a", "1"); ("b", "2") ];
  eq "flag (no =)" (H.parse_query "a&b=2") [ ("a", ""); ("b", "2") ];
  eq "empty value" (H.parse_query "a=") [ ("a", "") ];
  eq "trailing &" (H.parse_query "a=1&") [ ("a", "1") ];
  eq "leading &" (H.parse_query "&a=1") [ ("a", "1") ];
  eq "double &&" (H.parse_query "a=1&&b=2") [ ("a", "1"); ("b", "2") ];
  eq "value with = kept" (H.parse_query "a=1=2") [ ("a", "1=2") ];
  eq "only &" (H.parse_query "&&&") [];

  print_endline "Http.split_target:";
  eq "no query" (H.split_target "/a/b") ("/a/b", []);
  eq "with query" (H.split_target "/a?x=1") ("/a", [ ("x", "1") ]);
  eq "empty query after ?" (H.split_target "/a?") ("/a", []);
  eq "root" (H.split_target "/") ("/", []);
  eq "empty" (H.split_target "") ("", []);
  (* a second '?' becomes part of the query string (split on first only) *)
  eq "double ?" (H.split_target "/a?x=1?y=2") ("/a", [ ("x", "1?y=2") ]);
  eq "query only" (H.split_target "?x=1") ("", [ ("x", "1") ]);

  print_endline "Http.query helper + responses:";
  let r = { H.meth = H.GET; path = "/"; query = [ ("k", "v") ]; headers = []; body = "" } in
  eq "query found" (H.query r "k") (Some "v");
  eq "query missing" (H.query r "nope") None;
  let resp = H.json ~status:201 ~headers:[ ("x", "y") ] "{}" in
  eq "json status" resp.H.status 201;
  eq "json ct" (List.assoc "content-type" resp.H.headers) "application/json";
  check "json extra header kept" (List.mem ("x", "y") resp.H.headers);
  eq "text default status" (H.text "hi").H.status 200;
  eq "html ct" (List.assoc "content-type" (H.html "x").H.headers) "text/html; charset=utf-8";

  if !fails = 0 then print_endline "all Http tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
