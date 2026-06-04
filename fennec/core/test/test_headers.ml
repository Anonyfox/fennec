(* Unit tests for case-insensitive header ops + the centralized status/method tables. *)

module Headers = Fennec_core.Headers
module H = Fennec_core.Http

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)

let () =
  print_endline "Headers (case-insensitive):";
  let h = [ ("Content-Type", "text/html"); ("X-A", "1"); ("X-A", "2") ] in
  eq "get is ci" (Headers.get h "content-type") (Some "text/html");
  eq "get returns first of multi" (Headers.get h "x-a") (Some "1");
  eq "get_all returns all in order" (Headers.get_all h "X-A") [ "1"; "2" ];
  check "mem ci" (Headers.mem h "CONTENT-TYPE");
  check "not mem absent" (not (Headers.mem h "nope"));
  eq "delete removes every binding" (Headers.get_all (Headers.delete h "x-a") "x-a") [];
  eq "put replaces all with one" (Headers.get_all (Headers.put h "X-A" "9") "x-a") [ "9" ];
  eq "add appends, keeping order" (Headers.get_all (Headers.add h "X-A" "3") "x-a") [ "1"; "2"; "3" ];
  check "ci_equal positive" (Headers.ci_equal "Content-Type" "content-TYPE");
  check "ci_equal length differs" (not (Headers.ci_equal "a" "ab"));
  check "ci_equal content differs" (not (Headers.ci_equal "X-A" "X-B"));

  print_endline "Http.reason_phrase / string_of_meth:";
  eq "200" (H.reason_phrase 200) "OK";
  eq "201" (H.reason_phrase 201) "Created";
  eq "422" (H.reason_phrase 422) "Unprocessable Entity";
  eq "429" (H.reason_phrase 429) "Too Many Requests";
  eq "unknown code -> empty phrase" (H.reason_phrase 799) "";
  eq "meth GET" (H.string_of_meth H.GET) "GET";
  eq "meth round-trips" (H.string_of_meth (H.meth_of_string "DELETE")) "DELETE";
  eq "meth Other" (H.string_of_meth (H.Other "PURGE")) "PURGE";

  if !fails = 0 then print_endline "all Headers tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
