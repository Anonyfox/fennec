(* Host_pattern: parsing (every malformed shape too), matching, and specificity ordering. These
   pin the routing primitive the whole multi-tenant story rests on. *)

module P = Fennec_server.Host_pattern

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let err = function Error _ -> true | Ok _ -> false

let () =
  print_endline "Host_pattern.of_string:";
  check "exact" (P.of_string "acme.com" = Ok (P.Exact "acme.com"));
  check "exact lowercases" (P.of_string "ACME.com" = Ok (P.Exact "acme.com"));
  check "exact strips trailing dot" (P.of_string "acme.com." = Ok (P.Exact "acme.com"));
  check "suffix" (P.of_string "*.acme.com" = Ok (P.Suffix ".acme.com"));
  check "catch-all" (P.of_string "*" = Ok P.Any);
  check "empty -> error" (err (P.of_string ""));
  check "whitespace-only -> error" (err (P.of_string "   "));
  check "internal space -> error" (err (P.of_string "ac me.com"));
  check "'*.' alone -> error" (err (P.of_string "*."));
  check "mid '*' -> error" (err (P.of_string "a*b"));
  check "'*foo' (not '*.') -> error" (err (P.of_string "*foo"));
  check "double wildcard '*.*.com' -> error" (err (P.of_string "*.*.com"));

  print_endline "Host_pattern.matches:";
  let exact = Result.get_ok (P.of_string "acme.com") in
  let suf = Result.get_ok (P.of_string "*.acme.com") in
  check "exact matches" (P.matches exact ~host:"acme.com");
  check "exact matches w/ :port" (P.matches exact ~host:"acme.com:8020");
  check "exact case-insensitive" (P.matches exact ~host:"ACME.COM");
  check "exact rejects other" (not (P.matches exact ~host:"other.com"));
  check "suffix matches a subdomain" (P.matches suf ~host:"api.acme.com");
  check "suffix matches deep" (P.matches suf ~host:"a.b.acme.com");
  check "suffix needs >=1 label (base alone fails)" (not (P.matches suf ~host:"acme.com"));
  check "suffix rejects other base" (not (P.matches suf ~host:"api.other.com"));
  check "any matches anything" (P.matches P.Any ~host:"whatever.com");
  check "any matches empty host" (P.matches P.Any ~host:"");
  check "exact rejects empty host" (not (P.matches exact ~host:""));

  print_endline "Host_pattern.specificity:";
  let sp s = P.specificity (Result.get_ok (P.of_string s)) in
  check "exact > suffix" (sp "acme.com" > sp "*.acme.com");
  check "longer suffix > shorter suffix" (sp "*.api.acme.com" > sp "*.acme.com");
  check "suffix > any" (sp "*.acme.com" > sp "*");
  check "exact > any" (sp "acme.com" > sp "*");
  if !fails = 0 then print_endline "all Host_pattern tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
