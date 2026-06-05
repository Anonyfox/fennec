(* Host_router: the validation gate (every build error — ALL at once), routing precedence
   (specificity, not declaration order; the single default; unknown-host -> None). *)

module R = Fennec_server.Host_router

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let ok = function Ok _ -> true | Error _ -> false
let has_err k = function Error es -> List.exists k es | Ok _ -> false
let build pairs = R.build (List.map (fun (n, ps) -> (n, ps, n)) pairs)

let () =
  print_endline "Host_router.build (validation):";
  check "valid table" (ok (build [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]) ]));
  check "empty list is ok" (ok (build []));
  check "two catch-alls -> Multiple_catch_all" (has_err (function R.Multiple_catch_all _ -> true | _ -> false) (build [ ("web", [ "*" ]); ("other", [ "*" ]) ]));
  check "same exact on two endpoints -> Conflicting_pattern" (has_err (function R.Conflicting_pattern _ -> true | _ -> false) (build [ ("a", [ "x.com" ]); ("b", [ "x.com" ]) ]));
  check "duplicate name -> Duplicate_name" (has_err (function R.Duplicate_name _ -> true | _ -> false) (build [ ("web", [ "a.com" ]); ("web", [ "b.com" ]) ]));
  check "empty name -> Bad_name" (has_err (function R.Bad_name _ -> true | _ -> false) (build [ ("", [ "*" ]) ]));
  check "name with space -> Bad_name" (has_err (function R.Bad_name _ -> true | _ -> false) (build [ ("we b", [ "*" ]) ]));
  check "no patterns -> No_patterns" (has_err (function R.No_patterns _ -> true | _ -> false) (build [ ("web", []) ]));
  check "bad pattern -> Bad_pattern" (has_err (function R.Bad_pattern _ -> true | _ -> false) (build [ ("web", [ "a*b" ]) ]));
  check "exact + overlapping wildcard is NOT a conflict" (ok (build [ ("api", [ "api.acme.com" ]); ("rest", [ "*.acme.com" ]) ]));
  (* multi-error: TWO problems → both reported in one pass *)
  (match build [ ("", [ "*" ]); ("a", [ "x.com" ]); ("b", [ "x.com" ]) ] with
  | Error es ->
    check "multi-error: bad name AND conflict both reported"
      (List.exists (function R.Bad_name _ -> true | _ -> false) es
      && List.exists (function R.Conflicting_pattern _ -> true | _ -> false) es);
    check "multi-error: at least 2 errors" (List.length es >= 2)
  | Ok _ -> check "multi-error: should have failed" false);

  print_endline "Host_router.route (precedence):";
  let t = Result.get_ok (build [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]); ("api", [ "*.acme.com" ]) ]) in
  check "exact beats wildcard + default" (R.route t ~host:"admin.acme.com" = Some "admin");
  check "wildcard beats default" (R.route t ~host:"x.acme.com" = Some "api");
  check "unknown host falls to '*' default" (R.route t ~host:"totally.else.com" = Some "web");
  (* reversing the declaration order must NOT change precedence *)
  let t2 = Result.get_ok (build [ ("api", [ "*.acme.com" ]); ("admin", [ "admin.acme.com" ]); ("web", [ "*" ]) ]) in
  check "precedence is specificity, not declaration order" (R.route t2 ~host:"admin.acme.com" = Some "admin" && R.route t2 ~host:"x.acme.com" = Some "api");
  (* nested wildcards: the longer suffix wins at route time *)
  let tn = Result.get_ok (build [ ("broad", [ "*.acme.com" ]); ("narrow", [ "*.api.acme.com" ]) ]) in
  check "longer suffix wins" (R.route tn ~host:"x.api.acme.com" = Some "narrow");
  check "shorter suffix still catches its level" (R.route tn ~host:"x.acme.com" = Some "broad");
  (* no catch-all -> unknown host is None (404), matching host still routes *)
  let t3 = Result.get_ok (build [ ("admin", [ "admin.acme.com" ]) ]) in
  check "no '*' -> unknown host is None" (R.route t3 ~host:"nope.com" = None);
  check "no '*' -> matching host still routes" (R.route t3 ~host:"admin.acme.com" = Some "admin");

  print_endline "Host_router.entries (declaration order preserved):";
  let te = Result.get_ok (build [ ("web", [ "*" ]); ("admin", [ "admin.acme.com" ]) ]) in
  check "entries keep declaration order" (List.map (fun e -> e.R.name) (R.entries te) = [ "web"; "admin" ]);
  if !fails = 0 then print_endline "all Host_router tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
