(* Path matcher edge cases: exact, params, wildcard tail, precedence, trailing
   slashes, no-match. *)

module M = Fennec_matcher.Matcher

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let () =
  eq "root exact" (M.match_one ~pattern:"/" "/") (Some []);
  eq "exact" (M.match_one ~pattern:"/about" "/about") (Some []);
  eq "exact mismatch" (M.match_one ~pattern:"/about" "/contact") None;
  eq "trailing slash tolerated" (M.match_one ~pattern:"/about" "/about/") (Some []);
  eq "one param" (M.match_one ~pattern:"/users/:id" "/users/42") (Some [ ("id", "42") ]);
  eq "two params" (M.match_one ~pattern:"/u/:a/p/:b" "/u/1/p/2") (Some [ ("a", "1"); ("b", "2") ]);
  eq "param missing segment" (M.match_one ~pattern:"/users/:id" "/users") None;
  eq "too many segments" (M.match_one ~pattern:"/users/:id" "/users/1/extra") None;
  eq "wildcard tail" (M.match_one ~pattern:"/files/*" "/files/a/b/c") (Some [ ("*", "a/b/c") ]);
  eq "wildcard one" (M.match_one ~pattern:"/files/*" "/files/x") (Some [ ("*", "x") ]);
  (* find: first match wins *)
  let routes = [ ("/", `Home); ("/users/:id", `User); ("/*", `NotFound) ] in
  eq "find home" (M.find routes "/") (Some (`Home, []));
  eq "find user" (M.find routes "/users/7") (Some (`User, [ ("id", "7") ]));
  eq "find catch-all" (M.find routes "/anything/here") (Some (`NotFound, [ ("*", "anything/here") ]));
  (* param accessor *)
  (match M.match_one ~pattern:"/users/:id" "/users/99" with
  | Some ps -> eq "param accessor" (M.param ps "id") (Some "99")
  | None -> check "param accessor" false);

  if !fails = 0 then print_endline "all matcher tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
