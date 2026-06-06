(* The `fennec test` cut parsing — the pure surface of the command. *)

module R = Fennec_testcmd.Run

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  let rec at i j = j = ln || (i + j < lh && hay.[i + j] = needle.[j] && at i (j + 1)) in
  let rec scan i = i + ln <= lh && (at i 0 || scan (i + 1)) in
  ln = 0 || scan 0

let () =
  check "unit" (R.suite_of_string "unit" = Ok R.Unit);
  check "http" (R.suite_of_string "http" = Ok R.Http);
  check "browser" (R.suite_of_string "browser" = Ok R.Browser);
  check "all" (R.suite_of_string "all" = Ok R.All);
  check "case-insensitive" (R.suite_of_string "HTTP" = Ok R.Http);
  check "unknown → error naming the valid set" (match R.suite_of_string "bogus" with Error m -> contains m "unit, http, browser, all" | Ok _ -> false);
  check "round-trips" (List.for_all (fun s -> R.suite_of_string (R.suite_to_string s) = Ok s) [ R.Unit; R.Http; R.Browser; R.All ]);
  check "default is the fast unit cut" (R.default_options.suite = R.Unit);
  check "default fail-fast on" (R.default_options.fail_fast = true);
  check "default base port clears dev's 4000" (R.default_options.base_port >= 7000);

  (* suite_args: the passthrough contract — only the browser runner honours these flags, and
     the order is stable (grep, headed, screenshots, jobs, reporter). *)
  let o = R.default_options in
  check "browser: --headed only when set" (R.suite_args ~cut:R.Browser { o with headed = true } = [ "--headed" ]);
  check "browser: no flags by default" (R.suite_args ~cut:R.Browser o = []);
  check "browser: grep passes through" (R.suite_args ~cut:R.Browser { o with grep = Some "checkout" } = [ "--grep"; "checkout" ]);
  check "browser: screenshots dir passes through" (R.suite_args ~cut:R.Browser { o with screenshots = Some "shots" } = [ "--screenshots"; "shots" ]);
  check "browser: jobs + reporter pass through" (R.suite_args ~cut:R.Browser { o with jobs = Some 3; reporter = Some "plain" } = [ "--jobs"; "3"; "--reporter"; "plain" ]);
  check "browser: stable flag order"
    (R.suite_args ~cut:R.Browser { o with grep = Some "g"; headed = true; screenshots = Some "d"; jobs = Some 2; reporter = Some "pretty" }
     = [ "--grep"; "g"; "--headed"; "--screenshots"; "d"; "--jobs"; "2"; "--reporter"; "pretty" ]);
  check "http: grep passes through, browser-only flags don't" (R.suite_args ~cut:R.Http { o with grep = Some "x"; headed = true; screenshots = Some "d" } = [ "--grep"; "x" ]);
  check "http: no grep → no argv" (R.suite_args ~cut:R.Http o = []);
  check "unit: no argv" (R.suite_args ~cut:R.Unit { o with grep = Some "x" } = []);
  check "all: no argv (dispatches per-cut)" (R.suite_args ~cut:R.All { o with headed = true } = []);

  if !fails = 0 then print_endline "all Run tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
