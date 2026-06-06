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
  if !fails = 0 then print_endline "all Run tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
