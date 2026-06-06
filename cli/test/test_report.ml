(* The cross-suite roll-up — pure aggregation at suite granularity. *)

module R = Fennec_testcmd.Report

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let ok name port = { R.name; port; ok = true }
let bad name port = { R.name; port; ok = false }

let () =
  (* failures count *)
  check "no suites → 0 failures" (R.failures [] = 0);
  check "all green → 0 failures" (R.failures [ ok "a" 8200; ok "b" 8300 ] = 0);
  check "counts only the failed" (R.failures [ ok "a" 8200; bad "b" 8300; bad "c" 8400 ] = 2);

  (* summary text — plural agreement + honest naming of the failed suites *)
  check "single green" (R.summary [ ok "smoke" 8200 ] = "1 suite passed");
  check "many green" (R.summary [ ok "a" 8200; ok "b" 8300; ok "c" 8400 ] = "3 suites passed");
  check "single failed names it" (R.summary [ bad "checkout" 8300 ] = "1 of 1 suite failed: checkout (:8300)");
  check "mixed names only the failures, in order"
    (R.summary [ ok "a" 8200; bad "checkout" 8300; ok "c" 8400; bad "users" 8500 ]
     = "2 of 4 suites failed: checkout (:8300), users (:8500)");

  if !fails = 0 then print_endline "all Report tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
