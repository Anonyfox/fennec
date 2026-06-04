(* Crash_limiter: retries with growing backoff up to the threshold, then gives up; a flat
   backoff for the port-busy case; reset clears the streak. Time is injected, so it's exact. *)

module C = Fennec_dev.Crash_limiter

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let is_retry = function C.Retry _ -> true | C.Give_up -> false
let is_giveup = function C.Give_up -> true | C.Retry _ -> false

let () =
  print_endline "Crash_limiter:";
  let t = C.create ~window:10.0 ~max:3 () in
  check "1st crash -> retry" (is_retry (C.record t ~now:0.0 ()));
  check "2nd crash -> retry" (is_retry (C.record t ~now:1.0 ()));
  check "3rd within window -> give up" (is_giveup (C.record t ~now:2.0 ()));

  (* crashes spaced beyond the window don't accumulate to a give-up *)
  let t = C.create ~window:10.0 ~max:3 () in
  check "crash at t=0 retry" (is_retry (C.record t ~now:0.0 ()));
  check "crash at t=20 retry (old one aged out)" (is_retry (C.record t ~now:20.0 ()));
  check "crash at t=40 retry (window slid)" (is_retry (C.record t ~now:40.0 ()));

  (* reset clears the streak (a good build is the user's fix) *)
  let t = C.create ~window:10.0 ~max:2 () in
  ignore (C.record t ~now:0.0 ());
  C.reset t;
  check "after reset, next crash retries (not give up)" (is_retry (C.record t ~now:0.1 ()));

  (* flat backoff (port busy) is exactly 1s *)
  let t = C.create () in
  check "flat backoff is 1.0s" (C.record t ~now:0.0 ~flat:true () = C.Retry 1.0);

  if !fails = 0 then print_endline "all Crash_limiter tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
