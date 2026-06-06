(* The bounded-concurrency map: results correct + in input order, the [jobs] bound respected,
   and exceptions surfaced (not swallowed). *)

module P = Fennec_testcmd.Pool

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  (* correctness + ordering, serial and parallel *)
  check "empty" (P.map ~jobs:4 (fun x -> x) [] = []);
  check "singleton runs inline" (P.map ~jobs:4 (fun x -> x * 2) [ 21 ] = [ 42 ]);
  check "jobs=1 sequential, in order" (P.map ~jobs:1 (fun x -> x * 2) [ 1; 2; 3 ] = [ 2; 4; 6 ]);
  check "jobs>1 preserves input order" (P.map ~jobs:4 (fun x -> x + 100) [ 5; 4; 3; 2; 1 ] = [ 105; 104; 103; 102; 101 ]);
  check "more items than jobs, all mapped, in order"
    (P.map ~jobs:3 (fun x -> x * x) [ 1; 2; 3; 4; 5; 6; 7 ] = [ 1; 4; 9; 16; 25; 36; 49 ]);

  (* the bound: with jobs=3 over 12 delayed tasks, peak concurrency is real (>=2) but never
     exceeds the bound (<=3) — tracked under a mutex *)
  let m = Mutex.create () in
  let cur = ref 0 and peak = ref 0 in
  let task x =
    Mutex.lock m; incr cur; if !cur > !peak then peak := !cur; Mutex.unlock m;
    Thread.delay 0.02;
    Mutex.lock m; decr cur; Mutex.unlock m;
    x
  in
  let out = P.map ~jobs:3 task (List.init 12 (fun i -> i)) in
  check "bound: result correct under concurrency" (out = List.init 12 (fun i -> i));
  check "bound: peak concurrency within [2,3]" (!peak >= 2 && !peak <= 3);
  check "bound: all threads drained (cur back to 0)" (!cur = 0);

  (* exceptions are propagated, not swallowed *)
  check "exception surfaces"
    (try ignore (P.map ~jobs:2 (fun x -> if x = 3 then failwith "boom" else x) [ 1; 2; 3; 4 ]); false
     with Failure m -> m = "boom" | _ -> false);

  if !fails = 0 then print_endline "all Pool tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
