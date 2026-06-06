(* The child reaper — kill_all really kills tracked children; untracked ones survive. Uses real
   /bin/sleep processes and blocking waitpid, so it's deterministic (no timing sleeps). *)

module R = Fennec_testcmd.Reaper

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let spawn_sleep () = Unix.create_process "/bin/sleep" [| "/bin/sleep"; "30" |] Unix.stdin Unix.stdout Unix.stderr
let killed pid = match Unix.waitpid [] pid with _, Unix.WSIGNALED s -> s = Sys.sigkill | _ -> false
let alive pid = match Unix.waitpid [ Unix.WNOHANG ] pid with 0, _ -> true | _ -> false | exception _ -> false

let () =
  (* tracked children are SIGKILLed by kill_all *)
  let p1 = spawn_sleep () and p2 = spawn_sleep () in
  R.track p1; R.track p2;
  R.kill_all ();
  check "kill_all SIGKILLs tracked p1" (killed p1);
  check "kill_all SIGKILLs tracked p2" (killed p2);
  R.untrack p1; R.untrack p2;

  (* an untracked child is left alone *)
  let p3 = spawn_sleep () in
  R.track p3; R.untrack p3;
  R.kill_all ();
  check "untracked pid survives kill_all" (alive p3);
  (try Unix.kill p3 Sys.sigkill with _ -> ()); (try ignore (Unix.waitpid [] p3) with _ -> ());

  (* kill_all on an empty registry is a no-op (no raise) *)
  check "kill_all on empty registry is safe" (try R.kill_all (); true with _ -> false);

  if !fails = 0 then print_endline "all Reaper tests passed." else (Printf.printf "%d FAILED\n" !fails; exit 1)
