(* The pure line classifier, pinned against REAL `dune build --watch` output captured
   from dune 3.23 (see the grammar in dune_watch.mli). If a future dune changes the
   wording, these fail loudly instead of the dev loop silently going deaf. *)

module W = Fennec_dev.Dune_watch

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)

let () =
  print_endline "Dune_watch.classify_line:";
  check "initial/edit success -> Settled Ok"
    (W.classify_line "Success, waiting for filesystem changes..." = W.Settled W.Ok);
  check "one error -> Settled (Errors 1)"
    (W.classify_line "Had 1 error, waiting for filesystem changes..." = W.Settled (W.Errors 1));
  check "many errors -> Settled (Errors 12)"
    (W.classify_line "Had 12 errors, waiting for filesystem changes..." = W.Settled (W.Errors 12));
  check "warning-as-error counts as a failed settle"
    (W.classify_line "Had 1 error, waiting for filesystem changes..." = W.Settled (W.Errors 1));
  check "single-file trigger"
    (W.classify_line "********** NEW BUILD (main.ml changed) **********" = W.Trigger "main.ml changed");
  check "multi-file trigger"
    (W.classify_line "********** NEW BUILD (helper.ml changed, and 1 more change) **********"
    = W.Trigger "helper.ml changed, and 1 more change");
  check "diagnostic location line -> Other"
    (W.classify_line {|File "main.ml", line 1, characters 19-31:|} = W.Other);
  check "Error message line -> Other" (W.classify_line "Error: Syntax error: operator expected." = W.Other);
  check "source excerpt -> Other" (W.classify_line "1 | let () = print_int \"x\"" = W.Other);
  check "blank line -> Other" (W.classify_line "" = W.Other);
  (* robustness: a 'waiting' line wins even if oddly formatted; never raises *)
  check "status detection is substring-based, not anchored"
    (match W.classify_line "  Success, waiting for filesystem changes... " with W.Settled W.Ok -> true | _ -> false)

let () =
  if !fails = 0 then print_endline "all Dune_watch tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
