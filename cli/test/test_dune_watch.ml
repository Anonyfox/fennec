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
    (match W.classify_line "  Success, waiting for filesystem changes... " with W.Settled W.Ok -> true | _ -> false);
  (* error count is robust to ANSI colour and to digits appearing before "Had " *)
  check "ANSI-wrapped error count parses the real number"
    (W.classify_line "\027[1;31mHad 3 errors, waiting for filesystem changes...\027[0m" = W.Settled (W.Errors 3));
  check "a digit before 'Had' doesn't corrupt the count"
    (W.classify_line "12:34 Had 3 errors, waiting for filesystem changes..." = W.Settled (W.Errors 3))

(* the real IO assembler over a pipe: line-splitting across reads + the EOF flush *)
let () =
  print_endline "Dune_watch IO assembler (over a pipe):";
  let rd, wr = Unix.pipe () in
  let t = W.of_fd rd in
  let w s = ignore (Unix.write_substring wr s 0 (String.length s)) in
  let rec drain n = if n <= 0 then None else match W.poll t ~timeout:0.2 with Some e -> Some e | None -> drain (n - 1) in
  w "********** NEW BUILD (a.ml changed) **********\n";
  w "Succ";
  check "a settle split across reads yields no event until complete" (W.poll t ~timeout:0.2 = None);
  w "ess, waiting for filesystem changes...\n";
  check "the rejoined line settles as Ok" (match drain 3 with Some (W.Settled_build { outcome = W.Ok; _ }) -> true | _ -> false);
  (* a final settle with NO trailing newline, then EOF, must not be lost as a bare Exited *)
  w "Had 2 errors, waiting for filesystem changes...";
  Unix.close wr;
  check "a newline-less final settle is flushed on EOF" (match drain 4 with Some (W.Settled_build { outcome = W.Errors 2; _ }) -> true | _ -> false);
  Unix.close rd

(* regression: after EOF, poll must return Exited but NOT spin — the supervisor's `newest`
   drains with timeout:0. in a loop, so if poll keeps returning Some Exited on a dead fd,
   `newest` recurses forever → 100% CPU. Verify Exited is returned, then the fd is inert. *)
let () =
  print_endline "Dune_watch EOF behaviour (anti-spin):";
  let rd2, wr2 = Unix.pipe () in
  let t2 = W.of_fd rd2 in
  Unix.close wr2; (* immediate EOF *)
  check "first poll after EOF → Exited" (W.poll t2 ~timeout:0. = Some W.Exited);
  (* the critical property: subsequent polls ALSO return Exited (eof is sticky), but this is
     safe because the supervisor's `newest` now stops draining at Exited. We just verify the
     contract: poll on a dead watcher never blocks and always signals Exited. *)
  check "second poll after EOF → still Exited (eof is sticky)" (W.poll t2 ~timeout:0. = Some W.Exited);
  Unix.close rd2

let () =
  if !fails = 0 then print_endline "all Dune_watch tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
