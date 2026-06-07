(* The System-suite entry point — runs every registered [let%system] scenario and exits with the
   right code. This whole file is the runner; the scenarios live in the sibling [*_test.ml]. You
   never edit this. *)
let () = exit (Fennec_hunt.System.run ())
