(* The Http-suite entry point — runs every registered [let%http] block and exits with the right
   code. This whole file is the runner; the suites live in the sibling [*_test.ml]. You never edit
   this. *)
let () = exit (Fennec_hunt.Http.run ())
