(* The Browser-suite entry point — runs every registered [let%browser] test (fresh isolated page
   each) via the full runner: base_url from FENNEC_TEST_URL, flags (--headed/--grep/--jobs/
   --screenshots/--only-file/…) from argv. This whole file is the runner; the tests live in the
   sibling [*_test.ml]. You never edit this. *)
let () = Fennec_hunt.Run.main_cli ()
