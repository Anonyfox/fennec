(* The merlin-extend-ABSENT branch of `(select reader_delegation_impl.ml …)`. Without merlin-extend we
   cannot speak the external-reader protocol to drive the readers, so the proof simply SKIPS — keeping
   `dune runtest` green in an environment that has no merlin. (The real branch runs the full proof.) *)

let run (_fennec_reader_exe : string) =
  print_endline "reader_delegation: merlin-extend absent — SKIPPED (build does not depend on merlin)"
