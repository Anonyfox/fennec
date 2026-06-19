(* Entry for the reader-delegation proof. The reader-under-test (ocamlmerlin-fennec-mlx) path is passed
   as argv.(1) by the runtest rule (a dune dep). All logic is in [Reader_delegation_impl], a
   `(select)` over the editor-only merlin-extend library (real proof vs. SKIP stub). *)

let () =
  let reader_exe = if Array.length Sys.argv >= 2 then Sys.argv.(1) else "" in
  Reader_delegation_impl.run reader_exe
