(* ════════════════════════════════════════════════════════════════════════════════════════
   ocamlmerlin-fennec-mlx — the executable Merlin invokes as the .mlx dialect reader.

   The dune `(dialect … (merlin_reader fennec-mlx))` makes Merlin spawn `ocamlmerlin-fennec-mlx` (the
   public_name of this executable) as the external reader for .mlx buffers in the editor. All the
   logic lives in [Fennec_mlx_reader], which is a dune `(select …)` over the optional editor-only
   `merlin-extend` library: the REAL delegating reader when merlin-extend is present, a build-only stub
   when it is absent (so the BUILD never depends on merlin). This entry is deliberately trivial. *)

let () = Fennec_mlx_reader.main ()
