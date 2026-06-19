(* The select-target signature shared by both branches of `(select fennec_mlx_reader.ml …)`:
   - fennec_mlx_reader.real.ml   — the real delegating reader (compiled when merlin-extend is present)
   - fennec_mlx_reader.stub.ml   — a no-op stub      (compiled when merlin-extend is absent)
   Both expose exactly [main], the executable entry. *)

(** Run the fennec `.mlx` merlin reader: speak the merlin external-reader protocol on stdin/stdout,
    running the fennec pre-pass over each buffer and delegating to the stock `ocamlmerlin-mlx` reader.
    The stub branch instead exits immediately (it has no merlin-extend to speak the protocol with). *)
val main : unit -> unit
