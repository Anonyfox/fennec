(* Select-target signature for the reader proof, shared by both branches of
   `(select reader_delegation_impl.ml …)`:
   - reader_delegation_impl.real.ml — drives the merlin protocol (compiled when merlin-extend present)
   - reader_delegation_impl.stub.ml — prints SKIP            (compiled when merlin-extend absent) *)

(** [run fennec_reader_exe] proves the fennec merlin reader against the stock one.
    [fennec_reader_exe] is the path to the built `ocamlmerlin-fennec-mlx`. Exits non-zero on a failed
    assertion; exits 0 (SKIP) when the stock `ocamlmerlin-mlx` is not on PATH or merlin-extend is
    absent. *)
val run : string -> unit
