(* The standalone Sift ppx driver — for libraries that DON'T use the fur/MLX ppx. Referencing the
   shared rules module forces its [@@deriving sift] / [@@deriving model] derivers to register into this
   ppx's single transformation. *)
let () =
  ignore Sift_ppx_rules.deriver_sift;
  ignore Sift_ppx_rules.deriver_model
