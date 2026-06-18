(* The standalone sift.mongo ppx — for libraries that DON'T use the fur/MLX ppx. Folds the query DSL
   ([%q] [%fields] [%sort] [%set] [%index]) PLUS the [@@deriving sift] codec deriver (a sift.mongo user
   models with both) into this ppx's single transformation. *)
let () = Ppxlib.Driver.register_transformation "sift_mongo" ~rules:Sift_mongo_ppx_rules.rules
let () = ignore Sift_ppx_rules.deriver_sift
