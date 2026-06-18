(* The standalone Sift ppx driver — the whole DX in one pass, for libraries that DON'T use the fur/MLX
   ppx. Force-loads the [@@deriving model] and [@@deriving collection] derivers (they auto-register on
   load) and registers the Mongo query DSL ([%q]/[%fields]/[%sort]/[%set]/[%index]). Libraries on
   (pps fennec.fur.ppx) get all of this for free, so they don't list this. *)
open Ppxlib

let () = ignore Sift_ppx_rules.deriver_model
let () = ignore Collection_deriver.deriver
let () = Driver.register_transformation "sift" ~rules:Sift_mongo_ppx_rules.rules
