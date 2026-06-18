(* Standalone driver: the [@@deriving collection] deriver (auto-registers on load — [ignore deriver]
   forces it, and transitively the [sift]/[model] derivers it reuses) PLUS sift.mongo's query DSL
   ([%q]/[%fields]/[%sort]/[%set]/[%index]). One transformation, one ppx process. *)
open Ppxlib

let () = ignore Fennec_pulse_collection_ppx_rules.deriver
let () = Driver.register_transformation "fennec_collection" ~rules:Sift_mongo_ppx_rules.rules
