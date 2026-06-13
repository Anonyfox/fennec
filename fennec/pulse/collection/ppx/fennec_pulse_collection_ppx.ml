(* Standalone driver: fold the shared collection rules into this ppx's single transformation. The
   deriver auto-registers when the rules module loads (referencing [rules] forces it). *)
open Ppxlib

let () =
  Driver.register_transformation "fennec_collection" ~rules:Fennec_pulse_collection_ppx_rules.rules
