(* Standalone test ppx driver — registers the shared rules for let%test / let%test_unit.
   Libraries using the fur ppx (fennec.fur.ppx) get these rules included automatically and
   don't need this driver separately. *)
let () =
  Ppxlib.Driver.register_transformation "fennec_hunt_test"
    ~rules:Fennec_hunt_ppx_rules.rules
