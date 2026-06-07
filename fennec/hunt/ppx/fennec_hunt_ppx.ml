(* Standalone test ppx driver — registers the shared rules for let%test / let%test_unit + the
   doctest pass (executable {@ocaml[ ]} doc-comment blocks → inline tests). Libraries using the fur
   ppx (fennec.fur.ppx) get all of these included automatically and don't need this driver too. *)
let () =
  Ppxlib.Driver.register_transformation "fennec_hunt_test"
    ~rules:Fennec_hunt_ppx_rules.rules
    ~impl:Fennec_hunt_ppx_rules.expand_doctests
