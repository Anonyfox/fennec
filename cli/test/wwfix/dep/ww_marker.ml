(* Loaded as the first link in the warm-worker cross-file-chain proof; [tag] is called by the leaf
   Widget module, so if this .cma weren't Dynlinked first the leaf's load would fail. *)
let tag = "WW:marker"
