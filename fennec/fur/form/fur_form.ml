(* A collision-free alias of {!Form} for callers that already define their OWN top-level [Form] — chiefly
   [Fennec_web.Form], which re-exports the client primitive ([signal]/[input_attrs]/[field_attrs]) under
   its own [Form] name and so cannot name the global [Form] from inside itself. Components reference the
   bare [Form]; that module references [Fur_form]. Same module, two names. *)
include Form
