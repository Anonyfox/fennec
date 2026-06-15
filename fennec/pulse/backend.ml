(* The storage seam now lives in the mongo package ({!Fennec_mongo_backend}); this is a thin re-export
   so the reactive layer keeps consuming it as [Fennec_pulse.Backend] unchanged. The [.mli] constrains
   it to exactly the seam interface. *)
include Fennec_mongo_backend
