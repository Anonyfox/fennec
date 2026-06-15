(* The backend implementations + the Dynamic selector + the mongosh wire endpoint now live in the mongo
   package ({!Fennec_mongo_backend}); this is a thin re-export so existing consumers keep using
   [Fennec_pulse_mongo] unchanged. The [.mli] constrains it to the documented surface. *)
include Fennec_mongo_backend
