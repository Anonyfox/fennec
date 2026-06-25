(* maintenance.ml — a PURE scheduled workflow: NOTHING in the app calls it. Without the wiring manifest
   its module would never be linked, so its [@every] would silently never register and the job would
   never run. The generated [Wiring.link] (referenced once from the server) force-links it, so the job
   registers at boot like any other. This file exists to prove that path. *)

open Ticket

(* every 6 hours: a housekeeping sweep (illustrative — just reads here). The [unit -> unit] shape is
   forced by [@every]. *)
let[@every 21600.] sweep_closed () = ignore (count [%q status = "closed"])
