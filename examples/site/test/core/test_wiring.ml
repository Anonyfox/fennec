(* The wiring manifest: maintenance.ml is a PURE [@every] that NOTHING in the app calls — without the
   manifest its module would never link and its job would never register. Referencing the generated
   [Wiring.link] once force-links every workflow module, so the job registers at boot. We check the
   Schedule registry (registration, not execution). *)

let () = Unix.putenv "MONGO_URL" ":memory:"

module Schedule = Fennec_pulse_workflow.Schedule

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let () =
  Wiring.link () (* the ONE reference — force-links every workflow module so each registers at boot *);
  let names = Schedule.job_names () in
  check "the manifest force-linked the pure @every nothing calls (maintenance.sweep_closed)"
    (List.mem "sweep_closed" names);
  check "and the @cron in the referenced ticket module too (tickets.auto_close_stale)"
    (List.mem "auto_close_stale" names);
  Printf.printf "all wiring-manifest tests passed\n%!"
