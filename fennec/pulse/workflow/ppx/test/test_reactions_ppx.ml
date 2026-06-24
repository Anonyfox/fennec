(* The reactions ppx, behaviourally: [@after]/[@before] desugar to Workflow registrations (calling the
   workflow fires the after-hook; the before-guard vetoes), and [@cron]/[@every] register scheduled
   jobs. (The compile-time circuit-breaker — a cyclic chain is a type/compile error — is verified by a
   negative build, not here.) *)

module W = Fennec_pulse_workflow.Workflow
module S = Fennec_pulse_workflow.Schedule

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let log = ref []

let place = W.make "place" (fun name -> String.uppercase_ascii name)
let[@after place] receipt (name : string) = log := ("receipt:" ^ name) :: !log
let[@before place] guard (name : string) = if name = "" then failwith "empty name"

(* @cron / @every type-force a unit -> unit job (the emitted Schedule.cron … job requires it) *)
let[@cron "*/5 * * * *"] cleanup () = ()
let[@every 60.] heartbeat () = ()

let () =
  let r = W.call place "ada" in
  check "@before passed + body ran" (r = "ADA");
  check "@after desugared and fired with the RESULT (not the input)" (!log = [ "receipt:ADA" ]);
  (try ignore (W.call place "") with Failure _ -> ());
  check "@before veto suppressed the after" (!log = [ "receipt:ADA" ]);
  check "@cron + @every registered two scheduled jobs" (List.length !S.jobs = 2);
  Printf.printf "all reactions-ppx tests passed\n%!"
