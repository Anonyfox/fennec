(* The reactions ppx, behaviourally: a [@workflow] function is called like a NORMAL function (no
   W.make / W.call), [@after]/[@before] attach as typed reactions (the after fires post-commit with the
   result; the before vetoes), and [@cron]/[@every] register scheduled jobs. The compile-time
   circuit-breaker (a cyclic chain is a build error) is verified by a negative build, not here. *)

module S = Fennec_pulse_workflow.Schedule

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)
let log = ref []

(* a workflow is just a function. The transaction + reactions are transparent. *)
let[@workflow] place name = String.uppercase_ascii name
let[@after place] receipt (name : string) = log := ("receipt:" ^ name) :: !log
let[@before place] guard (name : string) = if name = "" then failwith "empty name"

(* @cron / @every type-force a unit -> unit job *)
let[@cron "*/5 * * * *"] cleanup () = ()
let[@every 60.] heartbeat () = ()

let () =
  let r = place "ada" in
  (* called exactly like a normal function — the result is the body's result *)
  check "[@workflow] is called like a plain function" (r = "ADA");
  check "@after desugared and fired with the RESULT (not the input)" (!log = [ "receipt:ADA" ]);
  (try ignore (place "") with Failure _ -> ());
  check "@before guard vetoed (suppressed the after)" (!log = [ "receipt:ADA" ]);
  check "@cron + @every registered two scheduled jobs" (List.length (S.job_names ()) = 2);
  Printf.printf "all reactions-ppx tests passed\n%!"
