(* Schedule: the cron matcher (UTC), the at-most-once unique-insert claim, and the deterministic
   tick (every-jobs run once per duration slot; cron-jobs once per matching minute). *)

module S = Fennec_pulse_workflow.Schedule
module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

let check msg c = if c then Printf.printf "  ok: %s\n%!" msg else (Printf.printf "  FAIL: %s\n%!" msg; exit 1)

let uniq coll =
  D.ensure_index coll ~name:"uniq_job_slot"
    ~keys:(B.doc [ ("job", B.int 1); ("slot", B.int 1) ])
    ~unique:true ~sparse:false

let () =
  (* cron matcher (unix seconds interpreted UTC; 600s = 1970-01-01 00:10:00) *)
  check "*/5 matches 00:10" (S.cron_matches "*/5 * * * *" 600.);
  check "*/5 rejects 00:07" (not (S.cron_matches "*/5 * * * *" 420.));
  check "explicit minute 30" (S.cron_matches "30 * * * *" 1800.);
  check "list 0,30 matches 00:30" (S.cron_matches "0,30 * * * *" 1800.);
  check "range 0-5 matches 00:03" (S.cron_matches "0-5 * * * *" 180.);
  check "range 0-5 rejects 00:07" (not (S.cron_matches "0-5 * * * *" 420.));
  check "hour field: 0 0 matches midnight" (S.cron_matches "0 0 * * *" 0.);
  check "hour field: 0 0 rejects 00:10" (not (S.cron_matches "0 0 * * *" 600.));

  (* the at-most-once claim: first wins, a second attempt at the same (job, slot) is Taken *)
  let coll = D.mem (Minimongo.create ()) in
  uniq coll;
  check "first claim wins" (S.claim_in coll ~job:"j" ~slot:1 ~at:0 = S.Claimed);
  check "second claim of the same slot is taken" (S.claim_in coll ~job:"j" ~slot:1 ~at:0 = S.Taken);
  check "claim of the next slot wins" (S.claim_in coll ~job:"j" ~slot:2 ~at:0 = S.Claimed);
  check "claim of another job at the same slot wins" (S.claim_in coll ~job:"k" ~slot:1 ~at:0 = S.Claimed);

  (* every-job: runs once per duration slot (the local memo prevents re-run within a slot) *)
  S.reset ();
  let ran = ref 0 in
  S.every 10. ~name:"j" (fun () -> incr ran);
  let coll = D.mem (Minimongo.create ()) in
  uniq coll;
  S.tick coll ~now:5.;
  S.tick coll ~now:6.;
  check "every ran once in slot 0" (!ran = 1);
  S.tick coll ~now:15.;
  check "every ran again in slot 1" (!ran = 2);

  (* cron-job: runs once per matching minute, never on a non-matching minute *)
  S.reset ();
  let ran = ref 0 in
  S.cron "*/5 * * * *" ~name:"c" (fun () -> incr ran);
  let coll = D.mem (Minimongo.create ()) in
  uniq coll;
  S.tick coll ~now:600.;
  S.tick coll ~now:620.;
  check "cron ran once at the matching minute" (!ran = 1);
  S.tick coll ~now:660.;
  check "cron did not run at a non-matching minute" (!ran = 1);
  S.tick coll ~now:900.;
  check "cron ran at the next matching minute" (!ran = 2);

  Printf.printf "all schedule tests passed\n%!"
