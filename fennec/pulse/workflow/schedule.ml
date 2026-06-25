(* Schedule — recurring jobs ([@cron] / [@every]) that run AT-MOST-ONCE across horizontally-scaled
   replicas. On each due tick a replica tries to CLAIM (job, slot) with a unique insert into the
   internal [_fennec_cron] collection; the unique (job, slot) index means only one replica's insert
   wins, so the job body runs once per slot cluster-wide. It is lock-free (no lease to renew, so no
   deadlock and no orphaned lock if a replica dies mid-run), self-pruning (old claims are removed by
   timestamp), and degrades to "always succeeds" on a single server. Times are UTC so replicas in
   different zones agree on slot boundaries.

   The cron matcher and the [tick] pass are pure/deterministic (time is an argument) so they unit-test
   without the fiber; [start] forks the resident loop into the server's switch at boot. The reactions
   ppx emits {!every} / {!cron} from [@every] / [@cron] annotations. *)

module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

type schedule =
  | Every of float (* seconds between runs *)
  | Cron of (Unix.tm -> bool) (* matches a broken-down UTC time *)

type job = {
  jname : string;
  sched : schedule;
  body : unit -> unit;
  mutable last_slot : int option; (* local memo: the last slot this replica acted on *)
}

let jobs : job list ref = ref []

(* clear the registry (tests; a future hot-reload) *)
let reset () = jobs := []

(* the names of all registered jobs (the function names the [@cron]/[@every] ppx used) — for diagnostics
   and for proving the wiring manifest force-linked a workflow that nothing else references *)
let job_names () = List.map (fun (j : job) -> j.jname) !jobs

(* ---- cron parsing (5 fields: minute hour day-of-month month day-of-week, UTC) ----------------- *)

(* one comma-separated field → a membership predicate over its value range. Supports [*], [a], [a-b],
   [*/n], [a-b/n] and comma lists of those. *)
let cron_field ~lo ~hi spec : int -> bool =
  let one part =
    let base, step =
      match String.split_on_char '/' part with
      | [ b ] -> (b, 1)
      | [ b; s ] -> (b, int_of_string s)
      | _ -> invalid_arg ("cron: bad step in " ^ part)
    in
    let a, b =
      if base = "*" then (lo, hi)
      else
        match String.split_on_char '-' base with
        | [ x ] -> let v = int_of_string x in (v, v)
        | [ x; y ] -> (int_of_string x, int_of_string y)
        | _ -> invalid_arg ("cron: bad range in " ^ part)
    in
    let step = max 1 step in
    fun v -> v >= a && v <= b && (v - a) mod step = 0
  in
  let preds = String.split_on_char ',' spec |> List.map one in
  fun v -> List.exists (fun p -> p v) preds

let cron_matcher (expr : string) : Unix.tm -> bool =
  match List.filter (fun s -> s <> "") (String.split_on_char ' ' expr) with
  | [ mi; ho; dom; mo; dow ] ->
      let fmin = cron_field ~lo:0 ~hi:59 mi in
      let fhour = cron_field ~lo:0 ~hi:23 ho in
      let fdom = cron_field ~lo:1 ~hi:31 dom in
      let fmonth = cron_field ~lo:1 ~hi:12 mo in
      let fdow = cron_field ~lo:0 ~hi:6 dow in
      let dom_restricted = dom <> "*" and dow_restricted = dow <> "*" in
      fun tm ->
        (* standard crontab quirk: when BOTH day-of-month and day-of-week are restricted, the day
           matches if EITHER does; otherwise both must match. *)
        let day_ok =
          if dom_restricted && dow_restricted then fdom tm.Unix.tm_mday || fdow tm.Unix.tm_wday
          else fdom tm.Unix.tm_mday && fdow tm.Unix.tm_wday
        in
        fmin tm.Unix.tm_min && fhour tm.Unix.tm_hour && fmonth (tm.Unix.tm_mon + 1) && day_ok
  | _ -> invalid_arg ("cron: expected 5 space-separated fields, got: " ^ expr)

(* exposed for testing: does [expr] fire at unix time [t] (interpreted UTC)? *)
let cron_matches expr t = (cron_matcher expr) (Unix.gmtime t)

(* ---- registration (the ppx emits these from [@every] / [@cron]) ------------------------------- *)

let every seconds ~name body =
  jobs := !jobs @ [ { jname = name; sched = Every seconds; body; last_slot = None } ]

let cron expr ~name body =
  let m = cron_matcher expr in
  jobs := !jobs @ [ { jname = name; sched = Cron m; body; last_slot = None } ]

(* the slot for a job at UTC time [now]: [Every] buckets by duration; [Cron] uses the unix-minute
   when the expression matches, else [None] (not due this minute). *)
let slot_of job now =
  match job.sched with
  | Every dur -> Some (int_of_float (now /. dur))
  | Cron m -> if m (Unix.gmtime now) then Some (int_of_float (now /. 60.)) else None

(* ---- the at-most-once claim -------------------------------------------------------------------- *)

let claim_coll_name = "_fennec_cron"

type claim = Claimed | Taken | Failed

(* try to claim (job, slot) by a unique insert; the unique (job, slot) index rejects a second
   replica's attempt. A FRESH _id per attempt is required — minimongo overwrites a same-_id insert
   rather than colliding, so the (job, slot) index is what enforces the claim, uniformly across
   backends. *)
let claim_in coll ~job ~slot ~at : claim =
  try
    ignore (D.insert coll (B.doc [ ("job", B.str job); ("slot", B.int slot); ("at", B.int at) ]));
    Claimed
  with
  | Minimongo.Unique_violation _ -> Taken
  | _ -> Failed (* native/burrow duplicate-key or a transient error — treat as not-claimed, retry *)

let claim_index_ready = ref false
let cron_coll () = D.collection ~name:claim_coll_name ()

let ensure_claim_index coll =
  if not !claim_index_ready then begin
    (try
       D.ensure_index coll ~name:"uniq_job_slot"
         ~keys:(B.doc [ ("job", B.int 1); ("slot", B.int 1) ])
         ~unique:true ~sparse:false
     with _ -> ());
    claim_index_ready := true
  end

(* prune claims older than the retention window so [_fennec_cron] stays bounded *)
let retain_seconds = 3600.

let prune coll ~now =
  try ignore (D.remove coll (B.doc [ ("at", B.doc [ ("$lt", B.int (int_of_float (now -. retain_seconds))) ]) ]))
  with _ -> ()

(* a single due-check pass at [now] over [coll] — the scheduler loop body, factored out so tests can
   drive it deterministically. A claimed job runs in its own transaction, isolated. *)
let tick coll ~now =
  ensure_claim_index coll;
  List.iter
    (fun job ->
      match slot_of job now with
      | None -> ()
      | Some s ->
          if job.last_slot <> Some s then (
            match claim_in coll ~job:job.jname ~slot:s ~at:(int_of_float now) with
            | Claimed ->
                job.last_slot <- Some s;
                (try Fennec_mongo_dynamic.Tx.run job.body
                 with e -> Printf.eprintf "fennec: scheduled job %s raised: %s\n%!" job.jname (Printexc.to_string e))
            | Taken -> job.last_slot <- Some s (* another replica owns this slot *)
            | Failed -> () (* transient — retry next tick *)))
    !jobs

(* ---- the resident scheduler fiber ------------------------------------------------------------- *)

let tick_interval = 1.0
let started = ref false

(* fork the scheduler into the server's switch at boot. A no-op when there are no scheduled jobs, so
   apps without [@cron]/[@every] pay nothing. *)
let start ~sw ~clock =
  if (not !started) && !jobs <> [] then begin
    started := true;
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop last_prune =
          let now = Eio.Time.now clock in
          (try tick (cron_coll ()) ~now with _ -> ());
          let last_prune =
            if now -. last_prune > retain_seconds then (prune (cron_coll ()) ~now; now) else last_prune
          in
          Eio.Time.sleep clock tick_interval;
          loop last_prune
        in
        loop (Eio.Time.now clock))
  end
