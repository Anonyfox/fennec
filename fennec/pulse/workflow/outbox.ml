(* Outbox — the transactional effects outbox: exactly-once delivery of fire-and-forget effects (mail,
   webhooks, analytics — anything that ESCAPES the system). Userland never sees it; an effect verb
   (e.g. [Mail.send]) calls {!enqueue}, and the rest is invisible machinery.

   How exactly-once holds:
   - **Enqueue is tx-aware.** {!enqueue} is a plain [Dynamic.insert] into [_fennec_outbox], so inside a
     workflow it joins the transaction — the intent commits or ROLLS BACK with the workflow (a raised
     workflow sends nothing). Outside one (a post-commit reaction) it is a direct durable insert.
   - **A resident worker CLAIMS then delivers.** Each tick it reaps stale claims, then for every pending
     intent it can CLAIM (a guarded [pending -> sending] update — an atomic test-and-set, so exactly one
     replica wins), runs the registered handler, and DELETES on success. A handler raise reverts the
     intent to [pending] (+ an attempt) to retry next tick.
   - **The intent id is the idempotency key.** The worker hands the handler the intent's [_id]; an
     external system dedups on it. So a crash AFTER delivery but BEFORE the delete (the intent is reaped
     back to pending and redelivered) is a no-op there: exactly-once-effect = at-least-once-delivery +
     idempotent-handler. There is no true exactly-once delivery and we don't pretend otherwise. *)

module D = Fennec_mongo_dynamic.Dynamic
module B = Bson

let coll_name = "_fennec_outbox"

(* registered deliverers by kind. The handler gets the intent id (the dedup key) + the payload; a raise
   is a delivery failure (retried). *)
let handlers : (string, id:string -> B.t -> unit) Hashtbl.t = Hashtbl.create 8
let register ~kind handler = Hashtbl.replace handlers kind handler
let reset () = Hashtbl.clear handlers

let id_to_string = function B.Object_id h -> h | B.String s -> s | other -> B.to_string other
let now_s () = int_of_float (Unix.time ())

(* enqueue a fire-and-forget effect. Tx-aware (see the header): atomic with a workflow's writes inside
   one, a direct durable insert outside. Returns nothing — the effect is owed, not awaited. *)
let enqueue ~kind ~(payload : B.t) : unit =
  let coll = D.collection ~name:coll_name () in
  ignore
    (D.insert coll
       (B.doc
          [ ("kind", B.str kind); ("payload", payload); ("status", B.str "pending"); ("attempts", B.int 0);
            ("at", B.int (now_s ())) ]))

let lease_seconds = 60
let max_attempts = 12 (* give up on a poison effect after this many failures (dead-letter to the log) *)

(* reclaim intents stuck "sending" past the lease (a worker died mid-delivery) — back to pending; the
   handler's idempotency covers the possible double-send. *)
let reap coll ~now =
  try
    ignore
      (D.update coll ~multi:true ~upsert:false
         (B.doc [ ("status", B.str "sending"); ("leased", B.doc [ ("$lt", B.int (now - lease_seconds)) ]) ])
         (B.doc [ ("$set", B.doc [ ("status", B.str "pending") ]) ]))
  with _ -> ()

let pending_query =
  Fennec_mongo_backend.query ~selector:(B.doc [ ("status", B.str "pending") ]) ()

(* one delivery pass over [coll]: reap, then deliver every pending intent we can claim. Factored out so
   tests drive it deterministically (the resident loop just calls it on a timer). *)
let tick coll ~now =
  reap coll ~now;
  List.iter
    (fun intent ->
      let kind = Option.value ~default:"" (B.get_string intent "kind") in
      match Hashtbl.find_opt handlers kind with
      | None -> () (* no handler registered for this kind yet — leave it pending *)
      | Some h -> (
        match B.get intent "_id" with
        | None -> ()
        | Some idv ->
          let sel = B.doc [ ("_id", idv); ("status", B.str "pending") ] in
          (* CLAIM: pending -> sending (atomic test-and-set; one replica wins) *)
          let claimed =
            try D.update coll ~multi:false ~upsert:false sel (B.doc [ ("$set", B.doc [ ("status", B.str "sending"); ("leased", B.int now) ]) ]) = 1
            with _ -> false
          in
          if claimed then
            let id = id_to_string idv in
            let payload = Option.value ~default:(B.Document []) (B.get intent "payload") in
            (try
               h ~id payload;
               (* delivered: drop the intent *)
               ignore (D.remove coll (B.doc [ ("_id", idv) ]))
             with exn ->
               let attempts = Option.value ~default:0 (B.get_int intent "attempts") + 1 in
               if attempts >= max_attempts then (
                 (* poison: give up — dead-letter to the log and drop it, so it can't spin forever *)
                 Printf.eprintf "fennec: outbox effect %s (kind %s) gave up after %d attempts: %s\n%!" id kind attempts
                   (Printexc.to_string exn);
                 try ignore (D.remove coll (B.doc [ ("_id", idv) ])) with _ -> ())
               else
                 (* failed: back to pending (+ an attempt), retried next tick *)
                 try
                   ignore
                     (D.update coll ~multi:false ~upsert:false (B.doc [ ("_id", idv) ])
                        (B.doc [ ("$set", B.doc [ ("status", B.str "pending") ]); ("$inc", B.doc [ ("attempts", B.int 1) ]) ]))
                 with _ -> ())))
    (D.find coll pending_query)

(* ---- the resident worker fiber ---------------------------------------------------------------- *)

let tick_interval = 1.0
let started = ref false
let outbox_coll () = D.collection ~name:coll_name ()

(* fork the delivery worker into the server's switch at boot. A no-op when no effect handler is
   registered, so an app with no fire-and-forget effects pays nothing — exactly like the scheduler. *)
let start ~sw ~clock =
  if (not !started) && Hashtbl.length handlers > 0 then begin
    started := true;
    Eio.Fiber.fork ~sw (fun () ->
        let rec loop () =
          let now = int_of_float (Eio.Time.now clock) in
          (try tick (outbox_coll ()) ~now with _ -> ());
          Eio.Time.sleep clock tick_interval;
          loop ()
        in
        loop ())
  end
