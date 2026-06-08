(* `--mongo`: launch a managed single-node MongoDB replica set for the dev/test session and point the
   app at it via MONGO_URL. A replica set (not a standalone mongod) because change streams — the
   reactive observe primitive — require one.

   Two steps, matching comet's design: the process is spawned + reaped by the pure-Unix Mongod
   lifecycle (no-dangling), then the set is INITIATED and brought to PRIMARY via the driver
   (Server.start in reuse mode adopts the already-running mongod — proc=None — so the short Eio
   context that initiates it never owns or kills the process). An absent/failed mongod degrades to
   the in-memory backend (MONGO_URL stays unset) rather than breaking the run. *)

module Mongod = Fennec_mongo_mongod.Mongod
module Server = Fennec_mongo_driver.Server

let replset = "rs0"

(* Launch + initiate the set, export MONGO_URL, and return the managed process (the caller tracks /
   reaps it). [None] if no mongod is installed or the launch failed — the app then runs in memory. *)
let launch () : Mongod.t option =
  match Mongod.find () with
  | None ->
    Printf.eprintf "--mongo: no mongod found — using the in-memory backend.\n%s\n%!" (Mongod.install_hint ());
    None
  | Some _ -> (
    try
      let t = Mongod.start ~replset () in
      (* initiate + wait for PRIMARY via the driver; stop the process if that fails so it never dangles *)
      (try Eio_main.run (fun env -> Eio.Switch.run (fun sw -> ignore (Server.start ~env ~sw ~reuse:true ~replset ~port:(Mongod.port t) ())))
       with e -> (try Mongod.stop t with _ -> ()); raise e);
      Unix.putenv "MONGO_URL" (Printf.sprintf "mongodb://127.0.0.1:%d/?directConnection=true" (Mongod.port t));
      Printf.eprintf "--mongo: managed replica-set mongod at 127.0.0.1:%d\n%!" (Mongod.port t);
      Some t
    with e ->
      Printf.eprintf "--mongo: could not launch mongod (%s) — using the in-memory backend.\n%!" (Printexc.to_string e);
      None)
