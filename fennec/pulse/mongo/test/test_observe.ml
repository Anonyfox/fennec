(* observe_changes over a REAL mongod via CHANGE STREAMS: a live insert/update/remove must turn into
   added/changed/removed deltas — the realtime seam (the thing the differential CRUD test does NOT
   cover). Change streams require a replica set, so this runs against a hermetic single-node set
   (Server.with_ephemeral). Skips (passes) where the native driver wasn't built or no mongod exists.

   The observe daemon + the mutating fiber are cooperatively scheduled in one Eio domain, so the
   shared event lists are race-free; [settle] gives the change stream a moment to deliver. *)

module Mongo = Fennec_pulse_mongo
module Backend = Fennec_pulse.Backend
module Server = Fennec_mongo_driver.Server
module B = Bson

let%test "observe_changes over real mongo (change streams): live insert/update/remove become added/changed/removed" =
  if not (Mongo.available ()) then true (* native driver not built — skip *)
  else
    match Fennec_mongo_mongod.Mongod.find () with
    | None -> true (* no mongod installed — skip *)
    | Some _ ->
        Eio_main.run @@ fun env ->
        (* a throwaway single-node replica set so $changeStream is supported *)
        Server.with_ephemeral ~env @@ fun srv ->
        Eio.Switch.run @@ fun sw ->
        let clock = Eio.Stdenv.clock env in
        let conn = Mongo.connect (Server.uri_of srv) in
        let c = Mongo.collection ~sw conn ~db:"obs" ~name:"c" in
        let added = ref [] and changed = ref [] and removed = ref [] in
        let h =
          Mongo.observe_changes c (Backend.query ())
            ~added:(fun id _ -> added := id :: !added)
            ~changed:(fun id _ _ -> changed := id :: !changed)
            ~removed:(fun id -> removed := id :: !removed)
        in
        let settle () = Eio.Time.sleep clock 0.6 in
        settle () (* initial snapshot: empty, no events *);
        ignore (Mongo.insert c (B.doc [ ("_id", B.str "x"); ("n", B.int 1) ]));
        settle () (* the change stream delivers the insert → added "x" *);
        ignore
          (Mongo.update c ~multi:false ~upsert:false (B.doc [ ("_id", B.str "x") ]) (B.doc [ ("$set", B.doc [ ("n", B.int 2) ]) ]));
        settle () (* → changed "x" *);
        ignore (Mongo.remove c (B.doc [ ("_id", B.str "x") ]));
        settle () (* → removed "x" *);
        h.Backend.stop ();
        List.mem "x" !added && List.mem "x" !changed && List.mem "x" !removed

let () = exit (Fennec_hunt_unit.run ())
