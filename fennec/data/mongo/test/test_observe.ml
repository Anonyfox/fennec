(* observe_changes over a REAL mongod: the polling observer must turn live inserts/updates/removes
   into added/changed/removed deltas — the realtime seam (the thing the differential CRUD test does
   NOT cover). Skips (passes) where the native driver wasn't built or no mongod is installed.

   mongod is launched OUTSIDE Eio_main (Unix process mgmt vs Eio SIGCHLD); the observe fiber + the
   mutating fiber are cooperatively scheduled in one domain, so the shared event lists are race-free. *)

module Mongo = Fennec_data_mongo
module Backend = Fennec_data.Backend
module M = Fennec_mongo_mongod.Mongod
module B = Bson

let%test "observe_changes over real mongo: a live insert/update/remove become added/changed/removed" =
  if not (Mongo.available ()) then true
  else
    match M.find () with
    | None -> true
    | Some _ ->
        let t = M.start () in
        Fun.protect
          ~finally:(fun () -> M.stop t)
          (fun () ->
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            let clock = Eio.Stdenv.clock env in
            let conn = Mongo.connect (M.uri t) in
            let c = Mongo.collection ~poll:0.1 ~sw ~sleep:(Eio.Time.sleep clock) conn ~db:"obs" ~name:"c" in
            let added = ref [] and changed = ref [] and removed = ref [] in
            let h =
              Mongo.observe_changes c (Backend.query ())
                ~added:(fun id _ -> added := id :: !added)
                ~changed:(fun id _ _ -> changed := id :: !changed)
                ~removed:(fun id -> removed := id :: !removed)
            in
            let settle () = Eio.Time.sleep clock 0.35 in
            settle () (* initial snapshot: empty, no events *);
            ignore (Mongo.insert c (B.doc [ ("_id", B.str "x"); ("n", B.int 1) ]));
            settle () (* a poll sees the insert → added "x" *);
            ignore
              (Mongo.update c ~multi:false ~upsert:false (B.doc [ ("_id", B.str "x") ])
                 (B.doc [ ("$set", B.doc [ ("n", B.int 2) ]) ]));
            settle () (* → changed "x" *);
            ignore (Mongo.remove c (B.doc [ ("_id", B.str "x") ]));
            settle () (* → removed "x" *);
            h.Backend.stop ();
            List.mem "x" !added && List.mem "x" !changed && List.mem "x" !removed)

let () = exit (Fennec_hunt_unit.run ())
