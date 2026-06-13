(* The runtime-selectable backend: one Backend.S that is in-memory OR mongo-backed, chosen at boot.
   This proves the in-memory selection end to end (no mongod needed) and that Reactive.Make accepts
   Dynamic exactly as it accepts a single backend — the enabler for "real mongo one flag away". *)

module D = Fennec_pulse_mongo.Dynamic
module Backend = Fennec_pulse.Backend
module B = Bson
module Runtime = Fennec_mongo_driver.Runtime

let with_env name value f =
  let old = Sys.getenv_opt name in
  (match value with
  | Some value -> Unix.putenv name value
  | None -> Unix.putenv name "");
  Fun.protect f ~finally:(fun () ->
      match old with
      | Some value -> Unix.putenv name value
      | None -> Unix.putenv name "")

(* compile-only: the whole reactive stack runs over the dynamic backend *)
module _ = Fennec_pulse.Reactive.Make (D)

let%test "Dynamic (in-memory selection): full CRUD through Backend.S" =
  let c = D.mem (Minimongo.create ()) in
  let id = D.insert c (B.doc [ ("n", B.int 1) ]) in
  let by_id = Backend.query ~selector:(B.doc [ ("_id", B.str id) ]) () in
  let inserted = match D.find c by_id with [ d ] -> B.get d "n" = Some (B.Int 1) | _ -> false in
  let counted = D.count c (B.doc []) = 1 in
  let modified =
    D.update c ~multi:false ~upsert:false (B.doc [ ("_id", B.str id) ]) (B.doc [ ("$set", B.doc [ ("n", B.int 2) ]) ]) = 1
  in
  let updated = match D.find c (Backend.query ()) with [ d ] -> B.get d "n" = Some (B.Int 2) | _ -> false in
  let removed = D.remove c (B.doc []) = 1 && D.count c (B.doc []) = 0 in
  inserted && counted && modified && updated && removed

let%test "Dynamic missing Mongo fails operations clearly" =
  with_env Runtime.mongo_url_env None (fun () ->
      Eio_main.run (fun _env ->
          Eio.Switch.run (fun sw ->
              let c = D.from_env ~sw ~db:"fennec_test" ~name:"missing" () in
              match D.count c (B.doc []) with
              | _ -> false
              | exception Failure msg -> Fennec_hunt_unit.str_contains msg "MONGO_URL is not set")))

let () = exit (Fennec_hunt_unit.run ())
