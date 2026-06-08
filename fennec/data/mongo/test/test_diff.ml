(* Differential correctness: the SAME operations through Backend.Mini (in-memory) and the native
   libmongoc backend against a REAL mongod must agree — query selectors, update operators, removal,
   counts. This is what proves Minimongo is a faithful swap for real mongo "across all ops angles".
   Skips (passes) where the native driver wasn't built or no mongod is installed.

   mongod is launched OUTSIDE Eio_main (its Unix process management must not race Eio's SIGCHLD
   handling); the backend ops then run inside Eio_main (each blocking call offloads to a systhread). *)

module Mongo = Fennec_data_mongo
module Mini = Fennec_data.Backend.Mini
module Backend = Fennec_data.Backend
module M = Fennec_mongo_mongod.Mongod
module Diff = Query.Diff
module B = Bson

(* compile-only proof that the reactive stack runs over real mongo unchanged: the native backend
   satisfies Backend.S, so Reactive.Make accepts it exactly as it accepts Backend.Mini *)
module _ = Fennec_data.Reactive.Make (Mongo)

(* order- and field-order-insensitive document equality (mongo may reorder; Bson.equal is ordered) *)
let rec norm = function
  | B.Document kvs -> B.Document (List.sort (fun (a, _) (b, _) -> compare a b) (List.map (fun (k, v) -> (k, norm v)) kvs))
  | B.Array xs -> B.Array (List.map norm xs)
  | v -> v

let by_id docs = List.sort (fun a b -> compare (Diff.doc_id a) (Diff.doc_id b)) docs

let eq_docs a b =
  List.length a = List.length b && List.for_all2 (fun x y -> B.equal (norm x) (norm y)) (by_id a) (by_id b)

let q sel = Backend.query ~selector:sel ()

let%test "insert/find/update/remove/count agree between Minimongo and a real mongod" =
  if not (Mongo.available ()) then true (* native driver not built — skip *)
  else
    match M.find () with
    | None -> true (* no mongod — skip *)
    | Some _ ->
        let t = M.start () in
        Fun.protect
          ~finally:(fun () -> M.stop t)
          (fun () ->
            Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            let conn = Mongo.connect (M.uri t) in
            let mc = Mongo.collection ~sw ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env)) conn ~db:"diff" ~name:"c" in
            let mini = Minimongo.create () in
            let docs =
              [ B.doc [ ("_id", B.str "1"); ("name", B.str "ann"); ("age", B.int 30); ("tags", B.array [ B.str "a"; B.str "b" ]) ];
                B.doc [ ("_id", B.str "2"); ("name", B.str "bob"); ("age", B.int 25); ("tags", B.array [ B.str "b" ]) ];
                B.doc [ ("_id", B.str "3"); ("name", B.str "cy"); ("age", B.int 30); ("tags", B.array []) ] ]
            in
            List.iter (fun d -> ignore (Mongo.insert mc d); ignore (Mini.insert mini d)) docs;
            (* a battery of selectors exercising eq / $gte / array-contains / $in / $or *)
            let selectors =
              [ B.doc [];
                B.doc [ ("age", B.int 30) ];
                B.doc [ ("age", B.doc [ ("$gte", B.int 26) ]) ];
                B.doc [ ("tags", B.str "b") ];
                B.doc [ ("name", B.doc [ ("$in", B.array [ B.str "ann"; B.str "cy" ]) ]) ];
                B.doc [ ("$or", B.array [ B.doc [ ("age", B.int 25) ]; B.doc [ ("name", B.str "cy") ] ]) ] ]
            in
            let find_ok = List.for_all (fun s -> eq_docs (Mongo.find mc (q s)) (Mini.find mini (q s))) selectors in
            let count_ok = List.for_all (fun s -> Mongo.count mc s = Mini.count mini s) selectors in
            (* multi $set, then compare the whole collection *)
            let usel = B.doc [ ("age", B.int 30) ] and umod = B.doc [ ("$set", B.doc [ ("active", B.bool true) ]) ] in
            let nu_m = Mongo.update mc ~multi:true ~upsert:false usel umod in
            let nu_i = Mini.update mini ~multi:true ~upsert:false usel umod in
            let update_ok = nu_m = nu_i && eq_docs (Mongo.find mc (q (B.doc []))) (Mini.find mini (q (B.doc []))) in
            (* remove, then compare *)
            let rsel = B.doc [ ("name", B.str "bob") ] in
            let nr_m = Mongo.remove mc rsel and nr_i = Mini.remove mini rsel in
            let remove_ok = nr_m = nr_i && eq_docs (Mongo.find mc (q (B.doc []))) (Mini.find mini (q (B.doc []))) in
            find_ok && count_ok && update_ok && remove_ok)

let () = exit (Fennec_hunt_unit.run ())
