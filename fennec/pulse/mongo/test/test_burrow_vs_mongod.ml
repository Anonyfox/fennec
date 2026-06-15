(* The ultimate compatibility proof: run the SAME operations through the embedded Burrow engine and a
   REAL mongod, and assert they agree — query selectors (eq / $gte / array-contains / $in / $or),
   count, distinct, multi-update, remove, and an aggregation pipeline (in result order). Burrow is the
   in-process engine; mongod is the ground truth. Skips (passes) when the native driver wasn't built or
   no mongod is installed. mongod is launched outside Eio_main (its process management must not race
   Eio's SIGCHLD); the ops run inside Eio_main. *)

module Mongo = Fennec_pulse_mongo
module Backend = Fennec_pulse.Backend
module M = Fennec_mongo_mongod.Mongod
module Eng = Burrow.Engine
module S = Burrow_store.Store
module Diff = Query.Diff
module B = Bson

(* order- and field-order-insensitive document equality (both engines may differ in field/result order
   where the query doesn't pin it) *)
let rec norm = function
  | B.Document kvs -> B.Document (List.sort (fun (a, _) (b, _) -> compare a b) (List.map (fun (k, v) -> (k, norm v)) kvs))
  | B.Array xs -> B.Array (List.map norm xs)
  | v -> v

let by_id docs = List.sort (fun a b -> compare (Diff.doc_id a) (Diff.doc_id b)) docs
let eq_docs a b = List.length a = List.length b && List.for_all2 (fun x y -> B.equal (norm x) (norm y)) (by_id a) (by_id b)
let q sel = Backend.query ~selector:sel ()

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_vsmongo_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let bfind eng c sel = Eng.find eng c ~selector:sel ~sort:(B.Document []) ~skip:0 ~limit:0 ~fields:(B.Document [])

let%test "Burrow agrees with a real mongod across find/count/distinct/update/remove/aggregate" =
  if not (Mongo.available ()) then true
  else
    match M.find () with
    | None -> true
    | Some _ ->
      let t = M.start () in
      Fun.protect
        ~finally:(fun () -> M.stop t)
        (fun () ->
          Eio_main.run @@ fun _env ->
          Eio.Switch.run @@ fun sw ->
          let conn = Mongo.connect (M.uri t) in
          let mc = Mongo.collection ~sw conn ~db:"bdiff" ~name:"c" in
          let eng = Eng.open_ ~sw ~durability:S.No_sync (tmpdir ()) in
          let bc = Eng.collection eng "c" in
          (* index the Burrow side so the index access paths + sort-via-index are what gets compared *)
          Eng.ensure_index eng bc ~name:"age_1" ~keys:(B.doc [ ("age", B.int 1) ]) ~unique:false;
          Eng.ensure_index eng bc ~name:"name_1" ~keys:(B.doc [ ("name", B.int 1) ]) ~unique:false;
          let docs =
            [ B.doc [ ("_id", B.str "1"); ("name", B.str "ann"); ("age", B.int 30); ("tags", B.array [ B.str "a"; B.str "b" ]) ];
              B.doc [ ("_id", B.str "2"); ("name", B.str "bob"); ("age", B.int 25); ("tags", B.array [ B.str "b" ]) ];
              B.doc [ ("_id", B.str "3"); ("name", B.str "cy"); ("age", B.int 30); ("tags", B.array []) ] ]
          in
          List.iter (fun d -> ignore (Mongo.insert mc d); ignore (Eng.insert eng bc d)) docs;
          let selectors =
            [ B.doc [];
              B.doc [ ("age", B.int 30) ];
              B.doc [ ("age", B.doc [ ("$gte", B.int 26) ]) ];
              B.doc [ ("tags", B.str "b") ];
              B.doc [ ("name", B.doc [ ("$in", B.array [ B.str "ann"; B.str "cy" ]) ]) ];
              B.doc [ ("$or", B.array [ B.doc [ ("age", B.int 25) ]; B.doc [ ("name", B.str "cy") ] ]) ] ]
          in
          let find_ok = List.for_all (fun s -> eq_docs (Mongo.find mc (q s)) (bfind eng bc s)) selectors in
          let count_ok = List.for_all (fun s -> Mongo.count mc s = Eng.count eng bc ~selector:s) selectors in
          let set vs = List.sort compare (List.map norm vs) in
          let distinct_ok =
            set (Mongo.distinct mc "age" (B.doc [])) = set (Eng.distinct eng bc ~key:"age" ~selector:(B.doc []))
            && set (Mongo.distinct mc "tags" (B.doc [])) = set (Eng.distinct eng bc ~key:"tags" ~selector:(B.doc []))
          in
          (* sort-via-index (Burrow uses age_1) must produce the SAME sequence as mongod, including the
             _id tiebreak — order-preserving comparison *)
          let eq_ordered a b = List.length a = List.length b && List.for_all2 (fun x y -> B.equal (norm x) (norm y)) a b in
          let sort = B.doc [ ("age", B.int 1); ("_id", B.int 1) ] in
          let sorted_ok =
            eq_ordered
              (Mongo.find mc (Backend.query ~sort ()))
              (Eng.find eng bc ~selector:(B.doc []) ~sort ~skip:0 ~limit:0 ~fields:(B.doc []))
          in
          let usel = B.doc [ ("age", B.int 30) ] and umod = B.doc [ ("$set", B.doc [ ("active", B.bool true) ]) ] in
          let nu_m = Mongo.update mc ~multi:true ~upsert:false usel umod in
          let nu_b = Eng.update eng bc ~multi:true ~upsert:false usel umod in
          let update_ok = nu_m = nu_b && eq_docs (Mongo.find mc (q (B.doc []))) (bfind eng bc (B.doc [])) in
          let rsel = B.doc [ ("name", B.str "bob") ] in
          let nr_m = Mongo.remove mc rsel and nr_b = Eng.remove eng bc rsel in
          let remove_ok = nr_m = nr_b && eq_docs (Mongo.find mc (q (B.doc []))) (bfind eng bc (B.doc [])) in
          let agg_pipeline =
            [ B.doc [ ("$match", B.doc [ ("age", B.int 30) ]) ];
              B.doc [ ("$sort", B.doc [ ("name", B.int 1) ]) ];
              B.doc [ ("$project", B.doc [ ("name", B.int 1) ]) ] ]
          in
          let am = Mongo.aggregate mc agg_pipeline and ab = Eng.aggregate eng bc agg_pipeline in
          let agg_ok = List.length am = List.length ab && List.for_all2 (fun x y -> B.equal (norm x) (norm y)) am ab in
          Eng.close eng;
          find_ok && count_ok && distinct_ok && sorted_ok && update_ok && remove_ok && agg_ok)

let () = exit (Fennec_hunt_unit.run ())
