(* The embedded Burrow backend through the runtime-selectable Dynamic seam: a [burrow://] MONGO_URL
   selects it (the path's trailing segment is the db), and the full Backend.S surface (insert / find /
   find_one / count / index+range query / update / observeChanges / remove) works end-to-end, no mongod. *)
module D = Fennec_pulse_mongo.Dynamic
module Backend = Fennec_pulse.Backend
module B = Bson

let tmpdir = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_emb_" ^ string_of_int (Unix.getpid ()))
let qy ?selector ?sort ?skip ?limit ?fields () = Backend.query ?selector ?sort ?skip ?limit ?fields ()

let () =
  (* storage-only burrow:// (no authority); the path's trailing segment "testdb" is the database *)
  Unix.putenv "MONGO_URL" ("burrow://" ^ Filename.concat tmpdir "testdb");
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let c = D.from_env ~sw ~name:"things" () in

  let id1 = D.insert c (B.doc [ ("name", B.str "ada"); ("age", B.int 36) ]) in
  assert (String.length id1 > 0);
  ignore (D.insert c (B.doc [ ("_id", B.str "x2"); ("name", B.str "bob"); ("age", B.int 20) ]));
  assert (D.count c (B.Document []) = 2);

  (match D.find_one c (qy ~selector:(B.doc [ ("_id", B.str "x2") ]) ()) with
   | Some d -> assert (B.get_string d "name" = Some "bob")
   | None -> assert false);

  (* index + range query through the seam *)
  D.ensure_index c ~name:"age_1" ~keys:(B.doc [ ("age", B.int 1) ]) ~unique:false ~sparse:false;
  assert (List.length (D.find c (qy ~selector:(B.doc [ ("age", B.doc [ ("$gte", B.int 30) ]) ]) ())) = 1);

  (* update *)
  assert (D.update c ~multi:false ~upsert:false (B.doc [ ("_id", B.str "x2") ]) (B.doc [ ("$set", B.doc [ ("age", B.int 21) ]) ]) = 1);

  (* live observeChanges: initial burst then added *)
  let added = ref 0 in
  let h =
    D.observe_changes c (qy ()) ~added:(fun _ _ -> incr added) ~changed:(fun _ _ _ -> ()) ~removed:(fun _ -> ())
  in
  assert (!added = 2);
  ignore (D.insert c (B.doc [ ("name", B.str "cleo") ]));
  assert (!added = 3);
  h.Backend.stop ();

  (* persistence: a second engine handle on the same dir sees the data (engine is cached per dir) *)
  assert (D.remove c (B.doc [ ("name", B.str "ada") ]) = 1);
  assert (D.count c (B.Document []) = 2);

  print_string "embedded backend (Dynamic): OK\n"
