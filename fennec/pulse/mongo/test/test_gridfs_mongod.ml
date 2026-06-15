(* GridFS three ways: the SAME files uploaded + downloaded through the embedded Burrow engine, minimongo,
   and a REAL mongod — all byte-identical — and the mongod-side fs.files/fs.chunks structure verified to
   the GridFS spec (chunk count + length), so a file written via Burrow/minimongo GridFS is exactly what
   a real GridFS client would read. The SAME pure Gridfs functor is instantiated over each store. Skips
   when the native driver / mongod isn't available. *)

module Mongo = Fennec_pulse_mongo
module Backend = Fennec_pulse.Backend
module M = Fennec_mongo_mongod.Mongod
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_gridfs_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let fields_of = function B.Document kvs -> List.map fst kvs | _ -> []

module BurrowStore = struct
  type collection = Eng.t * Eng.collection

  let insert (e, c) d = ignore (Eng.insert e c d)
  let find (e, c) ~selector ~sort = Eng.find e c ~selector ~sort ~skip:0 ~limit:0 ~fields:(B.Document [])
  let remove (e, c) sel = Eng.remove e c sel
  let ensure_index (e, c) ~name ~keys ~unique = Eng.ensure_index e c ~name ~keys ~unique
end

module MongodStore = struct
  type collection = Mongo.collection

  let insert c d = ignore (Mongo.insert c d)
  let find c ~selector ~sort = Mongo.find c (Backend.query ~selector ~sort ())
  let remove c sel = Mongo.remove c sel
  let ensure_index c ~name ~keys ~unique = Mongo.ensure_index c ~name ~keys ~unique
end

module MiniStore = struct
  type collection = Minimongo.t

  let insert c d = ignore (Minimongo.insert c d)
  let find c ~selector ~sort = Minimongo.fetch (Minimongo.find c ~selector ~sort ())
  let remove c sel = Minimongo.remove c sel
  let ensure_index c ~name ~keys ~unique = Minimongo.ensure_index c ~name ~fields:(fields_of keys) ~unique
end

module Gb = Gridfs.Make (BurrowStore)
module Gm = Gridfs.Make (MongodStore)
module Gx = Gridfs.Make (MiniStore)

let%test "GridFS round-trips identically on Burrow, minimongo, and a real mongod (wire-compatible)" =
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
          let mfiles = Mongo.collection ~sw conn ~db:"gfs" ~name:"fs.files"
          and mchunks = Mongo.collection ~sw conn ~db:"gfs" ~name:"fs.chunks" in
          let eng = Eng.open_ ~sw ~durability:S.No_sync (tmpdir ()) in
          let bfiles = (eng, Eng.collection eng "fs.files") and bchunks = (eng, Eng.collection eng "fs.chunks") in
          let xfiles = Minimongo.create () and xchunks = Minimongo.create () in
          let cs = 100 in
          let bb = Gb.bucket ~chunk_size:cs ~files:bfiles ~chunks:bchunks () in
          let mb_ = Gm.bucket ~chunk_size:cs ~files:mfiles ~chunks:mchunks () in
          let xb = Gx.bucket ~chunk_size:cs ~files:xfiles ~chunks:xchunks () in
          let now = 1_700_000_000_000L in
          let ok = ref true in
          let check c = if not c then ok := false in
          List.iter
            (fun n ->
              let p = String.init n (fun i -> Char.chr (((i * 131) + 7) land 0xff)) in
              (* upload + download through each store; bytes must come back identical everywhere *)
              check (Gb.download bb (Gb.upload bb ~now p) = Some p);
              check (Gx.download xb (Gx.upload xb ~now p) = Some p);
              let idm = Gm.upload mb_ ~now p in
              check (Gm.download mb_ idm = Some p);
              (* mongod-side GridFS structure: ceil(len/cs) chunks, files.length = len (any numeric type) *)
              let expected = if n = 0 then 0 else ((n + cs - 1) / cs) in
              check (List.length (MongodStore.find mchunks ~selector:(B.doc [ ("files_id", B.oid idm) ]) ~sort:(B.Document [])) = expected);
              match Gm.stat mb_ idm with Some d -> check (B.as_float (Option.value ~default:B.Null (B.get d "length")) = Some (float_of_int n)) | None -> check false)
            [ 0; 1; 99; 100; 101; 350; 4096 ];
          Eng.close eng;
          !ok)

let () = exit (Fennec_hunt_unit.run ())
