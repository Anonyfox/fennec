(* Three-way performance stress test: the embedded Burrow engine vs a REAL mongod (native libmongoc
   driver) vs minimongo (in-memory reference) — identical data, identical indexes, identical operations.
   Burrow runs in-process (no wire), so it should beat mongod across the board, and beat minimongo
   wherever a real index beats an in-memory scan. mongod is launched outside Eio_main; ops run inside.
   Skips when the native driver / mongod isn't available. *)

module Mongo = Fennec_mongo_dynamic
module Backend = Fennec_mongo_backend
module M = Fennec_mongo_mongod.Mongod
module Eng = Burrow.Engine
module S = Burrow_store.Store
module MM = Minimongo
module B = Bson

let tmpdir () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_bench3_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let n = 20_000
let empty = B.Document []
let doc fields = B.Document fields

(* a realistic document: indexed uid (~10 docs each), status (5 values), score, created (monotonic), payload *)
let mkdoc k =
  doc
    [ ("_id", B.str (Printf.sprintf "u%07d" k));
      ("uid", B.int (k mod 2000));
      ("status", B.int (k mod 5));
      ("score", B.int (k mod 1000));
      ("created", B.int k);
      ("payload", B.str "the quick brown fox jumps over the lazy dog 0123456789") ]

(* uniform per-backend operation surface *)
type be = {
  name : string;
  insert : B.t -> unit;
  find : selector:B.t -> sort:B.t -> skip:int -> limit:int -> B.t list;
  count : B.t -> int;
  update_one : B.t -> B.t -> int;
}

let now () = Unix.gettimeofday ()

(* µs per op over [iters], replaying inputs.(i mod len); returns (µs, last-result-size for a sanity check) *)
let time iters (inputs : 'a array) (op : 'a -> int) =
  let len = Array.length inputs in
  let last = ref 0 in
  let t0 = now () in
  for i = 0 to iters - 1 do
    last := op inputs.(i mod len)
  done;
  ((now () -. t0) /. float iters *. 1e6, !last)

let () =
  if not (Mongo.available ()) then print_endline "native driver unavailable — skipping"
  else
    match M.find () with
    | None -> print_endline "no mongod installed — skipping"
    | Some _ ->
      let t = M.start () in
      Fun.protect ~finally:(fun () -> M.stop t) @@ fun () ->
      Eio_main.run @@ fun _env ->
      Eio.Switch.run @@ fun sw ->
      (* --- three backends, same data + indexes --- *)
      let eng = Eng.open_ ~sw ~durability:S.No_sync (tmpdir ()) in
      let bc = Eng.collection eng "bench" in
      let conn = Mongo.connect (M.uri t) in
      let mc = Mongo.collection ~sw conn ~db:"bench" ~name:"bench" in
      let mm = MM.create () in

      let burrow = { name = "Burrow"; insert = (fun d -> ignore (Eng.insert eng bc d));
                     find = (fun ~selector ~sort ~skip ~limit -> Eng.find eng bc ~selector ~sort ~skip ~limit ~fields:empty);
                     count = (fun s -> Eng.count eng bc ~selector:s);
                     update_one = (fun sel m -> Eng.update eng bc ~multi:false ~upsert:false sel m) } in
      let mongod = { name = "mongod"; insert = (fun d -> ignore (Mongo.insert mc d));
                     find = (fun ~selector ~sort ~skip ~limit -> Mongo.find mc (Backend.query ~selector ~sort ~skip ~limit ()));
                     count = (fun s -> Mongo.count mc s);
                     update_one = (fun sel m -> Mongo.update mc ~multi:false ~upsert:false sel m) } in
      let mini = { name = "minimongo"; insert = (fun d -> ignore (MM.insert mm d));
                   find = (fun ~selector ~sort ~skip ~limit -> MM.fetch (MM.find mm ~selector ~sort ~skip ~limit ()));
                   count = (fun s -> MM.count (MM.find mm ~selector:s ()));
                   update_one = (fun sel m -> MM.update mm ~multi:false ~upsert:false sel m) } in
      let bes = [ burrow; mongod; mini ] in

      (* indexes (where each engine supports them) *)
      List.iter
        (fun (nm, keys) ->
          Eng.ensure_index eng bc ~name:nm ~keys ~unique:false ~sparse:false;
          Mongo.ensure_index mc ~name:nm ~keys ~unique:false ~sparse:false)
        [ ("uid_1", doc [ ("uid", B.int 1) ]); ("status_1", doc [ ("status", B.int 1) ]);
          ("score_1", doc [ ("score", B.int 1) ]); ("created_-1", doc [ ("created", B.int (-1)) ]);
          ("status_created", doc [ ("status", B.int 1); ("created", B.int (-1)) ]) ];
      List.iter (fun (nm, fields) -> MM.ensure_index mm ~name:nm ~fields ~unique:false ~sparse:false)
        [ ("uid_1", [ "uid" ]); ("status_1", [ "status" ]); ("score_1", [ "score" ]); ("created_1", [ "created" ]) ];

      Printf.printf "\n=== Burrow vs mongod vs minimongo — %d docs ===\n\n" n;
      Printf.printf "%-44s %12s %12s %12s    %s\n" "scenario (us/op, lower=better)" "Burrow" "mongod" "minimongo" "Burrow speedup";
      Printf.printf "%s\n" (String.make 110 '-');

      let row label iters inputs (op : be -> 'a -> int) =
        let r = List.map (fun be -> let us, hits = time iters inputs (op be) in (be.name, us, hits)) bes in
        let us name = let _, u, _ = List.find (fun (n, _, _) -> n = name) r in u in
        let _, _, hits = List.hd r in
        Printf.printf "%-44s %12.2f %12.2f %12.2f    %4.0fx vs mongod, %4.1fx vs mini  (%d hits)\n"
          label (us "Burrow") (us "mongod") (us "minimongo") (us "mongod" /. us "Burrow") (us "minimongo" /. us "Burrow") hits
      in

      (* --- load: insert N docs into each (timed separately, not in the table) --- *)
      let load be =
        let t0 = now () in
        for k = 0 to n - 1 do be.insert (mkdoc k) done;
        (now () -. t0)
      in
      Printf.printf "load %d docs (insert, one-by-one):\n" n;
      List.iter (fun be -> let dt = load be in Printf.printf "  %-12s %8.2f s   (%.0f ins/s)\n%!" be.name dt (float n /. dt)) bes;
      Printf.printf "\n";

      (* --- inputs --- *)
      Random.init 12345;
      let ids = Array.init 1000 (fun _ -> doc [ ("_id", B.str (Printf.sprintf "u%07d" (Random.int n))) ]) in
      let uids = Array.init 1000 (fun _ -> doc [ ("uid", B.int (Random.int 2000)) ]) in
      let stats = Array.init 5 (fun k -> doc [ ("status", B.int k) ]) in
      let ranges = Array.init 1000 (fun _ -> let lo = Random.int 950 in doc [ ("score", doc [ ("$gte", B.int lo); ("$lt", B.int (lo + 50)) ]) ]) in
      let feeds = Array.init 5 (fun k -> doc [ ("status", B.int k) ]) in
      let created_desc = doc [ ("created", B.int (-1)) ] in

      row "point by _id" 5000 ids (fun be sel -> List.length (be.find ~selector:sel ~sort:empty ~skip:0 ~limit:0));
      row "equality on uid (~10 docs)" 3000 uids (fun be sel -> List.length (be.find ~selector:sel ~sort:empty ~skip:0 ~limit:0));
      row "equality on status (~4000 docs)" 100 stats (fun be sel -> List.length (be.find ~selector:sel ~sort:empty ~skip:0 ~limit:0));
      row "range on score (~1000 docs)" 300 ranges (fun be sel -> List.length (be.find ~selector:sel ~sort:empty ~skip:0 ~limit:0));
      row "sort created desc, limit 20" 2000 [| empty |] (fun be _ -> List.length (be.find ~selector:empty ~sort:created_desc ~skip:0 ~limit:20));
      row "feed: status=k sort created desc limit 20" 2000 feeds (fun be sel -> List.length (be.find ~selector:sel ~sort:created_desc ~skip:0 ~limit:20));
      row "count status=k" 1000 stats (fun be sel -> be.count sel);
      row "update one by _id ($inc score)" 2000 ids (fun be sel -> be.update_one sel (doc [ ("$inc", doc [ ("score", B.int 1) ]) ]));

      Eng.close eng;
      Printf.printf "\n(Burrow durability = No_sync, comparable to mongod default w:1 async-journal)\n"
