(* Map usage reporting: real bytes in use vs the configured virtual ceiling, so an operator can alarm
   before MDB_MAP_FULL. Opens with a small (1 GiB) map to assert the ceiling exactly; the map is sparse,
   so used_bytes is real on-disk growth that rises with a large write. *)
module Eng = Burrow.Engine
module S = Burrow_store.Store
module B = Bson

let temp () =
  let d = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "burrow_usage_%d" (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat d f) with _ -> ()) [ "data.mdb"; "lock.mdb" ];
  d

let doc fields = B.Document fields
let s = B.str

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp () in
  let eng = Eng.open_ ~sw ~durability:S.No_sync ~map_size_gb:1 dir in
  let c = Eng.collection eng "t" in

  ignore (Eng.insert eng c (doc [ ("_id", s "a"); ("v", s "hello") ]));
  let u0 = Eng.usage eng in
  assert (u0.S.map_bytes = 1073741824L) (* exactly the 1 GiB virtual ceiling *);
  assert (Int64.compare u0.S.used_bytes 0L > 0) (* real pages allocated *);
  assert (u0.S.fraction > 0. && u0.S.fraction < 1.);

  (* a large value grows real usage; the ceiling is unchanged *)
  let big = String.make (4 * 1024 * 1024) 'x' in
  ignore (Eng.insert eng c (doc [ ("_id", s "big"); ("blob", s big) ]));
  let u1 = Eng.usage eng in
  assert (Int64.compare u1.S.used_bytes u0.S.used_bytes > 0);
  assert (u1.S.map_bytes = u0.S.map_bytes);

  Eng.close eng;
  print_string "usage: OK\n"
