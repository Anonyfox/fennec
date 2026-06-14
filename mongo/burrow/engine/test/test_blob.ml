(* Large-blob storage + real (F_FULLFSYNC) durability. Burrow has no 16 MB document cap (LMDB values go
   up to the map size), so a multi-megabyte value is just a normal document field. Uses the default
   durable mode and asserts the blob survives a close + reopen. *)
module Eng = Burrow.Engine
module B = Bson

let tmp label =
  let d = Filename.concat (Filename.get_temp_dir_name ()) ("burrow_" ^ label ^ "_" ^ string_of_int (Unix.getpid ())) in
  (try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

let doc fields = B.Document fields
let empty = B.Document []
let mb n = n * 1024 * 1024

let () =
  Eio_main.run @@ fun _env ->
  let dir = tmp "blob" in
  (* default open = Full durability (group-committed F_FULLFSYNC) *)
  let eng = Eng.open_ dir in
  let c = Eng.collection eng "files" in
  let big = String.make (mb 4) 'x' in
  ignore (Eng.insert eng c (doc [ ("name", B.str "big.bin"); ("data", B.Binary { subtype = "00"; base64 = big }) ]));

  let read_blob e coll =
    match Eng.find_one e coll ~selector:(doc [ ("name", B.str "big.bin") ]) ~sort:empty ~skip:0 ~fields:empty with
    | Some d -> ( match B.get d "data" with Some (B.Binary { base64; _ }) -> base64 | _ -> "")
    | None -> ""
  in
  assert (read_blob eng c = big);
  Eng.close eng;

  (* reopen: the durable write persisted the 4 MB blob intact *)
  let eng2 = Eng.open_ dir in
  let c2 = Eng.collection eng2 "files" in
  let got = read_blob eng2 c2 in
  assert (String.length got = mb 4 && got = big);
  Eng.close eng2;
  print_string "blob + durability: OK\n"
