(* GridFS round-trips over minimongo (the pure, JS-side store): base64 for every length class, then
   upload/download across chunk boundaries (tiny chunk_size so multi-chunk + partial-last-chunk paths
   are exercised), plus metadata, chunk count, and delete. *)
module B = Bson

module MinimongoStore = struct
  type collection = Minimongo.t

  let insert c d = ignore (Minimongo.insert c d)
  let find c ~selector ~sort = Minimongo.fetch (Minimongo.find c ~selector ~sort ())
  let remove c sel = Minimongo.remove c sel
  let fields_of = function B.Document kvs -> List.map fst kvs | _ -> []
  let ensure_index c ~name ~keys ~unique = Minimongo.ensure_index c ~name ~fields:(fields_of keys) ~unique ~sparse:false
end

module G = Gridfs.Make (MinimongoStore)

let () =
  (* base64 must round-trip arbitrary bytes for every length mod 3 *)
  Random.init 7;
  for _ = 1 to 1000 do
    let s = String.init (Random.int 300) (fun _ -> Char.chr (Random.int 256)) in
    assert (Gridfs.base64_decode (Gridfs.base64_encode s) = s)
  done;

  let files = Minimongo.create () and chunks = Minimongo.create () in
  let b = G.bucket ~chunk_size:64 ~files ~chunks () in
  let now = 1_700_000_000_000L in

  (* round-trip across the chunk boundary (64): empty, sub-chunk, exact, +1, many, large *)
  List.iter
    (fun n ->
      let s = String.init n (fun i -> Char.chr (((i * 37) + 11) land 0xff)) in
      let id = G.upload b ~now s in
      match G.download b id with Some out -> assert (out = s) | None -> assert false)
    [ 0; 1; 63; 64; 65; 127; 128; 200; 1000; 5000 ];

  (* metadata + chunk count (200 bytes / 64 = 4 chunks) *)
  let payload = String.make 200 'Z' in
  let id = G.upload b ~now ~filename:"big.bin" ~metadata:(B.doc [ ("owner", B.str "ada") ]) payload in
  (match G.stat b id with
   | Some d ->
     assert (B.get_string d "filename" = Some "big.bin");
     assert (B.get_int d "chunkSize" = Some 64);
     assert (B.get d "length" = Some (B.Int64 200L));
     (match B.get d "metadata" with Some m -> assert (B.get_string m "owner" = Some "ada") | None -> assert false)
   | None -> assert false);
  assert (List.length (MinimongoStore.find chunks ~selector:(B.doc [ ("files_id", B.oid id) ]) ~sort:(B.Document [])) = 4);
  assert (G.download b id = Some payload);

  (* delete removes the file and its chunks *)
  G.delete b id;
  assert (G.download b id = None);
  assert (MinimongoStore.find chunks ~selector:(B.doc [ ("files_id", B.oid id) ]) ~sort:(B.Document []) = []);

  print_string "gridfs (minimongo / JS-side): OK\n"
