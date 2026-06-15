module B = Bson

let default_chunk_size = 255 * 1024

(* ---- base64 (pure; a chunk's bytes <-> its Bson.Binary base64 payload) --------------------- *)

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode (s : string) : string =
  let n = String.length s in
  let buf = Buffer.create (((n + 2) / 3) * 4) in
  let enc x = Buffer.add_char buf alphabet.[x land 0x3f] in
  let i = ref 0 in
  while !i + 3 <= n do
    let b0 = Char.code s.[!i] and b1 = Char.code s.[!i + 1] and b2 = Char.code s.[!i + 2] in
    enc (b0 lsr 2);
    enc ((b0 lsl 4) lor (b1 lsr 4));
    enc ((b1 lsl 2) lor (b2 lsr 6));
    enc b2;
    i := !i + 3
  done;
  (match n - !i with
   | 1 ->
     let b0 = Char.code s.[!i] in
     enc (b0 lsr 2);
     enc (b0 lsl 4);
     Buffer.add_string buf "=="
   | 2 ->
     let b0 = Char.code s.[!i] and b1 = Char.code s.[!i + 1] in
     enc (b0 lsr 2);
     enc ((b0 lsl 4) lor (b1 lsr 4));
     enc (b1 lsl 2);
     Buffer.add_char buf '='
   | _ -> ());
  Buffer.contents buf

let base64_decode (s : string) : string =
  let v c =
    if c >= 'A' && c <= 'Z' then Char.code c - 65
    else if c >= 'a' && c <= 'z' then Char.code c - 71
    else if c >= '0' && c <= '9' then Char.code c + 4
    else if c = '+' then 62
    else if c = '/' then 63
    else -1
  in
  let buf = Buffer.create (String.length s * 3 / 4) in
  let acc = ref 0 and bits = ref 0 in
  String.iter
    (fun c ->
      let d = v c in
      if d >= 0 then begin
        acc := (!acc lsl 6) lor d;
        bits := !bits + 6;
        if !bits >= 8 then begin
          bits := !bits - 8;
          Buffer.add_char buf (Char.chr ((!acc lsr !bits) land 0xff))
        end
      end)
    s;
  Buffer.contents buf

(* ---- the functor --------------------------------------------------------------------------- *)

module type STORE = sig
  type collection

  val insert : collection -> Bson.t -> unit
  val find : collection -> selector:Bson.t -> sort:Bson.t -> Bson.t list
  val remove : collection -> Bson.t -> int
  val ensure_index : collection -> name:string -> keys:Bson.t -> unique:bool -> unit
end

module Make (S : STORE) = struct
  type bucket = { files : S.collection; chunks : S.collection; chunk_size : int }

  let bucket ?(chunk_size = default_chunk_size) ~files ~chunks () =
    (* the standard GridFS chunks index: unique, ordered (files_id, n) *)
    S.ensure_index chunks ~name:"files_id_1_n_1" ~keys:(B.Document [ ("files_id", B.Int 1); ("n", B.Int 1) ]) ~unique:true;
    { files; chunks; chunk_size }

  let by_id fid = B.Document [ ("_id", B.Object_id fid) ]

  let upload b ~now ?filename ?metadata bytes =
    let fid = Query.Id.object_id () in
    let len = String.length bytes in
    (* chunks first, so a file's metadata only appears once every chunk exists *)
    let n_chunks = if len = 0 then 0 else ((len + b.chunk_size - 1) / b.chunk_size) in
    for i = 0 to n_chunks - 1 do
      let off = i * b.chunk_size in
      let sz = min b.chunk_size (len - off) in
      S.insert b.chunks
        (B.Document
           [ ("_id", B.Object_id (Query.Id.object_id ()));
             ("files_id", B.Object_id fid);
             ("n", B.Int i);
             ("data", B.Binary { subtype = "00"; base64 = base64_encode (String.sub bytes off sz) }) ])
    done;
    let meta =
      [ ("_id", B.Object_id fid);
        ("length", B.Int64 (Int64.of_int len));
        ("chunkSize", B.Int b.chunk_size);
        ("uploadDate", B.Date now) ]
      @ (match filename with Some f -> [ ("filename", B.String f) ] | None -> [])
      @ (match metadata with Some m -> [ ("metadata", m) ] | None -> [])
    in
    S.insert b.files (B.Document meta);
    fid

  let stat b fid = match S.find b.files ~selector:(by_id fid) ~sort:(B.Document []) with d :: _ -> Some d | [] -> None

  let download b fid =
    match stat b fid with
    | None -> None
    | Some _ ->
      let chunks =
        S.find b.chunks ~selector:(B.Document [ ("files_id", B.Object_id fid) ]) ~sort:(B.Document [ ("n", B.Int 1) ])
      in
      let buf = Buffer.create 4096 in
      List.iter (fun ch -> match B.get ch "data" with Some (B.Binary { base64; _ }) -> Buffer.add_string buf (base64_decode base64) | _ -> ()) chunks;
      Some (Buffer.contents buf)

  let list b = S.find b.files ~selector:(B.Document []) ~sort:(B.Document [])

  let delete b fid =
    ignore (S.remove b.files (by_id fid));
    ignore (S.remove b.chunks (B.Document [ ("files_id", B.Object_id fid) ]))
end
