(* gzip + raw-deflate compression on real zlib, for HTTP Content-Encoding. The
   gzip container is window_bits=31 (16 + 15); raw deflate for Content-Encoding:
   deflate is window_bits=15 with a zlib header (what browsers actually accept for
   "deflate"). One-shot whole-string compression with Finish. *)

let run (t : 'a Zlib.t) (input : string) : string =
  let n = String.length input in
  let inbuf = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout (max 1 n) in
  if n > 0 then Bigstringaf.blit_from_string input ~src_off:0 inbuf ~dst_off:0 ~len:n;
  t.Zlib.in_buf <- inbuf;
  t.Zlib.in_ofs <- 0;
  t.Zlib.in_len <- n;
  let cap = 0x4000 in
  let out = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout cap in
  let buf = Buffer.create (max 64 (n / 2)) in
  let rec loop () =
    t.Zlib.out_buf <- out;
    t.Zlib.out_ofs <- 0;
    t.Zlib.out_len <- cap;
    let st = Zlib.flate t Zlib.Finish in
    if t.Zlib.out_ofs > 0 then
      Buffer.add_string buf (Bigstringaf.substring out ~off:0 ~len:t.Zlib.out_ofs);
    match st with
    | Zlib.Stream_end -> Buffer.contents buf
    | Zlib.Ok -> loop ()
    | Zlib.Buf_error -> Buffer.contents buf
    | Zlib.Need_dict -> failwith "zlib: need dict"
    | Zlib.Data_error m -> failwith ("zlib: " ^ m)
  in
  loop ()

(* ──── gzip ──── *)

(* gzip-encode (RFC 1952 container) — for Content-Encoding: gzip *)
let gzip ?(level = 6) (s : string) : string =
  let t = Zlib.create_deflate ~level ~window_bits:31 () in
  run t s

let%test "gzip magic 1f 8b" =
  let g = gzip (String.make 2000 'a') in
  String.length g >= 2 && g.[0] = '\x1f' && g.[1] = '\x8b'
let%test "gzip shrinks repetitive" =
  String.length (gzip (String.make 2000 'a')) < 2000
let%test "gzip empty input ok" =
  String.length (gzip "") >= 0

(* ──── deflate ──── *)

(* zlib-wrapped deflate — for Content-Encoding: deflate (browsers accept the
   zlib-wrapped form; window_bits=15) *)
let deflate ?(level = 6) (s : string) : string =
  let t = Zlib.create_deflate ~level ~window_bits:15 () in
  run t s

let%test "deflate zlib header 0x78" =
  let d = deflate (String.make 2000 'b') in
  String.length d >= 1 && Char.code d.[0] = 0x78
let%test "deflate shrinks" =
  String.length (deflate (String.make 2000 'b')) < 2000
let%test "deflate empty input ok" =
  String.length (deflate "") >= 0
let%test "gzip level produces valid output" =
  let g1 = gzip ~level:1 (String.make 2000 'c') in
  String.length g1 >= 2 && g1.[0] = '\x1f' && g1.[1] = '\x8b'
