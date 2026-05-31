(* permessage-deflate (RFC 7692) for the websocket transport, on real zlib (raw
   DEFLATE, window_bits = -15). We negotiate no_context_takeover, so each message
   is compressed/decompressed with a fresh stream:

     send:    deflate(payload) with Z_SYNC_FLUSH, drop the trailing 00 00 FF FF
     receive: append 00 00 FF FF to the RSV1 payload, then inflate

   zlib's Sync_flush ends a message with a non-final, byte-aligned empty stored
   block (the 00 00 FF FF marker) — exactly what browsers expect, unlike a BFINAL
   stream. This is required for real Meteor/DDP interop (the stock client
   negotiates permessage-deflate). *)

let enabled = ref true
let tail = "\x00\x00\xff\xff"

(* drive zlib's [flate] over a whole string, collecting all output *)
let run (t : 'a Zlib.t) (input : string) (fl : Zlib.flush) : string =
  let n = String.length input in
  let inbuf = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout (max 1 n) in
  if n > 0 then Bigstringaf.blit_from_string input ~src_off:0 inbuf ~dst_off:0 ~len:n;
  t.Zlib.in_buf <- inbuf;
  t.Zlib.in_ofs <- 0;
  t.Zlib.in_len <- n;
  let cap = 0x4000 in
  let out = Bigarray.Array1.create Bigarray.Char Bigarray.C_layout cap in
  let buf = Buffer.create (max 16 n) in
  let rec loop () =
    t.Zlib.out_buf <- out;
    t.Zlib.out_ofs <- 0;
    t.Zlib.out_len <- cap;
    let st = Zlib.flate t fl in
    if t.Zlib.out_ofs > 0 then
      Buffer.add_string buf (Bigstringaf.substring out ~off:0 ~len:t.Zlib.out_ofs);
    match st with
    | Zlib.Stream_end -> Buffer.contents buf
    | Zlib.Buf_error -> Buffer.contents buf (* no progress possible -> done *)
    | Zlib.Ok -> if t.Zlib.in_len > 0 || t.Zlib.out_len = 0 then loop () else Buffer.contents buf
    | Zlib.Need_dict -> failwith "zlib: need dict"
    | Zlib.Data_error m -> failwith ("zlib: " ^ m)
  in
  loop ()

(* compress a message body for a permessage-deflate frame (RSV1=1) *)
let compress (payload : string) : string =
  let t = Zlib.create_deflate ~window_bits:(-15) () in
  let out = run t payload Zlib.Sync_flush in
  let n = String.length out in
  if n >= 4 && String.sub out (n - 4) 4 = tail then String.sub out 0 (n - 4) else out

(* decompress a permessage-deflate frame body (RSV1 set) *)
let decompress (payload : string) : string =
  let t = Zlib.create_inflate ~window_bits:(-15) () in
  run t (payload ^ tail) Zlib.No_flush
