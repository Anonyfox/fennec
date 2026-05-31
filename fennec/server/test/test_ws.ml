(* Unit tests for the RFC 6455 codec: handshake accept-key, masked frame
   round-trips across length boundaries, and the hardening rules — unmasked client
   frame rejected, oversized length rejected, control-frame constraints, reserved
   bits. Buf_read/Buf_write need an Eio context. *)

module Ws = Fennec_server.Ws

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

(* encode a frame as a CLIENT would (masked), read it back through read_frame *)
let roundtrip (f : Ws.frame) : Ws.read_result =
  let buf = Buffer.create 256 in
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buf) (fun w -> Ws.write_masked_frame w f);
  let s = Buffer.contents buf in
  let r = Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(64 * 1024 * 1024) in
  Ws.read_frame r

(* read raw bytes through read_frame *)
let read_bytes (s : string) : Ws.read_result =
  let r = Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(64 * 1024 * 1024) in
  Ws.read_frame r

let mk ?(fin = true) ?(rsv1 = false) opcode payload = { Ws.fin; rsv1; opcode; payload }

let test_accept () =
  print_endline "handshake:";
  eq "RFC accept-key" (Ws.accept_key "dGhlIHNhbXBsZSBub25jZQ==") "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

let test_roundtrip () =
  print_endline "masked frame round-trip:";
  let cases =
    [ ("empty text", mk Ws.Text "");
      ("short", mk Ws.Text "reload");
      ("125 (7-bit max)", mk Ws.Text (String.make 125 'a'));
      ("126 (16-bit len)", mk Ws.Text (String.make 126 'b'));
      ("1000", mk Ws.Text (String.make 1000 'c'));
      ("70000 (64-bit len)", mk Ws.Text (String.make 70000 'd'));
      ("binary", mk Ws.Binary "\x00\x01\x02\xff");
      ("utf8", mk Ws.Text {js|café ✨ 🦊|js});
      ("rsv1 set", mk ~rsv1:true Ws.Text "compressed") ]
  in
  List.iter
    (fun (name, f) ->
      match roundtrip f with
      | Ws.Frame g ->
        eq (name ^ " opcode") g.Ws.opcode f.Ws.opcode;
        eq (name ^ " payload") g.Ws.payload f.Ws.payload;
        eq (name ^ " fin") g.Ws.fin f.Ws.fin;
        eq (name ^ " rsv1") g.Ws.rsv1 f.Ws.rsv1
      | _ -> check (name ^ " decoded") false)
    cases

let test_hardening () =
  print_endline "hardening:";
  (* an UNMASKED client frame must be rejected (server write_frame is unmasked) *)
  let unmasked =
    let buf = Buffer.create 16 in
    Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buf) (fun w ->
        Ws.write_frame w (mk Ws.Text "hi"));
    Buffer.contents buf
  in
  (match read_bytes unmasked with
   | Ws.Protocol_error _ -> check "unmasked frame -> protocol error" true
   | _ -> check "unmasked frame -> protocol error" false);

  (* a 64-bit length with the top bit set (>= 2^63) must be rejected before any
     huge read. Bytes: fin+text, masked+127, then 8 length bytes 0x80.. *)
  let huge_len =
    "\x81\xff\x80\x00\x00\x00\x00\x00\x00\x01" ^ "\x00\x00\x00\x00" (* mask *)
  in
  (match read_bytes huge_len with
   | Ws.Protocol_error _ -> check "huge 64-bit length -> protocol error" true
   | _ -> check "huge 64-bit length -> protocol error" false);

  (* a control frame (Close=0x8) longer than 125 must be rejected. We claim len
     126 (16-bit) on a control opcode. *)
  let big_control = "\x88\xfe\x00\x80" ^ "\x00\x00\x00\x00" (* mask *) ^ String.make 128 'x' in
  (match read_bytes big_control with
   | Ws.Protocol_error _ -> check "oversized control frame -> protocol error" true
   | _ -> check "oversized control frame -> protocol error" false);

  (* a fragmented control frame (fin=0 on Ping=0x9) must be rejected *)
  let frag_control = "\x09\x81" ^ "\x00\x00\x00\x00" ^ "\x00" in
  (* fin=0,ping; masked,len1; mask; 1 masked byte *)
  (match read_bytes frag_control with
   | Ws.Protocol_error _ -> check "fragmented control -> protocol error" true
   | _ -> check "fragmented control -> protocol error" false);

  (* reserved bits (rsv2/rsv3) set must be rejected *)
  let rsv_set = "\x91\x81" ^ "\x00\x00\x00\x00" ^ "\x00" in
  (* fin, rsv2 set (0x10), text; masked len1 *)
  (match read_bytes rsv_set with
   | Ws.Protocol_error _ -> check "reserved bits -> protocol error" true
   | _ -> check "reserved bits -> protocol error" false);

  (* empty stream -> Eof *)
  (match read_bytes "" with Ws.Eof -> check "empty -> Eof" true | _ -> check "empty -> Eof" false);

  (* close_frame carries the 2-byte code *)
  let cf = Ws.close_frame ~code:1002 () in
  eq "close_frame opcode" cf.Ws.opcode Ws.Close;
  eq "close_frame code bytes" cf.Ws.payload "\x03\xea" (* 1002 = 0x03ea *)

let () =
  Eio_main.run (fun _env ->
      test_accept ();
      test_roundtrip ();
      test_hardening ());
  if !fails = 0 then print_endline "all Ws tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
