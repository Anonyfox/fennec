(* A minimal RFC 6455 WebSocket codec over Eio buffered IO: the upgrade handshake
   and the frame read/write needed to carry text messages (livereload now; the
   DDP link later). Server->client frames are unmasked; client->server frames MUST
   be masked (RFC 6455 §5.1) and we unmask them. Sized for protocol traffic, not
   file streaming — frames over [max_frame_size] are rejected. *)

type opcode = Continuation | Text | Binary | Close | Ping | Pong | Other of int

let opcode_of_int = function
  | 0x0 -> Continuation
  | 0x1 -> Text
  | 0x2 -> Binary
  | 0x8 -> Close
  | 0x9 -> Ping
  | 0xA -> Pong
  | n -> Other n

let int_of_opcode = function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xA
  | Other n -> n

let is_control = function Close | Ping | Pong -> true | _ -> false

(* rsv1 carries the permessage-deflate "compressed" bit (RFC 7692) *)
type frame = { fin : bool; rsv1 : bool; opcode : opcode; payload : string }

(* the outcome of reading a frame: a valid frame, a clean end-of-stream, or a
   protocol violation the caller should answer with a Close (code 1002/1009). *)
type read_result =
  | Frame of frame
  | Eof
  | Protocol_error of string

(* Per-frame and per-message payload ceilings. Control frames are capped at 125
   bytes by the spec; data frames/messages are bounded so a hostile length field
   can't force a huge allocation. 16 MiB is generous for protocol traffic. *)
let max_frame_size = 16 * 1024 * 1024
let max_message_size = 16 * 1024 * 1024

(* ---- handshake ----------------------------------------------------------- *)

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_key (key : string) : string =
  Base64.encode_string (Digestif.SHA1.to_raw_string (Digestif.SHA1.digest_string (key ^ guid)))

(* ---- frame read ---------------------------------------------------------- *)

let u8 r = Char.code (Eio.Buf_read.any_char r)

(* Read a single frame, enforcing RFC 6455 invariants:
   - the 64-bit extended length must have its top bit clear (§5.2) and fit our
     [max_frame_size]; a length that would overflow OCaml's native int or exceed
     the cap is a protocol error rather than a giant [take];
   - client->server frames MUST be masked;
   - control frames must be <=125 bytes and not fragmented (§5.5). *)
let read_frame (r : Eio.Buf_read.t) : read_result =
  if Eio.Buf_read.at_end_of_input r then Eof
  else
    match
      let b0 = u8 r in
      let b1 = u8 r in
      let fin = b0 land 0x80 <> 0 in
      let rsv1 = b0 land 0x40 <> 0 in
      let rsv_other = b0 land 0x30 <> 0 in
      let opcode = opcode_of_int (b0 land 0x0f) in
      let masked = b1 land 0x80 <> 0 in
      let len0 = b1 land 0x7f in
      (* extended length *)
      let len =
        if len0 < 126 then `Ok len0
        else if len0 = 126 then (
          let s = Eio.Buf_read.take 2 r in
          `Ok ((Char.code s.[0] lsl 8) lor Char.code s.[1]))
        else (
          let s = Eio.Buf_read.take 8 r in
          (* reject any length with the high bit set (negative as native int) or
             exceeding the frame cap, BEFORE reading the body *)
          if Char.code s.[0] land 0x80 <> 0 then `Too_large
          else
            let v = ref 0 in
            String.iter (fun c -> v := (!v lsl 8) lor Char.code c) s;
            if !v > max_frame_size || !v < 0 then `Too_large else `Ok !v)
      in
      match len with
      | `Too_large -> Protocol_error "frame too large"
      | `Ok len ->
        if len > max_frame_size then Protocol_error "frame too large"
        else if rsv_other then Protocol_error "reserved bits set"
        else if (not masked) then Protocol_error "client frame not masked"
        else if is_control opcode && (len > 125 || not fin) then
          Protocol_error "invalid control frame"
        else begin
          let mask = Eio.Buf_read.take 4 r in
          let payload = if len = 0 then "" else Eio.Buf_read.take len r in
          let payload =
            String.mapi
              (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3]))
              payload
          in
          Frame { fin; rsv1; opcode; payload }
        end
    with
    | result -> result
    | exception (End_of_file | Failure _) -> Eof
    | exception Invalid_argument _ -> Protocol_error "malformed frame"

(* ---- frame write (server -> client, unmasked) ---------------------------- *)

let write_frame (w : Eio.Buf_write.t) (f : frame) : unit =
  Eio.Buf_write.uint8 w
    ((if f.fin then 0x80 else 0) lor (if f.rsv1 then 0x40 else 0) lor int_of_opcode f.opcode);
  let len = String.length f.payload in
  if len < 126 then Eio.Buf_write.uint8 w len
  else if len < 65536 then (
    Eio.Buf_write.uint8 w 126;
    Eio.Buf_write.uint8 w (len lsr 8);
    Eio.Buf_write.uint8 w (len land 0xff))
  else (
    Eio.Buf_write.uint8 w 127;
    for i = 7 downto 0 do
      Eio.Buf_write.uint8 w ((len lsr (i * 8)) land 0xff)
    done);
  Eio.Buf_write.string w f.payload

(* a Close frame with a status code (RFC 6455 §5.5.1: 2-byte big-endian code) *)
let close_frame ?(code = 1000) () : frame =
  let payload = Printf.sprintf "%c%c" (Char.chr ((code lsr 8) land 0xff)) (Char.chr (code land 0xff)) in
  { fin = true; rsv1 = false; opcode = Close; payload }

(* Write a MASKED frame, as a client would (a fixed mask for determinism). Used
   by tests to exercise [read_frame], which requires client frames be masked. *)
let write_masked_frame ?(mask = "\x12\x34\x56\x78") (w : Eio.Buf_write.t) (f : frame) : unit =
  Eio.Buf_write.uint8 w
    ((if f.fin then 0x80 else 0) lor (if f.rsv1 then 0x40 else 0) lor int_of_opcode f.opcode);
  let len = String.length f.payload in
  (if len < 126 then Eio.Buf_write.uint8 w (0x80 lor len)
   else if len < 65536 then (
     Eio.Buf_write.uint8 w (0x80 lor 126);
     Eio.Buf_write.uint8 w (len lsr 8);
     Eio.Buf_write.uint8 w (len land 0xff))
   else (
     Eio.Buf_write.uint8 w (0x80 lor 127);
     for i = 7 downto 0 do
       Eio.Buf_write.uint8 w ((len lsr (i * 8)) land 0xff)
     done));
  Eio.Buf_write.string w mask;
  String.iteri
    (fun i c -> Eio.Buf_write.uint8 w (Char.code c lxor Char.code mask.[i land 3]))
    f.payload

(* ══════════════════════════════════════════════════════════════════════════ *)
(*  Inline tests — Eio_main.run wraps each test (Buf_write needs an effect     *)
(*  handler). Each test gets its own fresh event loop; no nesting.             *)
(* ══════════════════════════════════════════════════════════════════════════ *)

let roundtrip_ f = Eio_main.run @@ fun _env ->
  let buf = Buffer.create 256 in
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buf) (fun w -> write_masked_frame w f);
  let s = Buffer.contents buf in
  let r = Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(64 * 1024 * 1024) in
  read_frame r

let read_bytes_ s = Eio_main.run @@ fun _env ->
  let r = Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(64 * 1024 * 1024) in
  read_frame r

let mk_ ?(fin = true) ?(rsv1 = false) opcode payload = { fin; rsv1; opcode; payload }

(* ──── accept_key ──── *)

let%test "RFC accept-key" = accept_key "dGhlIHNhbXBsZSBub25jZQ==" = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

(* ──── frame round-trip ──── *)

(* Round-trip law: any well-formed data frame survives write→read. Covers fin/rsv1, all three
   data opcodes, binary payloads (Gen.char is the full 0..255 range), and the 7-bit↔16-bit length
   boundary (lengths ≤ 1000) — subsuming the old empty/short/125/126/binary/rsv1 examples. *)
let%prop "a well-formed frame survives write→read (masked)" =
  let open Fennec_hunt_prop in
  let frame_gen =
    Gen.(map
           (fun (opcode, payload, (fin, rsv1)) -> { fin; rsv1; opcode; payload })
           (triple (oneof_list [ Continuation; Text; Binary ]) (string_size (int_range 0 1000)) (pair bool bool)))
  in
  forall ~print:(fun f -> Printf.sprintf "{fin=%b rsv1=%b |payload|=%d}" f.fin f.rsv1 (String.length f.payload))
    frame_gen
    (fun f -> match roundtrip_ f with Frame g -> g = f | _ -> false)

(* the 64-bit length path (payload > 65535) stays an explicit example — too big to generate 100×. *)
let%test_unit "round-trip: 70000 (64-bit len)" =
  let s = String.make 70000 'd' in match roundtrip_ (mk_ Text s) with Frame g -> Fennec_hunt_unit.check "len" (g.payload = s) | _ -> failwith "decode"

(* ──── hardening ──── *)

let%test "unmasked frame → protocol error" =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 16 in
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buf) (fun w -> write_frame w (mk_ Text "hi"));
  match read_bytes_ (Buffer.contents buf) with Protocol_error _ -> true | _ -> false

let%test "huge 64-bit length → protocol error" =
  match read_bytes_ "\x81\xff\x80\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00" with Protocol_error _ -> true | _ -> false

let%test "oversized control frame → protocol error" =
  match read_bytes_ ("\x88\xfe\x00\x80\x00\x00\x00\x00" ^ String.make 128 'x') with Protocol_error _ -> true | _ -> false

let%test "fragmented control → protocol error" =
  match read_bytes_ "\x09\x81\x00\x00\x00\x00\x00" with Protocol_error _ -> true | _ -> false

let%test "reserved bits → protocol error" =
  match read_bytes_ "\x91\x81\x00\x00\x00\x00\x00" with Protocol_error _ -> true | _ -> false

let%test "empty → Eof" = match read_bytes_ "" with Eof -> true | _ -> false

(* ──── close_frame ──── *)

let%test "close_frame opcode" = (close_frame ~code:1002 ()).opcode = Close
let%test "close_frame code bytes" = (close_frame ~code:1002 ()).payload = "\x03\xea"
