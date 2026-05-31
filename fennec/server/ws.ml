(* A minimal RFC 6455 WebSocket codec over Eio buffered IO: the upgrade handshake
   and the frame read/write needed to carry text messages (livereload now; the
   DDP link later). Server->client frames are unmasked; client->server frames are
   masked and we unmask them. Sized for protocol traffic, not file streaming. *)

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

(* rsv1 carries the permessage-deflate "compressed" bit (RFC 7692) *)
type frame = { fin : bool; rsv1 : bool; opcode : opcode; payload : string }

(* ---- handshake ----------------------------------------------------------- *)

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let accept_key (key : string) : string =
  Base64.encode_string (Digestif.SHA1.to_raw_string (Digestif.SHA1.digest_string (key ^ guid)))

(* ---- frame read ---------------------------------------------------------- *)

let u8 r = Char.code (Eio.Buf_read.any_char r)

let read_frame (r : Eio.Buf_read.t) : frame option =
  if Eio.Buf_read.at_end_of_input r then None
  else
    let b0 = u8 r in
    let b1 = u8 r in
    let fin = b0 land 0x80 <> 0 in
    let rsv1 = b0 land 0x40 <> 0 in
    let opcode = opcode_of_int (b0 land 0x0f) in
    let masked = b1 land 0x80 <> 0 in
    let len0 = b1 land 0x7f in
    let len =
      if len0 < 126 then len0
      else if len0 = 126 then (
        let s = Eio.Buf_read.take 2 r in
        (Char.code s.[0] lsl 8) lor Char.code s.[1])
      else (
        let s = Eio.Buf_read.take 8 r in
        let v = ref 0 in
        String.iter (fun c -> v := (!v lsl 8) lor Char.code c) s;
        !v)
    in
    let mask = if masked then Eio.Buf_read.take 4 r else "" in
    let payload = if len = 0 then "" else Eio.Buf_read.take len r in
    let payload =
      if masked then
        String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code mask.[i land 3])) payload
      else payload
    in
    Some { fin; rsv1; opcode; payload }

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
