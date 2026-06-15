(* Spec-exact BSON binary codec for the MongoDB wire protocol — byte-for-byte what mongosh and every
   official driver send and expect on the wire.

   This is deliberately a SEPARATE codec from {!Burrow_binary}. Burrow's record codec optimises for
   compact, lossless on-disk storage and keeps [Binary]/[Decimal128] as their {!Bson.t} string forms;
   the wire has no such freedom — a driver decodes these bytes with libbson, so they must match the
   spec exactly:
     - [Binary] (0x05): int32 length + 1 subtype byte + the RAW payload bytes (we base64-decode the
       {!Bson.t} payload on the way out, and base64-encode the raw bytes on the way in);
     - [Decimal128] (0x13): the 16-byte IEEE-754 BID form. Not yet implemented — encountering one
       raises {!Unsupported}, which the server turns into a normal Mongo error reply rather than
       corrupting the stream. (Decimal128 only appears if a client explicitly sends NumberDecimal.)

   Everything else is byte-identical to spec BSON, so the structure mirrors {!Burrow_binary}. The module
   is pure (no IO, no backend) and bounds-safe: a malformed length makes [String.sub]/[String.get_*]
   raise [Invalid_argument] (they validate before allocating), which the caller catches per-message —
   so a hostile frame cannot allocate unboundedly, loop forever, or read out of bounds. *)

module B = Bson

exception Unsupported of string

(* ---- hex (Object_id: 24 hex chars <-> 12 bytes; Binary subtype: 2 hex chars <-> 1 byte) ------ *)

let hex_digit c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else invalid_arg "Bson_wire: bad hex digit"

let hex_to_bytes h =
  String.init 12 (fun i -> Char.chr ((hex_digit h.[i * 2] lsl 4) lor hex_digit h.[(i * 2) + 1]))

let hexchars = "0123456789abcdef"

let bytes_to_hex s =
  String.init 24 (fun i ->
      let byte = Char.code s.[i / 2] in
      if i land 1 = 0 then hexchars.[byte lsr 4] else hexchars.[byte land 0xf])

(* a 2-hex Binary subtype <-> its single byte *)
let subtype_to_byte st = if String.length st >= 2 then (hex_digit st.[0] lsl 4) lor hex_digit st.[1] else 0
let byte_to_subtype b = Printf.sprintf "%02x" (b land 0xff)

(* ---- encode -------------------------------------------------------------------------------- *)

let int32_min = -2147483648
let int32_max = 2147483647

let add_i32 buf n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Buffer.add_bytes buf b

let add_u32 buf n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int (n land 0xFFFFFFFF));
  Buffer.add_bytes buf b

let add_i64 buf n =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 n;
  Buffer.add_bytes buf b

(* spec BSON string: int32 (len+1) + bytes + NUL *)
let add_string buf s =
  add_i32 buf (String.length s + 1);
  Buffer.add_string buf s;
  Buffer.add_char buf '\000'

let tag_of = function
  | B.Float _ -> 0x01
  | B.String _ -> 0x02
  | B.Document _ -> 0x03
  | B.Array _ -> 0x04
  | B.Binary _ -> 0x05
  | B.Object_id _ -> 0x07
  | B.Bool _ -> 0x08
  | B.Date _ -> 0x09
  | B.Null -> 0x0A
  | B.Regex _ -> 0x0B
  | B.Code _ -> 0x0D
  | B.Symbol _ -> 0x0E
  | B.Code_with_scope _ -> 0x0F
  | B.Int n -> if n >= int32_min && n <= int32_max then 0x10 else 0x12
  | B.Timestamp _ -> 0x11
  | B.Int64 _ -> 0x12
  | B.Decimal128 _ -> 0x13
  | B.Min_key -> 0xFF
  | B.Max_key -> 0x7F

let rec emit_element buf name v =
  Buffer.add_char buf (Char.chr (tag_of v));
  Buffer.add_string buf name;
  Buffer.add_char buf '\000';
  emit_value buf v

and emit_value buf = function
  | B.Float f -> add_i64 buf (Int64.bits_of_float f)
  | B.String s | B.Code s | B.Symbol s -> add_string buf s
  | B.Document fields -> Buffer.add_string buf (encode_doc fields)
  | B.Array xs -> Buffer.add_string buf (encode_doc (List.mapi (fun i x -> (string_of_int i, x)) xs))
  | B.Binary { subtype; base64 } ->
    let raw = try Base64.decode_exn base64 with _ -> raise (Unsupported "Binary: malformed base64 payload") in
    add_i32 buf (String.length raw);
    Buffer.add_char buf (Char.chr (subtype_to_byte subtype));
    Buffer.add_string buf raw
  | B.Object_id h -> Buffer.add_string buf (hex_to_bytes h)
  | B.Bool b -> Buffer.add_char buf (if b then '\001' else '\000')
  | B.Date ms -> add_i64 buf ms
  | B.Null | B.Min_key | B.Max_key -> ()
  | B.Regex { pattern; options } ->
    Buffer.add_string buf pattern;
    Buffer.add_char buf '\000';
    Buffer.add_string buf options;
    Buffer.add_char buf '\000'
  | B.Code_with_scope (code, scope) ->
    let inner = Buffer.create 64 in
    add_string inner code;
    Buffer.add_string inner (encode_doc scope);
    let body = Buffer.contents inner in
    add_i32 buf (String.length body + 4);
    Buffer.add_string buf body
  | B.Int n -> if n >= int32_min && n <= int32_max then add_i32 buf n else add_i64 buf (Int64.of_int n)
  | B.Int64 n -> add_i64 buf n
  | B.Timestamp { t; i } ->
    add_u32 buf i;
    add_u32 buf t
  | B.Decimal128 _ -> raise (Unsupported "Decimal128 wire encoding")

and encode_doc (fields : (string * B.t) list) : string =
  let buf = Buffer.create 256 in
  List.iter (fun (name, v) -> emit_element buf name v) fields;
  Buffer.add_char buf '\000';
  let content = Buffer.contents buf in
  let total = String.length content + 4 in
  let out = Bytes.create total in
  Bytes.set_int32_le out 0 (Int32.of_int total);
  Bytes.blit_string content 0 out 4 (String.length content);
  Bytes.unsafe_to_string out

let encode = function
  | B.Document fields -> encode_doc fields
  | _ -> invalid_arg "Bson_wire.encode: top-level value must be a Document"

(* ---- read primitives (operate on the encoded string directly) ------------------------------ *)

let u8 s off = String.get_uint8 s off
let i32 s off = Int32.to_int (String.get_int32_le s off)
let u32 s off = Int32.to_int (String.get_int32_le s off) land 0xFFFFFFFF

(* spec BSON string at [off] (int32 len-with-NUL + bytes + NUL): (value, next_off) *)
let read_string_at s off =
  let len = i32 s off in
  (String.sub s (off + 4) (len - 1), off + 4 + len)

(* NUL-terminated string at [off]: (value, next_off past the NUL) *)
let read_cstring s off =
  let e = String.index_from s off '\000' in
  (String.sub s off (e - off), e + 1)

(* Bytes a value occupies, NOT counting the type byte or e_name. O(1) for nested docs/arrays (their
   leading int32 is the total size) and Binary (leading int32 + 1 subtype byte); a short walk only for
   Regex (two cstrings). *)
let value_size s tag off =
  match tag with
  | 0x01 | 0x09 | 0x11 | 0x12 -> 8
  | 0x02 | 0x0D | 0x0E -> 4 + i32 s off
  | 0x03 | 0x04 | 0x0F -> i32 s off
  | 0x05 -> 5 + i32 s off (* int32 len + 1 subtype byte + payload *)
  | 0x07 -> 12
  | 0x08 -> 1
  | 0x0A | 0xFF | 0x7F -> 0
  | 0x0B ->
    let e1 = String.index_from s off '\000' in
    let e2 = String.index_from s (e1 + 1) '\000' in
    e2 + 1 - off
  | 0x10 -> 4
  | 0x13 -> 16
  | _ -> failwith "Bson_wire: bad tag in value_size"

let rec read_value s tag off : B.t =
  match tag with
  | 0x01 -> B.Float (Int64.float_of_bits (String.get_int64_le s off))
  | 0x02 -> B.String (fst (read_string_at s off))
  | 0x03 -> B.Document (decode_doc_at s off)
  | 0x04 -> B.Array (List.map snd (decode_doc_at s off))
  | 0x05 ->
    let len = i32 s off in
    let sub = u8 s (off + 4) in
    let raw = String.sub s (off + 5) len in
    B.Binary { subtype = byte_to_subtype sub; base64 = Base64.encode_string raw }
  | 0x07 -> B.Object_id (bytes_to_hex (String.sub s off 12))
  | 0x08 -> B.Bool (u8 s off <> 0)
  | 0x09 -> B.Date (String.get_int64_le s off)
  | 0x0A -> B.Null
  | 0x0B ->
    let pattern, off2 = read_cstring s off in
    let options, _ = read_cstring s off2 in
    B.Regex { pattern; options }
  | 0x0D -> B.Code (fst (read_string_at s off))
  | 0x0E -> B.Symbol (fst (read_string_at s off))
  | 0x0F ->
    let code, off2 = read_string_at s (off + 4) in
    B.Code_with_scope (code, decode_doc_at s off2)
  | 0x10 -> B.Int (i32 s off)
  | 0x11 ->
    let i = u32 s off and t = u32 s (off + 4) in
    B.Timestamp { t; i }
  | 0x12 -> B.Int64 (String.get_int64_le s off)
  | 0x13 -> raise (Unsupported "Decimal128 wire decoding")
  | 0xFF -> B.Min_key
  | 0x7F -> B.Max_key
  | _ -> failwith "Bson_wire: bad tag"

(* iterate elements from [pos] (a type byte) until the 0x00 terminator *)
and decode_fields s pos : (string * B.t) list =
  let rec loop pos acc =
    let tag = u8 s pos in
    if tag = 0 then List.rev acc
    else
      let name, voff = read_cstring s (pos + 1) in
      let v = read_value s tag voff in
      loop (voff + value_size s tag voff) ((name, v) :: acc)
  in
  loop pos []

(* embedded document at [off] (skip its leading int32 length) *)
and decode_doc_at s off = decode_fields s (off + 4)

let decode_doc (s : string) : (string * B.t) list = decode_fields s 4
let decode (s : string) : B.t = B.Document (decode_doc s)

(* the total byte length of the document whose leading int32 length sits at [off] *)
let doc_len s off = i32 s off

(* decode the document at [off] and return it together with the offset just past it — for walking the
   sections of an OP_MSG frame, where several documents sit back-to-back in one buffer *)
let read_doc_at s off : B.t * int = (B.Document (decode_doc_at s off), off + doc_len s off)
