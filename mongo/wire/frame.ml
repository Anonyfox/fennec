(* MongoDB wire-protocol message framing — pure bytes <-> structured message.

   A wire message is a 16-byte header (messageLength, requestID, responseTo, opCode; all little-endian
   int32) followed by an opcode-specific body. We handle exactly what a modern client needs:

     - OP_MSG (2013): everything after the handshake. Body = uint32 flagBits + one or more sections.
       Section kind 0 is the command document; section kind 1 is a "document sequence" (an int32 size,
       a cstring identifier like "documents"/"updates"/"deletes", then back-to-back BSON docs) that the
       driver split out of the command for efficiency. We reassemble the logical command by folding each
       kind-1 sequence back in as an array field named by its identifier. An optional trailing CRC-32C
       (flag bit 0) is skipped on read and never written (TLS, not CRC, provides integrity).

     - OP_QUERY (2004): the legacy handshake some clients still open with, querying "<db>.$cmd" with an
       {isMaster}/{hello} document before they know our wire version. We parse the query doc as the
       command and inject "$db" from the namespace so downstream sees the same shape as OP_MSG. The
       reply for these goes back as the legacy OP_REPLY (1).

   Pure and bounds-safe: every read is bounds-checked by [String.get_*]/[String.sub]/[String.index_from]
   (they raise [Invalid_argument]/[Not_found] rather than over-read), surfaced here as {!Protocol_error}
   for the server to turn into a clean connection-level error. The server enforces the max message size
   before handing us a frame. *)

module Bw = Bson_wire

exception Protocol_error of string

let op_msg = 2013
let op_query = 2004
let op_reply = 1
let op_compressed = 2012

(* ---- little-endian readers (header + section lengths/flags/kinds) -------------------------- *)

let i32 s off = try Int32.to_int (String.get_int32_le s off) with _ -> raise (Protocol_error "truncated int32")
let u32 s off = i32 s off land 0xFFFFFFFF
let u8 s off = try String.get_uint8 s off with _ -> raise (Protocol_error "truncated byte")

let read_cstring s off =
  match String.index_from_opt s off '\000' with
  | Some e -> (String.sub s off (e - off), e + 1)
  | None -> raise (Protocol_error "unterminated cstring")

let read_doc s off = try Bw.read_doc_at s off with _ -> raise (Protocol_error "malformed BSON section")

(* ---- writers ------------------------------------------------------------------------------- *)

let add_i32 buf n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Buffer.add_bytes buf b

let add_i64 buf n =
  let b = Bytes.create 8 in
  Bytes.set_int64_le b 0 n;
  Buffer.add_bytes buf b

(* ---- parse --------------------------------------------------------------------------------- *)

(* the framing the reply must mirror — OP_MSG requests get OP_MSG replies, legacy OP_QUERY gets OP_REPLY *)
type reply_kind = Op_msg | Op_reply

type incoming = {
  request_id : int;          (* the request's requestID — echoed as the reply's responseTo *)
  command : Bson.t;          (* the reassembled command document (kind-0 body + kind-1 sequences) *)
  reply_kind : reply_kind;
  more_to_come : bool;       (* OP_MSG moreToCome: a fire-and-forget write expecting no reply *)
}

let fields = function Bson.Document f -> f | _ -> []

let parse_op_msg frame request_id =
  let flags = u32 frame 16 in
  let more_to_come = flags land 0x2 <> 0 in
  let checksummed = flags land 0x1 <> 0 in
  let msg_len = i32 frame 0 in
  let body_end = if checksummed then msg_len - 4 else msg_len in
  if body_end > String.length frame then raise (Protocol_error "messageLength exceeds frame");
  let rec walk off body seqs =
    if off >= body_end then (body, seqs)
    else
      match u8 frame off with
      | 0 ->
        (* the command body: exactly one per message in practice *)
        let doc, next = read_doc frame (off + 1) in
        walk next (body @ fields doc) seqs
      | 1 ->
        (* document sequence: int32 size (incl itself) + cstring identifier + docs until size consumed *)
        let sec_len = i32 frame (off + 1) in
        let sec_end = off + 1 + sec_len in
        if sec_len < 5 || sec_end > body_end then raise (Protocol_error "bad document sequence");
        let ident, doff = read_cstring frame (off + 5) in
        let rec docs off acc =
          if off >= sec_end then List.rev acc
          else
            let d, n = read_doc frame off in
            docs n (d :: acc)
        in
        walk sec_end body ((ident, Bson.Array (docs doff [])) :: seqs)
      | k -> raise (Protocol_error (Printf.sprintf "unknown OP_MSG section kind %d" k))
  in
  let body, seqs = walk 20 [] [] in
  { request_id; command = Bson.Document (body @ seqs); reply_kind = Op_msg; more_to_come }

let parse_op_query frame request_id =
  (* flags(4)@16, fullCollectionName cstring@20, numberToSkip(4), numberToReturn(4), query doc *)
  let ns, off = read_cstring frame 20 in
  let off = off + 8 in
  let query, _ = read_doc frame off in
  (* "<db>.$cmd" -> inject $db so the dispatcher reads the db uniformly with OP_MSG *)
  let db = match String.index_opt ns '.' with Some i -> String.sub ns 0 i | None -> ns in
  let f = fields query in
  let command = if List.mem_assoc "$db" f then query else Bson.Document (f @ [ ("$db", Bson.String db) ]) in
  { request_id; command; reply_kind = Op_reply; more_to_come = false }

let parse (frame : string) : incoming =
  if String.length frame < 16 then raise (Protocol_error "short header");
  let request_id = i32 frame 4 in
  let opcode = i32 frame 12 in
  if opcode = op_msg then parse_op_msg frame request_id
  else if opcode = op_query then parse_op_query frame request_id
  else if opcode = op_compressed then raise (Protocol_error "OP_COMPRESSED not supported")
  else raise (Protocol_error (Printf.sprintf "unsupported opcode %d" opcode))

(* the total message length declared in the first 4 bytes — the server reads this, then the remainder *)
let message_length (header : string) : int = i32 header 0

(* ---- serialize a reply --------------------------------------------------------------------- *)

let reply ~response_to (kind : reply_kind) (doc : Bson.t) : string =
  let body = Buffer.create 256 in
  (match kind with
   | Op_msg ->
     add_i32 body 0;                  (* flagBits = 0 *)
     Buffer.add_char body '\000';     (* section kind 0 *)
     Buffer.add_string body (Bw.encode doc)
   | Op_reply ->
     add_i32 body 0;                  (* responseFlags *)
     add_i64 body 0L;                 (* cursorID = 0 (command replies carry no legacy cursor) *)
     add_i32 body 0;                  (* startingFrom *)
     add_i32 body 1;                  (* numberReturned = 1 *)
     Buffer.add_string body (Bw.encode doc));
  let body = Buffer.contents body in
  let opcode = match kind with Op_msg -> op_msg | Op_reply -> op_reply in
  let out = Buffer.create (16 + String.length body) in
  add_i32 out (16 + String.length body);  (* messageLength *)
  add_i32 out 0;                           (* requestID (reply's own; clients match on responseTo) *)
  add_i32 out response_to;
  add_i32 out opcode;
  Buffer.add_string out body;
  Buffer.contents out

(* ---- small command helpers (used by the dispatcher) ---------------------------------------- *)

(* the command name is the FIRST field of the command document (Mongo convention) *)
let command_name (command : Bson.t) : string option =
  match command with Bson.Document ((name, _) :: _) -> Some name | _ -> None

let db (command : Bson.t) : string option =
  match command with
  | Bson.Document f -> ( match List.assoc_opt "$db" f with Some (Bson.String s) -> Some s | _ -> None)
  | _ -> None
