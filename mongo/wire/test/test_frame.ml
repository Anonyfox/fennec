(* Build real OP_MSG and OP_QUERY frames by hand, parse them, and check the reassembled command + reply
   framing — the protocol envelope mongosh and drivers actually put on the wire. Covers: an OP_MSG
   command body; the moreToCome (fire-and-forget) flag; a kind-1 document sequence folded back into the
   command (how drivers send insert/update/delete batches); the legacy OP_QUERY handshake with $db
   injected from the namespace; reply byte layout (length + responseTo + round-trippable body); and
   bounds-safety on a short/garbage frame. *)
module F = Mongo_wire.Frame
module Bw = Mongo_wire.Bson_wire
module B = Bson

let i32_le n =
  let b = Bytes.create 4 in
  Bytes.set_int32_le b 0 (Int32.of_int n);
  Bytes.unsafe_to_string b

let read_i32 s off = Int32.to_int (String.get_int32_le s off)

(* assemble a frame: 16-byte header (len filled in) + body *)
let frame ~request_id ~opcode body =
  let total = 16 + String.length body in
  String.concat ""
    [ i32_le total; i32_le request_id; i32_le 0 (* responseTo *); i32_le opcode; body ]

let op_msg ?(request_id = 7) ?(flags = 0) ?(seqs = []) doc =
  let body = Buffer.create 64 in
  Buffer.add_string body (i32_le flags);
  Buffer.add_char body '\000';
  (* section kind 0 *)
  Buffer.add_string body (Bw.encode doc);
  List.iter
    (fun (ident, docs) ->
      let inner = Buffer.create 64 in
      Buffer.add_string inner ident;
      Buffer.add_char inner '\000';
      List.iter (fun d -> Buffer.add_string inner (Bw.encode d)) docs;
      let payload = Buffer.contents inner in
      Buffer.add_char body '\001';
      (* section kind 1 *)
      Buffer.add_string body (i32_le (4 + String.length payload));
      Buffer.add_string body payload)
    seqs;
  frame ~request_id ~opcode:2013 (Buffer.contents body)

let op_query ?(request_id = 9) ~ns doc =
  let body = Buffer.create 64 in
  Buffer.add_string body (i32_le 0);
  (* flags *)
  Buffer.add_string body ns;
  Buffer.add_char body '\000';
  Buffer.add_string body (i32_le 0);
  (* numberToSkip *)
  Buffer.add_string body (i32_le (-1));
  (* numberToReturn *)
  Buffer.add_string body (Bw.encode doc);
  frame ~request_id ~opcode:2004 (Buffer.contents body)

let () =
  (* 1. OP_MSG command body *)
  let i = F.parse (op_msg (B.doc [ ("hello", B.int 1); ("$db", B.str "admin") ])) in
  assert (F.command_name i.command = Some "hello");
  assert (F.db i.command = Some "admin");
  assert (i.reply_kind = F.Op_msg && i.request_id = 7 && not i.more_to_come);

  (* 2. moreToCome (bit 1): a fire-and-forget write expecting no reply *)
  let i = F.parse (op_msg ~flags:0x2 (B.doc [ ("insert", B.str "c"); ("$db", B.str "d") ])) in
  assert i.more_to_come;

  (* 3. kind-1 document sequence is folded back into the command as an array field *)
  let i =
    F.parse
      (op_msg
         ~seqs:[ ("documents", [ B.doc [ ("x", B.int 1) ]; B.doc [ ("x", B.int 2) ] ]) ]
         (B.doc [ ("insert", B.str "c"); ("$db", B.str "d") ]))
  in
  assert (F.command_name i.command = Some "insert");
  (match i.command with
   | B.Document f -> assert (List.assoc "documents" f = B.array [ B.doc [ ("x", B.int 1) ]; B.doc [ ("x", B.int 2) ] ])
   | _ -> assert false);

  (* 4. legacy OP_QUERY handshake: query doc is the command, $db injected from "<db>.$cmd" *)
  let i = F.parse (op_query ~ns:"admin.$cmd" (B.doc [ ("isMaster", B.int 1) ])) in
  assert (F.command_name i.command = Some "isMaster");
  assert (F.db i.command = Some "admin");
  assert (i.reply_kind = F.Op_reply);

  (* 5. reply: correct length + responseTo echoes the request id; body parses back *)
  let r = F.reply ~response_to:7 F.Op_msg (B.doc [ ("ok", B.Float 1.0) ]) in
  assert (read_i32 r 0 = String.length r);
  assert (read_i32 r 8 = 7);
  (* responseTo *)
  assert (read_i32 r 12 = 2013);
  assert ((F.parse r).command = B.doc [ ("ok", B.Float 1.0) ]);
  (* an OP_REPLY for the legacy path is well-formed too *)
  let r = F.reply ~response_to:9 F.Op_reply (B.doc [ ("ismaster", B.bool true); ("ok", B.Float 1.0) ]) in
  assert (read_i32 r 0 = String.length r && read_i32 r 8 = 9 && read_i32 r 12 = 1);

  (* 6. bounds-safety: short header and garbage opcode raise Protocol_error, never crash *)
  (match F.parse "abc" with _ -> assert false | exception F.Protocol_error _ -> ());
  (match F.parse (frame ~request_id:1 ~opcode:9999 "") with _ -> assert false | exception F.Protocol_error _ -> ());

  (* 7. fuzz — a malicious client must not crash or hang the parser (DoS posture: a bad frame drops only
     that connection). This parser is deliberately lenient (a short/garbage body yields a nonsense command
     that dispatch then rejects — case 6 covers the frames that DO raise), so the property here is
     TERMINATION: crafted-malformed bodies and pseudo-random garbage — raw messages and 2013 bodies alike —
     always return or raise-and-drop, never loop or crash the process. [raises] runs parse to completion. *)
  let raises f = match f () with _ -> false | exception _ -> true in
  let body_of s = frame ~request_id:1 ~opcode:2013 s in
  List.iter
    (fun body -> ignore (raises (fun () -> F.parse (body_of body))))
    [ "";                                (* empty body *)
      i32_le 0;                          (* flags but no section byte *)
      i32_le 0 ^ "\000";                 (* kind-0 marker, no BSON doc *)
      i32_le 0 ^ "\000" ^ i32_le 999999; (* kind-0 BSON claims 999999 bytes, only 4 present *)
      i32_le 0 ^ "\099" ^ "junk";        (* unsupported section kind *)
      i32_le 0 ^ "\001" ^ i32_le 4;      (* kind-1 section, empty payload *)
      String.make 3 '\000' ];            (* body shorter than the flags int32 *)
  Random.init 20260701;
  for _ = 1 to 1000 do
    let s = String.init (Random.int 96) (fun _ -> Char.chr (Random.int 256)) in
    ignore (raises (fun () -> F.parse s)) (* raw: short/garbage header *);
    ignore (raises (fun () -> F.parse (body_of s))) (* as a 2013 body *)
  done;

  print_string "frame (OP_MSG / OP_QUERY): OK\n"
