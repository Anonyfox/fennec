(* A minimal MongoDB wire-protocol CLIENT: connect to a wire endpoint and run one command synchronously,
   returning the reply document. Reuses {!Mongo_wire.Frame} — OP_MSG is symmetric, so the same codec that
   frames a server reply frames a client request, and [Frame.parse] reads the reply. UNAUTHENTICATED — for
   loopback / trusted-network use (e.g. a same-host replica pulling the oplog); SCRAM client auth is a
   future extension. One in-flight request per connection (synchronous request/reply). *)

module Frame = Mongo_wire.Frame
module B = Bson

type t = { send : string -> unit; r : Eio.Buf_read.t }

let connect ~sw ~net addr =
  let flow = Eio.Net.connect ~sw net addr in
  { send = (fun s -> Eio.Flow.copy_string s flow); r = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) }

(* run one command (which must carry its own [$db] field for routing); returns the reply document *)
let run t (cmd : B.t) : B.t =
  t.send (Frame.reply ~response_to:0 Frame.Op_msg cmd);
  let hdr = Eio.Buf_read.take 4 t.r in
  let len = Frame.message_length hdr in
  let inc = Frame.parse (hdr ^ Eio.Buf_read.take (len - 4) t.r) in
  inc.Frame.command
