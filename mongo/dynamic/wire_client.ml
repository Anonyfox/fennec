(* A minimal MongoDB wire-protocol CLIENT: connect to a wire endpoint and run one command synchronously,
   returning the reply document. Reuses {!Mongo_wire.Frame} — OP_MSG is symmetric, so the same codec that
   frames a server reply frames a client request, and [Frame.parse] reads the reply. Optional SCRAM-SHA-256
   authentication ([~auth]) via {!Mongo_wire.Scram}'s client side; without it, plaintext for loopback /
   trusted-network use. One in-flight request per connection (synchronous request/reply). *)

module Frame = Mongo_wire.Frame
module Scram = Mongo_wire.Scram
module B = Bson

type t = { send : string -> unit; r : Eio.Buf_read.t }

(* run one command (which must carry its own [$db] field for routing); returns the reply document *)
let run t (cmd : B.t) : B.t =
  t.send (Frame.reply ~response_to:0 Frame.Op_msg cmd);
  let hdr = Eio.Buf_read.take 4 t.r in
  let len = Frame.message_length hdr in
  let inc = Frame.parse (hdr ^ Eio.Buf_read.take (len - 4) t.r) in
  inc.Frame.command

let rng_ready = ref false
let ensure_rng () = if not !rng_ready then (Mirage_crypto_rng_unix.use_default (); rng_ready := true)

let bin s = B.Binary { subtype = "00"; base64 = Base64.encode_string s }

let sasl_payload reply =
  match B.get reply "payload" with
  | Some (B.Binary { base64; _ }) -> ( try Base64.decode_exn base64 with _ -> failwith "wire client: malformed SASL payload")
  | _ -> failwith "wire client: missing SASL payload"

(* the SCRAM-SHA-256 client handshake: saslStart (client-first) -> saslContinue (client-final) -> verify the
   server's signature (mutual auth). Raises on a failed handshake. *)
let authenticate t ~user ~password =
  ensure_rng ();
  let nonce = Base64.encode_string (Mirage_crypto_rng.generate 18) in
  let bare = Scram.client_first_bare ~user ~nonce in
  let r1 =
    run t
      (B.Document
         [ ("saslStart", B.Int 1); ("mechanism", B.String "SCRAM-SHA-256"); ("payload", bin ("n,," ^ bare));
           ("$db", B.String "admin") ])
  in
  let conv = Option.value ~default:(B.Int 1) (B.get r1 "conversationId") in
  let final, expected = Scram.client_final ~password ~client_first_bare:bare ~server_first:(sasl_payload r1) in
  let r2 = run t (B.Document [ ("saslContinue", B.Int 1); ("conversationId", conv); ("payload", bin final); ("$db", B.String "admin") ]) in
  if not (Scram.verify_server_final ~expected_server_sig:expected (sasl_payload r2)) then
    failwith "wire client: SCRAM server verification failed"

let connect ?auth ~sw ~net addr =
  let flow = Eio.Net.connect ~sw net addr in
  let t = { send = (fun s -> Eio.Flow.copy_string s flow); r = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) } in
  (match auth with Some (user, password) -> authenticate t ~user ~password | None -> ());
  t
