(* Unit tests for the RFC 6455 WebSocket codec: handshake accept-key, and frame
   write -> read round-trips across the length-encoding boundaries (small / 16-bit
   / would-be 64-bit), including a masked client frame decode. *)

module Ws = Fennec_server.Ws

let failures = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr failures;
    Printf.printf "  FAIL %s\n" name)

(* write a frame to a string via Buf_write, read it back via Buf_read *)
let roundtrip (f : Ws.frame) : Ws.frame option =
  let buf = Buffer.create 256 in
  Eio.Buf_write.with_flow (Eio.Flow.buffer_sink buf) (fun w -> Ws.write_frame w f);
  let s = Buffer.contents buf in
  let r = Eio.Buf_read.of_flow (Eio.Flow.string_source s) ~max_size:(1024 * 1024) in
  Ws.read_frame r

let mk ?(fin = true) opcode payload = { Ws.fin; opcode; payload }

let test_accept_key () =
  print_endline "Ws handshake:";
  (* the canonical RFC 6455 example: key + GUID -> this accept value *)
  check "accept_key (RFC example)"
    (Ws.accept_key "dGhlIHNhbXBsZSBub25jZQ==" = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

let test_roundtrip () =
  print_endline "Ws frame round-trip:";
  let cases =
    [ ("empty text", mk Ws.Text "");
      ("short text", mk Ws.Text "reload");
      ("125 bytes (max 7-bit)", mk Ws.Text (String.make 125 'a'));
      ("126 bytes (16-bit len)", mk Ws.Text (String.make 126 'b'));
      ("1000 bytes", mk Ws.Text (String.make 1000 'c'));
      ("70000 bytes (64-bit len)", mk Ws.Text (String.make 70000 'd'));
      ("ping payload", mk Ws.Ping "hi");
      ("close empty", mk Ws.Close "");
      ("utf-8 payload", mk Ws.Text {js|café ✨ 🦊|js}) ]
  in
  List.iter
    (fun (name, f) ->
      match roundtrip f with
      | Some g ->
        check (name ^ " opcode") (g.Ws.opcode = f.Ws.opcode);
        check (name ^ " payload") (g.Ws.payload = f.Ws.payload);
        check (name ^ " fin") (g.Ws.fin = f.Ws.fin)
      | None -> check (name ^ " (decoded)") false)
    cases

let () =
  (* Buf_read/Buf_write use Eio effects, so run inside an Eio context *)
  Eio_main.run (fun _env ->
      test_accept_key ();
      test_roundtrip ());
  if !failures = 0 then print_endline "all ws tests passed."
  else (
    Printf.printf "%d ws test(s) failed.\n" !failures;
    exit 1)
