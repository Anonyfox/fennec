(* See console_client.mli. A thin Unix-socket client speaking Console_protocol frames. *)

module P = Console_protocol

type t = { fd : Unix.file_descr; reader : P.reply P.Reader.t; buf : bytes }

let connect path =
  let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (try Unix.connect fd (Unix.ADDR_UNIX path)
   with e -> (try Unix.close fd with _ -> ()); raise e);
  { fd; reader = P.Reader.for_replies (); buf = Bytes.create 8192 }

let fd t = t.fd

let send t (r : P.request) =
  let s = P.frame_request r in
  let n = String.length s in
  let rec go off = if off < n then go (off + Unix.write_substring t.fd s off (n - off)) in
  (try go 0 with _ -> ())

let receive t =
  match Unix.read t.fd t.buf 0 (Bytes.length t.buf) with
  | 0 -> `Closed
  | n ->
    P.Reader.feed t.reader (Bytes.sub_string t.buf 0 n);
    let rec drain acc = match P.Reader.next t.reader with Some r -> drain (r :: acc) | None -> List.rev acc in
    `Replies (drain [])
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> `Replies []
  | exception _ -> `Closed

let close t = try Unix.close t.fd with _ -> ()
