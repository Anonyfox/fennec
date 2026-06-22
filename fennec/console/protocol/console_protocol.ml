(* See console_protocol.mli. Frames are [4-byte big-endian length][marshaled payload]; the payload is
   one [request]/[reply] value. Both ends link this library, so Marshal round-trips the exact types —
   the contract is the type, not a hand-written codec. *)

type request =
  | Hello of { cols : int }
  | Eval of { id : int; phrase : string }
  | Cancel of { id : int }
  | Complete of { id : int; prefix : string }

type banner = { app : string; backend : string; version : string; opened : string list }

type reply =
  | Banner of banner
  | Output of { id : int; text : string; is_error : bool }
  | Done of { id : int }
  | Completions of { id : int; items : string list }

(* a generous cap so a corrupt length can't make us allocate the world; far above any real frame *)
let max_frame = 64 * 1024 * 1024

let frame (payload : string) : string =
  let n = String.length payload in
  let b = Bytes.create (4 + n) in
  Bytes.set_int32_be b 0 (Int32.of_int n);
  Bytes.blit_string payload 0 b 4 n;
  Bytes.unsafe_to_string b

let frame_request (r : request) : string = frame (Marshal.to_string r [])
let frame_reply (r : reply) : string = frame (Marshal.to_string r [])

module Reader = struct
  type 'a t = { buf : Buffer.t; decode : string -> 'a }

  let create decode = { buf = Buffer.create 256; decode }
  let for_requests () : request t = create (fun s -> (Marshal.from_string s 0 : request))
  let for_replies () : reply t = create (fun s -> (Marshal.from_string s 0 : reply))

  let feed t s = Buffer.add_string t.buf s

  let next t =
    let contents = Buffer.contents t.buf in
    let len = String.length contents in
    if len < 4 then None
    else
      let n = Int32.to_int (String.get_int32_be contents 0) in
      if n < 0 || n > max_frame then failwith "console_protocol: corrupt frame length";
      if len < 4 + n then None
      else begin
        let payload = String.sub contents 4 n in
        (* keep whatever followed this frame for the next [next] *)
        Buffer.clear t.buf;
        Buffer.add_substring t.buf contents (4 + n) (len - 4 - n);
        Some (t.decode payload)
      end
end

(* ── tests ─────────────────────────────────────────────────────────────────────────────────────── *)

let%test "a request round-trips through frame + reader" =
  let rd = Reader.for_requests () in
  Reader.feed rd (frame_request (Eval { id = 7; phrase = "1 + 1" }));
  match Reader.next rd with Some (Eval { id = 7; phrase = "1 + 1" }) -> true | _ -> false

let%test "a reply round-trips, preserving every field" =
  let rd = Reader.for_replies () in
  Reader.feed rd (frame_reply (Banner { app = "demo"; backend = "burrow://."; version = "0.0.1"; opened = [ "Fennec"; "Paw" ] }));
  match Reader.next rd with
  | Some (Banner { app = "demo"; opened = [ "Fennec"; "Paw" ]; _ }) -> true
  | _ -> false

let%test "next is None until a full frame has arrived (partial feeds buffer)" =
  let whole = frame_reply (Output { id = 1; text = "hello"; is_error = false }) in
  let mid = String.length whole / 2 in
  let rd = Reader.for_replies () in
  Reader.feed rd (String.sub whole 0 mid);
  let none = Reader.next rd = None in
  Reader.feed rd (String.sub whole mid (String.length whole - mid));
  let got = match Reader.next rd with Some (Output { text = "hello"; _ }) -> true | _ -> false in
  none && got

let%test "two frames concatenated in one feed are both extracted, in order" =
  let rd = Reader.for_requests () in
  Reader.feed rd (frame_request (Cancel { id = 1 }) ^ frame_request (Complete { id = 2; prefix = "Pulse." }));
  match (Reader.next rd, Reader.next rd, Reader.next rd) with
  | Some (Cancel { id = 1 }), Some (Complete { id = 2; prefix = "Pulse." }), None -> true
  | _ -> false
