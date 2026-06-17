(* The zero-copy request-HEAD parser: a thin OCaml face over the hand-crafted C stub
   ({!file:http_parse_stubs.c}). It scans the request line + headers in place over an Eio buffer and
   writes byte offsets into a preallocated int array — zero allocation, reentrant (parallel-safe),
   and crash-safe (every byte read is bounds-checked in C). See the C file for the full contract. *)

(* [parse buf off len out maxh] parses the request head in [buf]'s window [off, off+len) into [out]
   (capacity [7 + 4*maxh]). Returns the head byte length, or {!incomplete}/{!malformed}/{!too_many}. *)
external parse : Bigstringaf.t -> int -> int -> int array -> int -> int = "paw_http_parse" [@@noalloc]

let incomplete = -1
let malformed = -2
let too_many = -3

let max_headers = 100 (* mirrors the prior max_header_count *)
let out_size = 7 + (4 * max_headers)
let make_out () : int array = Array.make out_size 0

(* ──── http_parse: correctness ──── *)

let of_string s =
  let n = String.length s in
  let b = Bigstringaf.create (max 1 n) in
  Bigstringaf.blit_from_string s ~src_off:0 b ~dst_off:0 ~len:n;
  b

(* parse [s] and return (head_len, method, target, version, (name,value) headers) on success *)
let parse_str s =
  let b = of_string s and out = make_out () in
  let n = parse b 0 (String.length s) out max_headers in
  if n < 0 then `Err n
  else
    let sub off len = Bigstringaf.substring b ~off ~len in
    let nh = out.(6) in
    let hs = List.init nh (fun i -> let o = 7 + (4 * i) in (sub out.(o) out.(o + 1), sub out.(o + 2) out.(o + 3))) in
    `Ok (n, sub out.(0) out.(1), sub out.(2) out.(3), sub out.(4) out.(5), hs)

let req_ = "GET /a/b?x=1 HTTP/1.1\r\nHost: ex.com\r\nAccept: */*\r\n\r\n"

let%test "parses method/target/version" =
  match parse_str req_ with `Ok (_, m, t, v, _) -> m = "GET" && t = "/a/b?x=1" && v = "HTTP/1.1" | _ -> false
let%test "parses headers (original case, OWS-trimmed values)" =
  match parse_str req_ with `Ok (_, _, _, _, hs) -> hs = [ ("Host", "ex.com"); ("Accept", "*/*") ] | _ -> false
let%test "head length points exactly past the blank line" =
  match parse_str req_ with `Ok (n, _, _, _, _) -> n = String.length req_ | _ -> false
let%test "no headers" =
  match parse_str "GET / HTTP/1.1\r\n\r\n" with `Ok (_, m, _, _, hs) -> m = "GET" && hs = [] | _ -> false
let%test "POST" =
  match parse_str "POST /x HTTP/1.1\r\nContent-Length: 3\r\n\r\n" with `Ok (_, m, t, _, hs) -> m = "POST" && t = "/x" && hs = [ ("Content-Length", "3") ] | _ -> false
let%test "value with internal colon + spaces trimmed" =
  match parse_str "GET / HTTP/1.1\r\nX:   a: b   \r\n\r\n" with `Ok (_, _, _, _, [ (k, v) ]) -> k = "X" && v = "a: b" | _ -> false
let%test "tab OWS trimmed" = (match parse_str "GET / HTTP/1.1\r\nX:\tv\t\r\n\r\n" with `Ok (_, _, _, _, [ (_, v) ]) -> v = "v" | _ -> false)
let%test "bare LF line endings tolerated" =
  match parse_str "GET / HTTP/1.1\nHost: x\n\n" with `Ok (_, m, _, _, hs) -> m = "GET" && hs = [ ("Host", "x") ] | _ -> false
let%test "colon-less header line skipped (lenient, as before)" =
  match parse_str "GET / HTTP/1.1\r\ngarbage line\r\nHost: x\r\n\r\n" with `Ok (_, _, _, _, hs) -> hs = [ ("Host", "x") ] | _ -> false
let%test "extra body bytes after the head are NOT consumed" =
  match parse_str "GET / HTTP/1.1\r\n\r\nBODYBODY" with `Ok (n, _, _, _, _) -> n = String.length "GET / HTTP/1.1\r\n\r\n" | _ -> false

(* ──── http_parse: incomplete (need more bytes) ──── *)

let%test "incomplete: truncated request line" = parse_str "GET /a HTTP/1.1" = `Err incomplete
let%test "incomplete: headers not terminated" = parse_str "GET / HTTP/1.1\r\nHost: x\r\n" = `Err incomplete
let%test "incomplete: empty" = parse_str "" = `Err incomplete
let%test "incomplete: partial header value" = parse_str "GET / HTTP/1.1\r\nHost: ex" = `Err incomplete
(* every prefix of a valid request is either incomplete or (at the very end) complete — never a crash *)
let%test "every prefix is incomplete until the head completes (no crash, no false-complete)" =
  let full = req_ in
  let head_len = String.length req_ in
  let ok = ref true in
  for k = 0 to head_len - 1 do
    (match parse_str (String.sub full 0 k) with `Err e when e = incomplete -> () | `Err _ -> () | `Ok _ -> ok := false)
  done;
  !ok && (match parse_str full with `Ok (n, _, _, _, _) -> n = head_len | _ -> false)

(* ──── http_parse: malformed ──── *)

let%test "malformed: empty method (leading space)" = parse_str " / HTTP/1.1\r\n\r\n" = `Err malformed
let%test "malformed: no version (CR right after target space)" = parse_str "GET /\r\n\r\n" = `Err malformed
let%test "malformed: CR not followed by LF in request line" = parse_str "GET / HTTP/1.1\rX\r\n\r\n" = `Err malformed

(* ──── http_parse: too many headers ──── *)

let%test "too many headers -> too_many (never overflows the out array)" =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "GET / HTTP/1.1\r\n";
  for i = 0 to max_headers + 10 do Buffer.add_string buf (Printf.sprintf "H%d: v\r\n" i) done;
  Buffer.add_string buf "\r\n";
  parse_str (Buffer.contents buf) = `Err too_many

(* ──── http_parse: crash-safety on hostile / arbitrary input ──── *)

(* the parser must NEVER fault on arbitrary bytes — only ever return a code. Property over random
   byte strings (incl. NULs, high bytes, lone CR/LF, colons) + a few adversarial fixtures. *)
let%test "crash-safe on arbitrary bytes (incl. NULs, lone CR/LF, high bytes)" =
  Random.self_init ();
  let ok = ref true in
  for _ = 1 to 20000 do
    let n = Random.int 64 in
    let s = String.init n (fun _ -> Char.chr (Random.int 256)) in
    match parse_str s with `Ok _ | `Err _ -> () | exception _ -> ok := false
  done;
  (* a few targeted nasties *)
  List.iter
    (fun s -> match parse_str s with `Ok _ | `Err _ -> () | exception _ -> ok := false)
    [ "\000\000\000"; "\r\r\r\r"; "\n\n\n\n"; ":::::"; "GET\000/\000HTTP/1.1\r\n\r\n"; "G\xff\xfe / HTTP/1.1\r\n\xff: \xfe\r\n\r\n"; String.make 50 ':' ];
  !ok
