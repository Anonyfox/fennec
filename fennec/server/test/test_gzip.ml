(* Unit tests for Gzip (HTTP Content-Encoding) and Deflate (WS permessage-deflate)
   — real zlib. Verify magic bytes/headers, that compression shrinks repetitive
   data, and that the permessage-deflate round-trip is lossless. *)

module Gzip = Fennec_server.Gzip
module Deflate = Fennec_server.Deflate

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let () =
  print_endline "gzip:";
  let g = Gzip.gzip (String.make 2000 'a') in
  check "gzip magic 1f 8b" (String.length g >= 2 && g.[0] = '\x1f' && g.[1] = '\x8b');
  check "gzip shrinks repetitive" (String.length g < 2000);
  check "gzip empty input ok" (String.length (Gzip.gzip "") >= 0);
  let d = Gzip.deflate (String.make 2000 'b') in
  check "deflate zlib header 0x78" (String.length d >= 1 && Char.code d.[0] = 0x78);
  check "deflate shrinks" (String.length d < 2000);

  print_endline "permessage-deflate round-trip:";
  let cases = [ ""; "x"; "hello world"; String.make 5000 'z'; {js|café ✨ 🦊|js};
                "\x00\x01\x02\xff binary-ish" ] in
  List.iter
    (fun s ->
      let round = Deflate.decompress (Deflate.compress s) in
      check (Printf.sprintf "rt %d bytes" (String.length s)) (round = s))
    cases;
  let big = String.make 5000 'a' in
  check "compresses repetitive" (String.length (Deflate.compress big) < String.length big);

  if !fails = 0 then print_endline "all Gzip/Deflate tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
