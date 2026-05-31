(* Unit tests for Static — path-traversal safety, symlink escape, null-byte
   rejection, MIME, conditional 304, Range 206/416, and Responder finalization
   (compression negotiation, ETag, HEAD). Uses a real temp public/ tree so the
   realpath/symlink checks are exercised for real. *)

module H = Fennec_core.Http
module Static = Fennec_server.Static
module Responder = Fennec_server.Responder

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let write path contents =
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with _ -> ())

let req ?(meth = H.GET) ?(headers = []) path = { H.meth; path; query = []; headers; body = "" }
let status_of = function Some (r : H.response) -> r.H.status | None -> 0
let body_of = function Some (r : H.response) -> r.H.body | None -> ""
let hdr r k = match r with Some (resp : H.response) -> Fennec_core.Http_semantics.header resp.H.headers k | None -> None

let () =
  (* build a temp public tree:
       <root>/index.html, /robots.txt, /img/logo.svg, /big.bin
       a symlink /escape -> /etc/hosts (escape attempt) *)
  let root = Filename.temp_file "fennec_static" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  write (Filename.concat root "index.html") "<h1>home</h1>";
  write (Filename.concat root "robots.txt") "User-agent: *\n";
  mkdir_p (Filename.concat root "img");
  write (Filename.concat root "img/logo.svg") "<svg/>";
  write (Filename.concat root "big.bin") (String.make 1000 'X');
  (* symlink that escapes the root *)
  let escape_target = "/etc/hosts" in
  (try Unix.symlink escape_target (Filename.concat root "escape") with _ -> ());

  let src = Static.Dir root in
  let serve r = Static.respond src r in

  print_endline "Static — happy path:";
  eq "index served" (status_of (serve (req "/"))) 200;
  eq "index body" (body_of (serve (req "/"))) "<h1>home</h1>";
  eq "robots served" (status_of (serve (req "/robots.txt"))) 200;
  eq "nested svg" (status_of (serve (req "/img/logo.svg"))) 200;
  eq "svg mime" (hdr (serve (req "/img/logo.svg")) "content-type") (Some "image/svg+xml");
  eq "missing -> None (404 fallthrough)" (status_of (serve (req "/nope.txt"))) 0;

  print_endline "Static — traversal & escape:";
  eq ".. rejected (403)" (status_of (serve (req "/../etc/passwd"))) 403;
  eq "deep .. rejected" (status_of (serve (req "/img/../../etc/passwd"))) 403;
  eq ". segment rejected" (status_of (serve (req "/./robots.txt"))) 403;
  eq "double slash rejected" (status_of (serve (req "/img//logo.svg"))) 403;
  eq "null byte rejected" (status_of (serve (req "/robots.txt\000.png"))) 403;
  eq "control char rejected" (status_of (serve (req "/img/\tlogo.svg"))) 403;
  (* symlink escaping the root must NOT be served (realpath check) — None or not 200 *)
  let esc = serve (req "/escape") in
  check "symlink escape not served" (status_of esc <> 200);

  print_endline "Static — conditional & range:";
  let etag = match hdr (serve (req "/robots.txt")) "etag" with Some e -> e | None -> "" in
  check "has etag" (etag <> "");
  eq "If-None-Match -> 304"
    (status_of (serve (req ~headers:[ ("If-None-Match", etag) ] "/robots.txt"))) 304;
  eq "Range -> 206" (status_of (serve (req ~headers:[ ("Range", "bytes=0-9") ] "/big.bin"))) 206;
  eq "Range body length" (String.length (body_of (serve (req ~headers:[ ("Range", "bytes=0-9") ] "/big.bin")))) 10;
  eq "Range content-range"
    (hdr (serve (req ~headers:[ ("Range", "bytes=0-9") ] "/big.bin")) "content-range")
    (Some "bytes 0-9/1000");
  eq "unsatisfiable Range -> 416"
    (status_of (serve (req ~headers:[ ("Range", "bytes=5000-6000") ] "/big.bin"))) 416;
  eq "HEAD range empty body"
    (String.length (body_of (serve (req ~meth:H.HEAD ~headers:[ ("Range", "bytes=0-9") ] "/big.bin")))) 0;

  print_endline "Static — caching cache (mtime reuse):";
  (* two reads of the same file return the same etag (cache hit path) *)
  let e1 = hdr (serve (req "/big.bin")) "etag" in
  let e2 = hdr (serve (req "/big.bin")) "etag" in
  eq "stable etag across reads" e1 e2;

  print_endline "Responder.finalize:";
  let big_html = H.html (String.make 2000 'a') in
  (* gzip negotiated for compressible body over min size *)
  let gz =
    Responder.finalize ~now:0.0 ~req:(req ~headers:[ ("Accept-Encoding", "gzip") ] "/") big_html
  in
  eq "gzip content-encoding" (Fennec_core.Http_semantics.header gz.H.headers "content-encoding") (Some "gzip");
  check "gzip vary set" (Fennec_core.Http_semantics.header gz.H.headers "vary" = Some "Accept-Encoding");
  check "gzip body smaller" (String.length gz.H.body < 2000);
  (* no Accept-Encoding -> identity *)
  let id = Responder.finalize ~now:0.0 ~req:(req "/") big_html in
  eq "identity (no encoding header)" (Fennec_core.Http_semantics.header id.H.headers "content-encoding") None;
  (* tiny body not compressed even if gzip requested *)
  let tiny =
    Responder.finalize ~now:0.0 ~req:(req ~headers:[ ("Accept-Encoding", "gzip") ] "/") (H.html "hi")
  in
  eq "tiny body not compressed" (Fennec_core.Http_semantics.header tiny.H.headers "content-encoding") None;
  (* png-like (non-compressible) not compressed *)
  let png =
    Responder.finalize ~now:0.0 ~req:(req ~headers:[ ("Accept-Encoding", "gzip") ] "/")
      (H.respond ~content_type:"image/png" (String.make 2000 'x'))
  in
  eq "png not compressed" (Fennec_core.Http_semantics.header png.H.headers "content-encoding") None;
  (* ETag added; conditional 304 via finalize *)
  let etag2 = match Fennec_core.Http_semantics.header id.H.headers "etag" with Some e -> e | None -> "" in
  let cond = Responder.finalize ~now:0.0 ~req:(req ~headers:[ ("If-None-Match", etag2) ] "/") big_html in
  eq "finalize conditional 304" cond.H.status 304;
  check "304 has empty body" (cond.H.body = "");
  (* HEAD -> empty body, headers preserved *)
  let head = Responder.finalize ~now:0.0 ~req:(req ~meth:H.HEAD "/") big_html in
  check "HEAD empty body" (head.H.body = "");
  check "HEAD has content-length" (Fennec_core.Http_semantics.header head.H.headers "content-length" <> None);

  (* cleanup *)
  (try Sys.remove (Filename.concat root "escape") with _ -> ());
  List.iter (fun f -> try Sys.remove (Filename.concat root f) with _ -> ())
    [ "index.html"; "robots.txt"; "img/logo.svg"; "big.bin" ];
  (try Unix.rmdir (Filename.concat root "img") with _ -> ());
  (try Unix.rmdir root with _ -> ());

  if !fails = 0 then print_endline "all Static/Responder tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
