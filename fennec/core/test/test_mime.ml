(* Unit tests for Fennec_core.Mime — extension -> content-type and the
   compressible predicate. Edge cases: case, multi-dot, no ext, unknown,
   compressible boundaries. *)

module Mime = Fennec_core.Mime

let fails = ref 0

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (
    incr fails;
    Printf.printf "  FAIL %s\n" name)

let eq name a b = check name (a = b)

let () =
  print_endline "of_path:";
  eq "html" (Mime.of_path "x.html") "text/html; charset=utf-8";
  eq "css path" (Mime.of_path "/a/b/app.css") "text/css; charset=utf-8";
  eq "js" (Mime.of_path "main.js") "text/javascript; charset=utf-8";
  eq "mjs" (Mime.of_path "m.mjs") "text/javascript; charset=utf-8";
  eq "json" (Mime.of_path "data.json") "application/json; charset=utf-8";
  eq "svg" (Mime.of_path "logo.svg") "image/svg+xml";
  eq "png" (Mime.of_path "img.png") "image/png";
  eq "woff2" (Mime.of_path "f.woff2") "font/woff2";
  eq "wasm" (Mime.of_path "m.wasm") "application/wasm";
  eq "uppercase ext" (Mime.of_path "IMG.PNG") "image/png";
  eq "mixed case" (Mime.of_path "Style.Css") "text/css; charset=utf-8";
  eq "multi-dot uses last" (Mime.of_path "app.min.js") "text/javascript; charset=utf-8";
  eq "unknown -> octet" (Mime.of_path "x.qqq") "application/octet-stream";
  eq "no ext -> octet" (Mime.of_path "README") "application/octet-stream";
  eq "dotfile no ext -> octet" (Mime.of_path ".gitignore") "application/octet-stream";
  eq "trailing dot -> octet" (Mime.of_path "x.") "application/octet-stream";

  print_endline "compressible:";
  check "html" (Mime.compressible "text/html; charset=utf-8");
  check "plain text/*" (Mime.compressible "text/plain");
  check "json" (Mime.compressible "application/json");
  check "json w/ charset" (Mime.compressible "application/json; charset=utf-8");
  check "svg" (Mime.compressible "image/svg+xml");
  check "wasm" (Mime.compressible "application/wasm");
  check "javascript" (Mime.compressible "application/javascript");
  check "NOT png" (not (Mime.compressible "image/png"));
  check "NOT jpeg" (not (Mime.compressible "image/jpeg"));
  check "NOT woff2" (not (Mime.compressible "font/woff2"));
  check "NOT mp4" (not (Mime.compressible "video/mp4"));
  check "NOT octet" (not (Mime.compressible "application/octet-stream"));
  check "case-insensitive" (Mime.compressible "TEXT/HTML");
  check "whitespace tolerant" (Mime.compressible "  application/json ; charset=utf-8");

  if !fails = 0 then print_endline "all Mime tests passed."
  else (
    Printf.printf "%d FAILED\n" !fails;
    exit 1)
