(* Exercises the full native pipeline: esbuild bundling (cold + warm rebuild +
   build error) and the Lightning CSS / grass engine. *)

let tmp = Filename.get_temp_dir_name ()

let write name contents =
  let path = Filename.concat tmp name in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path

let check name cond =
  if cond then Printf.printf "  ok   %s\n" name
  else (Printf.printf "  FAIL %s\n" name; exit 1)

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let starts s pfx = String.length s >= String.length pfx && String.sub s 0 (String.length pfx) = pfx

(* a 2x2 24-bit BMP, built by hand (no codec, no CRC) so the image roundtrip has a real input to decode *)
let tiny_bmp () : bytes =
  let w = 2 and h = 2 in
  let stride = (w * 3 + 3) / 4 * 4 in
  let pixels = stride * h in
  let size = 54 + pixels in
  let b = Buffer.create size in
  let u8 n = Buffer.add_char b (Char.chr (n land 0xff)) in
  let u16 n = u8 n; u8 (n lsr 8) in
  let u32 n = u16 n; u16 (n lsr 16) in
  u8 0x42; u8 0x4d;                                    (* "BM" *)
  u32 size; u16 0; u16 0; u32 54;                      (* file size, reserved, pixel offset *)
  u32 40; u32 w; u32 h; u16 1; u16 24;                 (* DIB: header size, w, h, planes, bpp *)
  u32 0; u32 pixels; u32 2835; u32 2835; u32 0; u32 0; (* compression, image size, ppm x/y, colors *)
  u8 255; u8 0; u8 0; u8 0; u8 255; u8 0; u8 0; u8 0;  (* row 0 (BGR): blue, green, 2 pad *)
  u8 0; u8 0; u8 255; u8 255; u8 255; u8 255; u8 0; u8 0; (* row 1: red, white, 2 pad *)
  Buffer.to_bytes b

let () =
  print_endline "esbuild:";
  let _ = write "fk_dep.js" "export const greet = (n) => `hi ${n}`;\n" in
  let entry =
    write "fk_app.js"
      "import { greet } from './fk_dep.js';\nwindow.msg = greet('fennec');\n"
  in

  (* one-shot bundle: import is resolved & inlined *)
  let js = Fennec_buildkit.Esbuild.build ~entry () in
  check "bundles and resolves imports" (contains js "hi ");
  check "produces non-empty output" (String.length js > 0);

  (* minified bundle is smaller *)
  let min = Fennec_buildkit.Esbuild.build ~entry ~minify:true () in
  check "minify shrinks output" (String.length min < String.length js);

  (* warm context: repeated rebuilds are stable *)
  let ctx = Fennec_buildkit.Esbuild.create ~entry () in
  let a = Fennec_buildkit.Esbuild.rebuild ctx in
  let b = Fennec_buildkit.Esbuild.rebuild ctx in
  check "warm rebuild is deterministic" (a = b);
  Fennec_buildkit.Esbuild.dispose ctx;

  (* build error surfaces as Failure *)
  let bad = write "fk_bad.js" "import { x } from './does_not_exist.js';\n" in
  check "build error raises Failure"
    (try ignore (Fennec_buildkit.Esbuild.build ~entry:bad ()); false
     with Failure _ -> true);

  print_endline "css:";
  let css = Fennec_buildkit.Css.transform ~minify:true ".a { color: #ffffff; }" in
  check "minifies css" (contains css "#fff" || contains css "white");

  let scss =
    Fennec_buildkit.Css.scss ~minify:true
      "$c: red;\n.btn { color: $c; &:hover { color: darken($c, 10%); } }"
  in
  check "compiles scss nesting + vars" (contains scss ".btn" && contains scss ":hover");

  print_endline "image:";
  let bmp = tiny_bmp () in
  let proc fmt opts = Bytes.to_string (Fennec_buildkit.Image.process ~input:bmp ~format:fmt ~opts) in
  let png = proc "png" "w=4;h=4" in
  check "image: -> png (signature)" (starts png "\137PNG\r\n\026\n" && String.length png > 16);
  let jpg = proc "jpeg" "w=8;h=8;q=70" in
  check "image: -> jpeg (SOI marker)" (starts jpg "\255\216" && String.length jpg > 4);
  check "image: -> gif (GIF8 magic)" (starts (proc "gif" "") "GIF8");
  let webp = proc "webp" "w=4;h=4" in
  check "image: -> webp (RIFF/WEBP)" (starts webp "RIFF" && contains webp "WEBP");
  check "image: -> ico (ICONDIR header)" (starts (proc "ico" "w=16;h=16") "\000\000\001\000");
  check "image: resize + fit=cover yields output" (String.length (proc "webp" "w=64;h=32;fit=cover") > 0);
  check "image: bad input raises Failure"
    (try ignore (Fennec_buildkit.Image.process ~input:(Bytes.of_string "not an image") ~format:"png" ~opts:""); false
     with Failure _ -> true);
  check "image: unknown format raises Failure"
    (try ignore (proc "tiff" ""); false with Failure _ -> true);

  print_endline "all buildkit tests passed."
