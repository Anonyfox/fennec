(* Integration test for fennec_image: drive the TYPED engine + favicon generation on a real image
   (a hand-built 2x2 BMP — no codec/CRC needed to make it), through the vendored Rust engine. This
   proves the FFI roundtrip end-to-end via the typed API, complementing the pure inline tests. *)

module I = Fennec_image

let check name cond = if cond then Printf.printf "  ok   %s\n" name else (Printf.printf "  FAIL %s\n" name; exit 1)
let starts s p = String.length s >= String.length p && String.sub s 0 (String.length p) = p

let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

(* a 2x2 24-bit BMP, built by hand (no codec, no CRC) *)
let tiny_bmp () : bytes =
  let w = 2 and h = 2 in
  let stride = (w * 3 + 3) / 4 * 4 in
  let pixels = stride * h in
  let size = 54 + pixels in
  let b = Buffer.create size in
  let u8 n = Buffer.add_char b (Char.chr (n land 0xff)) in
  let u16 n = u8 n; u8 (n lsr 8) in
  let u32 n = u16 n; u16 (n lsr 16) in
  u8 0x42; u8 0x4d; u32 size; u16 0; u16 0; u32 54;
  u32 40; u32 w; u32 h; u16 1; u16 24; u32 0; u32 pixels; u32 2835; u32 2835; u32 0; u32 0;
  u8 255; u8 0; u8 0; u8 0; u8 255; u8 0; u8 0; u8 0;
  u8 0; u8 0; u8 255; u8 255; u8 255; u8 255; u8 0; u8 0;
  Buffer.to_bytes b

let proc bmp fmt op =
  match I.Engine.process ~input:bmp ~format:fmt ~op with
  | Ok b -> Bytes.to_string b
  | Error e -> Printf.printf "  FAIL engine.process: %s\n" e; exit 1

let () =
  let bmp = tiny_bmp () in
  let box n = { I.Op.default with resize = Some (I.Geometry.Box (n, n)); fit = I.Op.Cover } in
  print_endline "image (typed engine):";
  check "Png roundtrip (signature)" (starts (proc bmp I.Format.Png (box 8)) "\137PNG\r\n\026\n");
  check "Jpeg roundtrip (SOI + quality opt)" (starts (proc bmp I.Format.Jpeg { I.Op.default with quality = Some 70 }) "\255\216");
  check "Gif roundtrip (GIF8)" (starts (proc bmp I.Format.Gif I.Op.default) "GIF8");
  check "Webp roundtrip (RIFF/WEBP)" (let s = proc bmp I.Format.Webp (box 4) in starts s "RIFF" && contains s "WEBP");
  check "Ico roundtrip (ICONDIR)" (starts (proc bmp I.Format.Ico (box 16)) "\000\000\001\000");
  check "width-only resize produces output" (String.length (proc bmp I.Format.Webp { I.Op.default with resize = Some (I.Geometry.Width 32) }) > 0);
  check "corrupt input is Error, not an exception"
    (match I.Engine.process ~input:(Bytes.of_string "not an image") ~format:I.Format.Png ~op:I.Op.default with Error _ -> true | Ok _ -> false);

  print_endline "favicon:";
  let dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "fennec-favicon-%d" (Unix.getpid ())) in
  (match I.Favicon.generate ~input:bmp ~dir with
   | Error e -> Printf.printf "  FAIL favicon.generate: %s\n" e; exit 1
   | Ok snippet ->
     check "snippet links the manifest" (contains snippet "manifest.webmanifest");
     List.iter
       (fun (i : I.Favicon.icon) ->
         let p = Filename.concat dir i.I.Favicon.filename in
         check (Printf.sprintf "wrote %s (non-empty)" i.I.Favicon.filename)
           (Sys.file_exists p && (Unix.stat p).Unix.st_size > 0))
       I.Favicon.set;
     check "wrote manifest.webmanifest" (Sys.file_exists (Filename.concat dir "manifest.webmanifest"));
     (* the .ico carries the ICONDIR header *)
     let ic = open_in_bin (Filename.concat dir "favicon.ico") in
     let hdr = really_input_string ic 4 in
     close_in ic;
     check "favicon.ico has the ICONDIR header" (hdr = "\000\000\001\000"));
  print_endline "all image tests passed."
