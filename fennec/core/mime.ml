(* Map a filename to its MIME content-type. Pure, Stdlib only. Covers the web
   asset types a static server serves; unknown extensions fall back to
   application/octet-stream. The [compressible] predicate tells the response layer
   which types are worth gzip'ing (text-ish) vs already-compressed (media/fonts).*)

let lower_ext (path : string) : string =
  match Filename.extension path with
  | "" -> ""
  | e -> String.lowercase_ascii (String.sub e 1 (String.length e - 1))

(* extension (no dot) -> content-type. charset added for text types. *)
let table =
  [ (* text / markup *)
    ("html", "text/html; charset=utf-8");
    ("htm", "text/html; charset=utf-8");
    ("css", "text/css; charset=utf-8");
    ("js", "text/javascript; charset=utf-8");
    ("mjs", "text/javascript; charset=utf-8");
    ("cjs", "text/javascript; charset=utf-8");
    ("json", "application/json; charset=utf-8");
    ("map", "application/json; charset=utf-8");
    ("xml", "application/xml; charset=utf-8");
    ("txt", "text/plain; charset=utf-8");
    ("md", "text/markdown; charset=utf-8");
    ("csv", "text/csv; charset=utf-8");
    ("svg", "image/svg+xml");
    ("webmanifest", "application/manifest+json; charset=utf-8");
    ("wasm", "application/wasm");
    (* images *)
    ("png", "image/png");
    ("jpg", "image/jpeg");
    ("jpeg", "image/jpeg");
    ("gif", "image/gif");
    ("webp", "image/webp");
    ("avif", "image/avif");
    ("ico", "image/x-icon");
    ("bmp", "image/bmp");
    (* fonts *)
    ("woff", "font/woff");
    ("woff2", "font/woff2");
    ("ttf", "font/ttf");
    ("otf", "font/otf");
    ("eot", "application/vnd.ms-fontobject");
    (* media *)
    ("mp4", "video/mp4");
    ("webm", "video/webm");
    ("ogg", "audio/ogg");
    ("mp3", "audio/mpeg");
    ("wav", "audio/wav");
    (* misc *)
    ("pdf", "application/pdf");
    ("zip", "application/zip");
    ("gz", "application/gzip") ]

let of_path (path : string) : string =
  match List.assoc_opt (lower_ext path) table with
  | Some ct -> ct
  | None -> "application/octet-stream"

let%test "html" = of_path "index.html" = "text/html; charset=utf-8"
let%test "css path" = of_path "/a/b/app.css" = "text/css; charset=utf-8"
let%test "js"   = of_path "main.js" = "text/javascript; charset=utf-8"
let%test "mjs"  = of_path "m.mjs" = "text/javascript; charset=utf-8"
let%test "css"  = of_path "style.css" = "text/css; charset=utf-8"
let%test "json" = of_path "data.json" = "application/json; charset=utf-8"
let%test "svg"  = of_path "logo.svg" = "image/svg+xml"
let%test "png"  = of_path "logo.png" = "image/png"
let%test "woff2" = of_path "font.woff2" = "font/woff2"
let%test "wasm" = of_path "m.wasm" = "application/wasm"
let%test "uppercase ext" = of_path "IMG.PNG" = "image/png"
let%test "mixed case" = of_path "Style.Css" = "text/css; charset=utf-8"
let%test "multi-dot uses last" = of_path "app.min.js" = "text/javascript; charset=utf-8"
let%test "unknown ext" = of_path "data.xyz" = "application/octet-stream"
let%test "no ext" = of_path "Makefile" = "application/octet-stream"
let%test "dotfile no ext" = of_path ".gitignore" = "application/octet-stream"
let%test "trailing dot" = of_path "x." = "application/octet-stream"
let%test "case insensitive" = of_path "FILE.HTML" = "text/html; charset=utf-8"

(* Is this content-type worth compressing? text/*, application/json|xml|wasm|
   manifest, image/svg+xml, application/javascript — but NOT already-compressed
   media, fonts (woff2 is brotli'd), or octet-stream. *)
let compressible (content_type : string) : bool =
  let ct =
    match String.index_opt content_type ';' with
    | Some i -> String.sub content_type 0 i
    | None -> content_type
  in
  let ct = String.trim (String.lowercase_ascii ct) in
  let starts p = String.length ct >= String.length p && String.sub ct 0 (String.length p) = p in
  starts "text/"
  || List.mem ct
       [ "application/json";
         "application/xml";
         "application/javascript";
         "application/manifest+json";
         "application/wasm";
         "image/svg+xml";
         "application/x-javascript" ]

let%test "text/html compressible" = compressible "text/html; charset=utf-8"
let%test "text/plain compressible" = compressible "text/plain"
let%test "application/json compressible" = compressible "application/json"
let%test "json w/ charset compressible" = compressible "application/json; charset=utf-8"
let%test "image/svg+xml compressible" = compressible "image/svg+xml"
let%test "wasm compressible" = compressible "application/wasm"
let%test "javascript compressible" = compressible "application/javascript"
let%test "NOT image/png" = not (compressible "image/png")
let%test "NOT image/jpeg" = not (compressible "image/jpeg")
let%test "NOT font/woff2" = not (compressible "font/woff2")
let%test "NOT video/mp4" = not (compressible "video/mp4")
let%test "NOT octet-stream" = not (compressible "application/octet-stream")
let%test "compressible case-insensitive" = compressible "TEXT/HTML"
let%test "compressible ws tolerant" = compressible "  application/json ; charset=utf-8"
