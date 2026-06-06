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
let%test "js"   = of_path "main.js" = "text/javascript; charset=utf-8"
let%test "css"  = of_path "style.css" = "text/css; charset=utf-8"
let%test "png"  = of_path "logo.png" = "image/png"
let%test "woff2" = of_path "font.woff2" = "font/woff2"
let%test "unknown ext → octet-stream" = of_path "data.xyz" = "application/octet-stream"
let%test "no ext → octet-stream" = of_path "Makefile" = "application/octet-stream"
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

let%test "text/html is compressible" = compressible "text/html; charset=utf-8"
let%test "application/json is compressible" = compressible "application/json"
let%test "image/png is NOT compressible" = not (compressible "image/png")
let%test "font/woff2 is NOT compressible" = not (compressible "font/woff2")
let%test "image/svg+xml IS compressible" = compressible "image/svg+xml"
