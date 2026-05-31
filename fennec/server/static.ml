(* Static file serving for the public/ tree. One [source] abstracts the two
   modes:
     - Dir path: read from disk (dev — edits are live)
     - Embedded map: path -> bytes baked into the binary at build time (prod —
       single self-contained executable)

   Serving is airtight: correct Content-Type (by extension), strong ETag (content
   hash) + conditional 304, Last-Modified, Cache-Control, Range requests (206 /
   416), HEAD. Compression is applied by [Responder.finalize] on the way out for
   the whole-body case; Range responses are sent uncompressed (correct: ranges
   are over the identity representation). *)

module H = Fennec_core.Http
module Sem = Fennec_core.Http_semantics
module Date = Fennec_core.Http_date
module Mime = Fennec_core.Mime

type entry = { bytes : string; etag : string; mtime : float }

(* [Dir path]: read from disk (dev). [Embedded lookup]: a path -> bytes function
   baked into the binary (prod); the ETag is the content hash and Last-Modified is
   a fixed build epoch (assets are immutable for the binary's lifetime). *)
type source = Dir of string | Embedded of (string -> string option)

(* a fixed mtime for embedded assets: they don't change for the binary's life.
   0.0 (epoch) is fine — clients validate via the strong ETag, not the date. *)
let embedded_mtime = 0.0

(* normalize a URL path to a safe relative lookup key:
   - strip the leading '/'
   - reject any segment that is "" (//) , "." or ".." (path traversal)
   - map "/" (or empty) to "index.html"
   Returns None if unsafe. *)
let safe_key (url_path : string) : string option =
  let p = if String.length url_path > 0 && url_path.[0] = '/' then String.sub url_path 1 (String.length url_path - 1) else url_path in
  let p = if p = "" then "index.html" else p in
  let segs = String.split_on_char '/' p in
  let bad = List.exists (fun s -> s = "" || s = "." || s = "..") segs in
  if bad then None
  else
    (* a trailing-slash dir request -> index.html within it *)
    let p = if String.length p > 0 && p.[String.length p - 1] = '/' then p ^ "index.html" else p in
    Some p

let dir_lookup (root : string) (key : string) : entry option =
  let path = Filename.concat root key in
  if not (Sys.file_exists path) || Sys.is_directory path then None
  else
    try
      let bytes = In_channel.with_open_bin path In_channel.input_all in
      let mtime = (Unix.stat path).Unix.st_mtime in
      Some { bytes; etag = Sem.make_etag (Digest.to_hex (Digest.string bytes)); mtime }
    with _ -> None

let lookup (src : source) (key : string) : entry option =
  match src with
  | Dir root -> dir_lookup root key
  | Embedded f -> (
    match f key with
    | None -> None
    | Some bytes ->
      Some { bytes; etag = Sem.make_etag (Digest.to_hex (Digest.string bytes)); mtime = embedded_mtime })

(* Build a response for a static asset. Returns None when there is no such asset
   (caller falls through to the next route / 404). [now] is epoch seconds. *)
let respond ?(cache_control = "public, max-age=3600") (src : source) (req : H.request) :
    H.response option =
  match safe_key req.H.path with
  | None -> Some (H.text ~status:403 "Forbidden")
  | Some key -> (
    match lookup src key with
    | None -> None
    | Some e ->
      let ct = Mime.of_path key in
      let base_headers =
        [ ("Content-Type", ct);
          ("ETag", e.etag);
          ("Last-Modified", Date.format e.mtime);
          ("Cache-Control", cache_control);
          ("Accept-Ranges", "bytes") ]
      in
      let len = String.length e.bytes in
      (* conditional: If-None-Match / If-Modified-Since -> 304 (handled centrally
         too, but we short-circuit here to skip Range work) *)
      let not_modified =
        Sem.if_none_match_satisfied ~etag:e.etag req.H.headers
        || ((Sem.header req.H.headers "if-none-match" = None)
           && Sem.if_modified_since_satisfied ~mtime:e.mtime req.H.headers)
      in
      if not_modified && (req.H.meth = H.GET || req.H.meth = H.HEAD) then
        Some { H.status = 304; headers = base_headers; body = "" }
      else
        (* Range (single range only); ranges are over identity bytes *)
        match Sem.parse_range ~len req.H.headers with
        | `Unsatisfiable ->
          Some
            {
              H.status = 416;
              headers =
                ("Content-Range", Printf.sprintf "bytes */%d" len) :: base_headers;
              body = "";
            }
        | `Range { first; last } ->
          let slice = String.sub e.bytes first (last - first + 1) in
          let headers =
            ("Content-Range", Printf.sprintf "bytes %d-%d/%d" first last len)
            :: ("Content-Length", string_of_int (String.length slice))
            :: ("Content-Encoding", "identity") (* tell Responder not to gzip *)
            :: base_headers
          in
          Some
            {
              H.status = 206;
              headers;
              body = (if req.H.meth = H.HEAD then "" else slice);
            }
        | `None ->
          Some { H.status = 200; headers = base_headers; body = e.bytes })

(* mount as a fallthrough handler: returns Some response when the path matches an
   asset, None otherwise (so the caller can fall through to pages / 404). *)
let handler ?cache_control (src : source) : H.request -> H.response option =
 fun req -> respond ?cache_control src req
