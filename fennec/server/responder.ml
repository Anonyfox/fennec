(* Response finalization — the airtight HTTP layer applied to EVERY response
   (dynamic SSR and static alike):

     - content negotiation: gzip/deflate per Accept-Encoding (+ Vary), only for
       compressible types and bodies past a min size
     - conditional requests: strong ETag (content hash) + If-None-Match -> 304;
       Last-Modified + If-Modified-Since -> 304
     - HEAD: same headers, empty body
     - always-correct Content-Length, Date

   Pure-ish: the only effect is gzip (CPU). Range handling for static files lives
   in [Static] because it needs the file length up front; this module handles the
   whole-body path. *)

module H = Fennec_core.Http
module Sem = Fennec_core.Http_semantics
module Date = Fennec_core.Http_date
module Mime = Fennec_core.Mime

(* don't bother compressing tiny bodies — the gzip header overhead dominates and
   many proxies skip below ~1KB too *)
let min_compress_size = 1024

let header_ci headers k = Sem.header headers k

let content_type_of (resp : H.response) : string =
  match header_ci resp.H.headers "content-type" with
  | Some ct -> ct
  | None -> "application/octet-stream"

(* hex md5 of the body — a strong validator for ETag (content-addressed) *)
let body_etag (body : string) : string = Sem.make_etag (Digest.to_hex (Digest.string body))

let set_header headers k v =
  (* replace any existing (case-insensitive), then add *)
  let kl = String.lowercase_ascii k in
  (k, v) :: List.filter (fun (hk, _) -> String.lowercase_ascii hk <> kl) headers

let has_header headers k = Sem.header headers k <> None

(* Apply compression + validators + conditional handling to a response, given the
   request that produced it. [now] is epoch seconds for the Date header. *)
let finalize ?(now = 0.0) ~(req : H.request) (resp : H.response) : H.response =
  let is_head = req.H.meth = H.HEAD in
  let ct = content_type_of resp in

  (* 1. ETag (only if the handler didn't set one) *)
  let etag =
    match header_ci resp.H.headers "etag" with Some e -> e | None -> body_etag resp.H.body
  in
  let headers = if has_header resp.H.headers "etag" then resp.H.headers else set_header resp.H.headers "ETag" etag in

  (* 2. conditional GET/HEAD: If-None-Match (preferred) or If-Modified-Since *)
  let not_modified =
    Sem.if_none_match_satisfied ~etag req.H.headers
    || ((not (has_header req.H.headers "if-none-match"))
       && Sem.if_modified_since_satisfied ~mtime:now req.H.headers)
  in
  if (req.H.meth = H.GET || is_head) && not_modified then
    (* 304: keep validators + cache-control, drop the body *)
    let keep =
      List.filter
        (fun (k, _) ->
          let kl = String.lowercase_ascii k in
          List.mem kl [ "etag"; "cache-control"; "vary"; "last-modified"; "content-type" ])
        headers
    in
    let keep = set_header keep "Date" (Date.format now) in
    { H.status = 304; headers = keep; body = "" }
  else begin
    (* 3. compression negotiation *)
    let accept = header_ci req.H.headers "accept-encoding" in
    let want = Sem.negotiate_encoding ~accept () in
    let compressible =
      Mime.compressible ct
      && String.length resp.H.body >= min_compress_size
      && (not (has_header resp.H.headers "content-encoding"))
    in
    let body, headers =
      match want with
      | Sem.Gzip when compressible ->
        let z = Gzip.gzip resp.H.body in
        (z, set_header (set_header headers "Content-Encoding" "gzip") "Vary" "Accept-Encoding")
      | Sem.Deflate when compressible ->
        let z = Gzip.deflate resp.H.body in
        (z, set_header (set_header headers "Content-Encoding" "deflate") "Vary" "Accept-Encoding")
      | _ ->
        (* still advertise that representation varies by encoding for caches *)
        let headers = if Mime.compressible ct then set_header headers "Vary" "Accept-Encoding" else headers in
        (resp.H.body, headers)
    in

    (* 4. Date + Content-Length; HEAD keeps headers but empty body *)
    let headers = set_header headers "Date" (Date.format now) in
    let headers = set_header headers "Content-Length" (string_of_int (String.length body)) in
    { H.status = resp.H.status; headers; body = (if is_head then "" else body) }
  end
