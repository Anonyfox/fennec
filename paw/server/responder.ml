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

module H = Http
module Sem = Http_semantics
module Date = Http_date
module Mime = Mime

(* don't bother compressing tiny bodies — the gzip header overhead dominates and
   many proxies skip below ~1KB too *)
let min_compress_size = 1024

let header_ci headers k = Sem.header headers k

let content_type_of (resp : H.response) : string =
  match header_ci resp.H.headers "content-type" with
  | Some ct -> ct
  | None -> "application/octet-stream"

(* A content ETag for the body. An ETag is a CACHE validator, not a security primitive, so a 128-bit
   crypto digest (MD5) is wasteful: a fast non-cryptographic 63-bit hash (FNV-1a-style, native-int, no
   boxing) is plenty — a collision only ever costs a missed 304, never wrong content. Still a STRONG
   validator (a deterministic function of the exact bytes), quoted per RFC 9110. Allocates only the
   result. *)
let body_etag (body : string) : string =
  let h = ref 0x1000000000000077 (* odd 63-bit seed *) in
  for i = 0 to String.length body - 1 do
    h := (!h lxor Char.code (String.unsafe_get body i)) * 0x100000001b3 (* FNV-1a prime *)
  done;
  Printf.sprintf "\"%x\"" (!h land max_int)

(* replace any existing (case-insensitive), then prepend — via {!Headers.put}, whose compare is
   allocation-free (no per-element lowercased copy), since finalize calls this several times/response *)
let set_header headers k v = Headers.put headers k v

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
    let keep = set_header keep "Date" (Date.format_cached now) in
    { H.status = 304; headers = keep; body = "" }
  else begin
    (* 3. compression — negotiate the Accept-Encoding ONLY when the body is a compressible type, large
       enough to be worth it, and not already encoded; otherwise skip the parse entirely. A small
       compressible body still advertises Vary so caches know the representation varies by encoding. *)
    let ct_compressible = Mime.compressible ct in
    let body, headers =
      if ct_compressible && String.length resp.H.body >= min_compress_size && not (has_header resp.H.headers "content-encoding") then
        match Sem.negotiate_encoding ~accept:(header_ci req.H.headers "accept-encoding") () with
        | Sem.Gzip -> (Gzip.gzip resp.H.body, set_header (set_header headers "Content-Encoding" "gzip") "Vary" "Accept-Encoding")
        | Sem.Deflate -> (Gzip.deflate resp.H.body, set_header (set_header headers "Content-Encoding" "deflate") "Vary" "Accept-Encoding")
        | Sem.Identity -> (resp.H.body, set_header headers "Vary" "Accept-Encoding")
      else if ct_compressible then (resp.H.body, set_header headers "Vary" "Accept-Encoding")
      else (resp.H.body, headers)
    in

    (* 4. Date + Content-Length; HEAD keeps headers but empty body *)
    let headers = set_header headers "Date" (Date.format_cached now) in
    let headers = set_header headers "Content-Length" (string_of_int (String.length body)) in
    { H.status = resp.H.status; headers; body = (if is_head then "" else body) }
  end

(* ──── content_type_of ──── *)
let%test "content_type_of: explicit ct" =
  content_type_of { H.status = 200; headers = [("content-type", "text/html")]; body = "" } = "text/html"

let%test "content_type_of: missing ct" =
  content_type_of { H.status = 200; headers = []; body = "" } = "application/octet-stream"

(* ──── body_etag ──── *)
let%test "body_etag: quoted" =
  let e = body_etag "hello" in
  String.length e > 2 && e.[0] = '"' && e.[String.length e - 1] = '"'

let%test "body_etag: deterministic" =
  body_etag "hello" = body_etag "hello"

let%test "body_etag: different bodies differ" =
  body_etag "hello" <> body_etag "world"

(* ──── set_header / has_header ──── *)
let%test "set_header: adds header"   = has_header (set_header [] "X-Foo" "bar") "X-Foo"
let%test "has_header: missing"       = not (has_header [] "X-Foo")
let%test "set_header: replaces ci"   =
  let h = set_header [("content-type", "text/plain")] "Content-Type" "text/html" in
  Sem.header h "content-type" = Some "text/html"

(* ──── finalize ──── *)
let%test_unit "finalize: adds ETag and Content-Length" =
  let req = H.make_request ~meth:H.GET ~path:"/" () in
  let resp = { H.status = 200; headers = [("content-type", "text/plain")]; body = "hi" } in
  let r = finalize ~now:0.0 ~req resp in
  Fennec_hunt_unit.check "has ETag" (has_header r.H.headers "etag");
  Fennec_hunt_unit.check "has Content-Length" (has_header r.H.headers "content-length");
  Fennec_hunt_unit.check_eq "Content-Length" ~expected:"2" ~got:(Option.get (Sem.header r.H.headers "content-length"))

let%test_unit "finalize: HEAD returns empty body with headers" =
  let req = H.make_request ~meth:H.HEAD ~path:"/" () in
  let resp = { H.status = 200; headers = [("content-type", "text/plain")]; body = "hi" } in
  let r = finalize ~now:0.0 ~req resp in
  Fennec_hunt_unit.check "body is empty" (r.H.body = "");
  Fennec_hunt_unit.check "has Content-Length" (has_header r.H.headers "content-length")

let%test "finalize: preserves existing ETag" =
  let req = H.make_request ~meth:H.GET ~path:"/" () in
  let resp = { H.status = 200; headers = [("ETag", "\"custom\"")]; body = "x" } in
  let r = finalize ~now:0.0 ~req resp in
  Sem.header r.H.headers "etag" = Some "\"custom\""

let%test "finalize: small body not compressed" =
  let req = H.make_request ~meth:H.GET ~path:"/" ~headers:[("accept-encoding", "gzip")] () in
  let resp = { H.status = 200; headers = [("content-type", "text/html")]; body = "tiny" } in
  let r = finalize ~now:0.0 ~req resp in
  Sem.header r.H.headers "content-encoding" = None

let%test "finalize: Date header present" =
  let req = H.make_request ~meth:H.GET ~path:"/" () in
  let resp = { H.status = 200; headers = []; body = "" } in
  let r = finalize ~now:0.0 ~req resp in
  has_header r.H.headers "date"
