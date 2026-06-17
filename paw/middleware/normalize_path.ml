(* Canonicalize the path by stripping trailing slashes — ["/foo/" → "/foo"], ["/a/b///" → "/a/b"] —
   with a 308 redirect (permanent, method- and body-preserving, so a POST to ["/x/"] re-issues as a
   POST to ["/x"]). Keeps URLs canonical for caches and search engines, and means routes only ever
   need the slash-free form. Root ["/"] is left alone. Declines after a single char-compare when there
   is no trailing slash (the overwhelming common case), so it adds ~nothing to the hot path. *)

module Conn = Conn
module H = Http

let make ?(status = 308) () : Pipeline.t =
 fun c ->
  let p = Conn.path c in
  let len = String.length p in
  if len > 1 && p.[len - 1] = '/' then begin
    let n = ref len in
    while !n > 1 && p.[!n - 1] = '/' do decr n done;
    let canon = String.sub p 0 !n in
    let qs = (Conn.req c).H.query_string in
    Conn.redirect ~status c (if qs = "" then canon else canon ^ "?" ^ qs)
  end
  else c

(* ──── normalize_path tests ──── *)

let conn_ ?(query_string = "") path = Conn.make (H.make_request ~meth:H.GET ~path ~query_string ())
let declined c = Conn.resp c = None
let loc_ c = let r = Option.value (Conn.resp c) ~default:(H.text "") in (r.H.status, Headers.get r.H.headers "location")

let%test "trailing slash redirects 308 to canonical" = loc_ (make () (conn_ "/foo/")) = (308, Some "/foo")
let%test "multiple trailing slashes collapse" = loc_ (make () (conn_ "/a/b///")) = (308, Some "/a/b")
let%test "preserves the query string" = loc_ (make () (conn_ ~query_string:"x=1" "/foo/")) = (308, Some "/foo?x=1")
let%test "root is left alone" = declined (make () (conn_ "/"))
let%test "no trailing slash declines" = declined (make () (conn_ "/foo"))
let%test "custom status" = fst (loc_ (make ~status:301 () (conn_ "/foo/"))) = 301
