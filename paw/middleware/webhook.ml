(* Verify the HMAC signature on an inbound webhook (GitHub, Slack, and providers that sign the raw body
   with a shared secret) before trusting it. {!verify} is the pure check — constant-time, tolerant of a
   [sha256=]/[sha1=] provider prefix and of upper/lower-case hex — for wiring a provider's exact scheme
   (e.g. Stripe's timestamped [t=…,v1=…], which you split yourself). {!guard} is the drop-in paw for the
   common case: it reads the signature header and 401s unless it verifies over the raw request body.

   {[ hooks |> Endpoint.use (Webhook.guard ~secret:gh_secret ()) |> Endpoint.post "/gh" handle ]} *)

module Conn = Conn
module H = Http

type algo = SHA1 | SHA256

let hmac_hex algo ~(secret : string) (body : string) : string =
  match algo with SHA1 -> Digestif.SHA1.(to_hex (hmac_string ~key:secret body)) | SHA256 -> Digestif.SHA256.(to_hex (hmac_string ~key:secret body))

(* constant-time equality over equal-length strings (digest length isn't secret — hex is fixed width) *)
let constant_eq a b = String.length a = String.length b && (let acc = ref 0 in String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a; !acc = 0)

(* drop a [sha256=]/[sha1=] provider prefix; a bare hex digest is left as-is *)
let strip_algo s =
  let pre p = String.length s >= String.length p && String.sub s 0 (String.length p) = p in
  if pre "sha256=" then String.sub s 7 (String.length s - 7) else if pre "sha1=" then String.sub s 5 (String.length s - 5) else s

let verify ?(algo = SHA256) ~(secret : string) ~(body : string) (signature : string) : bool =
  constant_eq (String.lowercase_ascii (strip_algo (String.trim signature))) (hmac_hex algo ~secret body)

let guard ?(header = "x-hub-signature-256") ?(algo = SHA256) ~(secret : string) () : Pipeline.t =
 fun c -> ( match Conn.req_header c header with Some s when verify ~algo ~secret ~body:(Conn.req c).H.body s -> c | _ -> Conn.text ~status:401 c "Invalid signature")

(* ──── webhook tests ──── *)

let body_ = {|{"event":"push","n":1}|}
let secret_ = "s3cr3t"
let sig256_ = hmac_hex SHA256 ~secret:secret_ body_

let%test "verify: a correct SHA256 hex passes" = verify ~secret:secret_ ~body:body_ sig256_
let%test "verify: GitHub's sha256= prefix is accepted" = verify ~secret:secret_ ~body:body_ ("sha256=" ^ sig256_)
let%test "verify: a wrong secret fails" = not (verify ~secret:"nope" ~body:body_ sig256_)
let%test "verify: a tampered body fails" = not (verify ~secret:secret_ ~body:{|{"event":"push","n":2}|} sig256_)
let%test "verify: uppercase hex is tolerated" = verify ~secret:secret_ ~body:body_ (String.uppercase_ascii sig256_)
let%test "verify: SHA1 with its prefix" = verify ~algo:SHA1 ~secret:secret_ ~body:body_ ("sha1=" ^ hmac_hex SHA1 ~secret:secret_ body_)
let%test "verify: a garbage signature fails (no crash)" = not (verify ~secret:secret_ ~body:body_ "sha256=zzzz")

let conn_ ?sig_ () = Conn.make (H.make_request ~meth:H.POST ~path:"/hook" ~headers:(match sig_ with Some s -> [ ("x-hub-signature-256", s) ] | None -> []) ~body:body_ ())
let status_ c = match Conn.resp c with Some r -> r.H.status | None -> 0
let declined c = Conn.resp c = None

let%test "guard: a valid signature passes" = declined (guard ~secret:secret_ () (conn_ ~sig_:("sha256=" ^ sig256_) ()))
let%test "guard: a missing signature is 401" = status_ (guard ~secret:secret_ () (conn_ ())) = 401
let%test "guard: a bad signature is 401" = status_ (guard ~secret:secret_ () (conn_ ~sig_:"sha256=deadbeef" ())) = 401
