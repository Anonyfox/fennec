(* Signed-cookie sessions (Plug.Session's default store). The session is a small
   [string -> string] map serialized into a cookie, HMAC-SHA256 signed with a server
   secret so the client cannot tamper with it (it is signed, not encrypted, so do not
   store secrets in it). Verification is constant-time.

   Usage: put {!plug} early in a pipeline, then read/write with {!get}/{!put} from any
   downstream paw. The plug loads + verifies the cookie on the way in and, via a
   before_send hook, re-signs and re-sets the cookie on the way out — but only if the
   session actually changed, so unchanged requests don't churn Set-Cookie. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Assigns = Fennec_paw.Assigns
module H = Fennec_core.Http
module Cookie = Fennec_core.Cookie

(* ---- crypto + (de)serialization (pure) ---- *)

let b64e (s : string) : string = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d (s : string) : string option =
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

let hmac ~(secret : string) (msg : string) : string =
  Digestif.SHA256.(to_raw_string (hmac_string ~key:secret msg))

(* constant-time equality (no early exit on the first differing byte) *)
let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

(* the session map <-> a url-safe payload string ("k=v&k2=v2", each part %-encoded) *)
let encode (data : (string * string) list) : string =
  String.concat "&"
    (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) data)

let decode (payload : string) : (string * string) list = H.parse_query payload

(* "<b64 payload>.<b64 hmac(payload)>" *)
let sign ~(secret : string) (payload : string) : string =
  let p = b64e payload in
  p ^ "." ^ b64e (hmac ~secret p)

let verify ~(secret : string) (token : string) : string option =
  match String.index_opt token '.' with
  | None -> None
  | Some i ->
    let p = String.sub token 0 i in
    let sig_ = String.sub token (i + 1) (String.length token - i - 1) in
    if constant_eq sig_ (b64e (hmac ~secret p)) then b64d p else None

(* ---- the per-request session state, carried in a typed assign ---- *)

type t = { mutable data : (string * string) list; loaded : string }

let key : t Assigns.key = Assigns.key "fennec.session"

let current (c : Conn.t) : t option = Conn.get c key

(* read/write the session from any paw downstream of {!plug} *)
let get (c : Conn.t) (k : string) : string option =
  match current c with Some s -> List.assoc_opt k s.data | None -> None

let get_all (c : Conn.t) : (string * string) list =
  match current c with Some s -> s.data | None -> []

let set (c : Conn.t) (k : string) (v : string) : Conn.t =
  (match current c with Some s -> s.data <- (k, v) :: List.remove_assoc k s.data | None -> ());
  c

let delete (c : Conn.t) (k : string) : Conn.t =
  (match current c with Some s -> s.data <- List.remove_assoc k s.data | None -> ());
  c

let clear (c : Conn.t) : Conn.t =
  (match current c with Some s -> s.data <- [] | None -> ());
  c

(* The session plug. [secret] signs the cookie (keep it secret + stable). The cookie is
   HttpOnly + SameSite=Lax by default; [secure] defaults to whether the request is https. *)
let plug ~(secret : string) ?(cookie = "_fennec_session") ?(path = "/") ?max_age
    ?(same_site = Cookie.Lax) ?(http_only = true) ?secure () : Paw.t =
 fun c ->
  let loaded =
    match Conn.cookie c cookie with
    | Some tok -> ( match verify ~secret tok with Some p -> p | None -> "")
    | None -> ""
  in
  let sess = { data = decode loaded; loaded } in
  let secure = match secure with Some b -> b | None -> Conn.scheme c = "https" in
  let c = Conn.assign c key sess in
  Conn.before_send c (fun r ->
      let now = encode sess.data in
      if now = sess.loaded then r (* unchanged — don't reset the cookie *)
      else
        let sc =
          Cookie.to_set_cookie ~name:cookie ~value:(sign ~secret now) ~path ?max_age ~secure
            ~http_only ~same_site ()
        in
        { r with H.headers = ("set-cookie", sc) :: r.H.headers })
