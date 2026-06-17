(* HTTP Bearer-token auth (RFC 6750) — answer 401 with a challenge unless the request carries an
   [Authorization: Bearer <token>] that [verify] accepts. [verify] does the lookup (and should compare
   secrets in constant time); it may do IO (DB / cache) since it runs in the request fiber. *)

module Conn = Conn

let lower = String.lowercase_ascii

(* the auth scheme is case-insensitive (RFC 7235); pull the token out of "Bearer <token>" *)
let strip_bearer (v : string) : string option =
  if String.length v >= 7 && lower (String.sub v 0 7) = "bearer " then Some (String.trim (String.sub v 7 (String.length v - 7))) else None

(* escape a realm for a quoted-string so a stray quote/backslash can't break the header *)
let quote_realm (realm : string) : string =
  let b = Buffer.create (String.length realm + 2) in
  String.iter (fun ch -> if ch = '"' || ch = '\\' then Buffer.add_char b '\\'; Buffer.add_char b ch) realm;
  Buffer.contents b

let make ~(verify : string -> bool) ?(realm = "Restricted") () : Pipeline.t =
 fun c ->
  match Option.bind (Conn.req_header c "authorization") strip_bearer with
  | Some token when verify token -> c
  | _ -> Conn.text ~status:401 (Conn.set_header c "www-authenticate" (Printf.sprintf "Bearer realm=\"%s\"" (quote_realm realm))) "Unauthorized"

(* ──── bearer_auth tests ──── *)

module H_ = Http
module Headers_ = Headers
let req_ ?(headers = []) path = H_.make_request ~meth:H_.GET ~path ~headers ()
let resp_of_ c = Option.value (Conn.resp c) ~default:(H_.text ~status:404 "")
let ba_ = make ~verify:(fun t -> t = "s3cret") ~realm:"API" ()

let%test "valid token declines (passes through)" = not (Conn.answered (ba_ (Conn.make (req_ ~headers:[ ("authorization", "Bearer s3cret") ] "/"))))
let%test "lowercase scheme accepted" = not (Conn.answered (ba_ (Conn.make (req_ ~headers:[ ("authorization", "bearer s3cret") ] "/"))))
let%test "wrong token -> 401" = (resp_of_ (ba_ (Conn.make (req_ ~headers:[ ("authorization", "Bearer nope") ] "/")))).H_.status = 401
let%test "missing header -> 401" = (resp_of_ (ba_ (Conn.make (req_ "/")))).H_.status = 401
let%test "non-bearer scheme -> 401" = (resp_of_ (ba_ (Conn.make (req_ ~headers:[ ("authorization", "Basic abc") ] "/")))).H_.status = 401
let%test_unit "401 carries a Bearer challenge" =
  Fennec_hunt_unit.check "www-authenticate present" (Headers_.mem (resp_of_ (ba_ (Conn.make (req_ "/")))).H_.headers "www-authenticate")
