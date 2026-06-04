(* HTTP Basic auth — answer 401 with a challenge unless the credentials match. Credentials
   are compared in constant time (no early exit on a length/byte mismatch). *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw

let lower = String.lowercase_ascii

(* constant-time string equality (no early exit) — for comparing credentials *)
let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i ch -> acc := !acc lor (Char.code ch lxor Char.code b.[i])) a;
  !acc = 0

(* the auth scheme is case-insensitive (RFC 7617): accept "Basic ", "basic ", "BASIC " *)
let strip_basic (v : string) : string option =
  if String.length v >= 6 && lower (String.sub v 0 6) = "basic " then
    Some (String.sub v 6 (String.length v - 6))
  else None

(* escape a realm for a quoted-string so a stray quote/backslash can't break the header *)
let quote_realm (realm : string) : string =
  let b = Buffer.create (String.length realm + 2) in
  String.iter (fun ch -> if ch = '"' || ch = '\\' then Buffer.add_char b '\\'; Buffer.add_char b ch) realm;
  Buffer.contents b

let make ~username ~password ?(realm = "Restricted") () : Paw.t =
 fun c ->
  let ok =
    match Option.bind (Conn.req_header c "authorization") strip_basic with
    | Some enc -> (
      match Base64.decode enc with
      | Ok creds -> constant_eq creds (username ^ ":" ^ password)
      | Error _ -> false)
    | None -> false
  in
  if ok then c
  else
    Conn.text ~status:401
      (Conn.set_header c "www-authenticate" (Printf.sprintf "Basic realm=\"%s\"" (quote_realm realm)))
      "Unauthorized"
