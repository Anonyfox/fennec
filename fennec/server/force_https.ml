(* Force HTTPS — redirect plain-http requests to https, honouring an upstream
   X-Forwarded-Proto from a TLS-terminating proxy. Already-https requests pass through. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

(* the client-side scheme: an upstream X-Forwarded-Proto (from a TLS-terminating proxy) wins
   over [Conn.scheme]. The header may list several ("https, http"); take the first, else a
   value like "https, http" would never equal "https" and we'd redirect an already-secure
   request into a loop. *)
let client_scheme (c : Conn.t) : string =
  match Conn.req_header c "x-forwarded-proto" with
  | Some p -> (
    match String.split_on_char ',' p with
    | first :: _ when String.trim first <> "" -> String.lowercase_ascii (String.trim first)
    | _ -> Conn.scheme c)
  | None -> Conn.scheme c

(* [status] defaults to 308 (permanent + method/body preserving) so a redirected POST keeps
   its method and body; a 301/302 would have the browser retry as GET and drop the body.
   [hsts], if set, is a Strict-Transport-Security max-age (seconds) emitted on already-secure
   responses so the browser upgrades future requests itself (added only if absent). *)
let make ?(status = 308) ?hsts () : Paw.t =
 fun c ->
  let https = client_scheme c = "https" in
  if https || Conn.host c = "" then
    match hsts with
    | Some max_age when https ->
      Conn.before_send c (fun r ->
          if List.exists (fun (k, _) -> String.lowercase_ascii k = "strict-transport-security") r.H.headers then r
          else
            { r with H.headers = ("Strict-Transport-Security", Printf.sprintf "max-age=%d; includeSubDomains" max_age) :: r.H.headers })
    | _ -> c
  else
    let qs = (Conn.req c).H.query_string in
    let target = "https://" ^ Conn.host c ^ Conn.path c ^ (if qs = "" then "" else "?" ^ qs) in
    Conn.redirect ~status c target

(* ──── force_https tests ──── *)

module Headers_ = Fennec_core.Headers
let req_ ?(host = "") ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ~host ()
let resp_of_ c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")
let finalize_ c = Conn.apply_before_send c (resp_of_ c)

let%test "http -> 308 (method/body preserving)" =
  let fh = make () in
  let c = fh (Conn.make (req_ ~host:"example.com" "/a/b")) in
  (resp_of_ c).H.status = 308

let%test "redirects to https target" =
  let fh = make () in
  let c = fh (Conn.make (req_ ~host:"example.com" "/a/b")) in
  Headers_.get (resp_of_ c).H.headers "location" = Some "https://example.com/a/b"

let%test "already https declines" =
  let fh = make () in
  let c2 = fh (Conn.make (req_ ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https") ] "/")) in
  not (Conn.answered c2)

let%test "X-Forwarded-Proto list 'https, http' declines (no loop)" =
  let fh = make () in
  let c3 = fh (Conn.make (req_ ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https, http") ] "/")) in
  not (Conn.answered c3)

let%test_unit "hsts emitted on https response" =
  let fh2 = make ~hsts:31536000 () in
  let cs = fh2 (Conn.make (req_ ~host:"example.com" ~headers:[ ("x-forwarded-proto", "https") ] "/")) in
  let rs = finalize_ (Conn.text cs "x") in
  Fennec_hunt_unit.check "hsts present" (Headers_.get rs.H.headers "strict-transport-security" <> None)
