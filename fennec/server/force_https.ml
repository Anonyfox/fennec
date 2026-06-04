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
