(* CORS — Cross-Origin Resource Sharing. Reflects/allows an origin, answers preflight (OPTIONS +
   Access-Control-Request-Method) with 204 + the negotiated headers, and stamps actual responses with
   Access-Control-Allow-Origin (+ credentials / exposed headers). A request without an Origin header
   is not a CORS request and passes through untouched. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

(* which origins to allow: any, or an explicit allowlist (reflected back when matched) *)
type origin = Any | These of string list

let allowed origins req_origin = match origins with Any -> true | These list -> List.mem req_origin list

(* the value for Access-Control-Allow-Origin. "*" is illegal with credentials, so when credentials
   are on we always reflect the concrete origin (and Vary: Origin keeps caches correct). *)
let allow_origin_value ~credentials origins req_origin =
  match origins with Any when not credentials -> "*" | _ -> req_origin

let make ?(origins = Any) ?(methods = [ "GET"; "HEAD"; "POST"; "PUT"; "PATCH"; "DELETE"; "OPTIONS" ])
    ?(headers = [ "Content-Type"; "Authorization" ]) ?(expose = []) ?(credentials = false) ?(max_age = 600) () : Paw.t =
 fun c ->
  match Conn.req_header c "origin" with
  | None -> c (* not a cross-origin request *)
  | Some origin when not (allowed origins origin) -> c (* origin not allowed — emit no CORS headers; the browser blocks *)
  | Some origin ->
    let allow = allow_origin_value ~credentials origins origin in
    let base =
      ("Access-Control-Allow-Origin", allow)
      :: ("Vary", "Origin")
      :: (if credentials then [ ("Access-Control-Allow-Credentials", "true") ] else [])
    in
    let is_preflight = (Conn.req c).H.meth = H.OPTIONS && Conn.req_header c "access-control-request-method" <> None in
    if is_preflight then
      let pre =
        base
        @ [ ("Access-Control-Allow-Methods", String.concat ", " methods);
            ("Access-Control-Allow-Headers", String.concat ", " headers);
            ("Access-Control-Max-Age", string_of_int max_age) ]
      in
      Conn.text ~status:204 ~headers:pre c ""
    else
      let extra = if expose = [] then [] else [ ("Access-Control-Expose-Headers", String.concat ", " expose) ] in
      Conn.before_send c (fun r -> { r with H.headers = base @ extra @ r.H.headers })

(* ──── cors tests ──── *)

let req_ ?(meth = H.GET) ?(headers = []) path = H.make_request ~meth ~path ~headers ~host:"app.test" ()
let resp_of_ c = Option.value (Conn.resp c) ~default:(H.text ~status:404 "")
let finalize_ c = Conn.apply_before_send c (resp_of_ c)
let hdr_ r k = List.assoc_opt k (List.map (fun (a, b) -> (String.lowercase_ascii a, b)) r.H.headers)

let%test "no Origin → pass through, no CORS headers" =
  let c = (make ()) (Conn.make (req_ "/x")) in
  Conn.resp c = None && hdr_ (finalize_ c) "access-control-allow-origin" = None

let%test "actual request → reflects allow-origin (* for Any)" =
  let c = (make ()) (Conn.make (req_ ~headers:[ ("origin", "https://a.com") ] "/x")) in
  hdr_ (finalize_ c) "access-control-allow-origin" = Some "*"

let%test "preflight → 204 with methods" =
  let c = (make ()) (Conn.make (req_ ~meth:H.OPTIONS ~headers:[ ("origin", "https://a.com"); ("access-control-request-method", "POST") ] "/x")) in
  let r = resp_of_ c in
  r.H.status = 204 && hdr_ r "access-control-allow-methods" <> None

let%test "allowlist: disallowed origin gets no CORS headers" =
  let c = (make ~origins:(These [ "https://ok.com" ]) ()) (Conn.make (req_ ~headers:[ ("origin", "https://evil.com") ] "/x")) in
  hdr_ (finalize_ c) "access-control-allow-origin" = None

let%test "credentials → reflects concrete origin (never *)" =
  let c = (make ~credentials:true ()) (Conn.make (req_ ~headers:[ ("origin", "https://a.com") ] "/x")) in
  let r = finalize_ c in
  hdr_ r "access-control-allow-origin" = Some "https://a.com" && hdr_ r "access-control-allow-credentials" = Some "true"
