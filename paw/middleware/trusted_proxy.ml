(* Recover the real client IP and scheme when the app sits behind a reverse proxy / load balancer.
   Only acts when the transport peer is itself trusted (default: loopback + RFC1918 private + IPv6
   ULA), so a direct client can't forge [X-Forwarded-For]. The client IP is the RIGHTMOST entry that
   isn't a trusted proxy — proxies append the peer they saw, so walking from the right past the known
   proxies lands on the real client and ignores anything a client spoofed on the left. The scheme is
   the (leftmost = original) [X-Forwarded-Proto]. Both are written as conn overrides, so downstream
   {!Conn.remote_ip} / {!Conn.scheme} (and every paw built on them — Force_https, Rate_limit, Logger)
   see the true values. Transparent: never answers; mount it FIRST. *)

module Conn = Conn
module H = Http

let starts_with pre s = String.length s >= String.length pre && String.sub s 0 (String.length pre) = pre

(* loopback + RFC1918 private + IPv6 loopback/ULA — the ranges a reverse proxy realistically sits in *)
let default_trusted ip =
  let ip = String.lowercase_ascii ip in
  starts_with "127." ip || starts_with "10." ip || starts_with "192.168." ip || ip = "::1" || starts_with "fc" ip || starts_with "fd" ip
  || (starts_with "172." ip && match String.split_on_char '.' ip with _ :: o2 :: _ -> ( match int_of_string_opt o2 with Some n -> n >= 16 && n <= 31 | None -> false) | _ -> false)

(* every X-Forwarded-For entry, left to right, across repeated headers and comma lists *)
let forwarded_entries c header = Conn.req_headers c header |> List.concat_map (String.split_on_char ',') |> List.map String.trim |> List.filter (fun s -> s <> "")

(* the real client = first non-trusted entry scanning from the right; all-trusted → the leftmost (origin) *)
let client_ip trusted entries =
  match entries with
  | [] -> None
  | _ -> ( match List.find_opt (fun ip -> not (trusted ip)) (List.rev entries) with Some ip -> Some ip | None -> Some (List.hd entries))

let forwarded_proto c header =
  match Conn.req_header c header with
  | None -> None
  | Some v -> ( match String.split_on_char ',' v with first :: _ -> ( let s = String.lowercase_ascii (String.trim first) in if s = "http" || s = "https" then Some s else None) | [] -> None)

let make ?(trusted = default_trusted) ?(for_header = "x-forwarded-for") ?(proto_header = "x-forwarded-proto") () : Pipeline.t =
 fun c ->
  match (Conn.req c).H.remote_ip with
  | Some peer when trusted peer ->
    let c = match client_ip trusted (forwarded_entries c for_header) with Some ip -> Conn.override_remote_ip c ip | None -> c in
    let c = match forwarded_proto c proto_header with Some s -> Conn.override_scheme c s | None -> c in
    c
  | _ -> c

(* ──── trusted_proxy tests ──── *)

let conn_ ?(peer = "127.0.0.1") ?(headers = []) () = Conn.make (H.make_request ~meth:H.GET ~path:"/" ~remote_ip:(Some peer) ~headers ())

let%test "trusted peer: single XFF entry becomes the client IP" =
  Conn.remote_ip (make () (conn_ ~headers:[ ("x-forwarded-for", "203.0.113.7") ] ())) = Some "203.0.113.7"
let%test "untrusted peer: XFF is ignored (no spoofing)" =
  Conn.remote_ip (make () (conn_ ~peer:"203.0.113.9" ~headers:[ ("x-forwarded-for", "10.0.0.1") ] ())) = Some "203.0.113.9"
let%test "spoofed leftmost is skipped; rightmost-untrusted wins" =
  (* client sent a fake, proxy appended the real client: 'fake, real-client' *)
  Conn.remote_ip (make () (conn_ ~headers:[ ("x-forwarded-for", "1.2.3.4, 203.0.113.7") ] ())) = Some "203.0.113.7"
let%test "chain of trusted proxies is walked past to the client" =
  Conn.remote_ip (make () (conn_ ~headers:[ ("x-forwarded-for", "203.0.113.7, 10.0.0.2, 10.0.0.3") ] ())) = Some "203.0.113.7"
let%test "all-trusted chain falls back to the leftmost (origin)" =
  Conn.remote_ip (make () (conn_ ~headers:[ ("x-forwarded-for", "10.0.0.2, 10.0.0.3") ] ())) = Some "10.0.0.2"
let%test "X-Forwarded-Proto rewrites the scheme" =
  Conn.scheme (make () (conn_ ~headers:[ ("x-forwarded-proto", "https") ] ())) = "https"
let%test "untrusted peer: scheme is left alone" =
  Conn.scheme (make () (conn_ ~peer:"203.0.113.9" ~headers:[ ("x-forwarded-proto", "https") ] ())) = "http"
let%test "no XFF: remote_ip stays the transport peer" =
  Conn.remote_ip (make () (conn_ ~peer:"10.0.0.5" ())) = Some "10.0.0.5"
let%test "172.16/12 is trusted, 172.32 is not" = default_trusted "172.16.0.1" && default_trusted "172.31.255.1" && not (default_trusted "172.32.0.1")
let%test "custom trusted predicate" =
  Conn.remote_ip (make ~trusted:(fun ip -> ip = "198.51.100.1") () (conn_ ~peer:"198.51.100.1" ~headers:[ ("x-forwarded-for", "203.0.113.7") ] ())) = Some "203.0.113.7"
