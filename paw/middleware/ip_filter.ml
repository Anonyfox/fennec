(* Allow or deny a request by client IP — for locking an admin/internal endpoint to known networks, or
   restricting a webhook to its sender's IP ranges. Rules are exact IPs (["203.0.113.7"], ["::1"]) or
   IPv4 CIDR (["10.0.0.0/8"], ["192.168.1.0/24"]); [allow] passes only matching clients (everything
   else 403), [deny] blocks matching clients (everything else passes). Reads {!Conn.remote_ip}, so
   mount {!Trusted_proxy} first to filter the REAL client behind a reverse proxy. A request with no
   known peer IP fails closed for [allow] (blocked) and open for [deny] (passes). Rules are parsed once
   at construction; per request it's an int compare over the (few) rules. *)

module Conn = Conn
module H = Http

(* dotted-quad → 32-bit int, rejecting out-of-range / non-numeric octets; [None] if not an IPv4 *)
let ipv4_to_int s =
  match String.split_on_char '.' s with
  | [ a; b; c; d ] -> (
    match (int_of_string_opt a, int_of_string_opt b, int_of_string_opt c, int_of_string_opt d) with
    | Some a, Some b, Some c, Some d when a land lnot 255 = 0 && b land lnot 255 = 0 && c land lnot 255 = 0 && d land lnot 255 = 0 -> Some ((a lsl 24) lor (b lsl 16) lor (c lsl 8) lor d)
    | _ -> None)
  | _ -> None

(* an exact (string-compared, covers IPv6) rule, or an IPv4 network + mask *)
type rule = Exact of string | Cidr of int * int

let parse_rule s =
  match String.index_opt s '/' with
  | Some i -> (
    match (ipv4_to_int (String.sub s 0 i), int_of_string_opt (String.sub s (i + 1) (String.length s - i - 1))) with
    | Some net, Some bits when bits >= 0 && bits <= 32 ->
      let mask = if bits = 0 then 0 else (0xFFFFFFFF lsl (32 - bits)) land 0xFFFFFFFF in
      Cidr (net land mask, mask)
    | _ -> Exact s (* unparseable CIDR → exact string (won't match a real IP — fail safe) *))
  | None -> Exact s

let rule_matches client_int client_str = function
  | Exact s -> s = client_str
  | Cidr (net, mask) -> ( match client_int with Some ci -> ci land mask = net | None -> false)

(* parse the rule set ONCE; the returned predicate is the per-request hot path *)
let build (ips : string list) =
  let rules = List.map parse_rule ips in
  fun client_str -> let ci = ipv4_to_int client_str in List.exists (rule_matches ci client_str) rules

let allow (ips : string list) : Pipeline.t =
  let matches = build ips in
  fun c -> ( match Conn.remote_ip c with Some ip when matches ip -> c | _ -> Conn.text ~status:403 c "Forbidden")

let deny (ips : string list) : Pipeline.t =
  let matches = build ips in
  fun c -> ( match Conn.remote_ip c with Some ip when matches ip -> Conn.text ~status:403 c "Forbidden" | _ -> c)

(* ──── ip_filter tests ──── *)

let conn_ ?ip () = Conn.make (H.make_request ~meth:H.GET ~path:"/" ~remote_ip:ip ())
let blocked c = match Conn.resp c with Some r -> r.H.status = 403 | None -> false
let passed c = Conn.resp c = None

let%test "allow: exact IP passes" = passed (allow [ "203.0.113.7" ] (conn_ ~ip:"203.0.113.7" ()))
let%test "allow: a non-listed IP is 403" = blocked (allow [ "203.0.113.7" ] (conn_ ~ip:"203.0.113.8" ()))
let%test "allow: an IPv4 /24 matches inside" = passed (allow [ "192.168.1.0/24" ] (conn_ ~ip:"192.168.1.55" ()))
let%test "allow: outside the /24 is 403" = blocked (allow [ "192.168.1.0/24" ] (conn_ ~ip:"192.168.2.1" ()))
let%test "allow: a /8 matches broadly" = passed (allow [ "10.0.0.0/8" ] (conn_ ~ip:"10.255.3.9" ()))
let%test "allow: a /32 is a single host" = passed (allow [ "1.2.3.4/32" ] (conn_ ~ip:"1.2.3.4" ())) && blocked (allow [ "1.2.3.4/32" ] (conn_ ~ip:"1.2.3.5" ()))
let%test "allow: IPv6 exact passes" = passed (allow [ "::1" ] (conn_ ~ip:"::1" ()))
let%test "allow: an unknown peer fails closed (blocked)" = blocked (allow [ "10.0.0.0/8" ] (conn_ ()))
let%test "deny: a listed IP is 403" = blocked (deny [ "10.0.0.0/8" ] (conn_ ~ip:"10.1.2.3" ()))
let%test "deny: an unlisted IP passes" = passed (deny [ "10.0.0.0/8" ] (conn_ ~ip:"203.0.113.7" ()))
let%test "deny: an unknown peer fails open (passes)" = passed (deny [ "10.0.0.0/8" ] (conn_ ()))
let%test "allow: mixed exact + CIDR" = let f = allow [ "203.0.113.7"; "10.0.0.0/8" ] in passed (f (conn_ ~ip:"10.9.9.9" ())) && passed (f (conn_ ~ip:"203.0.113.7" ())) && blocked (f (conn_ ~ip:"8.8.8.8" ()))
