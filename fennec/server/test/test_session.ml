(* Signed-cookie sessions: HMAC sign/verify (incl. tamper + wrong-secret rejection)
   and a full paw round-trip (write -> Set-Cookie -> feed back -> read). *)

module Session = Fennec_server.Session
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http
module Headers = Fennec_core.Headers

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let req ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ()
let secret = "a-stable-server-secret"

(* the name=value pair to echo back as a Cookie request header (everything before ';') *)
let cookie_kv set_cookie =
  match String.index_opt set_cookie ';' with Some i -> String.sub set_cookie 0 i | None -> set_cookie

let finalize c = Conn.apply_before_send c (Option.get (Conn.resp c))

let () =
  print_endline "Session sign/verify:";
  let tok = Session.sign ~secret "hello world" in
  eq "verify round-trips" (Session.verify ~secret tok) (Some "hello world");
  eq "wrong secret rejected" (Session.verify ~secret:"other" tok) None;
  eq "tampered token rejected" (Session.verify ~secret (tok ^ "x")) None;
  eq "no signature -> rejected" (Session.verify ~secret "nodot") None;
  eq "empty -> rejected" (Session.verify ~secret "") None

let () =
  print_endline "Session paw (round-trip):";
  let sp = Session.make ~secret () in
  (* request 1: no cookie; put a value *)
  let c1 = sp (Conn.make (req "/")) in
  let c1 = Session.set c1 "user" "ada" in
  eq "read within the same request" (Session.get c1 "user") (Some "ada");
  let c1 = Conn.text c1 "ok" in
  let r1 = finalize c1 in
  let set1 = match Headers.get_all r1.H.headers "set-cookie" with [ s ] -> s | _ -> "" in
  check "a session cookie is set after a write" (set1 <> "");
  check "cookie is HttpOnly" (cookie_kv set1 <> set1 (* has attributes after ';' *));

  (* request 2: echo the cookie back; the session restores *)
  let kv = cookie_kv set1 in
  let c2 = sp (Conn.make (req ~headers:[ ("Cookie", kv) ] "/")) in
  eq "session restored on the next request" (Session.get c2 "user") (Some "ada");

  (* request 3: read only (no change) -> no Set-Cookie churn *)
  let c3 = sp (Conn.make (req ~headers:[ ("Cookie", kv) ] "/")) in
  let _ = Session.get c3 "user" in
  let c3 = Conn.text c3 "x" in
  let r3 = finalize c3 in
  eq "unchanged session emits no Set-Cookie" (Headers.get_all r3.H.headers "set-cookie") [];

  (* tampered cookie -> empty session (rejected, not trusted) *)
  let c4 = sp (Conn.make (req ~headers:[ ("Cookie", kv ^ "x") ] "/")) in
  eq "tampered cookie yields empty session" (Session.get c4 "user") None;

  (* clear empties it *)
  let c5 = sp (Conn.make (req ~headers:[ ("Cookie", kv) ] "/")) in
  let c5 = Session.clear c5 in
  eq "clear empties the session" (Session.get c5 "user") None

let set_cookie c =
  match Headers.get_all (finalize c).H.headers "set-cookie" with s :: _ -> s | [] -> ""

let () =
  print_endline "Session expiry + refresh:";
  let cookie payload = "_fennec_session=" ^ Session.sign ~secret payload in
  (* a far-future _exp loads normally *)
  let c = Session.make ~secret () (Conn.make (req ~headers:[ (cookie "_exp=9999999999&user=ada" |> fun kv -> ("Cookie", kv)) ] "/")) in
  eq "unexpired session loads its data" (Session.get c "user") (Some "ada");
  (* a past _exp loads empty *)
  let c = Session.make ~secret () (Conn.make (req ~headers:[ ("Cookie", cookie "_exp=1&user=ada") ] "/")) in
  eq "expired session loads empty" (Session.get c "user") None;
  (* _exp is hidden from get_all *)
  let c = Session.make ~secret () (Conn.make (req ~headers:[ ("Cookie", cookie "_exp=9999999999&user=ada") ] "/")) in
  eq "_exp hidden from get_all" (Session.get_all c) [ ("user", "ada") ];
  (* a past-half-life session is auto-refreshed even if unchanged *)
  let near = Printf.sprintf "_exp=%.0f&user=ada" (Unix.gettimeofday () +. 10.) in
  let c = Session.make ~secret ~lifetime:100. () (Conn.make (req ~headers:[ ("Cookie", cookie near) ] "/")) in
  let c = Conn.text c "x" in
  check "half-expired session is refreshed (Set-Cookie emitted)" (set_cookie c <> "");
  (* a fresh, unchanged session is not refreshed *)
  let fresh = Printf.sprintf "_exp=%.0f&user=ada" (Unix.gettimeofday () +. 1000.) in
  let c = Session.make ~secret ~lifetime:100. () (Conn.make (req ~headers:[ ("Cookie", cookie fresh) ] "/")) in
  let c = Conn.text c "x" in
  check "fresh unchanged session: no refresh" (set_cookie c = "")

let () =
  print_endline "Session server-side store:";
  let store = Session.memory_store () in
  (* request 1: write a value via the store *)
  let c1 = Session.make ~secret ~store () (Conn.make (req "/")) in
  let c1 = Session.set c1 "user" "ada" in
  let c1 = Conn.text c1 "ok" in
  let kv = cookie_kv (set_cookie c1) in
  let value = match String.index_opt kv '=' with Some i -> String.sub kv (i + 1) (String.length kv - i - 1) | None -> "" in
  let sid = Option.get (Session.verify ~secret value) in
  (* the DATA lives server-side (the cookie only carries the signed id) *)
  eq "data is in the server store, keyed by id" (store.Session.load sid) (Some [ ("user", "ada") ]);
  (* request 2: echo the id cookie -> the store rehydrates the session *)
  let c2 = Session.make ~secret ~store () (Conn.make (req ~headers:[ ("Cookie", kv) ] "/")) in
  eq "store: session rehydrated by id" (Session.get c2 "user") (Some "ada")

let contains s sub =
  let ls = String.length s and lb = String.length sub in
  let rec go i = i + lb <= ls && (String.sub s i lb = sub || go (i + 1)) in
  lb = 0 || go 0

let () =
  print_endline "Session Secure flag (proxy-aware):";
  (* a write so a Set-Cookie is emitted, then inspect its attributes *)
  let cookie_for headers =
    let c = Session.make ~secret () (Conn.make (req ~headers "/")) in
    set_cookie (Conn.text (Session.set c "user" "ada") "ok")
  in
  (* behind a TLS-terminating proxy the inner scheme is http but X-Forwarded-Proto is https,
     so the cookie MUST still be marked Secure *)
  check "X-Forwarded-Proto=https -> Secure cookie" (contains (cookie_for [ ("X-Forwarded-Proto", "https") ]) "Secure");
  check "plain http -> cookie not Secure" (not (contains (cookie_for []) "Secure"))

let () =
  if !fails = 0 then print_endline "all Session tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
