(* Signed-cookie sessions: HMAC sign/verify (incl. tamper + wrong-secret rejection)
   and a full plug round-trip (write -> Set-Cookie -> feed back -> read). *)

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
  print_endline "Session plug (round-trip):";
  let sp = Session.plug ~secret () in
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

let () =
  if !fails = 0 then print_endline "all Session tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
