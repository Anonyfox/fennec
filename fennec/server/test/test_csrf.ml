(* CSRF protection: masked-token generation, verify (accept/tamper/missing), the
   double-submit through the session, and the plug gating unsafe methods. *)

module Csrf = Fennec_server.Csrf
module Session = Fennec_server.Session
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let req ?(meth = H.GET) ?(headers = []) ?(body = "") path = H.make_request ~meth ~path ~headers ~body ()
let secret = "csrf-test-secret"

(* a conn with an active session (so CSRF has somewhere to store its secret) *)
let with_session ?(cookie = "") () =
  let headers = if cookie = "" then [] else [ ("Cookie", cookie) ] in
  Session.plug ~secret () (Conn.make (req ~headers "/"))

(* extract the session cookie's name=value to echo back on the next request *)
let session_cookie c =
  let r = Conn.apply_before_send c (Option.value (Conn.resp c) ~default:(H.text "")) in
  match Fennec_core.Headers.get_all r.H.headers "set-cookie" with
  | s :: _ -> ( match String.index_opt s ';' with Some i -> String.sub s 0 i | None -> s)
  | [] -> ""

let () =
  print_endline "Csrf token + verify:";
  let c = with_session () in
  let t1 = Csrf.token c in
  let t2 = Csrf.token c in
  check "tokens are non-empty" (t1 <> "" && t2 <> "");
  check "each render masks differently (BREACH-safe)" (t1 <> t2);
  check "a freshly minted token verifies" (Csrf.verify c t1);
  check "the other masked token also verifies" (Csrf.verify c t2);
  check "a tampered token is rejected" (not (Csrf.verify c (t1 ^ "AA")));
  check "garbage is rejected" (not (Csrf.verify c "!!!not-base64!!!"));
  (* a different session (no shared secret) must not validate this token *)
  let other = with_session () in
  check "token from another session is rejected" (not (Csrf.verify other t1))

let () =
  print_endline "Csrf plug:";
  (* establish a session + token on a GET, capture the cookie *)
  let g = with_session () in
  let tok = Csrf.token g in
  let g = Conn.text g "form" in
  let cookie = session_cookie g in
  let mk ?(meth = H.POST) ?(headers = []) ?(body = "") () =
    Session.plug ~secret () (Conn.make (req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = Csrf.plug () in
  (* safe method: always passes *)
  check "GET is not gated" (not (Conn.answered (csrf (mk ~meth:H.GET ()))));
  (* unsafe with a valid token in the body field: passes *)
  let body = "_csrf_token=" ^ tok in
  let c_ok =
    csrf (mk ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body ())
  in
  check "POST with a valid token passes" (not (Conn.answered c_ok));
  (* unsafe with a valid token in the header: passes *)
  let c_hdr = csrf (mk ~headers:[ ("x-csrf-token", tok) ] ()) in
  check "POST with a valid header token passes" (not (Conn.answered c_hdr));
  (* unsafe with no token: 403 *)
  let c_no = csrf (mk ()) in
  eq "POST with no token -> 403" (match Conn.resp c_no with Some r -> r.H.status | None -> 0) 403;
  (* unsafe with a wrong token: 403 *)
  let c_bad = csrf (mk ~headers:[ ("x-csrf-token", "wrong") ] ()) in
  eq "POST with a bad token -> 403" (match Conn.resp c_bad with Some r -> r.H.status | None -> 0) 403

let () =
  if !fails = 0 then print_endline "all CSRF tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
