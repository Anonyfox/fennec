(* CSRF: masked + signed + expiring tokens; the distinguishable outcomes
   (Ok/Expired/Wrong_session/Invalid); and the paw gating unsafe methods. *)

module Csrf = Fennec_server.Csrf
module Session = Fennec_server.Session
module Conn = Fennec_paw.Conn
module H = Fennec_core.Http

let fails = ref 0
let check name c = if c then Printf.printf "  ok   %s\n" name else (incr fails; Printf.printf "  FAIL %s\n" name)
let eq name a b = check name (a = b)
let req ?(meth = H.GET) ?(headers = []) ?(body = "") path = H.make_request ~meth ~path ~headers ~body ()
let app_secret = "app-signing-secret"
let sess_secret = "session-signing-secret"

(* a conn with an active session (so CSRF has somewhere to store its per-session secret) *)
let with_session () = Session.make ~secret:sess_secret () (Conn.make (req "/"))

let () =
  print_endline "Csrf token + verify (outcomes):";
  let c = with_session () in
  let t1 = Csrf.token ~secret:app_secret c in
  let t2 = Csrf.token ~secret:app_secret c in
  check "tokens non-empty" (t1 <> "" && t2 <> "");
  check "each render masks differently (BREACH-safe)" (t1 <> t2);
  eq "a fresh token is Ok" (Csrf.verify ~secret:app_secret c t1) Csrf.Ok;
  eq "the other masked token is also Ok" (Csrf.verify ~secret:app_secret c t2) Csrf.Ok;
  eq "wrong app secret -> Invalid" (Csrf.verify ~secret:"other" c t1) Csrf.Invalid;
  eq "tampered token -> Invalid" (Csrf.verify ~secret:app_secret c (t1 ^ "AA")) Csrf.Invalid;
  eq "garbage -> Invalid" (Csrf.verify ~secret:app_secret c "no-dot") Csrf.Invalid;
  (* a token bound to a different session -> Wrong_session *)
  let other = with_session () in
  eq "token from another session -> Wrong_session" (Csrf.verify ~secret:app_secret other t1) Csrf.Wrong_session;
  (* an already-expired token -> Expired *)
  let expired = Csrf.token ~secret:app_secret ~valid_for:(-1.0) c in
  eq "past-expiry token -> Expired" (Csrf.verify ~secret:app_secret c expired) Csrf.Expired

let () =
  print_endline "Csrf paw:";
  (* establish a session + token on a GET, capture the session cookie *)
  let g = with_session () in
  let tok = Csrf.token ~secret:app_secret g in
  let g = Conn.text g "form" in
  let cookie =
    match
      Fennec_core.Headers.get_all
        (Conn.apply_before_send g (Option.value (Conn.resp g) ~default:(H.text ""))).H.headers
        "set-cookie"
    with
    | s :: _ -> ( match String.index_opt s ';' with Some i -> String.sub s 0 i | None -> s)
    | [] -> ""
  in
  let mk ?(meth = H.POST) ?(headers = []) ?(body = "") () =
    Session.make ~secret:sess_secret () (Conn.make (req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = Csrf.make ~secret:app_secret () in
  check "GET is not gated" (not (Conn.answered (csrf (mk ~meth:H.GET ()))));
  check "POST with a valid header token passes" (not (Conn.answered (csrf (mk ~headers:[ ("x-csrf-token", tok) ] ()))));
  let body = "_csrf_token=" ^ tok in
  check "POST with a valid body token passes"
    (not (Conn.answered (csrf (mk ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body ()))));
  eq "POST with no token -> 403" (match Conn.resp (csrf (mk ())) with Some r -> r.H.status | None -> 0) 403;
  eq "POST with a bad token -> 403"
    (match Conn.resp (csrf (mk ~headers:[ ("x-csrf-token", "wrong") ] ())) with Some r -> r.H.status | None -> 0)
    403

let () =
  print_endline "Csrf requires a session:";
  let no_sess () = Conn.make (req "/") in
  check "make raises without an upstream session"
    (try ignore (Csrf.make ~secret:app_secret () (no_sess ())); false with Failure _ -> true);
  check "token raises without an upstream session"
    (try ignore (Csrf.token ~secret:app_secret (no_sess ())); false with Failure _ -> true)

let () =
  if !fails = 0 then print_endline "all CSRF tests passed."
  else (Printf.printf "%d FAILED\n" !fails; exit 1)
