(* CSRF protection (Dream.csrf-grade). A per-session secret guards
   state-changing requests. A token embedded in a form is:
     - MASKED with fresh randomness each render (so the value differs every time —
       defeating BREACH-style compression oracles),
     - carries an EXPIRY, and
     - is HMAC-signed with the app secret (so the expiry/payload can't be forged).
   {!verify} returns a distinguishable outcome (Ok / Expired / Wrong_session / Invalid),
   and {!make} rejects unsafe requests whose token isn't [Ok]. Constant-time throughout.
   Requires {!Session.make} earlier in the pipeline. *)

module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module H = Fennec_core.Http

(** Why a CSRF token did or didn't validate. [Expired]/[Wrong_session] happen in normal use
    (a form or session that aged out); [Invalid] means a bad signature or forged payload. *)
type outcome = Ok | Expired | Wrong_session | Invalid

let token_len = 18 (* per-session secret bytes *)
let session_key = "_csrf_secret"
let now () = Unix.gettimeofday ()

(* ──── b64e ──── *)

let b64e (s : string) : string = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s

(* ──── b64d ──── *)

let b64d (s : string) : string option =
  match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

(* ──── hmac ──── *)

let hmac ~(secret : string) (msg : string) : string =
  Digestif.SHA256.(to_raw_string (hmac_string ~key:secret msg))

(* ──── secure_random ──── *)

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg ->
    (* fail CLOSED: never weaken a CSRF secret with a non-CSPRNG fallback *)
    failwith ("fennec: secure randomness unavailable (/dev/urandom): " ^ msg)

(* ──── xor ──── *)

let xor (a : string) (b : string) : string =
  String.mapi (fun i ch -> Char.chr (Char.code ch lxor Char.code b.[i])) a

(* ──── constant_eq ──── *)

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i ch -> acc := !acc lor (Char.code ch lxor Char.code b.[i])) a;
  !acc = 0

(* ──── require_session ──── *)

(* CSRF stores its per-session secret in the session, so a session paw MUST run first. A
   misordered pipeline would otherwise fail silently (tokens unverifiable, every POST 403) —
   surface it loudly instead. *)
let require_session (c : Conn.t) : unit =
  if not (Session.active c) then
    failwith "Fennec.Paw.Csrf: no active session — add Paw.Session.make earlier in the pipeline"

(* ──── session_secret ──── *)

(* the per-session CSRF secret, creating + storing one if absent (for {!token}) *)
let session_secret (c : Conn.t) : string =
  match Option.bind (Session.get c session_key) b64d with
  | Some raw when String.length raw = token_len -> raw
  | _ ->
    let raw = secure_random token_len in
    ignore (Session.set c session_key (b64e raw));
    raw

(* ──── session_secret_opt ──── *)

(* the per-session CSRF secret WITHOUT creating one (for {!verify}) *)
let session_secret_opt (c : Conn.t) : string option =
  match Option.bind (Session.get c session_key) b64d with
  | Some raw when String.length raw = token_len -> Some raw
  | _ -> None

(* ──── token ──── *)

(* a fresh, embeddable token: masked secret + expiry, signed with the app [secret] *)
let token ~(secret : string) ?(valid_for = 3600.) (c : Conn.t) : string =
  require_session c;
  let raw = session_secret c in
  let mask = secure_random token_len in
  let masked = b64e (mask ^ xor mask raw) in
  let payload = Printf.sprintf "%s|%.0f" masked (now () +. valid_for) in
  payload ^ "." ^ b64e (hmac ~secret payload)

let _req ?(meth = H.GET) ?(headers = []) ?(body = "") path =
  H.make_request ~meth ~path ~headers ~body ()

let _app_secret = "app-signing-secret"
let _sess_secret = "session-signing-secret"
let _with_session () = Session.make ~secret:_sess_secret () (Conn.make (_req "/"))

let%test "tokens non-empty" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  t1 <> ""

let%test "each render masks differently" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  let t2 = token ~secret:_app_secret c in
  t1 <> t2

let%test "token raises without session" =
  let no_sess () = Conn.make (_req "/") in
  (try ignore (token ~secret:_app_secret (no_sess ())); false with Failure _ -> true)

(* ──── verify ──── *)

(* validate a submitted token against the app [secret] and the session secret *)
let verify ~(secret : string) (c : Conn.t) (tok : string) : outcome =
  match String.rindex_opt tok '.' with
  | None -> Invalid
  | Some i ->
    let payload = String.sub tok 0 i and sig_ = String.sub tok (i + 1) (String.length tok - i - 1) in
    if not (constant_eq sig_ (b64e (hmac ~secret payload))) then Invalid
    else (
      match String.index_opt payload '|' with
      | None -> Invalid
      | Some j -> (
        let masked_b64 = String.sub payload 0 j in
        let exp_str = String.sub payload (j + 1) (String.length payload - j - 1) in
        match float_of_string_opt exp_str with
        | None -> Invalid
        | Some exp when now () > exp -> Expired
        | Some _ -> (
          match (b64d masked_b64, session_secret_opt c) with
          | Some s, Some raw when String.length s = 2 * token_len ->
            let mask = String.sub s 0 token_len and m = String.sub s token_len token_len in
            if constant_eq (xor mask m) raw then Ok else Wrong_session
          | _, None -> Wrong_session
          | _ -> Invalid)))

let%test "fresh token is Ok" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  verify ~secret:_app_secret c t1 = Ok

let%test "other masked token also Ok" =
  let c = _with_session () in
  let _t1 = token ~secret:_app_secret c in
  let t2 = token ~secret:_app_secret c in
  verify ~secret:_app_secret c t2 = Ok

let%test "wrong app secret -> Invalid" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  verify ~secret:"other" c t1 = Invalid

let%test "tampered token -> Invalid" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  verify ~secret:_app_secret c (t1 ^ "AA") = Invalid

let%test "garbage -> Invalid" =
  let c = _with_session () in
  verify ~secret:_app_secret c "no-dot" = Invalid

let%test "other session -> Wrong_session" =
  let c = _with_session () in
  let t1 = token ~secret:_app_secret c in
  let other = _with_session () in
  verify ~secret:_app_secret other t1 = Wrong_session

let%test "past-expiry -> Expired" =
  let c = _with_session () in
  let expired = token ~secret:_app_secret ~valid_for:(-1.0) c in
  verify ~secret:_app_secret c expired = Expired

(* ──── make ──── *)

(* The CSRF paw: verify the token on unsafe methods (from the [header] or a body [field]),
   answer 403 unless it is [Ok], decline on [safe] methods. [secret] signs the tokens. *)
let make ~(secret : string) ?(field = "_csrf_token") ?(header = "x-csrf-token")
    ?(safe = [ "GET"; "HEAD"; "OPTIONS" ]) () : Paw.t =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Paw.Csrf.make: ~secret must be at least 16 bytes (got %d) — use a long random string"
         (String.length secret));
  fun c ->
  require_session c;
  if List.mem (H.string_of_meth (Conn.meth c)) safe then c
  else
    let submitted =
      match Conn.req_header c header with Some v -> Some v | None -> Conn.body_param c field
    in
    match submitted with
    | Some tok when verify ~secret c tok = Ok -> c
    | _ -> Conn.text ~status:403 c "CSRF token invalid or missing"

let%test "weak secret rejected" =
  (match (try Some (make ~secret:"short" ()) with Invalid_argument _ -> None) with Some _ -> false | None -> true)

let%test "make raises without session" =
  let no_sess () = Conn.make (_req "/") in
  (try ignore (make ~secret:_app_secret () (no_sess ())); false with Failure _ -> true)

let%test_unit "GET is not gated" =
  let g = _with_session () in
  let _tok = token ~secret:_app_secret g in
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
  let mk ?(meth = H.GET) ?(headers = []) ?(body = "") () =
    Session.make ~secret:_sess_secret () (Conn.make (_req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = make ~secret:_app_secret () in
  Fennec_hunt_unit.check "GET not gated" (not (Conn.answered (csrf (mk ~meth:H.GET ()))))

let%test_unit "POST with valid header passes" =
  let g = _with_session () in
  let tok = token ~secret:_app_secret g in
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
    Session.make ~secret:_sess_secret () (Conn.make (_req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = make ~secret:_app_secret () in
  Fennec_hunt_unit.check "POST header passes" (not (Conn.answered (csrf (mk ~headers:[ ("x-csrf-token", tok) ] ()))))

let%test_unit "POST with valid body token passes" =
  let g = _with_session () in
  let tok = token ~secret:_app_secret g in
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
    Session.make ~secret:_sess_secret () (Conn.make (_req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = make ~secret:_app_secret () in
  let body = "_csrf_token=" ^ tok in
  Fennec_hunt_unit.check "POST body passes"
    (not (Conn.answered (csrf (mk ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body ()))))

let%test_unit "POST with no token -> 403" =
  let g = _with_session () in
  let _tok = token ~secret:_app_secret g in
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
    Session.make ~secret:_sess_secret () (Conn.make (_req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = make ~secret:_app_secret () in
  Fennec_hunt_unit.check "no token -> 403"
    ((match Conn.resp (csrf (mk ())) with Some r -> r.H.status | None -> 0) = 403)

let%test_unit "POST with bad token -> 403" =
  let g = _with_session () in
  let _tok = token ~secret:_app_secret g in
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
    Session.make ~secret:_sess_secret () (Conn.make (_req ~meth ~headers:(("Cookie", cookie) :: headers) ~body "/"))
  in
  let csrf = make ~secret:_app_secret () in
  Fennec_hunt_unit.check "bad token -> 403"
    ((match Conn.resp (csrf (mk ~headers:[ ("x-csrf-token", "wrong") ] ())) with Some r -> r.H.status | None -> 0) = 403)
