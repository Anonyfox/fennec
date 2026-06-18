(* accounts_secrets.ml — the security-critical core primitives, carved VERBATIM from
   accounts_base.ml (git diff -w on this file is a pure relocation):
     - CSPRNG + hashing + constant-time compare: secure_random / random_id / sha256_hex /
       hmac_sha256 / constant_eq, plus b64 and the email/username normalizers and [now];
     - the signed-session codec: encode/decode_session, the factor-list (de)serializer, [sign],
       [verify_session], and the server-side revocation gate [token_live];
     - the MFA-secret seal (authenticated HMAC-CTR): xor_with_stream / seal_mfa_secret /
       unseal_mfa_secret (fail-closed on any non-v1 value);
     - the pure factor-set + lookup helpers shared by the request and login layers.
   Touch with extreme care: the enumeration/replay/seal guarantees live here. *)

open Accounts_types

(* ---- clock, CSPRNG, hashing, constant-time compare, normalizers ---- *)
let now () = Unix.gettimeofday ()

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64e s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d s = match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

let random_id ?(bytes = 18) () = b64e (secure_random bytes)
let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))
let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))
let constant_eq a b =
  let la = String.length a and lb = String.length b in
  let diff = ref (la lxor lb) in
  let max_len = max la lb in
  for i = 0 to max_len - 1 do
    let ca = if i < la then Char.code a.[i] else 0 in
    let cb = if i < lb then Char.code b.[i] else 0 in
    diff := !diff lor (ca lxor cb)
  done;
  !diff = 0

let normalize_email s = String.lowercase_ascii (String.trim s)
let normalize_username s = String.lowercase_ascii (String.trim s)

(* ---- signed-session codec + factor-list (de)serialization + revocation gate ---- *)
let mfa_factor_name = Mfa.factor_to_name
let mfa_factor_of_name = Mfa.factor_of_name

let encode_mfa_factors factors = String.concat "," (List.map mfa_factor_name factors)
let decode_mfa_factors s =
  if String.trim s = "" then []
  else String.split_on_char ',' s |> List.filter_map mfa_factor_of_name

let encode_session s =
  encode_pairs
    [
      ("uid", s.uid);
      ("sid", s.sid);
      ("iat", Printf.sprintf "%.0f" s.iat);
      ("exp", Printf.sprintf "%.0f" s.exp);
      ("epoch", string_of_int s.auth_epoch);
      ("strategy", s.strategy);
      ("factors", encode_mfa_factors s.factors);
    ]

let decode_session payload =
  let pairs = H.parse_query payload in
  let get k = List.assoc_opt k pairs in
  match (get "uid", get "sid", get "iat", get "exp", get "epoch", get "strategy") with
  | Some uid, Some sid, Some iat, Some exp, Some epoch, Some strategy ->
    Some
      {
        uid;
        sid;
        iat = float_of_string_default 0. iat;
        exp = float_of_string_default 0. exp;
        auth_epoch = int_of_string_default 0 epoch;
        strategy;
        factors = Option.value ~default:[] (Option.map decode_mfa_factors (get "factors"));
      }
  | _ -> None

let sign t s = Session.sign ~secret:t.secret (encode_session s)

let verify_session t token =
  match Option.bind (Session.verify ~secret:t.secret token) decode_session with
  | None -> Error Invalid_token
  | Some s when now () > s.exp -> Error Invalid_token
  | Some s -> Ok s

(* The server-side revocation gate: a verified session is only honored if its [sid] still has a live,
   non-expired row whose stored hash matches the presented token. This is what makes per-session
   revocation and early server-side expiry possible (consequences a/c). Consulted on the token-
   presentation paths ([verify_token], resume) always, and in [paw] only when
   [validate_every_request] — the stateless cookie fast path stays zero-read by default. *)
let token_live t (s : session) token =
  match t.store.tokens.find_live ~sid:s.sid ~hashed:(sha256_hex token) ~now:(now ()) with
  | Error _ as e -> e
  | Ok (Some _) -> Ok ()
  | Ok None -> Error Invalid_token

(* ---- MFA-secret seal: authenticated HMAC keystream, fail-closed ---- *)
let seal_key t = hmac_sha256 ~key:t.secret "fennec.accounts.mfa.seal.v1"
let seal_prefix = "v1"

let xor_with_stream ~key ~nonce plaintext =
  let n = String.length plaintext in
  let out = Bytes.create n in
  let rec block counter offset =
    if offset < n then (
      let stream = hmac_sha256 ~key (nonce ^ "\000" ^ string_of_int counter) in
      let m = min (String.length stream) (n - offset) in
      for i = 0 to m - 1 do
        Bytes.set out (offset + i) (Char.chr (Char.code plaintext.[offset + i] lxor Char.code stream.[i]))
      done;
      block (counter + 1) (offset + m))
  in
  block 0 0;
  Bytes.unsafe_to_string out

let seal_mfa_secret t secret =
  let key = seal_key t in
  let nonce = secure_random 16 in
  let cipher = xor_with_stream ~key ~nonce secret in
  let payload = seal_prefix ^ "." ^ b64e nonce ^ "." ^ b64e cipher in
  payload ^ "." ^ b64e (hmac_sha256 ~key payload)

let unseal_mfa_secret t sealed =
  match String.split_on_char '.' sealed with
  | [ version; nonce64; cipher64; mac64 ] when version = seal_prefix -> (
    match (b64d nonce64, b64d cipher64, b64d mac64) with
    | Some nonce, Some cipher, Some mac ->
      let payload = version ^ "." ^ nonce64 ^ "." ^ cipher64 in
      if constant_eq mac (hmac_sha256 ~key:(seal_key t) payload) then
        Some (xor_with_stream ~key:(seal_key t) ~nonce cipher)
      else None
    | _ -> None)
  (* FAIL CLOSED on any non-sealed value. A TOTP secret is ALWAYS written through [seal_mfa_secret]
     (keyed by [t.secret]), so a stored value that is not a valid v1 seal is either tampered or
     foreign — trusting it as plaintext would let mere store-write access plant a known TOTP secret,
     defeating the seal's whole purpose (forging a seal requires [t.secret]). *)
  | _ -> None

(* ---- shared factor-set helpers (strategy->first-factor, dedup/merge) ---- *)
let factor_of_strategy strategy =
  match String.lowercase_ascii (String.trim strategy) with
  | "password" | "createuser" | "resetpassword" -> Some Mfa.Password
  | "verifyemail" | "email" | "email_otp" -> Some Mfa.Email
  | "oauth" -> Some Mfa.OAuth
  | "oidc" -> Some Mfa.Oidc
  | "saml" -> Some Mfa.Saml
  | "passkey" -> Some Mfa.Passkey
  | _ -> None

let same_mfa_factor a b = String.equal (mfa_factor_name a) (mfa_factor_name b)
let add_mfa_factor factors factor = if List.exists (same_mfa_factor factor) factors then factors else factors @ [ factor ]
let merge_mfa_factors a b = List.fold_left add_mfa_factor a b

(* ---- core store lookups shared by request/login/lifecycle ---- *)
let find_required_user t uid =
  match t.store.users.find_user_by_id uid with
  | Error _ as e -> e
  | Ok None -> Error User_not_found
  | Ok (Some u) -> Ok u
let find_by_selector t = function
  | By_id id -> t.store.users.find_user_by_id id
  | By_email e -> t.store.users.find_user_by_email (normalize_email e)
  | By_username u -> t.store.users.find_user_by_username (normalize_username u)
