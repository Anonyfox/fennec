module Challenge = Accounts_challenge
module Identity = Accounts_identity
module Bson = Bson

type address = string
type t = { secret : string; challenge : Challenge.t }

type error =
  | Invalid_email of string
  | Invalid_token
  | Email_mismatch
  | Otp_mismatch
  | Invalid_otp_code
  | Invalid_otp_config of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

let string_of_error = function
  | Invalid_email email -> "Invalid email address: " ^ email
  | Invalid_token -> "Invalid email challenge token"
  | Email_mismatch -> "Email challenge was issued for a different address"
  | Otp_mismatch -> "Incorrect email code"
  | Invalid_otp_code -> "Invalid email code"
  | Invalid_otp_config s -> s
  | Challenge_error e -> Challenge.string_of_error e
  | Identity_error e -> Identity.string_of_error e

let make ~secret ~challenge =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Accounts.Email.make: ~secret must be at least 16 bytes (got %d)"
         (String.length secret));
  { secret; challenge }

let contains pred s =
  let found = ref false in
  String.iter (fun c -> if pred c then found := true) s;
  !found

let normalize raw =
  let email = String.lowercase_ascii (String.trim raw) in
  let at_count = ref 0 in
  String.iter (fun c -> if c = '@' then incr at_count) email;
  match String.index_opt email '@' with
  | None -> Error (Invalid_email raw)
  | Some 0 -> Error (Invalid_email raw)
  | Some i when i = String.length email - 1 -> Error (Invalid_email raw)
  | Some i
    when !at_count <> 1
         || contains
              (function
                | ' ' | '\t' | '\n' | '\r' -> true
                | _ -> false)
              email
         || String.sub email (i + 1) (String.length email - i - 1) = "" ->
    Error (Invalid_email raw)
  | Some _ -> Ok email

let address_to_string a = a

let identity ~verified address =
  match Identity.email ~verified address with Ok key -> Ok key | Error e -> Error (Identity_error e)

type binding = {
  email : address;
  user_id : string option;
  org_id : string option;
  connection_id : string option;
  redirect : string option;
}

let binding ?user_id ?org_id ?connection_id ?redirect email =
  { email; user_id; org_id; connection_id; redirect }

let metadata ?(data = []) b : Challenge.metadata =
  {
    email = Some b.email;
    user_id = b.user_id;
    org_id = b.org_id;
    connection_id = b.connection_id;
    redirect = b.redirect;
    data;
  }

type issued = {
  token : Challenge.token;
  record : Challenge.record;
  binding : binding;
}

let issue t purpose ?ttl b =
  match Challenge.create t.challenge ~purpose ~metadata:(metadata b) ?ttl () with
  | Ok issued -> Ok { token = issued.token; record = issued.record; binding = b }
  | Error e -> Error (Challenge_error e)

let issue_verification t ?ttl b = issue t Challenge.Email_verification ?ttl b
let issue_login_link t ?ttl b = issue t Challenge.Email_login ?ttl b

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_token
  | Some 0 -> Error Invalid_token
  | Some i -> Ok (String.sub raw 0 i)

let precheck_email t ?expected token =
  match expected with
  | None -> Ok ()
  | Some expected -> (
    match token_id token with
    | Error _ as e -> e
    | Ok id -> (
      match Challenge.find t.challenge id with
      | Error e -> Error (Challenge_error e)
      | Ok None -> Error Invalid_token
      | Ok (Some record) when record.Challenge.metadata.email = Some expected -> Ok ()
      | Ok (Some _) -> Error Email_mismatch))

let consume t purpose ?expected token =
  match precheck_email t ?expected token with
  | Error _ as e -> e
  | Ok () -> (
    match Challenge.consume t.challenge ~purpose token with
    | Ok record -> Ok record
    | Error e -> Error (Challenge_error e))

let consume_verification t ?expected token = consume t Challenge.Email_verification ?expected token
let consume_login_link t ?expected token = consume t Challenge.Email_login ?expected token

type otp = {
  token : Challenge.token;
  code : string;
  record : Challenge.record;
  binding : binding;
}

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg ->
    failwith ("Fennec.Accounts.Email: secure randomness unavailable (/dev/urandom): " ^ msg)

let random_code ~digits =
  if digits < 4 || digits > 12 then Error (Invalid_otp_config "OTP digits must be between 4 and 12")
  else
    let alphabet = "0123456789" in
    let len = String.length alphabet in
    let limit = 256 - (256 mod len) in
    let out = Bytes.create digits in
    let rec fill i =
      if i = digits then Ok (Bytes.unsafe_to_string out)
      else
        let byte = Char.code (secure_random 1).[0] in
        if byte >= limit then fill i
        else (
          Bytes.set out i alphabet.[byte mod len];
          fill (i + 1))
    in
    fill 0

let b64e s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))
let otp_hash t code = b64e (hmac_sha256 ~key:t.secret ("email-otp\000" ^ code))

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

let otp_hash_of_record record =
  match List.assoc_opt "otp_hash" record.Challenge.metadata.data with
  | Some (Bson.String hash) -> Some hash
  | _ -> None

let issue_otp t ?ttl ?(digits = 6) b =
  match random_code ~digits with
  | Error _ as e -> e
  | Ok code -> (
    let data = [ ("otp_hash", Bson.String (otp_hash t code)) ] in
    match Challenge.create t.challenge ~purpose:Challenge.Email_login ~metadata:(metadata ~data b) ?ttl () with
    | Ok issued -> Ok { token = issued.token; code; record = issued.record; binding = b }
    | Error e -> Error (Challenge_error e))

let normalize_code code =
  let code = String.trim code in
  if code = "" || contains (function '0' .. '9' -> false | _ -> true) code then Error Invalid_otp_code
  else Ok code

let consume_otp t ~token ~code =
  match (token_id token, normalize_code code) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok id, Ok code -> (
    match Challenge.find t.challenge id with
    | Error e -> Error (Challenge_error e)
    | Ok None -> Error Invalid_token
    | Ok (Some record) -> (
      match otp_hash_of_record record with
      | Some hash when constant_eq hash (otp_hash t code) -> (
        match Challenge.consume t.challenge ~purpose:Challenge.Email_login token with
        | Ok record -> Ok record
        | Error e -> Error (Challenge_error e))
      | _ -> Error Otp_mismatch))

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"email-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~secret:"email-helper-secret" ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let%test "normalize lowercases and trims addresses" =
  normalize " ADA@Example.COM " = Ok "ada@example.com"

let%test "normalize rejects malformed addresses" =
  List.for_all
    (fun email -> Result.is_error (normalize email))
    [ ""; "ada"; "@example.com"; "ada@"; "ada@@example.com"; "ada @example.com" ]

let%test "identity delegates to verified email identity" =
  let address = ok (normalize "ADA@example.com") in
  match identity ~verified:true address with
  | Ok key -> Identity.is_verified_email key && Identity.subject key = "ada@example.com"
  | Error _ -> false

let%test "verification challenge is purpose-bound" =
  let t, _ = test_service () in
  let b = binding (ok (normalize "ada@example.com")) in
  match issue_verification t b with
  | Error _ -> false
  | Ok issued ->
    Result.is_ok (consume_verification t issued.token)
    && consume_verification t issued.token = Error (Challenge_error Challenge.Already_consumed)

let%test "login link cannot verify email" =
  let t, _ = test_service () in
  let b = binding (ok (normalize "ada@example.com")) in
  match issue_login_link t b with
  | Error _ -> false
  | Ok issued -> consume_verification t issued.token = Error (Challenge_error Challenge.Wrong_purpose)

let%test "expected email mismatch does not consume challenge" =
  let t, _ = test_service () in
  let ada = ok (normalize "ada@example.com") in
  let grace = ok (normalize "grace@example.com") in
  match issue_login_link t (binding ada) with
  | Error _ -> false
  | Ok issued ->
    consume_login_link t ~expected:grace issued.token = Error Email_mismatch
    && Result.is_ok (consume_login_link t ~expected:ada issued.token)

let%test "expired email challenge is rejected" =
  let t, set_now = test_service ~ttl:10. () in
  let b = binding (ok (normalize "ada@example.com")) in
  match issue_login_link t b with
  | Error _ -> false
  | Ok issued ->
    set_now 1_011.;
    consume_login_link t issued.token = Error (Challenge_error Challenge.Expired)

let%test "otp issue returns numeric code and consumes with token plus code" =
  let t, _ = test_service () in
  let b = binding (ok (normalize "ada@example.com")) in
  match issue_otp t ~digits:6 b with
  | Error _ -> false
  | Ok otp ->
    String.length otp.code = 6
    && String.for_all (function '0' .. '9' -> true | _ -> false) otp.code
    && Result.is_ok (consume_otp t ~token:otp.token ~code:otp.code)

let%test "otp wrong code does not consume challenge" =
  let t, _ = test_service () in
  let b = binding (ok (normalize "ada@example.com")) in
  match issue_otp t ~digits:6 b with
  | Error _ -> false
  | Ok otp ->
    consume_otp t ~token:otp.token ~code:"000000" = Error Otp_mismatch
    && Result.is_ok (consume_otp t ~token:otp.token ~code:otp.code)

let%test "otp rejects invalid digit configuration and code shape" =
  let t, _ = test_service () in
  let b = binding (ok (normalize "ada@example.com")) in
  Result.is_error (issue_otp t ~digits:3 b)
  &&
  match issue_otp t b with
  | Error _ -> false
  | Ok otp -> consume_otp t ~token:otp.token ~code:"12 34" = Error Invalid_otp_code
