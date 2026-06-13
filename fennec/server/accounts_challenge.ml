module Bson = Bson

type purpose =
  | Email_login
  | Email_verification
  | Password_reset
  | Passkey_registration
  | Passkey_assertion
  | OAuth_state
  | Oidc_state
  | Saml_request
  | Mfa_step_up
  | Recovery

let string_of_purpose = function
  | Email_login -> "email_login"
  | Email_verification -> "email_verification"
  | Password_reset -> "password_reset"
  | Passkey_registration -> "passkey_registration"
  | Passkey_assertion -> "passkey_assertion"
  | OAuth_state -> "oauth_state"
  | Oidc_state -> "oidc_state"
  | Saml_request -> "saml_request"
  | Mfa_step_up -> "mfa_step_up"
  | Recovery -> "recovery"

let purpose_of_string = function
  | "email_login" -> Some Email_login
  | "email_verification" -> Some Email_verification
  | "password_reset" -> Some Password_reset
  | "passkey_registration" -> Some Passkey_registration
  | "passkey_assertion" -> Some Passkey_assertion
  | "oauth_state" -> Some OAuth_state
  | "oidc_state" -> Some Oidc_state
  | "saml_request" -> Some Saml_request
  | "mfa_step_up" -> Some Mfa_step_up
  | "recovery" -> Some Recovery
  | _ -> None

type metadata = {
  user_id : string option;
  email : string option;
  org_id : string option;
  connection_id : string option;
  redirect : string option;
  data : (string * Bson.t) list;
}

let empty_metadata =
  { user_id = None; email = None; org_id = None; connection_id = None; redirect = None; data = [] }

type record = {
  id : string;
  purpose : purpose;
  metadata : metadata;
  created_at : float;
  expires_at : float;
  consumed_at : float option;
  revoked_at : float option;
  attempts : int;
  max_attempts : int option;
}

type token = string

let token_of_string s = s
let token_to_string t = t

type issued = { token : token; record : record }

type error =
  | Invalid_token
  | Wrong_purpose
  | Expired
  | Already_consumed
  | Revoked
  | Too_many_attempts
  | Duplicate_id of string
  | Invalid_request of string
  | Store_error of string

let string_of_error = function
  | Invalid_token -> "Invalid challenge token"
  | Wrong_purpose -> "Challenge token has the wrong purpose"
  | Expired -> "Challenge token expired"
  | Already_consumed -> "Challenge token was already consumed"
  | Revoked -> "Challenge token was revoked"
  | Too_many_attempts -> "Too many challenge attempts"
  | Duplicate_id id -> "Challenge id already exists: " ^ id
  | Invalid_request s -> s
  | Store_error s -> s

type store = {
  insert : record -> secret_hash:string -> (unit, error) result;
  find : string -> (record option, error) result;
  consume : string -> purpose -> secret_hash:string -> now:float -> (record, error) result;
  revoke : string -> now:float -> (bool, error) result;
  revoke_user : ?purpose:purpose -> string -> now:float -> (int, error) result;
  revoke_email : ?purpose:purpose -> string -> now:float -> (int, error) result;
  gc_expired : now:float -> (int, error) result;
}

type t = {
  secret : string;
  store : store;
  ttl : float;
  token_bytes : int;
  id_bytes : int;
  now : unit -> float;
}

let now () = Unix.gettimeofday ()

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg ->
    failwith ("Fennec.Accounts.Challenge: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64e s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))

let normalize_email s = String.lowercase_ascii (String.trim s)
let nonblank s = String.trim s <> ""
let option_exists f = function None -> false | Some x -> f x

let normalize_metadata m =
  { m with email = Option.map normalize_email m.email; data = List.filter (fun (k, _) -> nonblank k) m.data }

let validate_metadata m =
  let check_opt name = function
    | None -> Ok ()
    | Some s when nonblank s -> Ok ()
    | Some _ -> Error (Invalid_request (name ^ " cannot be blank"))
  in
  Result.bind (check_opt "user_id" m.user_id) (fun () ->
      Result.bind (check_opt "email" m.email) (fun () ->
          Result.bind (check_opt "org_id" m.org_id) (fun () ->
              Result.bind (check_opt "connection_id" m.connection_id) (fun () ->
                  check_opt "redirect" m.redirect))))

let make ~secret ~store ?(ttl = 600.) ?(token_bytes = 32) ?(id_bytes = 16) ?(now = now) () =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Accounts.Challenge.make: ~secret must be at least 16 bytes (got %d)"
         (String.length secret));
  if ttl <= 0. then invalid_arg "Fennec.Accounts.Challenge.make: ~ttl must be positive";
  if token_bytes < 16 then invalid_arg "Fennec.Accounts.Challenge.make: ~token_bytes must be at least 16";
  if id_bytes < 8 then invalid_arg "Fennec.Accounts.Challenge.make: ~id_bytes must be at least 8";
  { secret; store; ttl; token_bytes; id_bytes; now }

let hash_secret t ~purpose ~id secret =
  b64e (hmac_sha256 ~key:t.secret (String.concat "\000" [ string_of_purpose purpose; id; secret ]))

let parse_token token =
  match String.index_opt token '.' with
  | None -> Error Invalid_token
  | Some 0 -> Error Invalid_token
  | Some i when i = String.length token - 1 -> Error Invalid_token
  | Some i ->
    let id = String.sub token 0 i in
    let secret = String.sub token (i + 1) (String.length token - i - 1) in
    Ok (id, secret)

let create t ~purpose ?(metadata = empty_metadata) ?ttl ?max_attempts () =
  let metadata = normalize_metadata metadata in
  let ttl = match ttl with Some ttl -> ttl | None -> t.ttl in
  if ttl <= 0. then Error (Invalid_request "ttl must be positive")
  else if option_exists (fun n -> n <= 0) max_attempts then Error (Invalid_request "max_attempts must be positive")
  else
    Result.bind (validate_metadata metadata) (fun () ->
        let rec attempt remaining last_duplicate =
          if remaining = 0 then Error (Duplicate_id last_duplicate)
          else
            let id = b64e (secure_random t.id_bytes) in
            let secret = b64e (secure_random t.token_bytes) in
            let created_at = t.now () in
            let record =
              {
                id;
                purpose;
                metadata;
                created_at;
                expires_at = created_at +. ttl;
                consumed_at = None;
                revoked_at = None;
                attempts = 0;
                max_attempts;
              }
            in
            let secret_hash = hash_secret t ~purpose ~id secret in
            match t.store.insert record ~secret_hash with
            | Ok () -> Ok { token = id ^ "." ^ secret; record }
            | Error (Duplicate_id duplicate) -> attempt (remaining - 1) duplicate
            | Error _ as e -> e
        in
        attempt 8 "")

let consume t ~purpose token =
  Result.bind (parse_token token) (fun (id, secret) ->
      let secret_hash = hash_secret t ~purpose ~id secret in
      t.store.consume id purpose ~secret_hash ~now:(t.now ()))

let find t id = t.store.find id
let revoke t id = t.store.revoke id ~now:(t.now ())
let revoke_user t ?purpose user_id = t.store.revoke_user ?purpose user_id ~now:(t.now ())
let revoke_email t ?purpose email = t.store.revoke_email ?purpose (normalize_email email) ~now:(t.now ())
let gc_expired t = t.store.gc_expired ~now:(t.now ())

let memory_store () =
  let rows : (string, record * string) Hashtbl.t = Hashtbl.create 64 in
  let m = Mutex.create () in
  let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f in
  let active r = r.consumed_at = None && r.revoked_at = None in
  let purpose_matches wanted r = match wanted with None -> true | Some p -> r.purpose = p in
  let insert record ~secret_hash =
    locked (fun () ->
        if Hashtbl.mem rows record.id then Error (Duplicate_id record.id)
        else (
          Hashtbl.add rows record.id (record, secret_hash);
          Ok ()))
  in
  let find id = locked (fun () -> Ok (Option.map fst (Hashtbl.find_opt rows id))) in
  let replace r hash =
    Hashtbl.replace rows r.id (r, hash);
    r
  in
  let consume id purpose ~secret_hash ~now =
    locked (fun () ->
        match Hashtbl.find_opt rows id with
        | None -> Error Invalid_token
        | Some (r, _) when r.purpose <> purpose -> Error Wrong_purpose
        | Some (r, _) when r.revoked_at <> None -> Error Revoked
        | Some (r, _) when r.consumed_at <> None -> Error Already_consumed
        | Some (r, _) when now > r.expires_at -> Error Expired
        | Some (r, _) when option_exists (fun max -> r.attempts >= max) r.max_attempts -> Error Too_many_attempts
        | Some (r, hash) when not (constant_eq hash secret_hash) ->
          let attempts = r.attempts + 1 in
          let r = replace { r with attempts } hash in
          if option_exists (fun max -> attempts >= max) r.max_attempts then Error Too_many_attempts
          else Error Invalid_token
        | Some (r, hash) ->
          let r = replace { r with consumed_at = Some now } hash in
          Ok r)
  in
  let revoke id ~now =
    locked (fun () ->
        match Hashtbl.find_opt rows id with
        | None -> Ok false
        | Some (r, _) when not (active r) -> Ok false
        | Some (r, hash) ->
          let _ = replace { r with revoked_at = Some now } hash in
          Ok true)
  in
  let revoke_where ?purpose pred ~now =
    locked (fun () ->
        let count = ref 0 in
        Hashtbl.iter
          (fun _ (r, hash) ->
            if active r && purpose_matches purpose r && pred r then (
              incr count;
              ignore (replace { r with revoked_at = Some now } hash)))
          rows;
        Ok !count)
  in
  let revoke_user ?purpose user_id ~now =
    revoke_where ?purpose (fun r -> r.metadata.user_id = Some user_id) ~now
  in
  let revoke_email ?purpose email ~now =
    let email = normalize_email email in
    revoke_where ?purpose (fun r -> r.metadata.email = Some email) ~now
  in
  let gc_expired ~now =
    locked (fun () ->
        let expired = Hashtbl.to_seq rows |> Seq.filter (fun (_, (r, _)) -> now > r.expires_at) |> List.of_seq in
        List.iter (fun (id, _) -> Hashtbl.remove rows id) expired;
        Ok (List.length expired))
  in
  { insert; find; consume; revoke; revoke_user; revoke_email; gc_expired }

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) ?(store = memory_store ()) () =
  let now, set_now = test_clock () in
  (make ~secret:"challenge-test-secret" ~store ~ttl ~now (), set_now)

let raises_invalid_arg f = match f () with exception Invalid_argument _ -> true | _ -> false

let%test "purpose names round-trip" =
  List.for_all
    (fun p -> purpose_of_string (string_of_purpose p) = Some p)
    [ Email_login; Email_verification; Password_reset; Passkey_registration; Passkey_assertion; OAuth_state; Oidc_state; Saml_request; Mfa_step_up; Recovery ]

let%test "make rejects weak configuration" =
  let store = memory_store () in
  raises_invalid_arg (fun () -> make ~secret:"short" ~store ())
  && raises_invalid_arg (fun () -> make ~secret:"challenge-test-secret" ~store ~ttl:0. ())
  && raises_invalid_arg (fun () -> make ~secret:"challenge-test-secret" ~store ~token_bytes:15 ())
  && raises_invalid_arg (fun () -> make ~secret:"challenge-test-secret" ~store ~id_bytes:7 ())

let%test "create validates per-challenge options and metadata" =
  let t, _ = test_service () in
  let blank_user = { empty_metadata with user_id = Some "  " } in
  create t ~purpose:Email_login ~ttl:0. () = Error (Invalid_request "ttl must be positive")
  && create t ~purpose:Email_login ~max_attempts:0 () = Error (Invalid_request "max_attempts must be positive")
  && create t ~purpose:Email_login ~metadata:blank_user () = Error (Invalid_request "user_id cannot be blank")

let%test "create and consume returns bound metadata once" =
  let t, _ = test_service () in
  let metadata = { empty_metadata with user_id = Some "u1"; email = Some "ADA@example.com" } in
  match create t ~purpose:Email_login ~metadata () with
  | Error _ -> false
  | Ok issued -> (
    match consume t ~purpose:Email_login issued.token with
    | Ok r ->
      r.id = issued.record.id
      && r.consumed_at <> None
      && r.metadata.user_id = Some "u1"
      && r.metadata.email = Some "ada@example.com"
    | Error _ -> false)

let%test "consuming twice is rejected" =
  let t, _ = test_service () in
  match create t ~purpose:Email_login () with
  | Error _ -> false
  | Ok issued ->
    Result.is_ok (consume t ~purpose:Email_login issued.token)
    && consume t ~purpose:Email_login issued.token = Error Already_consumed

let%test "purpose mismatch is rejected" =
  let t, _ = test_service () in
  match create t ~purpose:Password_reset () with
  | Error _ -> false
  | Ok issued -> consume t ~purpose:Email_login issued.token = Error Wrong_purpose

let%test "malformed tokens are rejected before store lookup" =
  let t, _ = test_service () in
  List.for_all
    (fun raw -> consume t ~purpose:Email_login (token_of_string raw) = Error Invalid_token)
    [ ""; "missing-dot"; ".secret"; "id." ]

let%test "expired challenge is rejected" =
  let t, set_now = test_service ~ttl:10. () in
  match create t ~purpose:Email_login () with
  | Error _ -> false
  | Ok issued ->
    set_now 1_011.;
    consume t ~purpose:Email_login issued.token = Error Expired

let%test "revoked challenge is rejected" =
  let t, _ = test_service () in
  match create t ~purpose:Email_login () with
  | Error _ -> false
  | Ok issued ->
    revoke t issued.record.id = Ok true
    && consume t ~purpose:Email_login issued.token = Error Revoked

let%test "wrong secret increments attempts and enforces max_attempts" =
  let t, _ = test_service () in
  match create t ~purpose:Email_login ~max_attempts:2 () with
  | Error _ -> false
  | Ok issued ->
    let bad = token_of_string (issued.record.id ^ ".wrong") in
    consume t ~purpose:Email_login bad = Error Invalid_token
    && consume t ~purpose:Email_login bad = Error Too_many_attempts
    && consume t ~purpose:Email_login issued.token = Error Too_many_attempts

let%test "revoke_user only revokes matching active challenges" =
  let t, _ = test_service () in
  let m1 = { empty_metadata with user_id = Some "u1" } in
  let m2 = { empty_metadata with user_id = Some "u2" } in
  match (create t ~purpose:Email_login ~metadata:m1 (), create t ~purpose:Email_login ~metadata:m2 ()) with
  | Ok a, Ok b ->
    revoke_user t "u1" = Ok 1
    && consume t ~purpose:Email_login a.token = Error Revoked
    && Result.is_ok (consume t ~purpose:Email_login b.token)
  | _ -> false

let%test "revoke_email normalizes email and can filter by purpose" =
  let t, _ = test_service () in
  let metadata = { empty_metadata with email = Some "ADA@example.com" } in
  match
    ( create t ~purpose:Email_login ~metadata (),
      create t ~purpose:Password_reset ~metadata () )
  with
  | Ok login, Ok reset ->
    revoke_email t ~purpose:Email_login "ada@example.com" = Ok 1
    && consume t ~purpose:Email_login login.token = Error Revoked
    && Result.is_ok (consume t ~purpose:Password_reset reset.token)
  | _ -> false

let%test "gc_expired removes only expired records" =
  let t, set_now = test_service ~ttl:10. () in
  match create t ~purpose:Email_login () with
  | Error _ -> false
  | Ok expired ->
    set_now 1_005.;
    (match create t ~purpose:Email_login () with
    | Error _ -> false
    | Ok live ->
      set_now 1_011.;
      gc_expired t = Ok 1 && find t expired.record.id = Ok None && find t live.record.id = Ok (Some live.record))

let%test "store find never exposes raw token or secret hash" =
  let t, _ = test_service () in
  match create t ~purpose:Email_login () with
  | Error _ -> false
  | Ok issued -> (
    match find t issued.record.id with
    | Ok (Some r) -> r.id = issued.record.id && token_to_string issued.token <> r.id
    | _ -> false)

let%test "memory_store rejects duplicate ids" =
  let store = memory_store () in
  let record =
    {
      id = "duplicate";
      purpose = Email_login;
      metadata = empty_metadata;
      created_at = 1.;
      expires_at = 2.;
      consumed_at = None;
      revoked_at = None;
      attempts = 0;
      max_attempts = None;
    }
  in
  store.insert record ~secret_hash:"h1" = Ok ()
  && store.insert record ~secret_hash:"h2" = Error (Duplicate_id "duplicate")

let%test "create retries rare duplicate ids" =
  let base = memory_store () in
  let first = ref true in
  let store =
    {
      base with
      insert =
        (fun record ~secret_hash ->
          if !first then (
            first := false;
            Error (Duplicate_id record.id))
          else base.insert record ~secret_hash);
    }
  in
  let t, _ = test_service ~store () in
  match create t ~purpose:Email_login () with Ok issued -> issued.record.id <> "" | Error _ -> false
