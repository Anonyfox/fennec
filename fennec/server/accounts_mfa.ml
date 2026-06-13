module Challenge = Accounts_challenge
module Bson = Bson

type t = { secret : string; challenge : Challenge.t }

type error =
  | Invalid_config of string
  | Invalid_code
  | Code_mismatch
  | Replay
  | Insufficient_assurance
  | Stale_assurance
  | Invalid_state
  | Challenge_error of Challenge.error

let string_of_error = function
  | Invalid_config s -> "Invalid MFA config: " ^ s
  | Invalid_code -> "Invalid MFA code"
  | Code_mismatch -> "Incorrect MFA code"
  | Replay -> "MFA code was already used"
  | Insufficient_assurance -> "Insufficient authentication assurance"
  | Stale_assurance -> "Authentication assurance is too old"
  | Invalid_state -> "Invalid MFA state"
  | Challenge_error e -> Challenge.string_of_error e

let make ~secret ~challenge =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Accounts.Mfa.make: ~secret must be at least 16 bytes (got %d)"
         (String.length secret));
  { secret; challenge }

type factor =
  | Password
  | Email
  | OAuth
  | Oidc
  | Saml
  | Passkey
  | Totp
  | Backup_code
  | Recovery_code

type level =
  | Anonymous
  | Single_factor
  | Phishing_resistant_single_factor
  | Multi_factor
  | Phishing_resistant_multi_factor

let factor_rank = function
  | Passkey -> 2
  | Password | Email | OAuth | Oidc | Saml | Totp | Backup_code | Recovery_code -> 1

let phishing_resistant = function Passkey -> true | _ -> false

let distinct_factors factors =
  List.fold_left (fun acc f -> if List.mem f acc then acc else f :: acc) [] factors |> List.rev

let level_of_factors factors =
  let factors = distinct_factors factors in
  let count = List.length (List.filter (fun f -> factor_rank f > 0) factors) in
  let has_pr = List.exists phishing_resistant factors in
  match (count, has_pr) with
  | 0, _ -> Anonymous
  | 1, true -> Phishing_resistant_single_factor
  | 1, false -> Single_factor
  | _, true -> Phishing_resistant_multi_factor
  | _ -> Multi_factor

let rank = function
  | Anonymous -> 0
  | Single_factor -> 1
  | Phishing_resistant_single_factor -> 2
  | Multi_factor -> 3
  | Phishing_resistant_multi_factor -> 4

let satisfies ~required actual = rank actual >= rank required

type assurance = {
  level : level;
  factors : factor list;
  authenticated_at : float;
}

type enrollment_status =
  | Pending
  | Active
  | Disabled

type enrollment = {
  id : string;
  user_id : string;
  factor : factor;
  label : string option;
  status : enrollment_status;
  secret : string option;
  backup_hashes : string list;
  last_step : int64 option;
  created_at : float;
  confirmed_at : float option;
  disabled_at : float option;
}

let assurance ?(now = Unix.gettimeofday) factors =
  let factors = distinct_factors factors in
  { level = level_of_factors factors; factors; authenticated_at = now () }

type requirement = {
  level : level;
  max_age : float option;
}

let requirement ?max_age level =
  match max_age with
  | Some age when age < 0. || classify_float age = FP_nan -> Error (Invalid_config "max_age must be non-negative")
  | _ -> Ok { level; max_age }

let require ?(now = Unix.gettimeofday) (requirement : requirement) (assurance : assurance) =
  if not (satisfies ~required:requirement.level assurance.level) then Error Insufficient_assurance
  else
    match requirement.max_age with
    | Some max_age when now () -. assurance.authenticated_at > max_age -> Error Stale_assurance
    | _ -> Ok ()

let level_name = function
  | Anonymous -> "anonymous"
  | Single_factor -> "single_factor"
  | Phishing_resistant_single_factor -> "phishing_resistant_single_factor"
  | Multi_factor -> "multi_factor"
  | Phishing_resistant_multi_factor -> "phishing_resistant_multi_factor"

let level_of_name = function
  | "anonymous" -> Some Anonymous
  | "single_factor" -> Some Single_factor
  | "phishing_resistant_single_factor" -> Some Phishing_resistant_single_factor
  | "multi_factor" -> Some Multi_factor
  | "phishing_resistant_multi_factor" -> Some Phishing_resistant_multi_factor
  | _ -> None

let bson_string key value = (key, Bson.String value)

let metadata ~user_id ?redirect ?(data = []) requirement : Challenge.metadata =
  let data = List.filter (fun (key, _) -> key <> "level" && key <> "max_age") data in
  let data =
    [ bson_string "level" (level_name requirement.level) ]
    @ data
    @
    match requirement.max_age with
    | None -> []
    | Some max_age -> [ ("max_age", Bson.Float max_age) ]
  in
  { user_id = Some user_id; email = None; org_id = None; connection_id = None; redirect; data }

type step_up = {
  token : Challenge.token;
  record : Challenge.record;
  user_id : string;
  requirement : requirement;
}

type step_up_state = {
  user_id : string;
  requirement : requirement;
  data : (string * Bson.t) list;
  record : Challenge.record;
}

let issue_step_up t ?ttl ?redirect ?data ~user_id requirement =
  let user_id = String.trim user_id in
  if user_id = "" then Error (Invalid_config "user_id cannot be blank")
  else
    match
      Challenge.create t.challenge ~purpose:Challenge.Mfa_step_up
        ~metadata:(metadata ~user_id ?redirect ?data requirement)
        ?ttl ()
    with
    | Error e -> Error (Challenge_error e)
    | Ok issued -> Ok { token = issued.token; record = issued.record; user_id; requirement }

let data_string key data = match List.assoc_opt key data with Some (Bson.String v) -> Some v | _ -> None
let data_float key data = match List.assoc_opt key data with Some (Bson.Float v) -> Some v | Some (Bson.Int v) -> Some (float_of_int v) | _ -> None

let state_of_record ?expected_user record =
  let user_ok =
    match (expected_user, record.Challenge.metadata.user_id) with
    | None, Some _ -> true
    | Some expected, Some actual -> String.trim expected = actual
    | _ -> false
  in
  if not user_ok then Error Invalid_state
  else
    let data = record.metadata.data in
    match (record.metadata.user_id, Option.bind (data_string "level" data) level_of_name) with
    | Some user_id, Some level ->
      Ok
        {
          user_id;
          requirement = { level; max_age = data_float "max_age" data };
          data;
          record;
        }
    | _ -> Error Invalid_state

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_state
  | Some 0 -> Error Invalid_state
  | Some i -> Ok (String.sub raw 0 i)

let precheck_user t ?expected_user token =
  match expected_user with
  | None -> Ok ()
  | Some _ -> (
    match token_id token with
    | Error _ as e -> e
    | Ok id -> (
      match Challenge.find t.challenge id with
      | Error e -> Error (Challenge_error e)
      | Ok None -> Error Invalid_state
      | Ok (Some record) -> state_of_record ?expected_user record |> Result.map (fun _ -> ())))

let consume_step_up t ?expected_user token =
  match precheck_user t ?expected_user token with
  | Error _ as e -> e
  | Ok () -> (
    match Challenge.consume t.challenge ~purpose:Challenge.Mfa_step_up token with
    | Error e -> Error (Challenge_error e)
    | Ok record -> state_of_record ?expected_user record)

type totp = {
  secret : string;
  issuer : string option;
  account : string option;
  digits : int;
  period : int;
}

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts.Mfa: secure randomness unavailable (/dev/urandom): " ^ msg)

let base32_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

let base32_encode raw =
  let b = Buffer.create ((String.length raw * 8 + 4) / 5) in
  let buffer = ref 0 in
  let bits = ref 0 in
  String.iter
    (fun c ->
      buffer := (!buffer lsl 8) lor Char.code c;
      bits := !bits + 8;
      while !bits >= 5 do
        bits := !bits - 5;
        Buffer.add_char b base32_alphabet.[(!buffer lsr !bits) land 31]
      done)
    raw;
  if !bits > 0 then Buffer.add_char b base32_alphabet.[(!buffer lsl (5 - !bits)) land 31];
  Buffer.contents b

let base32_value = function
  | 'A' .. 'Z' as c -> Some (Char.code c - Char.code 'A')
  | 'a' .. 'z' as c -> Some (Char.code c - Char.code 'a')
  | '2' .. '7' as c -> Some (26 + Char.code c - Char.code '2')
  | _ -> None

let base32_decode encoded =
  let b = Buffer.create (String.length encoded * 5 / 8) in
  let buffer = ref 0 in
  let bits = ref 0 in
  let ok = ref true in
  String.iter
    (function
      | '=' | ' ' | '\t' | '\n' | '\r' -> ()
      | c -> (
        match base32_value c with
        | None -> ok := false
        | Some v ->
          buffer := (!buffer lsl 5) lor v;
          bits := !bits + 5;
          if !bits >= 8 then (
            bits := !bits - 8;
            Buffer.add_char b (Char.chr ((!buffer lsr !bits) land 0xff)))))
    encoded;
  if !ok then Some (Buffer.contents b) else None

let generate_totp_secret ?(bytes = 20) () =
  if bytes <= 0 then invalid_arg "Fennec.Accounts.Mfa.generate_totp_secret: bytes must be positive";
  base32_encode (secure_random bytes)

let nonblank_opt s =
  let s = String.trim s in
  if s = "" then None else Some s

let totp ?issuer ?account ?(digits = 6) ?(period = 30) ~secret () =
  let secret = String.trim secret in
  if digits < 6 || digits > 8 then Error (Invalid_config "TOTP digits must be between 6 and 8")
  else if period <= 0 then Error (Invalid_config "TOTP period must be positive")
  else
    match base32_decode secret with
    | Some raw when String.length raw >= 10 ->
      Ok
        {
          secret = String.uppercase_ascii secret;
          issuer = Option.bind issuer nonblank_opt;
          account = Option.bind account nonblank_opt;
          digits;
          period;
        }
    | _ -> Error (Invalid_config "TOTP secret must be valid base32 with at least 80 bits")

let percent_encode = Fennec_core.Http.percent_encode

let provisioning_uri cfg =
  let label =
    match (cfg.issuer, cfg.account) with
    | Some issuer, Some account -> percent_encode (issuer ^ ":" ^ account)
    | None, Some account -> percent_encode account
    | Some issuer, None -> percent_encode issuer
    | None, None -> "fennec"
  in
  let params =
    [ ("secret", cfg.secret); ("digits", string_of_int cfg.digits); ("period", string_of_int cfg.period); ("algorithm", "SHA1") ]
    @ (match cfg.issuer with None -> [] | Some issuer -> [ ("issuer", issuer) ])
  in
  "otpauth://totp/" ^ label ^ "?"
  ^ String.concat "&" (List.map (fun (k, v) -> percent_encode k ^ "=" ^ percent_encode v) params)

let hotp ~secret ~counter ~digits =
  let msg =
    String.init 8 (fun i ->
        let shift = (7 - i) * 8 in
        Char.chr (Int64.(to_int (logand (shift_right_logical counter shift) 0xffL))))
  in
  let digest = Digestif.SHA1.(to_raw_string (hmac_string ~key:secret msg)) in
  let offset = Char.code digest.[String.length digest - 1] land 0x0f in
  let bin =
    ((Char.code digest.[offset] land 0x7f) lsl 24)
    lor (Char.code digest.[offset + 1] lsl 16)
    lor (Char.code digest.[offset + 2] lsl 8)
    lor Char.code digest.[offset + 3]
  in
  let modulo = int_of_float (10. ** float_of_int digits) in
  let raw = string_of_int (bin mod modulo) in
  if String.length raw >= digits then raw
  else String.make (digits - String.length raw) '0' ^ raw

let step_for_time cfg time = Int64.of_float (floor (time /. float_of_int cfg.period))

let totp_code ?(time = Unix.gettimeofday ()) cfg =
  match base32_decode cfg.secret with
  | None -> invalid_arg "Fennec.Accounts.Mfa.totp_code: invalid secret"
  | Some secret -> hotp ~secret ~counter:(step_for_time cfg time) ~digits:cfg.digits

let normalize_code ~digits code =
  let code = String.trim code in
  if String.length code <> digits then Error Invalid_code
  else
    let ok =
      String.for_all
        (function
          | '0' .. '9' -> true
          | _ -> false)
        code
    in
    if ok then Ok code else Error Invalid_code

let constant_eq (a : string) (b : string) : bool =
  String.length a = String.length b
  &&
  let acc = ref 0 in
  String.iteri (fun i c -> acc := !acc lor (Char.code c lxor Char.code b.[i])) a;
  !acc = 0

let verify_totp ?(time = Unix.gettimeofday ()) ?(window = 1) ?last_step cfg ~code =
  if window < 0 then Error (Invalid_config "TOTP window must be non-negative")
  else
    match (base32_decode cfg.secret, normalize_code ~digits:cfg.digits code) with
    | None, _ -> Error (Invalid_config "TOTP secret is invalid")
    | _, Error e -> Error e
    | Some secret, Ok code ->
      let current = step_for_time cfg time in
      let rec loop delta =
        if delta > window then Error Code_mismatch
        else
          let step = Int64.add current (Int64.of_int delta) in
          if step >= 0L && constant_eq code (hotp ~secret ~counter:step ~digits:cfg.digits) then
            match last_step with
            | Some last when Int64.compare step last <= 0 -> Error Replay
            | _ -> Ok step
          else loop (delta + 1)
      in
      let rec scan d =
        if d > window then Error Code_mismatch
        else
          match loop (-d) with
          | Ok _ as ok -> ok
          | Error Replay as e -> e
          | Error _ -> scan (d + 1)
      in
      scan 0

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))

let normalize_backup_code code =
  let b = Buffer.create (String.length code) in
  String.iter
    (function
      | ' ' | '\t' | '\n' | '\r' | '-' -> ()
      | c -> Buffer.add_char b (Char.uppercase_ascii c))
    code;
  let code = Buffer.contents b in
  if code = "" then Error Invalid_code else Ok code

let hash_code (t : t) code =
  let normalized = match normalize_backup_code code with Ok c -> c | Error _ -> "" in
  b64url (hmac_sha256 ~key:t.secret ("mfa-backup\000" ^ normalized))

type backup_codes = {
  codes : string list;
  hashes : string list;
}

let random_backup_code bytes =
  let code = base32_encode (secure_random bytes) in
  if String.length code <= 4 then code else String.sub code 0 4 ^ "-" ^ String.sub code 4 (String.length code - 4)

let generate_backup_codes (t : t) ?(count = 10) ?(bytes = 5) () =
  if count <= 0 || count > 100 then Error (Invalid_config "backup code count must be between 1 and 100")
  else if bytes < 4 || bytes > 32 then Error (Invalid_config "backup code bytes must be between 4 and 32")
  else
    let rec loop n acc =
      if n = 0 then List.rev acc
      else
        let code = random_backup_code bytes in
        if List.mem code acc then loop n acc else loop (n - 1) (code :: acc)
    in
    let codes = loop count [] in
    Ok { codes; hashes = List.map (hash_code t) codes }

let consume_backup_code (t : t) ~hashes ~code =
  match normalize_backup_code code with
  | Error e -> Error e
  | Ok _ ->
    let hash = hash_code t code in
    if not (List.exists (constant_eq hash) hashes) then Error Code_mismatch
    else Ok (hash, List.filter (fun h -> not (constant_eq h hash)) hashes)

let enrollment ?(now = Unix.gettimeofday) ?label ?(status = Pending) ?secret ?(backup_hashes = []) ?last_step
    ?confirmed_at ?disabled_at ~id ~user_id ~factor () =
  let id = String.trim id in
  let user_id = String.trim user_id in
  let label =
    Option.bind label (fun s ->
        let s = String.trim s in
        if s = "" then None else Some s)
  in
  let secret =
    Option.bind secret (fun s ->
        let s = String.trim s in
        if s = "" then None else Some s)
  in
  let backup_hashes =
    List.filter_map
      (fun s ->
        let s = String.trim s in
        if s = "" then None else Some s)
      backup_hashes
  in
  if id = "" then Error (Invalid_config "enrollment id cannot be blank")
  else if user_id = "" then Error (Invalid_config "user_id cannot be blank")
  else
    Ok
      {
        id;
        user_id;
        factor;
        label;
        status;
        secret;
        backup_hashes;
        last_step;
        created_at = now ();
        confirmed_at;
        disabled_at;
      }

type store = {
  find : string -> enrollment option;
  list : ?user_id:string -> ?factor:factor -> unit -> enrollment list;
  upsert : enrollment -> (unit, string) result;
  replace_if_current : current:enrollment -> enrollment -> (bool, string) result;
  delete : string -> (bool, string) result;
}

let memory_store () =
  let enrollments : (string, enrollment) Hashtbl.t = Hashtbl.create 64 in
  let mutex = Mutex.create () in
  let locked f = Mutex.lock mutex; Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f in
  let find id = locked (fun () -> Hashtbl.find_opt enrollments id) in
  let list ?user_id ?factor () =
    locked (fun () ->
        Hashtbl.to_seq_values enrollments
        |> List.of_seq
        |> List.filter (fun (e : enrollment) ->
               Option.fold ~none:true ~some:(String.equal e.user_id) user_id
               && Option.fold ~none:true ~some:(fun factor -> e.factor = factor) factor)
        |> List.sort (fun a b -> String.compare a.id b.id))
  in
  let upsert enrollment =
    locked (fun () ->
        Hashtbl.replace enrollments enrollment.id enrollment;
        Ok ())
  in
  let replace_if_current ~current enrollment =
    locked (fun () ->
        match Hashtbl.find_opt enrollments current.id with
        | Some stored when stored = current ->
          Hashtbl.replace enrollments enrollment.id enrollment;
          Ok true
        | _ -> Ok false)
  in
  let delete id =
    locked (fun () ->
        let existed = Hashtbl.mem enrollments id in
        Hashtbl.remove enrollments id;
        Ok existed)
  in
  { find; list; upsert; replace_if_current; delete }

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"mfa-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~secret:"mfa-helper-secret" ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let%test "level_of_factors derives assurance levels" =
  level_of_factors [] = Anonymous
  && level_of_factors [ Password ] = Single_factor
  && level_of_factors [ Passkey ] = Phishing_resistant_single_factor
  && level_of_factors [ Password; Totp ] = Multi_factor
  && level_of_factors [ Password; Passkey ] = Phishing_resistant_multi_factor

let%test "require enforces level and freshness" =
  let a = { level = Multi_factor; factors = [ Password; Totp ]; authenticated_at = 1_000. } in
  require ~now:(fun () -> 1_010.) (ok (requirement ~max_age:20. Multi_factor)) a = Ok ()
  && require ~now:(fun () -> 1_030.) (ok (requirement ~max_age:20. Multi_factor)) a = Error Stale_assurance
  && require (ok (requirement Phishing_resistant_multi_factor)) a = Error Insufficient_assurance

let%test "step_up is purpose-bound and single-use" =
  let t, _ = test_service () in
  let req = ok (requirement ~max_age:60. Multi_factor) in
  match issue_step_up t ~user_id:"user_1" req with
  | Error _ -> false
  | Ok issued -> (
    match consume_step_up t ~expected_user:"user_1" issued.token with
    | Error _ -> false
    | Ok state ->
      state.user_id = "user_1"
      && state.requirement = req
      && consume_step_up t ~expected_user:"user_1" issued.token = Error (Challenge_error Challenge.Already_consumed))

let%test "wrong step_up user does not consume state" =
  let t, _ = test_service () in
  let req = ok (requirement Multi_factor) in
  match issue_step_up t ~user_id:"user_1" req with
  | Error _ -> false
  | Ok issued ->
    consume_step_up t ~expected_user:"user_2" issued.token = Error Invalid_state
    && Result.is_ok (consume_step_up t ~expected_user:"user_1" issued.token)

let%test "totp follows RFC 6238 SHA1 vector" =
  let cfg = ok (totp ~digits:8 ~period:30 ~secret:(base32_encode "12345678901234567890") ()) in
  totp_code ~time:59. cfg = "94287082"
  && totp_code ~time:1_111_111_109. cfg = "07081804"
  && totp_code ~time:2_000_000_000. cfg = "69279037"

let%test "verify_totp accepts window and rejects replay" =
  let cfg = ok (totp ~digits:8 ~period:30 ~secret:(base32_encode "12345678901234567890") ()) in
  match verify_totp ~time:59. ~window:0 cfg ~code:"94287082" with
  | Error _ -> false
  | Ok step ->
    step = 1L
    && verify_totp ~time:59. ~window:0 ~last_step:step cfg ~code:"94287082" = Error Replay
    && verify_totp ~time:59. ~window:0 cfg ~code:"00000000" = Error Code_mismatch

let%test "backup codes hash and consume once" =
  let t, _ = test_service () in
  match generate_backup_codes t ~count:3 ~bytes:5 () with
  | Error _ -> false
  | Ok generated -> (
    match generated.codes with
    | code :: _ -> (
      match consume_backup_code t ~hashes:generated.hashes ~code with
      | Error _ -> false
      | Ok (hash, remaining) ->
        List.length remaining = 2
        && List.exists (( = ) hash) generated.hashes
        && consume_backup_code t ~hashes:remaining ~code = Error Code_mismatch)
    | [] -> false)

let%test "backup code normalization ignores separators and case" =
  let t, _ = test_service () in
  let hash = hash_code t "ABCD-EFGH" in
  Result.is_ok (consume_backup_code t ~hashes:[ hash ] ~code:"abcd efgh")

let%test "memory_store persists MFA enrollments by user and factor" =
  let store = memory_store () in
  let enrollment = ok (enrollment ~now:(fun () -> 10.) ~id:"mfa1" ~user_id:"u1" ~factor:Totp ~secret:"SECRET" ()) in
  store.upsert enrollment = Ok ()
  && store.find "mfa1" = Some enrollment
  && store.list ~user_id:"u1" ~factor:Totp () = [ enrollment ]
  && store.list ~user_id:"u2" () = []

let%test "memory_store compare-and-swap rejects stale MFA state" =
  let store = memory_store () in
  let before =
    ok
      (enrollment ~now:(fun () -> 10.) ~status:Active ~id:"mfa1" ~user_id:"u1" ~factor:Backup_code
         ~backup_hashes:[ "h1"; "h2" ] ())
  in
  let after = { before with backup_hashes = [ "h2" ] } in
  let stale = { before with backup_hashes = [ "h1"; "h2"; "h3" ] } in
  store.upsert before = Ok ()
  && store.replace_if_current ~current:before after = Ok true
  && store.replace_if_current ~current:before stale = Ok false
  && store.find "mfa1" = Some after
