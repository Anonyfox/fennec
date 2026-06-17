module Challenge = Accounts_challenge
module Identity = Accounts_identity
module Bson = Bson
module Json = Fennec_mongo_json.Json

type relying_party = {
  id : string;
  name : string;
  origins : string list;
  user_verification : bool;
}

type t = { challenge : Challenge.t }

type error =
  | Invalid_relying_party of string
  | Invalid_user of string
  | Invalid_state
  | Invalid_client_data of string
  | Invalid_attestation of string
  | Invalid_assertion of string
  | Unsupported_algorithm of string
  | Counter_rollback
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

let string_of_error = function
  | Invalid_relying_party s -> "Invalid passkey relying party: " ^ s
  | Invalid_user s -> "Invalid passkey user: " ^ s
  | Invalid_state -> "Invalid passkey state"
  | Invalid_client_data s -> "Invalid WebAuthn client data: " ^ s
  | Invalid_attestation s -> "Invalid WebAuthn attestation: " ^ s
  | Invalid_assertion s -> "Invalid WebAuthn assertion: " ^ s
  | Unsupported_algorithm s -> "Unsupported WebAuthn algorithm: " ^ s
  | Counter_rollback -> "Passkey sign counter rollback"
  | Challenge_error e -> Challenge.string_of_error e
  | Identity_error e -> Identity.string_of_error e

let make ~challenge : t = { challenge }

let trim = String.trim
let lower_trim s = String.lowercase_ascii (trim s)
let clean_list xs = xs |> List.map trim |> List.filter (( <> ) "")
let option_exists f = function Some x -> f x | None -> false

let relying_party ?(origins = []) ?(user_verification = false) ~id ~name () =
  let id = lower_trim id in
  let name = trim name in
  let origins = clean_list origins in
  if id = "" then Error (Invalid_relying_party "id cannot be blank")
  else if name = "" then Error (Invalid_relying_party "name cannot be blank")
  else if origins = [] then Error (Invalid_relying_party "origins cannot be empty")
  else Ok { id; name; origins; user_verification }

type user = {
  id : string;
  handle : string;
  name : string;
  display_name : string;
}

let user ~id ~handle ?display_name ~name () =
  let id = trim id in
  let handle = trim handle in
  let name = trim name in
  let display_name = Option.value ~default:name (Option.map trim display_name) in
  if id = "" then Error (Invalid_user "id cannot be blank")
  else if handle = "" then Error (Invalid_user "handle cannot be blank")
  else if name = "" then Error (Invalid_user "name cannot be blank")
  else if display_name = "" then Error (Invalid_user "display_name cannot be blank")
  else Ok { id; handle; name; display_name }

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts.Passkey: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let bson_string key value = (key, Bson.String value)

let challenge_of_token token = b64url (Challenge.token_to_string token)

type registration = {
  challenge : string;
  token : Challenge.token;
  record : Challenge.record;
  rp : relying_party;
  user : user;
}

type assertion_challenge = {
  challenge : string;
  token : Challenge.token;
  record : Challenge.record;
  rp : relying_party;
  user_id : string option;
  allowed_credentials : string list;
}

let registration_metadata (rp : relying_party) (user : user) ?redirect () : Challenge.metadata =
  {
    user_id = Some user.id;
    email = None;
    org_id = None;
    connection_id = Some rp.id;
    redirect;
    data =
      [
        bson_string "rp_id" rp.id;
        bson_string "user_id" user.id;
        bson_string "user_handle" user.handle;
        bson_string "user_name" user.name;
      ];
  }

let assertion_metadata (rp : relying_party) ?user_id ?redirect ?(allowed_credentials = []) () : Challenge.metadata =
  {
    user_id;
    email = None;
    org_id = None;
    connection_id = Some rp.id;
    redirect;
    data =
      [
        bson_string "rp_id" rp.id;
        ("allowed_credentials", Bson.Array (List.map Bson.str allowed_credentials));
      ];
  }

let begin_registration (t : t) ?ttl ?redirect rp user =
  match Challenge.create t.challenge ~purpose:Challenge.Passkey_registration ~metadata:(registration_metadata rp user ?redirect ()) ?ttl () with
  | Error e -> Error (Challenge_error e)
  | Ok issued ->
    Ok { challenge = challenge_of_token issued.token; token = issued.token; record = issued.record; rp; user }

let begin_assertion (t : t) ?ttl ?user_id ?redirect ?(allowed_credentials = []) rp =
  match Challenge.create t.challenge ~purpose:Challenge.Passkey_assertion ~metadata:(assertion_metadata rp ?user_id ?redirect ~allowed_credentials ()) ?ttl () with
  | Error e -> Error (Challenge_error e)
  | Ok issued ->
    Ok
      {
        challenge = challenge_of_token issued.token;
        token = issued.token;
        record = issued.record;
        rp;
        user_id;
        allowed_credentials = clean_list allowed_credentials;
      }

type credential = {
  id : string;
  user_id : string;
  user_handle : string;
  public_key : X509.Public_key.t;
  sign_count : int32;
  backup_eligible : bool;
  backed_up : bool;
  transports : string list;
  created_at : float;
  last_used_at : float option;
}

type registration_response = {
  id : string;
  raw_id : string;
  client_data_json : string;
  attestation_object : string;
  transports : string list;
}

type assertion_response = {
  id : string;
  raw_id : string;
  client_data_json : string;
  authenticator_data : string;
  signature : string;
  user_handle : string option;
}

type assertion = {
  credential : credential;
  user_present : bool;
  user_verified : bool;
  backup_eligible : bool;
  backed_up : bool;
}

type store = {
  find : string -> credential option;
  list : ?user_id:string -> unit -> credential list;
  insert : credential -> (unit, string) result;
  update : credential -> (unit, string) result;
  delete : string -> (bool, string) result;
}

let memory_store () =
  let credentials : (string, credential) Hashtbl.t = Hashtbl.create 64 in
  let mutex = Mutex.create () in
  let locked f =
    Mutex.lock mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f
  in
  let find id = locked (fun () -> Hashtbl.find_opt credentials id) in
  let list ?user_id () =
    locked (fun () ->
        Hashtbl.to_seq_values credentials
        |> List.of_seq
        |> List.filter (fun (credential : credential) ->
               match user_id with Some uid -> credential.user_id = uid | None -> true)
        |> List.sort (fun (a : credential) (b : credential) -> String.compare a.id b.id))
  in
  let insert (credential : credential) =
    locked (fun () ->
        if credential.id = "" then Error "passkey credential id cannot be blank"
        else if Hashtbl.mem credentials credential.id then Error "duplicate passkey credential id"
        else (
          Hashtbl.add credentials credential.id credential;
          Ok ()))
  in
  let update (credential : credential) =
    locked (fun () ->
        if Hashtbl.mem credentials credential.id then (
          Hashtbl.replace credentials credential.id credential;
          Ok ())
        else Error "passkey credential not found")
  in
  let delete id =
    locked (fun () ->
        let existed = Hashtbl.mem credentials id in
        Hashtbl.remove credentials id;
        Ok existed)
  in
  { find; list; insert; update; delete }

type state = {
  rp_id : string;
  user_id : string option;
  user_handle : string option;
  allowed_credentials : string list;
}

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_state
  | Some 0 -> Error Invalid_state
  | Some i -> Ok (String.sub raw 0 i)

let data_string key data = match List.assoc_opt key data with Some (Bson.String v) -> Some v | _ -> None
let data_strings key data = match List.assoc_opt key data with Some (Bson.Array xs) -> List.filter_map (function Bson.String s -> Some s | _ -> None) xs | _ -> []

let state_of_record purpose record =
  if record.Challenge.purpose <> purpose then Error Invalid_state
  else
    let data = record.metadata.data in
    match data_string "rp_id" data with
    | None -> Error Invalid_state
    | Some rp_id ->
      Ok
        {
          rp_id;
          user_id = record.metadata.user_id;
          user_handle = data_string "user_handle" data;
          allowed_credentials = data_strings "allowed_credentials" data;
        }

let state_for_token (t : t) purpose token =
  match token_id token with
  | Error _ as e -> e
  | Ok id -> (
    match Challenge.find t.challenge id with
    | Error e -> Error (Challenge_error e)
    | Ok None -> Error Invalid_state
    | Ok (Some record) -> state_of_record purpose record)

let consume_token (t : t) purpose token =
  match Challenge.consume t.challenge ~purpose token with
  | Error e -> Error (Challenge_error e)
  | Ok record -> state_of_record purpose record

let origin_allowed rp origin = List.exists (( = ) origin) rp.origins

let json_string name obj =
  match Json.member name obj with
  | Some (Json.String s) -> Some s
  | _ -> None

let parse_client_data ~expected_type ~challenge rp client_data_json =
  match Json.parse_opt client_data_json with
  | None -> Error (Invalid_client_data "json")
  | Some json -> (
    match (json_string "type" json, json_string "challenge" json, json_string "origin" json) with
    | Some typ, Some got_challenge, Some origin when typ = expected_type && got_challenge = challenge && origin_allowed rp origin -> Ok ()
    | Some typ, _, _ when typ <> expected_type -> Error (Invalid_client_data "type")
    | _, Some got, _ when got <> challenge -> Error (Invalid_client_data "challenge")
    | _, _, Some origin when not (origin_allowed rp origin) -> Error (Invalid_client_data ("origin: " ^ origin))
    | _ -> Error (Invalid_client_data "fields"))

type cbor =
  | Cbor_int of int
  | Cbor_neg of int
  | Cbor_bytes of string
  | Cbor_text of string
  | Cbor_array of cbor list
  | Cbor_map of (cbor * cbor) list
  | Cbor_bool of bool
  | Cbor_null

let uint16_be s off = (Char.code s.[off] lsl 8) lor Char.code s.[off + 1]
let int32_be s off = Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[off])) 24) (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[off + 1])) 16) (Int32.logor (Int32.shift_left (Int32.of_int (Char.code s.[off + 2])) 8) (Int32.of_int (Char.code s.[off + 3]))))

let cbor_uint s i addl =
  let n = String.length s in
  match addl with
  | x when x < 24 -> Some (x, i)
  | 24 when i < n -> Some (Char.code s.[i], i + 1)
  | 25 when i + 1 < n -> Some (uint16_be s i, i + 2)
  | 26 when i + 3 < n ->
    let v = Int32.to_int (int32_be s i) in
    if v < 0 then None else Some (v, i + 4)
  | _ -> None

let parse_cbor s =
  let n = String.length s in
  let rec item i =
    if i >= n then None
    else
      let b = Char.code s.[i] in
      let major = b lsr 5 in
      let addl = b land 0x1f in
      let i = i + 1 in
      match major with
      | 0 -> Option.map (fun (v, j) -> (Cbor_int v, j)) (cbor_uint s i addl)
      | 1 -> Option.map (fun (v, j) -> (Cbor_neg (-1 - v), j)) (cbor_uint s i addl)
      | 2 -> (
        match cbor_uint s i addl with
        | Some (len, j) when len >= 0 && j + len <= n -> Some (Cbor_bytes (String.sub s j len), j + len)
        | _ -> None)
      | 3 -> (
        match cbor_uint s i addl with
        | Some (len, j) when len >= 0 && j + len <= n -> Some (Cbor_text (String.sub s j len), j + len)
        | _ -> None)
      | 4 -> (
        match cbor_uint s i addl with
        | None -> None
        | Some (len, j) ->
          let rec loop k j acc =
            if k = 0 then Some (Cbor_array (List.rev acc), j)
            else
              match item j with
              | None -> None
              | Some (v, j) -> loop (k - 1) j (v :: acc)
          in
          loop len j [])
      | 5 -> (
        match cbor_uint s i addl with
        | None -> None
        | Some (len, j) ->
          let rec loop k j acc =
            if k = 0 then Some (Cbor_map (List.rev acc), j)
            else
              match item j with
              | None -> None
              | Some (key, j) -> (
                match item j with
                | None -> None
                | Some (value, j) -> loop (k - 1) j ((key, value) :: acc))
          in
          loop len j [])
      | 7 -> (
        match addl with
        | 20 -> Some (Cbor_bool false, i)
        | 21 -> Some (Cbor_bool true, i)
        | 22 -> Some (Cbor_null, i)
        | _ -> None)
      | _ -> None
  in
  match item 0 with
  | Some (v, i) when i = n -> Some v
  | _ -> None

let cbor_find_text key = function
  | Cbor_map fields -> List.find_map (function Cbor_text k, v when k = key -> Some v | _ -> None) fields
  | _ -> None

let cbor_find_int key = function
  | Cbor_map fields -> List.find_map (function (Cbor_int k | Cbor_neg k), v when k = key -> Some v | _ -> None) fields
  | _ -> None

let cose_key_to_public_key cbor =
  let int_value = function Cbor_int n | Cbor_neg n -> Some n | _ -> None in
  let bytes_value = function Cbor_bytes s -> Some s | _ -> None in
  match (Option.bind (cbor_find_int 1 cbor) int_value, Option.bind (cbor_find_int 3 cbor) int_value, Option.bind (cbor_find_int (-1) cbor) int_value, Option.bind (cbor_find_int (-2) cbor) bytes_value, Option.bind (cbor_find_int (-3) cbor) bytes_value) with
  | Some 2, Some (-7), Some 1, Some x, Some y when String.length x = 32 && String.length y = 32 -> (
    match Mirage_crypto_ec.P256.Dsa.pub_of_octets ("\004" ^ x ^ y) with
    | Ok key -> Ok (`P256 key : X509.Public_key.t)
    | Error _ -> Error (Invalid_attestation "public_key"))
  | _, Some alg, _, _, _ -> Error (Unsupported_algorithm (string_of_int alg))
  | _ -> Error (Invalid_attestation "cose_key")

type auth_data = {
  rp_id_hash : string;
  user_present : bool;
  user_verified : bool;
  backup_eligible : bool;
  backed_up : bool;
  sign_count : int32;
  attested_credential : (string * X509.Public_key.t) option;
}

let parse_auth_data ?(require_attested = false) bytes =
  let n = String.length bytes in
  if n < 37 then Error (Invalid_assertion "authenticator_data")
  else
    let flags = Char.code bytes.[32] in
    let user_present = flags land 0x01 <> 0 in
    let user_verified = flags land 0x04 <> 0 in
    let backup_eligible = flags land 0x08 <> 0 in
    let backed_up = flags land 0x10 <> 0 in
    let has_attested = flags land 0x40 <> 0 in
    if require_attested && not has_attested then Error (Invalid_attestation "attested_credential_data")
    else
      let sign_count = int32_be bytes 33 in
      if has_attested then
        if n < 55 then Error (Invalid_attestation "attested_credential_data")
        else
          let cred_len = uint16_be bytes 53 in
          let cred_start = 55 in
          let key_start = cred_start + cred_len in
          if cred_len <= 0 || key_start > n then Error (Invalid_attestation "credential_id")
          else
            let credential_id = String.sub bytes cred_start cred_len in
            let key_bytes = String.sub bytes key_start (n - key_start) in
            (match parse_cbor key_bytes with
            | None -> Error (Invalid_attestation "cose_key")
            | Some cose -> (
              match cose_key_to_public_key cose with
              | Error _ as e -> e
              | Ok public_key ->
                Ok
                  {
                    rp_id_hash = String.sub bytes 0 32;
                    user_present;
                    user_verified;
                    backup_eligible;
                    backed_up;
                    sign_count;
                    attested_credential = Some (credential_id, public_key);
                  }))
      else
        Ok
          {
            rp_id_hash = String.sub bytes 0 32;
            user_present;
            user_verified;
            backup_eligible;
            backed_up;
            sign_count;
            attested_credential = None;
          }

let check_auth_data (rp : relying_party) (auth : auth_data) =
  if auth.rp_id_hash <> sha256 rp.id then Error (Invalid_assertion "rp_id_hash")
  else if not auth.user_present then Error (Invalid_assertion "user_present")
  else if rp.user_verification && not auth.user_verified then Error (Invalid_assertion "user_verified")
  else Ok ()

let finish_registration t ?(now = Unix.gettimeofday) (rp : relying_party) (response : registration_response) ~token ~user_id =
  match state_for_token t Challenge.Passkey_registration token with
  | Error _ as e -> e
  | Ok state ->
    let challenge = challenge_of_token token in
    if state.rp_id <> rp.id then Error Invalid_state
    else if state.user_id <> Some user_id then Error Invalid_state
    else
      match parse_client_data ~expected_type:"webauthn.create" ~challenge rp response.client_data_json with
      | Error _ as e -> e
      | Ok () -> (
        match parse_cbor response.attestation_object with
        | None -> Error (Invalid_attestation "cbor")
        | Some attestation -> (
          match (cbor_find_text "fmt" attestation, cbor_find_text "authData" attestation, cbor_find_text "attStmt" attestation) with
          | Some (Cbor_text "none"), Some (Cbor_bytes auth_data), Some (Cbor_map []) -> (
            match parse_auth_data ~require_attested:true auth_data with
            | Error _ as e -> e
            | Ok auth -> (
              match check_auth_data rp auth with
              | Error (Invalid_assertion s) -> Error (Invalid_attestation s)
              | Error _ as e -> e
              | Ok () -> (
                match auth.attested_credential with
                | None -> Error (Invalid_attestation "credential")
                | Some (credential_id, public_key) ->
                  if credential_id <> response.raw_id || (response.id <> "" && response.id <> b64url response.raw_id) then Error (Invalid_attestation "credential_id")
                  else
                    match consume_token t Challenge.Passkey_registration token with
                    | Error _ as e -> e
                    | Ok _ ->
                      Ok
                        {
                          id = credential_id;
                          user_id;
                          user_handle = Option.value ~default:"" state.user_handle;
                          public_key;
                          sign_count = auth.sign_count;
                          backup_eligible = auth.backup_eligible;
                          backed_up = auth.backed_up;
                          transports = clean_list response.transports;
                          created_at = now ();
                          last_used_at = None;
                        })))
          | Some (Cbor_text fmt), _, _ -> Error (Invalid_attestation ("fmt: " ^ fmt))
          | _ -> Error (Invalid_attestation "attestation_object")))

let finish_assertion t ?(now = Unix.gettimeofday) (rp : relying_party) (credential : credential) (response : assertion_response) ~token =
  match state_for_token t Challenge.Passkey_assertion token with
  | Error _ as e -> e
  | Ok state ->
    let challenge = challenge_of_token token in
    if state.rp_id <> rp.id then Error Invalid_state
    else if response.raw_id <> credential.id || (response.id <> "" && response.id <> b64url response.raw_id) then Error (Invalid_assertion "credential_id")
    else if state.allowed_credentials <> [] && not (List.exists (( = ) credential.id) state.allowed_credentials) then Error (Invalid_assertion "allowed_credentials")
    else if option_exists (( <> ) credential.user_handle) response.user_handle then Error (Invalid_assertion "user_handle")
    else
      match parse_client_data ~expected_type:"webauthn.get" ~challenge rp response.client_data_json with
      | Error _ as e -> e
      | Ok () -> (
        match parse_auth_data response.authenticator_data with
        | Error _ as e -> e
        | Ok auth -> (
          match check_auth_data rp auth with
          | Error _ as e -> e
          | Ok () ->
            let signed = response.authenticator_data ^ sha256 response.client_data_json in
            (match X509.Public_key.verify `SHA256 ~scheme:`ECDSA ~signature:response.signature credential.public_key (`Message signed) with
            | Error _ -> Error (Invalid_assertion "signature")
            | Ok () ->
              let old_count = credential.sign_count in
              let new_count = auth.sign_count in
              if old_count <> 0l && new_count <> 0l && Int32.compare new_count old_count <= 0 then Error Counter_rollback
              else
                match consume_token t Challenge.Passkey_assertion token with
                | Error _ as e -> e
                | Ok _ ->
                  Ok
                    {
                      credential =
                        {
                          credential with
                          sign_count = new_count;
                          backup_eligible = auth.backup_eligible;
                          backed_up = auth.backed_up;
                          last_used_at = Some (now ());
                        };
                      user_present = auth.user_present;
                      user_verified = auth.user_verified;
                      backup_eligible = auth.backup_eligible;
                      backed_up = auth.backed_up;
                    })))

let identity (credential : credential) =
  match Identity.passkey ~credential_id:credential.id ~user_handle:credential.user_handle () with
  | Ok key -> Ok key
  | Error e -> Error (Identity_error e)

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"passkey-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_rp ?(uv = false) () =
  ok (relying_party ~id:"example.com" ~name:"Example" ~origins:[ "https://example.com" ] ~user_verification:uv ())

let test_user () = ok (user ~id:"user_1" ~handle:"handle_1" ~name:"ada@example.com" ())

let client_data typ challenge origin =
  Json.to_string (Json.Obj [ ("type", Json.String typ); ("challenge", Json.String challenge); ("origin", Json.String origin) ])

let cbor_int n =
  if n >= 0 && n < 24 then String.make 1 (Char.chr n)
  else if n >= 0 && n <= 255 then String.make 1 (Char.chr 0x18) ^ String.make 1 (Char.chr n)
  else invalid_arg "cbor_int"

let cbor_neg n =
  let v = -1 - n in
  if v >= 0 && v < 24 then String.make 1 (Char.chr (0x20 lor v)) else invalid_arg "cbor_neg"

let cbor_text s =
  if String.length s < 24 then String.make 1 (Char.chr (0x60 lor String.length s)) ^ s else invalid_arg "cbor_text"

let cbor_bytes s =
  let len = String.length s in
  if len < 24 then String.make 1 (Char.chr (0x40 lor len)) ^ s
  else if len <= 255 then "\x58" ^ String.make 1 (Char.chr len) ^ s
  else "\x59" ^ String.make 1 (Char.chr (len lsr 8)) ^ String.make 1 (Char.chr (len land 255)) ^ s

let cbor_map fields =
  if List.length fields >= 24 then invalid_arg "cbor_map";
  String.make 1 (Char.chr (0xa0 lor List.length fields)) ^ String.concat "" (List.map (fun (k, v) -> k ^ v) fields)

let cose_key public =
  match public with
  | `P256 pk ->
    let raw = Mirage_crypto_ec.P256.Dsa.pub_to_octets pk in
    cbor_map
      [
        (cbor_int 1, cbor_int 2);
        (cbor_int 3, cbor_neg (-7));
        (cbor_neg (-1), cbor_int 1);
        (cbor_neg (-2), cbor_bytes (String.sub raw 1 32));
        (cbor_neg (-3), cbor_bytes (String.sub raw 33 32));
      ]
  | _ -> invalid_arg "only P256 test keys"

let auth_data ?(flags = 0x41) ?(count = 1l) ?attested rp_id =
  let count =
    String.init 4 (fun i ->
        let shift = (3 - i) * 8 in
        Char.chr (Int32.(to_int (logand (shift_right_logical count shift) 0xffl))))
  in
  let base = sha256 rp_id ^ String.make 1 (Char.chr flags) ^ count in
  match attested with
  | None -> base
  | Some (credential_id, public_key) ->
    let len = String.length credential_id in
    base ^ String.make 16 '\000' ^ String.make 1 (Char.chr (len lsr 8)) ^ String.make 1 (Char.chr (len land 255)) ^ credential_id ^ cose_key public_key

let attestation_object auth_data =
  cbor_map [ (cbor_text "fmt", cbor_text "none"); (cbor_text "authData", cbor_bytes auth_data); (cbor_text "attStmt", cbor_map []) ]

let%test "relying_party and user validate required fields" =
  Result.is_error (relying_party ~id:"" ~name:"n" ~origins:[ "https://example.com" ] ())
  && Result.is_error (relying_party ~id:"example.com" ~name:"n" ())
  && Result.is_error (user ~id:"" ~handle:"h" ~name:"n" ())

let%test "begin_registration stores challenge metadata" =
  let t, _ = test_service () in
  let rp = test_rp () in
  let user = test_user () in
  match begin_registration t rp user with
  | Error _ -> false
  | Ok r -> r.challenge = challenge_of_token r.token && r.record.metadata.connection_id = Some "example.com"

let%test "finish_registration accepts none attestation ES256 credential" =
  Mirage_crypto_rng_unix.use_default ();
  let priv = X509.Private_key.generate `P256 in
  let public_key = X509.Private_key.public priv in
  let t, _ = test_service () in
  let rp = test_rp () in
  let user = test_user () in
  match begin_registration t rp user with
  | Error _ -> false
  | Ok reg ->
    let credential_id = secure_random 16 in
    let response =
      {
        id = b64url credential_id;
        raw_id = credential_id;
        client_data_json = client_data "webauthn.create" reg.challenge "https://example.com";
        attestation_object = attestation_object (auth_data ~attested:(credential_id, public_key) rp.id);
        transports = [ "internal" ];
      }
    in
    (match finish_registration t ~now:(fun () -> 1_000.) rp response ~token:reg.token ~user_id:user.id with
    | Ok cred ->
      cred.id = credential_id
      && cred.user_id = "user_1"
      && cred.user_handle = "handle_1"
      && cred.sign_count = 1l
      && cred.last_used_at = None
      && (match identity cred with Ok key -> Identity.kind key = Identity.Passkey && Identity.subject key = credential_id | _ -> false)
    | Error _ -> false)

let%test "finish_registration rejects wrong origin without consuming challenge" =
  Mirage_crypto_rng_unix.use_default ();
  let priv = X509.Private_key.generate `P256 in
  let public_key = X509.Private_key.public priv in
  let t, _ = test_service () in
  let rp = test_rp () in
  let user = test_user () in
  match begin_registration t rp user with
  | Error _ -> false
  | Ok reg ->
    let credential_id = secure_random 16 in
    let response origin =
      {
        id = b64url credential_id;
        raw_id = credential_id;
        client_data_json = client_data "webauthn.create" reg.challenge origin;
        attestation_object = attestation_object (auth_data ~attested:(credential_id, public_key) rp.id);
        transports = [];
      }
    in
    Result.is_error (finish_registration t rp (response "https://evil.example") ~token:reg.token ~user_id:user.id)
    && Result.is_ok (finish_registration t rp (response "https://example.com") ~token:reg.token ~user_id:user.id)

let%test "finish_assertion verifies signature and updates counter" =
  Mirage_crypto_rng_unix.use_default ();
  let priv = X509.Private_key.generate `P256 in
  let public_key = X509.Private_key.public priv in
  let t, _ = test_service () in
  let rp = test_rp () in
  let credential_id = secure_random 16 in
  let credential =
    {
      id = credential_id;
      user_id = "user_1";
      user_handle = "handle_1";
      public_key;
      sign_count = 1l;
      backup_eligible = false;
      backed_up = false;
      transports = [];
      created_at = 1_000.;
      last_used_at = None;
    }
  in
  match begin_assertion t ~user_id:"user_1" ~allowed_credentials:[ credential_id ] rp with
  | Error _ -> false
  | Ok challenge ->
    let client_data_json = client_data "webauthn.get" challenge.challenge "https://example.com" in
    let authenticator_data = auth_data ~flags:0x01 ~count:2l rp.id in
    let signature =
      match X509.Private_key.sign `SHA256 ~scheme:`ECDSA priv (`Message (authenticator_data ^ sha256 client_data_json)) with
      | Ok s -> s
      | Error (`Msg m) -> failwith m
    in
    let response =
      {
        id = b64url credential_id;
        raw_id = credential_id;
        client_data_json;
        authenticator_data;
        signature;
        user_handle = Some "handle_1";
      }
    in
    (match finish_assertion t ~now:(fun () -> 2_000.) rp credential response ~token:challenge.token with
    | Ok a -> a.credential.sign_count = 2l && a.credential.last_used_at = Some 2_000. && a.user_present
    | Error _ -> false)

let%test "finish_assertion rejects counter rollback" =
  Mirage_crypto_rng_unix.use_default ();
  let priv = X509.Private_key.generate `P256 in
  let public_key = X509.Private_key.public priv in
  let t, _ = test_service () in
  let rp = test_rp () in
  let credential_id = secure_random 16 in
  let credential =
    {
      id = credential_id;
      user_id = "user_1";
      user_handle = "handle_1";
      public_key;
      sign_count = 5l;
      backup_eligible = false;
      backed_up = false;
      transports = [];
      created_at = 1_000.;
      last_used_at = None;
    }
  in
  match begin_assertion t rp with
  | Error _ -> false
  | Ok challenge ->
    let client_data_json = client_data "webauthn.get" challenge.challenge "https://example.com" in
    let authenticator_data = auth_data ~flags:0x01 ~count:4l rp.id in
    let signature =
      match X509.Private_key.sign `SHA256 ~scheme:`ECDSA priv (`Message (authenticator_data ^ sha256 client_data_json)) with
      | Ok s -> s
      | Error (`Msg m) -> failwith m
    in
    let response = { id = b64url credential_id; raw_id = credential_id; client_data_json; authenticator_data; signature; user_handle = None } in
    finish_assertion t rp credential response ~token:challenge.token = Error Counter_rollback

let%test "memory_store persists unique credentials and updates counters" =
  Mirage_crypto_rng_unix.use_default ();
  let public_key = X509.Private_key.public (X509.Private_key.generate `P256) in
  let store = memory_store () in
  let credential =
    {
      id = "cred-1";
      user_id = "user-1";
      user_handle = "handle-1";
      public_key;
      sign_count = 1l;
      backup_eligible = false;
      backed_up = false;
      transports = [ "internal" ];
      created_at = 1.;
      last_used_at = None;
    }
  in
  store.insert credential = Ok ()
  && Result.is_error (store.insert credential)
  && store.find "cred-1" = Some credential
  &&
  let updated = { credential with sign_count = 2l; last_used_at = Some 2. } in
  store.update updated = Ok ()
  && store.find "cred-1" = Some updated
  && store.list ~user_id:"user-1" () = [ updated ]
  && store.delete "cred-1" = Ok true
  && store.find "cred-1" = None
