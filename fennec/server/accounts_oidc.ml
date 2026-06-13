module Challenge = Accounts_challenge
module Identity = Accounts_identity
module OAuth = Accounts_oauth
module H = Fennec_core.Http
module Bson = Bson
module Json = Fennec_mongo_json.Json

type connection = {
  id : string;
  issuer : string;
  authorize_url : string;
  client_id : string;
  redirect_uri : string;
  scopes : string list;
  extra_params : (string * string) list;
  org_id : string option;
  domains : string list;
  allow_jit : bool;
}

type preset = {
  id : string;
  display_name : string;
  issuer : string;
  authorize_url : string;
  default_scopes : string list;
  parameters : string list;
}

type t = { challenge : Challenge.t }

type error =
  | Invalid_connection of string
  | Invalid_callback of string
  | Invalid_state
  | Invalid_jwks of string
  | Invalid_id_token of string
  | Claim_mismatch of string
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

let string_of_error = function
  | Invalid_connection s -> "Invalid OIDC connection: " ^ s
  | Invalid_callback s -> "Invalid OIDC callback: " ^ s
  | Invalid_state -> "Invalid OIDC state"
  | Invalid_jwks s -> "Invalid OIDC JWKS: " ^ s
  | Invalid_id_token s -> "Invalid OIDC ID token: " ^ s
  | Claim_mismatch s -> "OIDC claim mismatch: " ^ s
  | Challenge_error e -> Challenge.string_of_error e
  | Identity_error e -> Identity.string_of_error e

let make ~challenge : t = { challenge }

let trim = String.trim
let lower_trim s = String.lowercase_ascii (trim s)
let clean_list xs = xs |> List.map trim |> List.filter (( <> ) "")
let clean_domains xs = xs |> List.map lower_trim |> List.filter (( <> ) "")
let preset ~id ~display_name ~issuer ~authorize_url ~default_scopes ?(parameters = []) () =
  { id; display_name; issuer; authorize_url; default_scopes; parameters }

let reserved_extra_param = function
  | "response_type" | "client_id" | "redirect_uri" | "state" | "scope" | "code_challenge" | "code_challenge_method" | "nonce" ->
    true
  | _ -> false

let clean_params params =
  params |> List.map (fun (k, v) -> (trim k, v)) |> List.filter (fun (k, _) -> k <> "")

let ensure_openid scopes =
  let scopes = clean_list scopes in
  if List.exists (( = ) "openid") scopes then scopes else "openid" :: scopes

let connection ?(scopes = [ "openid"; "email"; "profile" ]) ?(extra_params = []) ?org_id ?(domains = [])
    ?(allow_jit = true) ~id ~issuer ~authorize_url ~client_id ~redirect_uri () =
  let id = lower_trim id in
  let issuer = trim issuer in
  let authorize_url = trim authorize_url in
  let client_id = trim client_id in
  let redirect_uri = trim redirect_uri in
  let extra_params = clean_params extra_params in
  if id = "" then Error (Invalid_connection "id cannot be blank")
  else if issuer = "" then Error (Invalid_connection "issuer cannot be blank")
  else if authorize_url = "" then Error (Invalid_connection "authorize_url cannot be blank")
  else if client_id = "" then Error (Invalid_connection "client_id cannot be blank")
  else if redirect_uri = "" then Error (Invalid_connection "redirect_uri cannot be blank")
  else
    match List.find_opt (fun (k, _) -> reserved_extra_param k) extra_params with
    | Some (k, _) -> Error (Invalid_connection ("extra param is reserved: " ^ k))
    | None ->
      Ok
        {
          id;
          issuer;
          authorize_url;
          client_id;
          redirect_uri;
          scopes = ensure_openid scopes;
          extra_params;
          org_id = Option.map trim org_id;
          domains = clean_domains domains;
          allow_jit;
        }

module Providers = struct
  let google_preset =
    preset ~id:"google" ~display_name:"Google" ~issuer:"https://accounts.google.com"
      ~authorize_url:"https://accounts.google.com/o/oauth2/v2/auth"
      ~default_scopes:[ "openid"; "email"; "profile" ] ()

  let microsoft_entra_preset =
    preset ~id:"microsoft_entra" ~display_name:"Microsoft Entra ID"
      ~issuer:"https://login.microsoftonline.com/{tenant_id}/v2.0"
      ~authorize_url:"https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/authorize"
      ~default_scopes:[ "openid"; "email"; "profile" ] ~parameters:[ "tenant_id" ] ()

  let apple_preset =
    preset ~id:"apple" ~display_name:"Apple" ~issuer:"https://appleid.apple.com"
      ~authorize_url:"https://appleid.apple.com/auth/authorize"
      ~default_scopes:[ "openid"; "email"; "name" ] ()

  let linkedin_preset =
    preset ~id:"linkedin" ~display_name:"LinkedIn" ~issuer:"https://www.linkedin.com/oauth"
      ~authorize_url:"https://www.linkedin.com/oauth/v2/authorization"
      ~default_scopes:[ "openid"; "profile"; "email" ] ()

  let slack_preset =
    preset ~id:"slack" ~display_name:"Slack" ~issuer:"https://slack.com"
      ~authorize_url:"https://slack.com/openid/connect/authorize"
      ~default_scopes:[ "openid"; "email"; "profile" ] ()

  let gitlab_preset =
    preset ~id:"gitlab" ~display_name:"GitLab" ~issuer:"https://gitlab.com"
      ~authorize_url:"https://gitlab.com/oauth/authorize"
      ~default_scopes:[ "openid"; "profile"; "email" ] ()

  let twitch_preset =
    preset ~id:"twitch" ~display_name:"Twitch" ~issuer:"https://id.twitch.tv/oauth2"
      ~authorize_url:"https://id.twitch.tv/oauth2/authorize" ~default_scopes:[ "openid" ] ()

  let salesforce_preset =
    preset ~id:"salesforce" ~display_name:"Salesforce" ~issuer:"https://login.salesforce.com"
      ~authorize_url:"https://login.salesforce.com/services/oauth2/authorize"
      ~default_scopes:[ "openid"; "email"; "profile" ] ()

  let dropbox_preset =
    preset ~id:"dropbox" ~display_name:"Dropbox" ~issuer:"https://www.dropbox.com"
      ~authorize_url:"https://www.dropbox.com/oauth2/authorize"
      ~default_scopes:[ "openid"; "profile"; "email" ] ()

  let auth0_preset =
    preset ~id:"auth0" ~display_name:"Auth0" ~issuer:"https://{domain}/"
      ~authorize_url:"https://{domain}/authorize" ~default_scopes:[ "openid"; "email"; "profile" ]
      ~parameters:[ "domain" ] ()

  let okta_preset =
    preset ~id:"okta" ~display_name:"Okta" ~issuer:"https://{domain}/oauth2/{authorization_server_id}"
      ~authorize_url:"https://{domain}/oauth2/{authorization_server_id}/v1/authorize"
      ~default_scopes:[ "openid"; "email"; "profile" ]
      ~parameters:[ "domain"; "authorization_server_id" ] ()

  let keycloak_preset =
    preset ~id:"keycloak" ~display_name:"Keycloak" ~issuer:"{base_url}/realms/{realm}"
      ~authorize_url:"{base_url}/realms/{realm}/protocol/openid-connect/auth"
      ~default_scopes:[ "openid"; "email"; "profile" ] ~parameters:[ "base_url"; "realm" ] ()

  let all =
    [
      google_preset;
      microsoft_entra_preset;
      apple_preset;
      linkedin_preset;
      slack_preset;
      gitlab_preset;
      twitch_preset;
      salesforce_preset;
      dropbox_preset;
      auth0_preset;
      okta_preset;
      keycloak_preset;
    ]

  let find id =
    let id = lower_trim id in
    List.find_opt (fun preset -> preset.id = id) all

  let contains_template s = String.contains s '{' || String.contains s '}'

  let from_preset ?id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri p =
    if contains_template p.issuer || contains_template p.authorize_url then
      Error (Invalid_connection ("provider preset needs parameters: " ^ p.id))
    else
      let id = match id with Some id -> id | None -> p.id in
      let scopes = match scopes with Some scopes -> scopes | None -> p.default_scopes in
      connection ~id ~issuer:p.issuer ~authorize_url:p.authorize_url ~client_id ~redirect_uri ~scopes ?extra_params
        ?org_id ?domains ?allow_jit ()

  let with_scheme s =
    let s = trim s in
    if String.starts_with ~prefix:"https://" s || String.starts_with ~prefix:"http://" s then s
    else "https://" ^ s

  let strip_trailing_slash s =
    let rec loop i = if i > 0 && s.[i - 1] = '/' then loop (i - 1) else i in
    String.sub s 0 (loop (String.length s))

  let clean_base s = strip_trailing_slash (with_scheme s)

  let google ?(id = "google") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri google_preset

  let microsoft_entra ?(id = "microsoft_entra") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~tenant_id
      ~client_id ~redirect_uri () =
    let tenant_id = trim tenant_id in
    let issuer = "https://login.microsoftonline.com/" ^ tenant_id ^ "/v2.0" in
    let authorize_url = "https://login.microsoftonline.com/" ^ tenant_id ^ "/oauth2/v2.0/authorize" in
    let p = { microsoft_entra_preset with issuer; authorize_url; parameters = [] } in
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri p

  let apple ?(id = "apple") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri apple_preset

  let linkedin ?(id = "linkedin") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri linkedin_preset

  let slack ?(id = "slack") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri slack_preset

  let gitlab ?(id = "gitlab") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri gitlab_preset

  let twitch ?(id = "twitch") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri twitch_preset

  let salesforce ?(id = "salesforce") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id
      ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri salesforce_preset

  let dropbox ?(id = "dropbox") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri () =
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri dropbox_preset

  let auth0 ?(id = "auth0") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~domain ~client_id ~redirect_uri
      () =
    let issuer = clean_base domain ^ "/" in
    let p = { auth0_preset with issuer; authorize_url = issuer ^ "authorize"; parameters = [] } in
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri p

  let okta ?(id = "okta") ?(authorization_server_id = "default") ?scopes ?extra_params ?org_id ?domains
      ?allow_jit ~domain ~client_id ~redirect_uri () =
    let base = clean_base domain in
    let issuer = base ^ "/oauth2/" ^ trim authorization_server_id in
    let p = { okta_preset with issuer; authorize_url = issuer ^ "/v1/authorize"; parameters = [] } in
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri p

  let keycloak ?(id = "keycloak") ?scopes ?extra_params ?org_id ?domains ?allow_jit ~base_url ~realm ~client_id
      ~redirect_uri () =
    let issuer = clean_base base_url ^ "/realms/" ^ trim realm in
    let p =
      { keycloak_preset with issuer; authorize_url = issuer ^ "/protocol/openid-connect/auth"; parameters = [] }
    in
    from_preset ~id ?scopes ?extra_params ?org_id ?domains ?allow_jit ~client_id ~redirect_uri p
end

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts.Oidc: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let nonce () = b64url (secure_random 18)

let encode_query pairs =
  String.concat "&" (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) pairs)

let with_query base params =
  let sep =
    if String.contains base '?' then if String.ends_with ~suffix:"?" base || String.ends_with ~suffix:"&" base then "" else "&" else "?"
  in
  base ^ sep ^ encode_query params

let bson_string key value = (key, Bson.String value)

let metadata ~(connection : connection) ~pkce ~nonce ?user_id ?org_id ?redirect () : Challenge.metadata =
  let org_id = match org_id with Some _ -> org_id | None -> connection.org_id in
  {
    user_id;
    email = None;
    org_id;
    connection_id = Some connection.id;
    redirect;
    data =
      [
        bson_string "connection_id" connection.id;
        bson_string "issuer" connection.issuer;
        bson_string "client_id" connection.client_id;
        bson_string "redirect_uri" connection.redirect_uri;
        bson_string "code_verifier" pkce.OAuth.verifier;
        bson_string "nonce" nonce;
      ];
  }

type authorization = {
  url : string;
  state : Challenge.token;
  pkce : OAuth.pkce;
  nonce : string;
  record : Challenge.record;
  connection : connection;
}

let authorize (t : t) ?ttl ?user_id ?org_id ?redirect (connection : connection) =
  let pkce = OAuth.pkce () in
  let nonce = nonce () in
  match
    Challenge.create t.challenge ~purpose:Challenge.Oidc_state
      ~metadata:(metadata ~connection ~pkce ~nonce ?user_id ?org_id ?redirect ()) ?ttl ()
  with
  | Error e -> Error (Challenge_error e)
  | Ok issued ->
    let params =
      [
        ("response_type", "code");
        ("client_id", connection.client_id);
        ("redirect_uri", connection.redirect_uri);
        ("state", Challenge.token_to_string issued.token);
        ("scope", String.concat " " connection.scopes);
        ("nonce", nonce);
        ("code_challenge", pkce.challenge);
        ("code_challenge_method", "S256");
      ]
      @ connection.extra_params
    in
    Ok { url = with_query connection.authorize_url params; state = issued.token; pkce; nonce; record = issued.record; connection }

type callback = OAuth.callback =
  | Code of { code : string; state : Challenge.token }
  | Callback_error of { error : string; description : string option; state : Challenge.token option }

let parse_callback query =
  match OAuth.parse_callback query with Ok cb -> Ok cb | Error (OAuth.Invalid_callback s) -> Error (Invalid_callback s) | Error _ -> Error Invalid_state

type state = {
  connection_id : string;
  issuer : string;
  client_id : string;
  code_verifier : string;
  nonce : string;
  redirect_uri : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

let data_string key data = match List.assoc_opt key data with Some (Bson.String v) -> Some v | _ -> None

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_state
  | Some 0 -> Error Invalid_state
  | Some i -> Ok (String.sub raw 0 i)

let state_of_record ?expected_connection record =
  let data = record.Challenge.metadata.data in
  match
    ( data_string "connection_id" data,
      data_string "issuer" data,
      data_string "client_id" data,
      data_string "code_verifier" data,
      data_string "nonce" data,
      data_string "redirect_uri" data )
  with
  | Some connection_id, Some issuer, Some client_id, Some code_verifier, Some nonce, Some redirect_uri ->
    let connection_id = lower_trim connection_id in
    let expected_ok =
      match expected_connection with
      | None -> true
      | Some expected -> connection_id = lower_trim expected
    in
    if not expected_ok then Error Invalid_state
    else
      Ok
        {
          connection_id;
          issuer;
          client_id;
          code_verifier;
          nonce;
          redirect_uri;
          user_id = record.metadata.user_id;
          org_id = record.metadata.org_id;
          redirect = record.metadata.redirect;
          record;
        }
  | _ -> Error Invalid_state

let precheck_connection (t : t) ?expected_connection token =
  match expected_connection with
  | None -> Ok ()
  | Some _ -> (
    match token_id token with
    | Error _ as e -> e
    | Ok id -> (
      match Challenge.find t.challenge id with
      | Error e -> Error (Challenge_error e)
      | Ok None -> Error Invalid_state
      | Ok (Some record) -> state_of_record ?expected_connection record |> Result.map (fun _ -> ())))

let consume_state (t : t) ?expected_connection token =
  match precheck_connection t ?expected_connection token with
  | Error _ as e -> e
  | Ok () -> (
    match Challenge.consume t.challenge ~purpose:Challenge.Oidc_state token with
    | Error e -> Error (Challenge_error e)
    | Ok record -> state_of_record ?expected_connection record)

type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : float;
  not_before : float option;
  issued_at : float option;
  nonce : string option;
  email : string option;
  email_verified : bool option;
  hosted_domain : string option;
  tenant : string option;
  groups : string list;
}

type principal = {
  identity : Identity.key;
  email_identity : Identity.key option;
  email : string option;
  email_verified : bool;
  org_id : string option;
  groups : string list;
  claims : claims;
}

type jwk = {
  kid : string option;
  alg : string option;
  key : X509.Public_key.t;
}

let audience_contains client_id audience = List.exists (( = ) client_id) audience
let option_exists f = function Some x -> f x | None -> false

let b64url_decode s = Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s |> Result.to_option

let z_of_octets s =
  let z = ref Z.zero in
  String.iter (fun c -> z := Z.logor (Z.shift_left !z 8) (Z.of_int (Char.code c))) s;
  !z

let json_string key json = Option.bind (Json.member key json) Json.to_string_opt

let json_bool key json =
  match Json.member key json with
  | Some (Json.Bool b) -> Some b
  | _ -> None

let json_number key json =
  match Json.member key json with
  | Some (Json.Number n) -> Some n
  | _ -> None

let json_string_list key json =
  match Json.member key json with
  | Some (Json.String s) -> [ s ]
  | Some (Json.List xs) -> List.filter_map Json.to_string_opt xs
  | _ -> []

let rsa_jwk_of_json json =
  match (json_string "kty" json, json_string "n" json, json_string "e" json) with
  | Some "RSA", Some n64, Some e64 -> (
    match (b64url_decode n64, b64url_decode e64) with
    | Some n, Some e ->
      (match Mirage_crypto_pk.Rsa.pub ~n:(z_of_octets n) ~e:(z_of_octets e) with
      | Ok key -> Ok { kid = json_string "kid" json; alg = json_string "alg" json; key = `RSA key }
      | Error (`Msg msg) -> Error (Invalid_jwks msg))
    | _ -> Error (Invalid_jwks "RSA modulus/exponent are not base64url"))
  | Some _, _, _ -> Error (Invalid_jwks "only RSA keys are supported")
  | _ -> Error (Invalid_jwks "key requires kty, n, and e")

let jwks_of_string body =
  match Json.parse_opt body with
  | None -> Error (Invalid_jwks "malformed JSON")
  | Some json -> (
    match Option.bind (Json.member "keys" json) Json.to_list_opt with
    | None -> Error (Invalid_jwks "missing keys array")
    | Some keys ->
      let rec loop acc = function
        | [] -> if acc = [] then Error (Invalid_jwks "no usable signing keys") else Ok (List.rev acc)
        | key :: rest -> (
          match rsa_jwk_of_json key with
          | Ok key -> loop (key :: acc) rest
          | Error _ -> loop acc rest)
      in
      loop [] keys)

let email_domain email =
  match String.index_opt email '@' with
  | None -> None
  | Some i when i = String.length email - 1 -> None
  | Some i -> Some (String.sub email (i + 1) (String.length email - i - 1) |> lower_trim)

let domain_allowed connection claims email =
  match connection.domains with
  | [] -> true
  | domains ->
    let hd = Option.map lower_trim claims.hosted_domain in
    let email_domain = Option.bind email email_domain in
    List.exists (fun domain -> hd = Some domain || email_domain = Some domain) domains

let validate_claims ?(now = Unix.gettimeofday) ?(leeway = 60.) (connection : connection) (state : state) claims =
  let current = now () in
  if claims.issuer <> connection.issuer || state.issuer <> connection.issuer then Error (Claim_mismatch "issuer")
  else if state.connection_id <> connection.id then Error (Claim_mismatch "connection")
  else if state.client_id <> connection.client_id then Error (Claim_mismatch "client_id")
  else if state.redirect_uri <> connection.redirect_uri then Error (Claim_mismatch "redirect_uri")
  else if not (audience_contains connection.client_id claims.audience) then Error (Claim_mismatch "audience")
  else if claims.expires_at +. leeway < current then Error (Claim_mismatch "expired")
  else if option_exists (fun nbf -> nbf -. leeway > current) claims.not_before then Error (Claim_mismatch "not_before")
  else if option_exists (fun iat -> iat -. leeway > current) claims.issued_at then Error (Claim_mismatch "issued_at")
  else if claims.nonce <> Some state.nonce then Error (Claim_mismatch "nonce")
  else if not (domain_allowed connection claims claims.email) then Error (Claim_mismatch "domain")
  else
    match Identity.oidc ~issuer:connection.issuer ~connection:connection.id ~subject:claims.subject with
    | Error e -> Error (Identity_error e)
    | Ok identity -> (
      let email_verified = claims.email_verified = Some true in
      match claims.email with
      | Some email -> (
        match Identity.email ~verified:email_verified email with
        | Error e -> Error (Identity_error e)
        | Ok email_identity -> Ok { identity; email_identity = Some email_identity; email = Some (Identity.subject email_identity); email_verified; org_id = state.org_id; groups = claims.groups; claims })
      | None -> Ok { identity; email_identity = None; email = None; email_verified = false; org_id = state.org_id; groups = claims.groups; claims })

let claims_of_json json =
  match (json_string "iss" json, json_string "sub" json, json_number "exp" json) with
  | Some issuer, Some subject, Some expires_at ->
    let audience = json_string_list "aud" json in
    if audience = [] then Error (Invalid_id_token "missing audience")
    else
      Ok
        {
          issuer;
          subject;
          audience;
          expires_at;
          not_before = json_number "nbf" json;
          issued_at = json_number "iat" json;
          nonce = json_string "nonce" json;
          email = json_string "email" json;
          email_verified = json_bool "email_verified" json;
          hosted_domain = json_string "hd" json;
          tenant = json_string "tid" json;
          groups = json_string_list "groups" json;
        }
  | _ -> Error (Invalid_id_token "missing issuer, subject, or expiry")

let split_jwt token =
  match String.split_on_char '.' token with
  | [ header; payload; signature ] -> Ok (header, payload, signature)
  | _ -> Error (Invalid_id_token "JWT must have three sections")

let verify_jwt_signature jwks ~header64 ~payload64 ~signature64 header =
  match (json_string "alg" header, b64url_decode signature64) with
  | _, None -> Error (Invalid_id_token "signature is not base64url")
  | Some "RS256", Some signature ->
    let kid = json_string "kid" header in
    let signing_input = header64 ^ "." ^ payload64 in
    let keys =
      List.filter
        (fun key ->
          (match kid with None -> true | Some kid -> key.kid = Some kid)
          && Option.fold ~none:true ~some:(fun alg -> alg = "RS256") key.alg)
        jwks
    in
    if keys = [] then Error (Invalid_id_token "no matching signing key")
    else if
      List.exists
        (fun key ->
          Result.is_ok
            (X509.Public_key.verify `SHA256 ~scheme:`RSA_PKCS1 ~signature key.key
               (`Message signing_input)))
        keys
    then Ok ()
    else Error (Invalid_id_token "signature verification failed")
  | Some alg, _ -> Error (Invalid_id_token ("unsupported alg: " ^ alg))
  | None, _ -> Error (Invalid_id_token "missing alg")

let verify_id_token ?now ?leeway connection state jwks token =
  Result.bind (split_jwt token) (fun (header64, payload64, signature64) ->
      match (Option.bind (b64url_decode header64) Json.parse_opt, Option.bind (b64url_decode payload64) Json.parse_opt) with
      | Some header, Some payload ->
        Result.bind (verify_jwt_signature jwks ~header64 ~payload64 ~signature64 header) (fun () ->
            Result.bind (claims_of_json payload) (fun claims ->
                validate_claims ?now ?leeway connection state claims))
      | _ -> Error (Invalid_id_token "header or payload is not base64url JSON"))

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"oidc-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_connection () =
  ok
    (connection ~id:" Main " ~issuer:"https://idp.example" ~authorize_url:"https://idp.example/auth"
       ~client_id:"client" ~redirect_uri:"https://app.test/oidc/callback" ~scopes:[ "email"; " profile "; "" ]
       ~domains:[ "Example.COM" ] ~org_id:"org_1" ())

let query_of_url url =
  match String.index_opt url '?' with
  | None -> []
  | Some i -> H.parse_query (String.sub url (i + 1) (String.length url - i - 1))

let claims ?(issuer = "https://idp.example") ?(subject = "sub") ?(audience = [ "client" ]) ?(expires_at = 1_200.)
    ?not_before ?issued_at ?nonce ?email ?email_verified ?hosted_domain ?tenant ?(groups = []) () =
  { issuer; subject; audience; expires_at; not_before; issued_at; nonce; email; email_verified; hosted_domain; tenant; groups }

let%test "connection normalizes id, scopes, and domains" =
  let c = test_connection () in
  c.id = "main" && c.scopes = [ "openid"; "email"; "profile" ] && c.domains = [ "example.com" ]

let%test "google provider uses official OIDC issuer endpoint and account scopes" =
  match Providers.google ~client_id:"client" ~redirect_uri:"https://app.test/auth/google/callback" () with
  | Error _ -> false
  | Ok c ->
    c.id = "google"
    && c.issuer = "https://accounts.google.com"
    && c.authorize_url = "https://accounts.google.com/o/oauth2/v2/auth"
    && c.scopes = [ "openid"; "email"; "profile" ]

let%test "known oidc provider catalog covers common social and enterprise partners" =
  List.length Providers.all = 12
  && List.for_all
       (fun id ->
         match Providers.find id with
         | None -> false
         | Some p -> p.id = lower_trim id && p.issuer <> "" && p.authorize_url <> "" && p.default_scopes <> [])
       [
         "google";
         "microsoft_entra";
         "apple";
         "linkedin";
         "slack";
         "gitlab";
         "twitch";
         "salesforce";
         "dropbox";
         "auth0";
         "okta";
         "keycloak";
       ]

let%test "oidc provider presets build exact issuer connections" =
  let callback = "https://app.test/auth/callback" in
  let built =
    [
      Providers.apple ~client_id:"client" ~redirect_uri:callback ();
      Providers.linkedin ~client_id:"client" ~redirect_uri:callback ();
      Providers.slack ~client_id:"client" ~redirect_uri:callback ();
      Providers.gitlab ~client_id:"client" ~redirect_uri:callback ();
      Providers.twitch ~client_id:"client" ~redirect_uri:callback ();
      Providers.salesforce ~client_id:"client" ~redirect_uri:callback ();
      Providers.dropbox ~client_id:"client" ~redirect_uri:callback ();
      Providers.microsoft_entra ~tenant_id:"tenant-123" ~client_id:"client" ~redirect_uri:callback ();
      Providers.auth0 ~domain:"example.us.auth0.com" ~client_id:"client" ~redirect_uri:callback ();
      Providers.okta ~domain:"example.okta.com" ~client_id:"client" ~redirect_uri:callback ();
      Providers.keycloak ~base_url:"https://idp.example/auth/" ~realm:"main" ~client_id:"client"
        ~redirect_uri:callback ();
    ]
  in
  List.for_all Result.is_ok built
  &&
  match Providers.auth0 ~domain:"example.us.auth0.com" ~client_id:"client" ~redirect_uri:callback () with
  | Ok c -> c.issuer = "https://example.us.auth0.com/" && c.authorize_url = "https://example.us.auth0.com/authorize"
  | Error _ -> false

let%test "template oidc presets are discoverable but not directly runnable" =
  match Providers.find "okta" with
  | Some p -> Result.is_error (Providers.from_preset ~client_id:"client" ~redirect_uri:"https://app.test/cb" p)
  | None -> false

let%test "connection rejects blank fields and reserved params" =
  Result.is_error (connection ~id:"" ~issuer:"i" ~authorize_url:"u" ~client_id:"c" ~redirect_uri:"r" ())
  && Result.is_error
       (connection ~id:"x" ~issuer:"i" ~authorize_url:"u" ~client_id:"c" ~redirect_uri:"r" ~extra_params:[ ("nonce", "bad") ] ())

let%test "authorize builds oidc URL and stores state" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t ~user_id:"user_1" c with
  | Error _ -> false
  | Ok a ->
    let q = query_of_url a.url in
    List.assoc_opt "response_type" q = Some "code"
    && List.assoc_opt "client_id" q = Some "client"
    && List.assoc_opt "redirect_uri" q = Some c.redirect_uri
    && List.assoc_opt "state" q = Some (Challenge.token_to_string a.state)
    && List.assoc_opt "scope" q = Some "openid email profile"
    && List.assoc_opt "nonce" q = Some a.nonce
    && List.assoc_opt "code_challenge" q = Some a.pkce.challenge
    && a.record.metadata.connection_id = Some "main"

let%test "parse_callback delegates oauth callback parsing" =
  match parse_callback "code=abc&state=s" with
  | Ok (Code { code; state }) -> code = "abc" && Challenge.token_to_string state = "s"
  | _ -> false

let%test "consume_state is purpose-bound and single-use" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a -> (
    match consume_state t ~expected_connection:"main" a.state with
    | Error _ -> false
    | Ok state ->
      state.connection_id = "main"
      && state.code_verifier = a.pkce.verifier
      && state.nonce = a.nonce
      && consume_state t a.state = Error (Challenge_error Challenge.Already_consumed))

let%test "wrong connection does not consume state" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a -> consume_state t ~expected_connection:"other" a.state = Error Invalid_state && Result.is_ok (consume_state t ~expected_connection:"main" a.state)

let%test "expired state fails closed" =
  let t, set_now = test_service ~ttl:10. () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a ->
    set_now 1_011.;
    consume_state t a.state = Error (Challenge_error Challenge.Expired)

let%test "validate_claims derives oidc and verified email identities" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a -> (
    match consume_state t a.state with
    | Error _ -> false
    | Ok state -> (
      match validate_claims ~now:(fun () -> 1_000.) c state (claims ~nonce:a.nonce ~email:"Ada@Example.COM" ~email_verified:true ()) with
      | Error _ -> false
      | Ok p ->
        Identity.kind p.identity = Identity.Oidc
        && Identity.namespace p.identity = Some "https://idp.example\000main"
        && p.email = Some "ada@example.com"
        && option_exists Identity.is_verified_email p.email_identity))

let%test "validate_claims rejects issuer audience nonce and expiry mismatches" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a -> (
    match consume_state t a.state with
    | Error _ -> false
    | Ok state ->
      validate_claims ~now:(fun () -> 1_000.) c state (claims ~issuer:"https://other" ~nonce:a.nonce ()) = Error (Claim_mismatch "issuer")
      && validate_claims ~now:(fun () -> 1_000.) c state (claims ~audience:[ "other" ] ~nonce:a.nonce ()) = Error (Claim_mismatch "audience")
      && validate_claims ~now:(fun () -> 1_000.) c state (claims ~nonce:"wrong" ()) = Error (Claim_mismatch "nonce")
      && validate_claims ~now:(fun () -> 1_000.) c state (claims ~expires_at:900. ~nonce:a.nonce ()) = Error (Claim_mismatch "expired"))

let%test "validate_claims enforces domain policy" =
  let t, _ = test_service () in
  let c = test_connection () in
  match authorize t c with
  | Error _ -> false
  | Ok a -> (
    match consume_state t a.state with
    | Error _ -> false
    | Ok state ->
      validate_claims ~now:(fun () -> 1_000.) c state (claims ~nonce:a.nonce ~email:"ada@other.test" ~email_verified:true ())
      = Error (Claim_mismatch "domain"))

let z_octets (z : Z.t) =
  let len = max 1 ((Z.numbits z + 7) / 8) in
  String.init len (fun i ->
      Char.chr (Z.to_int (Z.logand (Z.shift_right z (8 * (len - 1 - i))) (Z.of_int 0xff))))

let test_jwks_and_key () =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let pub = match X509.Private_key.public key with `RSA pub -> pub | _ -> assert false in
  let jwks =
    Printf.sprintf
      {|{"keys":[{"kty":"RSA","kid":"main","alg":"RS256","n":"%s","e":"%s"}]}|}
      (b64url (z_octets pub.Mirage_crypto_pk.Rsa.n))
      (b64url (z_octets pub.Mirage_crypto_pk.Rsa.e))
  in
  (key, jwks)

let sign_jwt key ~nonce =
  let header = {|{"alg":"RS256","kid":"main","typ":"JWT"}|} in
  let payload =
    Printf.sprintf
      {|{"iss":"https://idp.example","sub":"sub","aud":"client","exp":1200,"nonce":"%s","email":"ada@example.com","email_verified":true}|}
      nonce
  in
  let h = b64url header and p = b64url payload in
  match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 key (`Message (h ^ "." ^ p)) with
  | Ok signature -> h ^ "." ^ p ^ "." ^ b64url signature
  | Error (`Msg msg) -> failwith msg

let%test "jwks_of_string parses RSA keys and verify_id_token accepts a signed token" =
  let key, jwks_body = test_jwks_and_key () in
  let c = test_connection () in
  let t, _ = test_service () in
  match (jwks_of_string jwks_body, authorize t c) with
  | Ok jwks, Ok issued -> (
    match consume_state t issued.state with
    | Error _ -> false
    | Ok state ->
      let token = sign_jwt key ~nonce:issued.nonce in
      (match verify_id_token ~now:(fun () -> 1_000.) c state jwks token with
      | Ok principal -> principal.email = Some "ada@example.com" && principal.email_verified
      | Error _ -> false)
      && Result.is_error (verify_id_token ~now:(fun () -> 1_000.) c state jwks (token ^ "x")))
  | _ -> false
