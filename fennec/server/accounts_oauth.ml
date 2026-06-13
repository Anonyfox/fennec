module Challenge = Accounts_challenge
module Identity = Accounts_identity
module H = Fennec_core.Http
module Bson = Bson

type provider = {
  name : string;
  authorize_url : string;
  client_id : string;
  redirect_uri : string;
  scopes : string list;
  extra_params : (string * string) list;
}

type preset = {
  id : string;
  display_name : string;
  authorize_url : string;
  default_scopes : string list;
}

type t = { challenge : Challenge.t }

type error =
  | Invalid_provider of string
  | Invalid_callback of string
  | Invalid_state
  | Challenge_error of Challenge.error
  | Identity_error of Identity.error

let string_of_error = function
  | Invalid_provider s -> "Invalid OAuth provider: " ^ s
  | Invalid_callback s -> "Invalid OAuth callback: " ^ s
  | Invalid_state -> "Invalid OAuth state"
  | Challenge_error e -> Challenge.string_of_error e
  | Identity_error e -> Identity.string_of_error e

let make ~challenge : t = { challenge }

let trim = String.trim
let lower_trim s = String.lowercase_ascii (trim s)
let clean_scopes scopes = scopes |> List.map trim |> List.filter (( <> ) "")
let clean_params params = params |> List.map (fun (k, v) -> (trim k, v)) |> List.filter (fun (k, _) -> k <> "")
let preset ~id ~display_name ~authorize_url ~default_scopes = { id; display_name; authorize_url; default_scopes }

let reserved_extra_param = function
  | "response_type" | "client_id" | "redirect_uri" | "state" | "code_challenge" | "code_challenge_method" | "scope" -> true
  | _ -> false

let provider ?(scopes = []) ?(extra_params = []) ~name ~authorize_url ~client_id ~redirect_uri () =
  let name = lower_trim name in
  let authorize_url = trim authorize_url in
  let client_id = trim client_id in
  let redirect_uri = trim redirect_uri in
  let extra_params = clean_params extra_params in
  if name = "" then Error (Invalid_provider "name cannot be blank")
  else if authorize_url = "" then Error (Invalid_provider "authorize_url cannot be blank")
  else if client_id = "" then Error (Invalid_provider "client_id cannot be blank")
  else if redirect_uri = "" then Error (Invalid_provider "redirect_uri cannot be blank")
  else
    match List.find_opt (fun (k, _) -> reserved_extra_param k) extra_params with
    | Some (k, _) -> Error (Invalid_provider ("extra param is reserved: " ^ k))
    | None ->
      Ok
        {
          name;
          authorize_url;
          client_id;
          redirect_uri;
          scopes = clean_scopes scopes;
          extra_params;
        }

type pkce = {
  verifier : string;
  challenge : string;
}

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts.OAuth: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let pkce () = let verifier = b64url (secure_random 32) in { verifier; challenge = b64url (sha256 verifier) }

let encode_query pairs =
  String.concat "&" (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) pairs)

let with_query base params =
  let sep = if String.contains base '?' then if String.ends_with ~suffix:"?" base || String.ends_with ~suffix:"&" base then "" else "&" else "?" in
  base ^ sep ^ encode_query params

let bson_string key value = (key, Bson.String value)
let metadata ~(provider : provider) ~(pkce : pkce) ?user_id ?org_id ?redirect () : Challenge.metadata =
  {
    user_id;
    email = None;
    org_id;
    connection_id = Some provider.name;
    redirect;
    data =
      [
        bson_string "provider" provider.name;
        bson_string "redirect_uri" provider.redirect_uri;
        bson_string "code_verifier" pkce.verifier;
        bson_string "client_id" provider.client_id;
      ]
  }

type authorization = {
  url : string;
  state : Challenge.token;
  pkce : pkce;
  record : Challenge.record;
  provider : provider;
}

let authorize (t : t) ?ttl ?user_id ?org_id ?redirect (provider : provider) =
  let pkce = pkce () in
  match Challenge.create t.challenge ~purpose:Challenge.OAuth_state ~metadata:(metadata ~provider ~pkce ?user_id ?org_id ?redirect ()) ?ttl () with
  | Error e -> Error (Challenge_error e)
  | Ok issued ->
    let params =
      [
        ("response_type", "code");
        ("client_id", provider.client_id);
        ("redirect_uri", provider.redirect_uri);
        ("state", Challenge.token_to_string issued.token);
        ("code_challenge", pkce.challenge);
        ("code_challenge_method", "S256");
      ]
      @ (match provider.scopes with [] -> [] | scopes -> [ ("scope", String.concat " " scopes) ])
      @ provider.extra_params
    in
    Ok { url = with_query provider.authorize_url params; state = issued.token; pkce; record = issued.record; provider }

type callback =
  | Code of { code : string; state : Challenge.token }
  | Callback_error of { error : string; description : string option; state : Challenge.token option }

let assoc_nonblank key pairs = match List.assoc_opt key pairs with Some v when trim v <> "" -> Some v | _ -> None

let parse_callback query =
  let pairs = H.parse_query query in
  match assoc_nonblank "error" pairs with
  | Some error ->
    let description = assoc_nonblank "error_description" pairs in
    let state = Option.map Challenge.token_of_string (assoc_nonblank "state" pairs) in
    Ok (Callback_error { error; description; state })
  | None -> (
    match (assoc_nonblank "code" pairs, assoc_nonblank "state" pairs) with
    | Some code, Some state -> Ok (Code { code; state = Challenge.token_of_string state })
    | None, _ -> Error (Invalid_callback "missing code")
    | _, None -> Error (Invalid_callback "missing state"))

type state = {
  provider : string;
  code_verifier : string;
  redirect_uri : string;
  user_id : string option;
  org_id : string option;
  redirect : string option;
  record : Challenge.record;
}

let data_string key data = match List.assoc_opt key data with Some (Bson.String v) -> Some v | _ -> None

let normalize_provider_for_match s = lower_trim s

let token_id token =
  let raw = Challenge.token_to_string token in
  match String.index_opt raw '.' with
  | None -> Error Invalid_state
  | Some 0 -> Error Invalid_state
  | Some i -> Ok (String.sub raw 0 i)

let state_of_record ?expected_provider record =
  let data = record.Challenge.metadata.data in
  match (data_string "provider" data, data_string "code_verifier" data, data_string "redirect_uri" data) with
  | Some provider, Some code_verifier, Some redirect_uri ->
    let provider = normalize_provider_for_match provider in
    let expected_ok =
      match expected_provider with
      | None -> true
      | Some expected -> provider = normalize_provider_for_match expected
    in
    if not expected_ok then Error Invalid_state
    else
      Ok
        {
          provider;
          code_verifier;
          redirect_uri;
          user_id = record.metadata.user_id;
          org_id = record.metadata.org_id;
          redirect = record.metadata.redirect;
          record;
        }
  | _ -> Error Invalid_state

let precheck_provider (t : t) ?expected_provider token =
  match expected_provider with
  | None -> Ok ()
  | Some _ -> (
    match token_id token with
    | Error _ as e -> e
    | Ok id -> (
      match Challenge.find t.challenge id with
      | Error e -> Error (Challenge_error e)
      | Ok None -> Error Invalid_state
      | Ok (Some record) -> state_of_record ?expected_provider record |> Result.map (fun _ -> ())))

let consume_state (t : t) ?expected_provider token =
  match precheck_provider t ?expected_provider token with
  | Error _ as e -> e
  | Ok () -> (
    match Challenge.consume t.challenge ~purpose:Challenge.OAuth_state token with
    | Error e -> Error (Challenge_error e)
    | Ok record -> state_of_record ?expected_provider record)

let identity provider ~subject =
  match Identity.oauth ~provider:provider.name ~subject with Ok key -> Ok key | Error e -> Error (Identity_error e)

module Providers = struct
  let github_preset =
    preset ~id:"github" ~display_name:"GitHub" ~authorize_url:"https://github.com/login/oauth/authorize"
      ~default_scopes:[ "read:user"; "user:email" ]

  let facebook_preset =
    preset ~id:"facebook" ~display_name:"Facebook" ~authorize_url:"https://www.facebook.com/v25.0/dialog/oauth"
      ~default_scopes:[ "email"; "public_profile" ]

  let discord_preset =
    preset ~id:"discord" ~display_name:"Discord" ~authorize_url:"https://discord.com/oauth2/authorize"
      ~default_scopes:[ "identify"; "email" ]

  let x_preset =
    preset ~id:"x" ~display_name:"X" ~authorize_url:"https://x.com/i/oauth2/authorize"
      ~default_scopes:[ "users.read"; "tweet.read" ]

  let spotify_preset =
    preset ~id:"spotify" ~display_name:"Spotify" ~authorize_url:"https://accounts.spotify.com/authorize"
      ~default_scopes:[ "user-read-email"; "user-read-private" ]

  let reddit_preset =
    preset ~id:"reddit" ~display_name:"Reddit" ~authorize_url:"https://www.reddit.com/api/v1/authorize"
      ~default_scopes:[ "identity" ]

  let amazon_preset =
    preset ~id:"amazon" ~display_name:"Amazon" ~authorize_url:"https://www.amazon.com/ap/oa"
      ~default_scopes:[ "profile" ]

  let bitbucket_preset =
    preset ~id:"bitbucket" ~display_name:"Bitbucket" ~authorize_url:"https://bitbucket.org/site/oauth2/authorize"
      ~default_scopes:[ "account"; "email" ]

  let all =
    [
      github_preset;
      facebook_preset;
      discord_preset;
      x_preset;
      spotify_preset;
      reddit_preset;
      amazon_preset;
      bitbucket_preset;
    ]

  let find id =
    let id = lower_trim id in
    List.find_opt (fun preset -> preset.id = id) all

  let from_preset ?scopes ?extra_params ~client_id ~redirect_uri p =
    let scopes = match scopes with Some scopes -> scopes | None -> p.default_scopes in
    provider ~name:p.id ~authorize_url:p.authorize_url ~client_id ~redirect_uri ~scopes ?extra_params ()

  let github ?(scopes = [ "read:user"; "user:email" ]) ?extra_params ~client_id ~redirect_uri () =
    from_preset ~scopes ?extra_params ~client_id ~redirect_uri github_preset

  let facebook ?(version = "v25.0") ?scopes ?extra_params ~client_id ~redirect_uri () =
    let authorize_url = "https://www.facebook.com/" ^ trim version ^ "/dialog/oauth" in
    let p = { facebook_preset with authorize_url } in
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri p

  let discord ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri discord_preset

  let x ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri x_preset

  let spotify ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri spotify_preset

  let extra_params_with defaults = function
    | None -> Some defaults
    | Some params -> Some (defaults @ params)

  let reddit ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params:(extra_params_with [ ("duration", "temporary") ] extra_params) ~client_id
      ~redirect_uri reddit_preset

  let amazon ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri amazon_preset

  let bitbucket ?scopes ?extra_params ~client_id ~redirect_uri () =
    from_preset ?scopes ?extra_params ~client_id ~redirect_uri bitbucket_preset
end

(* ---- inline tests ---- *)

let test_clock () =
  let t = ref 1_000. in
  ((fun () -> !t), fun x -> t := x)

let test_service ?(ttl = 60.) () =
  let now, set_now = test_clock () in
  let challenge =
    Challenge.make ~secret:"oauth-challenge-secret" ~store:(Challenge.memory_store ()) ~ttl ~now ()
  in
  (make ~challenge, set_now)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_provider () =
  ok
    (provider ~name:" GitHub " ~authorize_url:"https://github.com/login/oauth/authorize" ~client_id:"client"
       ~redirect_uri:"https://app.test/oauth/github/callback" ~scopes:[ " read:user "; ""; "user:email" ]
       ~extra_params:[ ("prompt", "consent") ] ())

let query_of_url url =
  match String.index_opt url '?' with
  | None -> []
  | Some i -> H.parse_query (String.sub url (i + 1) (String.length url - i - 1))

let%test "provider normalizes names and scopes" =
  let p = test_provider () in
  p.name = "github" && p.scopes = [ "read:user"; "user:email" ]

let%test "provider rejects blank required fields" =
  Result.is_error (provider ~name:" " ~authorize_url:"u" ~client_id:"c" ~redirect_uri:"r" ())
  && Result.is_error (provider ~name:"x" ~authorize_url:" " ~client_id:"c" ~redirect_uri:"r" ())

let%test "provider rejects reserved extra params" =
  Result.is_error (provider ~name:"x" ~authorize_url:"u" ~client_id:"c" ~redirect_uri:"r" ~extra_params:[ ("state", "bad") ] ())

let%test "github provider uses the official web authorization endpoint and account scopes" =
  match Providers.github ~client_id:"client" ~redirect_uri:"https://app.test/auth/github/callback" () with
  | Error _ -> false
  | Ok p ->
    p.name = "github"
    && p.authorize_url = "https://github.com/login/oauth/authorize"
    && p.scopes = [ "read:user"; "user:email" ]

let%test "known oauth provider catalog covers common social login partners" =
  List.length Providers.all = 8
  && List.for_all
       (fun id ->
         match Providers.find id with
         | None -> false
         | Some p -> p.id = lower_trim id && p.authorize_url <> "" && p.default_scopes <> [])
       [ "github"; "facebook"; "discord"; "x"; "spotify"; "reddit"; "amazon"; "bitbucket" ]

let%test "oauth provider presets build expected conservative defaults" =
  let callback = "https://app.test/auth/callback" in
  let built =
    [
      Providers.facebook ~client_id:"client" ~redirect_uri:callback ();
      Providers.discord ~client_id:"client" ~redirect_uri:callback ();
      Providers.x ~client_id:"client" ~redirect_uri:callback ();
      Providers.spotify ~client_id:"client" ~redirect_uri:callback ();
      Providers.reddit ~client_id:"client" ~redirect_uri:callback ();
      Providers.amazon ~client_id:"client" ~redirect_uri:callback ();
      Providers.bitbucket ~client_id:"client" ~redirect_uri:callback ();
    ]
  in
  List.for_all Result.is_ok built
  &&
  match Providers.reddit ~client_id:"client" ~redirect_uri:callback () with
  | Ok p -> List.assoc_opt "duration" p.extra_params = Some "temporary"
  | Error _ -> false

let%test "pkce verifier and challenge have OAuth-safe shape" =
  let p = pkce () in
  String.length p.verifier = 43
  && String.length p.challenge = 43
  && p.challenge = b64url (sha256 p.verifier)

let%test "authorize builds URL and stores state metadata" =
  let t, _ = test_service () in
  let p = test_provider () in
  match authorize t ~user_id:"user_1" ~org_id:"org_1" ~redirect:"/after" p with
  | Error _ -> false
  | Ok a ->
    let q = query_of_url a.url in
    List.assoc_opt "response_type" q = Some "code"
    && List.assoc_opt "client_id" q = Some "client"
    && List.assoc_opt "redirect_uri" q = Some p.redirect_uri
    && List.assoc_opt "state" q = Some (Challenge.token_to_string a.state)
    && List.assoc_opt "code_challenge" q = Some a.pkce.challenge
    && List.assoc_opt "code_challenge_method" q = Some "S256"
    && List.assoc_opt "scope" q = Some "read:user user:email"
    && a.record.metadata.connection_id = Some "github"

let%test "callback parser returns code and state" =
  match parse_callback "code=abc&state=state-token" with
  | Ok (Code { code; state }) -> code = "abc" && Challenge.token_to_string state = "state-token"
  | _ -> false

let%test "callback parser returns provider errors" =
  match parse_callback "error=access_denied&error_description=nope&state=s" with
  | Ok (Callback_error { error; description; state }) ->
    error = "access_denied" && description = Some "nope" && Option.map Challenge.token_to_string state = Some "s"
  | _ -> false

let%test "consume_state is purpose-bound and single-use" =
  let t, _ = test_service () in
  let p = test_provider () in
  match authorize t p with
  | Error _ -> false
  | Ok a -> (
    match consume_state t ~expected_provider:"github" a.state with
    | Error _ -> false
    | Ok state ->
      state.provider = "github"
      && state.code_verifier = a.pkce.verifier
      && state.redirect_uri = p.redirect_uri
      && consume_state t a.state = Error (Challenge_error Challenge.Already_consumed))

let%test "consume_state rejects expired state" =
  let t, set_now = test_service ~ttl:10. () in
  let p = test_provider () in
  match authorize t p with
  | Error _ -> false
  | Ok a ->
    set_now 1_011.;
    consume_state t a.state = Error (Challenge_error Challenge.Expired)

let%test "consume_state rejects wrong provider without consuming state" =
  let t, _ = test_service () in
  let p = test_provider () in
  match authorize t p with
  | Error _ -> false
  | Ok a ->
    consume_state t ~expected_provider:"google" a.state = Error Invalid_state
    && Result.is_ok (consume_state t ~expected_provider:"github" a.state)

let%test "identity delegates provider subject to Accounts.Identity" =
  let p = test_provider () in
  match identity p ~subject:"123" with
  | Ok key -> Identity.kind key = Identity.OAuth && Identity.namespace key = Some "github" && Identity.subject key = "123"
  | Error _ -> false
