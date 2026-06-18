(* accounts_provider_presets.ml — the config-level provider PRESETS: one-liner constructors that
   return the closed [external_identity provider] variant with the token-exchange recipe bundled.

   AUTH-SENSITIVE. These add ONLY (a) the provider's authorize/token/userinfo/JWKS endpoint config and
   (b) the HTTP call + response parsing. Every security primitive is REUSED from the feature modules and
   never reimplemented here:
     - PKCE verifier/challenge + the single-use [state] CSRF token  → {!Accounts_oauth} (authorize/consume_state)
     - OIDC [nonce] + RS256 ID-token signature + iss/aud/exp/nonce   → {!Accounts_oidc.verify_id_token}
     - SAML XML-signature + assertion validation                    → {!Accounts_saml.consume_response}
   The [*_callback_paw] route (mounted by {!Accounts_http.Wiring}) still owns consume_state BEFORE the
   exchange is called, so a bad/missing state is rejected before any HTTP fires. The exchange fails CLOSED
   on any HTTP / parse / verify error (→ the provider's error route). An email is treated as verified ONLY
   when the provider's own verification signal says so; an unverified email never becomes verified evidence.

   The default exchange runs the HTTP over the ambient {!Accounts_http_client} transport (Paw.Https_client,
   captured at boot — see that module), so [Accounts.OAuth.github ~client_id ~client_secret ()] is a true
   one-liner. A [?http] override lets tests drive a stub and lets an app supply a custom client. The
   explicit app-owned [~exchange] path stays available via [OAuth.custom] / [Oidc.from_connection] /
   [Saml.from_connection]. *)

open Accounts_types
open Accounts_identity_bridge
module Json = Fennec_mongo_json.Json
module Http = Accounts_http_client

(* ---- shared JSON / HTTP helpers ---- *)

let json_string key json = Option.bind (Json.member key json) Json.to_string_opt

(* a JSON field that may arrive as a string OR a number (provider user ids are sometimes integers) *)
let json_scalar key json =
  match Json.member key json with
  | Some (Json.String s) -> Some s
  | Some (Json.Number n) ->
    (* render an integral id without a trailing ".0"; keep precision for the rare fractional case *)
    if Float.is_integer n && Float.abs n < 1e15 then Some (Printf.sprintf "%.0f" n) else Some (string_of_float n)
  | _ -> None

let json_bool key json = match Json.member key json with Some (Json.Bool b) -> Some b | _ -> None

let ok_status s = s >= 200 && s < 300
let reject msg = Error (Login_rejected msg)

(* POST an x-www-form-urlencoded body and parse the JSON response, failing closed on any error. *)
let post_form ?(http = Http.transport ()) ~url params : (Json.t, error) result =
  let body = Http.form_encode params in
  match http ~meth:"POST" ~url ~headers:[ ("Content-Type", "application/x-www-form-urlencoded"); ("Accept", "application/json") ] ~body with
  | Error msg -> reject ("token endpoint unreachable: " ^ msg)
  | Ok resp when not (ok_status resp.Http.status) -> reject (Printf.sprintf "token endpoint returned HTTP %d" resp.Http.status)
  | Ok resp -> ( match Json.parse_opt resp.Http.body with Some j -> Ok j | None -> reject "token endpoint returned non-JSON")

(* GET a JSON resource (with optional bearer + extra headers), failing closed on any error. *)
let get_json ?(http = Http.transport ()) ?bearer ?(headers = []) ~url () : (Json.t, error) result =
  let headers = ("Accept", "application/json") :: headers in
  let headers = match bearer with Some tok -> ("Authorization", "Bearer " ^ tok) :: headers | None -> headers in
  match http ~meth:"GET" ~url ~headers ~body:"" with
  | Error msg -> reject ("profile endpoint unreachable: " ^ msg)
  | Ok resp when not (ok_status resp.Http.status) -> reject (Printf.sprintf "profile endpoint returned HTTP %d" resp.Http.status)
  | Ok resp -> ( match Json.parse_opt resp.Http.body with Some j -> Ok j | None -> reject "profile endpoint returned non-JSON")

(* GET a raw body (for JWKS, which Accounts_oidc.jwks_of_string parses itself). *)
let get_text ?(http = Http.transport ()) ~url () : (string, error) result =
  match http ~meth:"GET" ~url ~headers:[ ("Accept", "application/json") ] ~body:"" with
  | Error msg -> reject ("endpoint unreachable: " ^ msg)
  | Ok resp when not (ok_status resp.Http.status) -> reject (Printf.sprintf "endpoint returned HTTP %d" resp.Http.status)
  | Ok resp -> Ok resp.Http.body

(* ============================ OAuth presets ============================ *)

module OAuth = struct
  module P = Accounts_oauth

  (* an OAuth deployment: the typed provider value + the token/userinfo endpoints + the profile→identity
     mapping the default exchange needs. [profile] turns the userinfo JSON into (subject, email, verified). *)
  type recipe = {
    token_url : string;
    userinfo_url : string;
    user_agent : string option;
    profile : http:Http.transport -> access_token:string -> Json.t -> (string * string option * bool, error) result;
  }

  (* the default profile mapper: subject from "sub"/"id", email from "email", verified from
     "email_verified"/"verified_email" — and NEVER trusted unless the provider asserted it. *)
  let default_profile ~http:_ ~access_token:_ json =
    match json_scalar "sub" json |> function Some s -> Some s | None -> json_scalar "id" json with
    | None -> reject "userinfo missing subject"
    | Some subject ->
      let email = json_string "email" json in
      let verified = match json_bool "email_verified" json with Some b -> b | None -> ( match json_bool "verified_email" json with Some b -> b | None -> false) in
      Ok (subject, email, verified)

  (* the bundled code→identity exchange: POST token endpoint → access_token → GET userinfo → profile →
     {!oauth_identity}. Fails closed on every error. The OAuth.state was already consume_state-checked
     (CSRF) by the callback paw before we are called. *)
  let make_exchange (provider : P.provider) ~client_secret ~recipe ?http () : P.state -> code:string -> (external_identity, error) result =
   fun state ~code ->
    let http = match http with Some h -> h | None -> Http.transport () in
    let token_params =
      [
        ("grant_type", "authorization_code");
        ("code", code);
        ("redirect_uri", state.P.redirect_uri);
        ("client_id", provider.P.client_id);
        ("client_secret", client_secret);
        ("code_verifier", state.P.code_verifier);
      ]
    in
    match post_form ~http ~url:recipe.token_url token_params with
    | Error _ as e -> e
    | Ok token_json -> (
      match json_string "access_token" token_json with
      | None -> reject "token response missing access_token"
      | Some access_token -> (
        let headers = match recipe.user_agent with Some ua -> [ ("User-Agent", ua) ] | None -> [] in
        match get_json ~http ~bearer:access_token ~headers ~url:recipe.userinfo_url () with
        | Error _ as e -> e
        | Ok profile_json -> (
          match recipe.profile ~http ~access_token profile_json with
          | Error _ as e -> e
          | Ok (subject, email, email_verified) ->
            (* never pass an unverified email as verified evidence; oauth_identity normalizes it *)
            oauth_identity provider ~subject ?email ~email_verified)))

  (* build the provider variant from a recipe + a built OAuth.provider. The redirect_uri is supplied by
     the caller (the Wiring derives it from auth_prefix + id; tests pass an explicit one). *)
  let provider_of ?(link_verified_email = true) ?role_map ?http ~client_secret ~recipe ~success ~error (pr : P.provider) : external_identity provider =
    OAuth_provider
      {
        provider = pr;
        exchange = make_exchange pr ~client_secret ~recipe ?http ();
        link_verified_email;
        role_map;
        success;
        error;
      }

  (* default success/error redirect targets (overridable). Kept simple + root-relative so a zero-extra
     -config app gets sane behaviour; an app that wants different targets uses [custom]. *)
  let default_success = "/"
  let default_error = "/login"

  (* GitHub: the userinfo "email" is the PUBLIC profile email and may be null or unverified. The
     authoritative verified address comes from /user/emails (primary + verified). We therefore ignore
     the profile "email"/verified and resolve the verified primary explicitly. *)
  let github_recipe =
    {
      token_url = "https://github.com/login/oauth/access_token";
      userinfo_url = "https://api.github.com/user";
      user_agent = Some "fennec-accounts";
      profile =
        (fun ~http ~access_token json ->
          match json_scalar "id" json with
          | None -> reject "github user missing id"
          | Some subject -> (
            match get_json ~http ~bearer:access_token ~headers:[ ("User-Agent", "fennec-accounts") ] ~url:"https://api.github.com/user/emails" () with
            | Error _ ->
              (* no email scope granted: proceed with the subject, no verified email *)
              Ok (subject, None, false)
            | Ok emails_json -> (
              let entries = match Json.to_list_opt emails_json with Some l -> l | None -> [] in
              let primary_verified =
                List.find_opt (fun e -> json_bool "primary" e = Some true && json_bool "verified" e = Some true) entries
              in
              let any_verified = List.find_opt (fun e -> json_bool "verified" e = Some true) entries in
              match (primary_verified, any_verified) with
              | Some e, _ | None, Some e -> ( match json_string "email" e with Some addr -> Ok (subject, Some addr, true) | None -> Ok (subject, None, false))
              | None, None -> Ok (subject, None, false))));
    }

  let github ?(scopes = [ "read:user"; "user:email" ]) ?(success = default_success) ?(error = default_error)
      ?role_map ?http ~redirect_uri ~client_id ~client_secret () : (external_identity provider, P.error) result =
    match P.Providers.github ~scopes ~client_id ~redirect_uri () with
    | Error _ as e -> e
    | Ok pr -> Ok (provider_of ~link_verified_email:true ?role_map ?http ~client_secret ~recipe:github_recipe ~success ~error pr)

  (* Google OAuth2 (non-OIDC userinfo). Prefer Accounts.Oidc.google for full ID-token verification; this
     OAuth flavour reads the v3 userinfo endpoint, which reports "email_verified". *)
  let google_recipe =
    {
      token_url = "https://oauth2.googleapis.com/token";
      userinfo_url = "https://openidconnect.googleapis.com/v1/userinfo";
      user_agent = None;
      profile = default_profile;
    }

  let google ?(scopes = [ "openid"; "email"; "profile" ]) ?(success = default_success) ?(error = default_error)
      ?role_map ?http ~redirect_uri ~client_id ~client_secret () : (external_identity provider, P.error) result =
    match P.provider ~name:"google" ~authorize_url:"https://accounts.google.com/o/oauth2/v2/auth" ~client_id ~redirect_uri ~scopes () with
    | Error _ as e -> e
    | Ok pr -> Ok (provider_of ~link_verified_email:true ?role_map ?http ~client_secret ~recipe:google_recipe ~success ~error pr)

  (* the escape hatch: full control over the provider + the exchange (the original app-owned path). The
     exchange is whatever the app supplies; no preset endpoints are assumed. *)
  let custom ?(link_verified_email = true) ?role_map ?(success = default_success) ?(error = default_error)
      ~provider:(pr : P.provider) ~exchange () : external_identity provider =
    OAuth_provider { provider = pr; exchange; link_verified_email; role_map; success; error }
end

(* ============================ OIDC presets ============================ *)

module Oidc = struct
  module C = Accounts_oidc

  (* the default OIDC exchange: discover token_endpoint + jwks_uri from the (verified) issuer, POST the
     code for an id_token, fetch the JWKS, then REUSE {!Accounts_oidc.verify_id_token} — which verifies the
     RS256 signature and validates iss/aud/exp/nonce against the consumed state/connection. Nothing about
     the token is trusted before that verification. Fails closed on any error. [discovery_url] overrides the
     standard "<issuer>/.well-known/openid-configuration" (e.g. providers that host it elsewhere). *)
  let make_exchange (connection : C.connection) ~client_secret ?discovery_url ?http () : C.state -> code:string -> (C.principal, error) result =
   fun state ~code ->
    let http = match http with Some h -> h | None -> Http.transport () in
    let issuer = connection.C.issuer in
    let discovery_url = match discovery_url with Some u -> u | None -> (if String.length issuer > 0 && issuer.[String.length issuer - 1] = '/' then issuer ^ ".well-known/openid-configuration" else issuer ^ "/.well-known/openid-configuration") in
    let oidc_err e = Login_rejected (C.string_of_error e) in
    match get_json ~http ~url:discovery_url () with
    | Error _ as e -> e
    | Ok disco -> (
      match (json_string "token_endpoint" disco, json_string "jwks_uri" disco) with
      | None, _ -> reject "OIDC discovery missing token_endpoint"
      | _, None -> reject "OIDC discovery missing jwks_uri"
      | Some token_endpoint, Some jwks_uri -> (
        let token_params =
          [
            ("grant_type", "authorization_code");
            ("code", code);
            ("redirect_uri", state.C.redirect_uri);
            ("client_id", connection.C.client_id);
            ("client_secret", client_secret);
            ("code_verifier", state.C.code_verifier);
          ]
        in
        match post_form ~http ~url:token_endpoint token_params with
        | Error _ as e -> e
        | Ok token_json -> (
          match json_string "id_token" token_json with
          | None -> reject "token response missing id_token"
          | Some id_token -> (
            match get_text ~http ~url:jwks_uri () with
            | Error _ as e -> e
            | Ok jwks_body -> (
              match C.jwks_of_string jwks_body with
              | Error e -> Error (oidc_err e)
              | Ok jwks -> (
                (* the one verification that matters: RS256 signature + iss/aud/exp/nonce *)
                match C.verify_id_token connection state jwks id_token with
                | Error e -> Error (oidc_err e)
                | Ok principal -> Ok principal))))))

  let provider_of ?(link_verified_email = true) ?role_map ?discovery_url ?http ~client_secret ~success ~error (connection : C.connection) : external_identity provider =
    Oidc_provider
      {
        connection;
        exchange = make_exchange connection ~client_secret ?discovery_url ?http ();
        link_verified_email;
        role_map;
        success;
        error;
      }

  let default_success = "/"
  let default_error = "/login"

  (* one shared body for the static-issuer presets (google) that take only client_id/secret/redirect *)
  let of_connection_result ?role_map ?discovery_url ?http ~client_secret ~success ~error = function
    | Error _ as e -> e
    | Ok connection -> Ok (provider_of ?role_map ?discovery_url ?http ~client_secret ~success ~error connection)

  let google ?(success = default_success) ?(error = default_error) ?role_map ?http ~redirect_uri ~client_id ~client_secret () : (external_identity provider, C.error) result =
    of_connection_result ?role_map ?http ~client_secret ~success ~error (C.Providers.google ~client_id ~redirect_uri ())

  let microsoft ?(success = default_success) ?(error = default_error) ?role_map ?http ~tenant_id ~redirect_uri ~client_id ~client_secret () : (external_identity provider, C.error) result =
    of_connection_result ?role_map ?http ~client_secret ~success ~error (C.Providers.microsoft_entra ~tenant_id ~client_id ~redirect_uri ())

  let okta ?(success = default_success) ?(error = default_error) ?authorization_server_id ?role_map ?http ~domain ~redirect_uri ~client_id ~client_secret () : (external_identity provider, C.error) result =
    of_connection_result ?role_map ?http ~client_secret ~success ~error (C.Providers.okta ?authorization_server_id ~domain ~client_id ~redirect_uri ())

  let auth0 ?(success = default_success) ?(error = default_error) ?role_map ?http ~domain ~redirect_uri ~client_id ~client_secret () : (external_identity provider, C.error) result =
    of_connection_result ?role_map ?http ~client_secret ~success ~error (C.Providers.auth0 ~domain ~client_id ~redirect_uri ())

  let keycloak ?(success = default_success) ?(error = default_error) ?role_map ?http ~base_url ~realm ~redirect_uri ~client_id ~client_secret () : (external_identity provider, C.error) result =
    of_connection_result ?role_map ?http ~client_secret ~success ~error (C.Providers.keycloak ~base_url ~realm ~client_id ~redirect_uri ())

  (* the escape hatch: a pre-built connection + either the default exchange (give client_secret) or a
     fully app-owned exchange. When [exchange] is supplied it is used verbatim (the original "each
     deployment decides" path); otherwise the default discovery+verify exchange is built (requires
     [client_secret]). Named [from_connection] so it does not shadow {!Accounts_oidc.connection} (the
     connection builder) on the facade's [Accounts.Oidc] module. *)
  let from_connection ?(link_verified_email = true) ?role_map ?discovery_url ?http ?(success = default_success) ?(error = default_error) ?client_secret ?exchange ~connection:(c : C.connection) () : external_identity provider =
    let exchange =
      match exchange with
      | Some ex -> ex
      | None -> make_exchange c ~client_secret:(Option.value client_secret ~default:"") ?discovery_url ?http ()
    in
    Oidc_provider { connection = c; exchange; link_verified_email; role_map; success; error }
end

(* ============================ SAML presets ============================ *)

module Saml = struct
  module C = Accounts_saml

  let default_success = "/"
  let default_error = "/login"

  let provider_of ?role_map ?signing_key ~trusted_keys ~success ~error (connection : C.connection) : external_identity provider =
    Saml_provider { connection; trusted_keys; signing_key; role_map; success; error }

  (* Okta SAML: a thin connection helper. SAML has no token-exchange HTTP — the IdP POSTs a signed
     assertion to the ACS route, verified by {!Accounts_saml.consume_response} against [trusted_keys]. *)
  let okta ?(success = default_success) ?(error = default_error) ?role_map ?signing_key ?org_id ?domains ?trust_email ~id ~issuer ~sso_url ~entity_id ~acs_url ~trusted_keys () : (external_identity provider, C.error) result =
    match C.connection ?org_id ?domains ?trust_email ~id ~issuer ~sso_url ~entity_id ~acs_url () with
    | Error _ as e -> e
    | Ok connection -> Ok (provider_of ?role_map ?signing_key ~trusted_keys ~success ~error connection)

  (* the escape hatch: a pre-built connection + trusted keys. Named [from_connection] so it does not
     shadow {!Accounts_saml.connection} (the connection builder) on the facade's [Accounts.Saml]. *)
  let from_connection ?(success = default_success) ?(error = default_error) ?role_map ?signing_key ~connection:(c : C.connection) ~trusted_keys () : external_identity provider =
    provider_of ?role_map ?signing_key ~trusted_keys ~success ~error c
end

(* ============================ inline tests ============================

   The presets are auth-sensitive, so each exchange is driven through a STUB transport returning canned
   token / userinfo / JWKS responses and asserted on its SECURITY properties:
     (a) a well-formed response yields the right external_identity / principal (+ roles via ?role_map);
     (b) a bad / missing state is rejected (the callback paw's consume_state gate, before any HTTP);
     (c) a bad ID-token signature / wrong aud / expired / wrong nonce is rejected (OIDC);
     (d) an unverified email is NOT treated as verified;
   plus a differential check that the preset's exchange produces the SAME identity the explicit-~exchange
   path does for the same inputs. The crypto is the feature modules' — these tests prove the WIRING. *)

module Identity = Accounts_identity

(* a stub transport: a routing function from (meth, url) to a canned (status, body). Unmatched routes
   return a 404 so an exchange that calls an unexpected endpoint fails closed (and the test notices). *)
let stub_transport (routes : (string * string * int * string) list) : Http.transport =
 fun ~meth ~url ~headers:_ ~body:_ ->
  match List.find_opt (fun (m, u, _, _) -> m = meth && u = url) routes with
  | Some (_, _, status, body) -> Ok { Http.status; headers = []; body }
  | None -> Ok { Http.status = 404; headers = []; body = "" }

let ok = function Ok x -> x | Error _ -> failwith "expected Ok"

(* ---- OAuth: GitHub preset ---- *)

let oauth_exchange_of = function OAuth_provider { exchange; _ } -> exchange | _ -> failwith "expected OAuth_provider"
let oauth_provider_of = function OAuth_provider { provider; _ } -> provider | _ -> failwith "expected OAuth_provider"
let oauth_role_map_of = function OAuth_provider { role_map; _ } -> role_map | _ -> failwith "expected OAuth_provider"

(* run a github preset's exchange end to end against a stub, returning the resolved external_identity *)
let run_github_exchange ?(emails_status = 200) ?(emails_body = {|[{"email":"ada@example.com","primary":true,"verified":true}]|}) ?role_map () =
  let challenge = Accounts_challenge.make ~secret:"preset-test-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oauth = Accounts_oauth.make ~challenge in
  let redirect_uri = "https://app.test/auth/github/callback" in
  let stub =
    stub_transport
      [
        ("POST", "https://github.com/login/oauth/access_token", 200, {|{"access_token":"gho_token","token_type":"bearer"}|});
        ("GET", "https://api.github.com/user", 200, {|{"id":4242,"login":"ada"}|});
        ("GET", "https://api.github.com/user/emails", emails_status, emails_body);
      ]
  in
  let prov = ok (OAuth.github ?role_map ~http:stub ~redirect_uri ~client_id:"cid" ~client_secret:"secret" ()) in
  let pr = oauth_provider_of prov in
  let exchange = oauth_exchange_of prov in
  let auth = ok (Accounts_oauth.authorize oauth pr) in
  let state = ok (Accounts_oauth.consume_state oauth ~expected_provider:"github" auth.Accounts_oauth.state) in
  (prov, exchange state ~code:"the-code")

let%test "OAuth github preset: well-formed token+emails yields the github identity with a verified email" =
  let _, result = run_github_exchange () in
  match result with
  | Ok facts ->
    Identity.kind facts.key = Identity.OAuth
    && Identity.namespace facts.key = Some "github"
    && Identity.subject facts.key = "4242"
    && facts.email = Some "ada@example.com"
    && facts.email_verified = true
  | Error _ -> false

let%test "OAuth github preset: differential — the preset's facts match an explicit oauth_identity build" =
  let _, result = run_github_exchange () in
  match result with
  | Ok facts ->
    let provider = ok (Accounts_oauth.provider ~name:"github" ~authorize_url:"https://github.com/login/oauth/authorize" ~client_id:"cid" ~redirect_uri:"https://app.test/auth/github/callback" ()) in
    let explicit = ok (oauth_identity provider ~subject:"4242" ~email:"ada@example.com" ~email_verified:true) in
    (* same canonical identity key + same verified-email evidence as the hand-written exchange *)
    Identity.subject facts.key = Identity.subject explicit.key
    && Identity.namespace facts.key = Identity.namespace explicit.key
    && Identity.kind facts.key = Identity.kind explicit.key
    && facts.email = explicit.email
    && facts.email_verified = explicit.email_verified
  | Error _ -> false

let%test "OAuth github preset: an UNVERIFIED primary email is NOT trusted as verified" =
  (* the /user/emails entry is primary but verified:false → no verified email evidence *)
  let _, result = run_github_exchange ~emails_body:{|[{"email":"spoof@example.com","primary":true,"verified":false}]|} () in
  match result with
  | Ok facts -> facts.email_verified = false && facts.email = None
  | Error _ -> false

let%test "OAuth github preset: missing email scope (emails 404) still logs in, with no verified email" =
  let _, result = run_github_exchange ~emails_status:404 ~emails_body:"" () in
  match result with
  | Ok facts -> Identity.subject facts.key = "4242" && facts.email = None && facts.email_verified = false
  | Error _ -> false

let%test "OAuth github preset: a token endpoint with NO access_token fails closed" =
  let challenge = Accounts_challenge.make ~secret:"preset-test-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oauth = Accounts_oauth.make ~challenge in
  let stub = stub_transport [ ("POST", "https://github.com/login/oauth/access_token", 200, {|{"error":"bad_verification_code"}|}) ] in
  let prov = ok (OAuth.github ~http:stub ~redirect_uri:"https://app.test/auth/github/callback" ~client_id:"cid" ~client_secret:"secret" ()) in
  let exchange = oauth_exchange_of prov in
  let auth = ok (Accounts_oauth.authorize oauth (oauth_provider_of prov)) in
  let state = ok (Accounts_oauth.consume_state oauth ~expected_provider:"github" auth.Accounts_oauth.state) in
  Result.is_error (exchange state ~code:"the-code")

let%test "OAuth github preset: a bad/forged state is rejected by consume_state BEFORE the exchange runs" =
  (* the callback paw gate: consume_state must reject an unknown token; the exchange is never reached *)
  let challenge = Accounts_challenge.make ~secret:"preset-test-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oauth = Accounts_oauth.make ~challenge in
  Result.is_error (Accounts_oauth.consume_state oauth ~expected_provider:"github" (Accounts_challenge.token_of_string "forged.token"))

let%test "OAuth github preset: ?role_map maps the resolved identity to role strings" =
  let role_map (_ : external_identity) = [ "member"; "early-adopter" ] in
  let prov, _ = run_github_exchange ~role_map () in
  match oauth_role_map_of prov with
  | Some f -> f (external_identity (ok (Identity.oauth ~provider:"github" ~subject:"4242"))) = [ "member"; "early-adopter" ]
  | None -> false

(* ---- OIDC: from_connection + discovery + ID-token verification ---- *)

let b64url s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s

let z_octets (z : Z.t) =
  let len = max 1 ((Z.numbits z + 7) / 8) in
  String.init len (fun i -> Char.chr (Z.to_int (Z.logand (Z.shift_right z (8 * (len - 1 - i))) (Z.of_int 0xff))))

let oidc_test_key_and_jwks () =
  Mirage_crypto_rng_unix.use_default ();
  let key = X509.Private_key.generate ~bits:2048 `RSA in
  let pub = match X509.Private_key.public key with `RSA pub -> pub | _ -> assert false in
  let jwks =
    Printf.sprintf {|{"keys":[{"kty":"RSA","kid":"k1","alg":"RS256","n":"%s","e":"%s"}]}|}
      (b64url (z_octets pub.Mirage_crypto_pk.Rsa.n)) (b64url (z_octets pub.Mirage_crypto_pk.Rsa.e))
  in
  (key, jwks)

let sign_id_token key ~issuer ~aud ~exp ~nonce ~email ~email_verified =
  let header = {|{"alg":"RS256","kid":"k1","typ":"JWT"}|} in
  let payload =
    Printf.sprintf {|{"iss":"%s","sub":"oidc-sub","aud":"%s","exp":%d,"nonce":"%s","email":"%s","email_verified":%b}|}
      issuer aud exp nonce email email_verified
  in
  let h = b64url header and p = b64url payload in
  match X509.Private_key.sign `SHA256 ~scheme:`RSA_PKCS1 key (`Message (h ^ "." ^ p)) with
  | Ok sg -> h ^ "." ^ p ^ "." ^ b64url sg
  | Error (`Msg m) -> failwith m

let oidc_exchange_of = function Oidc_provider { exchange; _ } -> exchange | _ -> failwith "expected Oidc_provider"

let oidc_test_connection () =
  ok (Accounts_oidc.connection ~id:"idp" ~issuer:"https://idp.test" ~authorize_url:"https://idp.test/auth" ~client_id:"client" ~redirect_uri:"https://app.test/auth/idp/callback" ())

(* drive an OIDC preset's exchange against a stub: discovery → token (id_token) → JWKS → verify *)
let run_oidc_exchange ?(issuer = "https://idp.test") ?(aud = "client") ?(exp = 9_999_999_999) ?nonce ?(email = "ada@example.com") ?(email_verified = true) ?(tamper = false) ?role_map () =
  let key, jwks = oidc_test_key_and_jwks () in
  let challenge = Accounts_challenge.make ~secret:"preset-oidc-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oidc = Accounts_oidc.make ~challenge in
  let connection = oidc_test_connection () in
  let auth = ok (Accounts_oidc.authorize oidc connection) in
  let state = ok (Accounts_oidc.consume_state oidc ~expected_connection:"idp" auth.Accounts_oidc.state) in
  let nonce = match nonce with Some n -> n | None -> auth.Accounts_oidc.nonce in
  let id_token = sign_id_token key ~issuer ~aud ~exp ~nonce ~email ~email_verified in
  let id_token = if tamper then id_token ^ "x" else id_token in
  let stub =
    stub_transport
      [
        ("GET", "https://idp.test/.well-known/openid-configuration", 200, {|{"token_endpoint":"https://idp.test/token","jwks_uri":"https://idp.test/jwks"}|});
        ("POST", "https://idp.test/token", 200, Printf.sprintf {|{"id_token":"%s","token_type":"bearer"}|} id_token);
        ("GET", "https://idp.test/jwks", 200, jwks);
      ]
  in
  let prov = Oidc.from_connection ?role_map ~http:stub ~client_secret:"secret" ~connection () in
  (prov, (oidc_exchange_of prov) state ~code:"the-code")

let%test "OIDC preset: discovery + signed ID-token yields a verified principal" =
  let _, result = run_oidc_exchange () in
  match result with
  | Ok (p : Accounts_oidc.principal) ->
    Identity.kind p.identity = Identity.Oidc && p.email = Some "ada@example.com" && p.email_verified = true
  | Error _ -> false

let%test "OIDC preset: differential — preset principal matches a direct verify_id_token of the same token" =
  (* build the same signed token + JWKS, verify it directly, and compare the resolved identity/email *)
  let key, jwks_body = oidc_test_key_and_jwks () in
  let challenge = Accounts_challenge.make ~secret:"preset-oidc-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oidc = Accounts_oidc.make ~challenge in
  let connection = oidc_test_connection () in
  let auth = ok (Accounts_oidc.authorize oidc connection) in
  let state = ok (Accounts_oidc.consume_state oidc ~expected_connection:"idp" auth.Accounts_oidc.state) in
  let id_token = sign_id_token key ~issuer:"https://idp.test" ~aud:"client" ~exp:9_999_999_999 ~nonce:auth.Accounts_oidc.nonce ~email:"ada@example.com" ~email_verified:true in
  let jwks = ok (Accounts_oidc.jwks_of_string jwks_body) in
  let direct = ok (Accounts_oidc.verify_id_token connection state jwks id_token) in
  let stub =
    stub_transport
      [
        ("GET", "https://idp.test/.well-known/openid-configuration", 200, {|{"token_endpoint":"https://idp.test/token","jwks_uri":"https://idp.test/jwks"}|});
        ("POST", "https://idp.test/token", 200, Printf.sprintf {|{"id_token":"%s"}|} id_token);
        ("GET", "https://idp.test/jwks", 200, jwks_body);
      ]
  in
  let prov = Oidc.from_connection ~http:stub ~client_secret:"secret" ~connection () in
  match (oidc_exchange_of prov) state ~code:"c" with
  | Ok (p : Accounts_oidc.principal) -> Identity.subject p.identity = Identity.subject direct.identity && p.email = direct.email && p.email_verified = direct.email_verified
  | Error _ -> false

let%test "OIDC preset: a TAMPERED ID-token signature is rejected" =
  Result.is_error (snd (run_oidc_exchange ~tamper:true ()))

let%test "OIDC preset: a wrong audience is rejected" =
  Result.is_error (snd (run_oidc_exchange ~aud:"someone-else" ()))

let%test "OIDC preset: an expired ID-token is rejected" =
  Result.is_error (snd (run_oidc_exchange ~exp:1 ()))

let%test "OIDC preset: a wrong nonce is rejected (replay / fixation defense)" =
  Result.is_error (snd (run_oidc_exchange ~nonce:"not-the-issued-nonce" ()))

let%test "OIDC preset: an unverified email is carried but NOT marked verified" =
  match snd (run_oidc_exchange ~email_verified:false ()) with
  | Ok (p : Accounts_oidc.principal) -> p.email_verified = false
  | Error _ -> false

let%test "OIDC preset: discovery missing jwks_uri fails closed (no JWKS ⇒ no trust)" =
  let challenge = Accounts_challenge.make ~secret:"preset-oidc-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oidc = Accounts_oidc.make ~challenge in
  let connection = oidc_test_connection () in
  let auth = ok (Accounts_oidc.authorize oidc connection) in
  let state = ok (Accounts_oidc.consume_state oidc ~expected_connection:"idp" auth.Accounts_oidc.state) in
  let stub = stub_transport [ ("GET", "https://idp.test/.well-known/openid-configuration", 200, {|{"token_endpoint":"https://idp.test/token"}|}) ] in
  let prov = Oidc.from_connection ~http:stub ~client_secret:"secret" ~connection () in
  Result.is_error ((oidc_exchange_of prov) state ~code:"c")

let%test "OIDC preset constructors build exact-issuer connections (google/microsoft/okta/auth0/keycloak)" =
  let cb = "https://app.test/auth/cb" in
  List.for_all Result.is_ok
    [
      (Oidc.google ~redirect_uri:cb ~client_id:"c" ~client_secret:"s" () |> Result.map (fun _ -> ()));
      (Oidc.microsoft ~tenant_id:"tid" ~redirect_uri:cb ~client_id:"c" ~client_secret:"s" () |> Result.map (fun _ -> ()));
      (Oidc.okta ~domain:"example.okta.com" ~redirect_uri:cb ~client_id:"c" ~client_secret:"s" () |> Result.map (fun _ -> ()));
      (Oidc.auth0 ~domain:"example.us.auth0.com" ~redirect_uri:cb ~client_id:"c" ~client_secret:"s" () |> Result.map (fun _ -> ()));
      (Oidc.keycloak ~base_url:"https://idp.example/auth" ~realm:"main" ~redirect_uri:cb ~client_id:"c" ~client_secret:"s" () |> Result.map (fun _ -> ()));
    ]

(* ---- SAML: from_connection / okta (config only, no HTTP) ---- *)

let%test "SAML okta preset builds a Saml_provider carrying the connection + trusted keys" =
  match
    Saml.okta ~id:"okta" ~issuer:"https://sp.test" ~sso_url:"https://idp.test/sso" ~entity_id:"https://sp.test" ~acs_url:"https://app.test/auth/okta/callback" ~trusted_keys:[] ()
  with
  | Ok (Saml_provider { connection; trusted_keys; _ }) -> connection.Accounts_saml.id = "okta" && trusted_keys = []
  | _ -> false

let%test "SAML from_connection preset carries a role_map through to the variant" =
  let connection = ok (Accounts_saml.connection ~id:"corp" ~issuer:"https://sp.test" ~sso_url:"https://idp.test/sso" ~entity_id:"https://sp.test" ~acs_url:"https://app.test/auth/corp/callback" ()) in
  match Saml.from_connection ~role_map:(fun (_ : Accounts_saml.principal) -> [ "staff" ]) ~connection ~trusted_keys:[] () with
  | Saml_provider { role_map = Some f; connection = c; _ } ->
    c.Accounts_saml.id = "corp"
    && f { Accounts_saml.identity = ok (Identity.oauth ~provider:"x" ~subject:"y"); email_identity = None; email = None; allow_jit = true; org_id = None; session_index = None; signature_key_fingerprint = None; attributes = []; assertion = { issuer = ""; audience = ""; recipient = ""; destination = None; in_response_to = None; not_before = None; not_on_or_after = None; name_id = ""; name_id_format = None; external_id = None; email = None; attributes = []; session_index = None } } = [ "staff" ]
  | _ -> false

(* ---- the ambient transport: fail-closed when unconfigured ---- *)

let%test "ambient transport is fail-closed until set_transport installs one" =
  (* a fresh github preset with NO ?http uses the ambient transport; with none installed it errors *)
  Http.set_transport (fun ~meth:_ ~url:_ ~headers:_ ~body:_ -> Error "no transport");
  let challenge = Accounts_challenge.make ~secret:"preset-test-secret-0123456789" ~store:(Accounts_challenge.memory_store ()) () in
  let oauth = Accounts_oauth.make ~challenge in
  let prov = ok (OAuth.github ~redirect_uri:"https://app.test/auth/github/callback" ~client_id:"cid" ~client_secret:"secret" ()) in
  let auth = ok (Accounts_oauth.authorize oauth (oauth_provider_of prov)) in
  let state = ok (Accounts_oauth.consume_state oauth ~expected_provider:"github" auth.Accounts_oauth.state) in
  Result.is_error ((oauth_exchange_of prov) state ~code:"c")
