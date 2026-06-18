(* accounts_identity_bridge.ml — the bridge from each per-protocol feature to the engine's neutral
   [external_identity]: the [external_identity] constructor, the OAuth/OIDC/SAML/passkey/SCIM/email
   identity adapters, and the per-instance service factories (challenge/email/mfa) plus the
   [password_hasher] re-export. Carved verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets

let external_identity ?email ?(email_verified = false) ?username ?profile ?service key =
  {
    key;
    email = Option.map normalize_email (Option.bind email nonblank_opt);
    email_verified;
    username = Option.bind username nonblank_opt;
    profile;
    service =
      (match service with
      | Some (name, doc) -> Option.map (fun name -> (name, doc)) (nonblank_opt name)
      | None -> None);
  }

let identity_error e = Login_rejected (Identity.string_of_error e)
let email_error e = Login_rejected (Email.string_of_error e)

let email_identity ?username ?profile ?service address =
  let key =
    match Identity.email ~verified:true (Email.address_to_string address) with
    | Ok key -> key
    | Error e -> raise (Invalid_argument (Identity.string_of_error e))
  in
  external_identity key ~email:(Email.address_to_string address) ~email_verified:true ?username ?profile ?service

let oauth_identity ?email ?(email_verified = false) ?username ?profile ?service provider ~subject =
  match OAuth.identity provider ~subject with
  | Error (OAuth.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (OAuth.string_of_error e))
  | Ok key ->
    let service = Option.map (fun doc -> (provider.OAuth.name, doc)) service in
    Ok (external_identity key ?email ~email_verified ?username ?profile ?service)

let oidc_identity ?username ?profile ?service (principal : Oidc.principal) =
  let service = Option.map (fun doc -> ("oidc", doc)) service in
  external_identity principal.identity ?email:principal.email ~email_verified:principal.email_verified ?username
    ?profile ?service

let saml_identity ?username ?profile ?service (principal : Saml.principal) =
  let service = Option.map (fun doc -> ("saml", doc)) service in
  external_identity principal.identity ?email:principal.email ~email_verified:(Option.is_some principal.email_identity)
    ?username ?profile ?service

let passkey_identity ?service (assertion : Passkey.assertion) =
  match Passkey.identity assertion.credential with
  | Error (Passkey.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (Passkey.string_of_error e))
  | Ok key ->
    let service = Option.map (fun doc -> ("passkey", doc)) service in
    Ok (external_identity key ?service)

let scim_identity ?username ?profile ?service connection user =
  match Scim.identity connection user with
  | Error (Scim.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (Scim.string_of_error e))
  | Ok key ->
    let email = match user.Scim.emails with email :: _ -> Some email | [] -> None in
    let username = match username with Some _ -> username | None -> Some user.user_name in
    let service = Option.map (fun doc -> ("scim", doc)) service in
    Ok (external_identity key ?email ~email_verified:true ?username ?profile ?service)

let password_hasher = Password.password_hasher

let challenge_service t ?ttl () =
  let secret = t.secret ^ "\000accounts-challenge" in
  match ttl with
  | None -> Challenge.make ~secret ~store:t.store.challenges ()
  | Some ttl -> Challenge.make ~secret ~store:t.store.challenges ~ttl ()

let email_service t ?ttl () =
  let challenge = challenge_service t ?ttl () in
  Email.make ~secret:(t.secret ^ "\000accounts-email") ~challenge

(* per-instance MFA service (challenge-backed); shared by enrollment + step-up *)
let mfa_service t =
  Mfa.make ~secret:(t.secret ^ "\000accounts-mfa") ~challenge:(challenge_service t ())
