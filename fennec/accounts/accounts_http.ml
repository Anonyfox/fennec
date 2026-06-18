(* accounts_http.ml — the HTTP/route surface: the *_paw route constructors (password reset,
   enrollment, email verification, magic-link + OTP login, MFA TOTP/backup-code, passkey
   register/assert, SCIM, OAuth/OIDC/SAML authorize+callback), the SCIM provisioning glue that
   drives create_user/attach/upsert from a SCIM sync (engine-side: it orchestrates the store, so
   it cannot move into the pure Accounts_scim feature without inverting the dependency), the JSON
   response helpers, and the BSON document accessors. Sits on top of every engine layer. Carved
   verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets
open Accounts_identity_bridge
open Accounts_request
open Accounts_lifecycle
open Accounts_login

let send_failed c exn = Conn.text ~status:500 c ("Accounts delivery failed: " ^ Printexc.to_string exn)

let password_reset_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.Route.post path (fun c ->
      match Conn.param c email_param with
      | None -> Conn.redirect c error
      | Some email -> (
        match issue_password_reset t email with
        | Error _ -> Conn.redirect c error
        | Ok None -> Conn.redirect c success
        | Ok (Some reset) -> (
          match send reset with
          | () -> Conn.redirect c success
          | exception exn -> send_failed c exn)))

let append_query url params =
  let query =
    String.concat "&" (List.map (fun (key, value) -> H.percent_encode key ^ "=" ^ H.percent_encode value) params)
  in
  if query = "" then url
  else
    let sep = if String.contains url '?' then "&" else "?" in
    url ^ sep ^ query

let mfa_redirect target (step_up : login_step_up) =
  append_query target
    [
      ("mfaToken", Challenge.token_to_string step_up.step_up.token);
      ("userId", step_up.user.id);
    ]

let redirect_step_up c ?mfa_required ~error step_up =
  let target = Option.value ~default:error mfa_required in
  Conn.redirect c (mfa_redirect target step_up)

let redirect_login_completion t c ?mfa_required ~success ~error = function
  | Error _ -> Conn.redirect c error
  | Ok (Complete_login (_, session)) -> Conn.redirect (set_login_cookie t c session) success
  | Ok (Step_up_required step_up) -> redirect_step_up c ?mfa_required ~error step_up

let redirect_identity_completion t c ?mfa_required ~success ~error = function
  | Error _ -> Conn.redirect c error
  | Ok (Complete_identity_login login) -> Conn.redirect (set_login_cookie t c login.token) success
  | Ok (Identity_step_up_required step_up) -> redirect_step_up c ?mfa_required ~error step_up

let password_reset_paw t ?(token_param = "token") ?(password_param = "password") ?mfa_required ~path
    ~success ~error () =
  Paw.Route.post path (fun c ->
      match (Conn.param c token_param, Conn.param c password_param) with
      | Some token, Some password -> (
        match reset_password_completion t (Challenge.token_of_string token) ~password with
        | result -> redirect_login_completion t c ?mfa_required ~success ~error result)
      | _ -> Conn.redirect c error)

let enrollment_paw t ?(token_param = "token") ?(password_param = "password") ?mfa_required ~path ~success
    ~error () =
  Paw.Route.post path (fun c ->
      match (Conn.param c token_param, Conn.param c password_param) with
      | Some token, Some password ->
        redirect_login_completion t c ?mfa_required ~success ~error
          (enroll_account_completion t (Challenge.token_of_string token) ~password)
      | _ -> Conn.redirect c error)

let email_verification_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.Route.post path (fun c ->
      match (user_id c, Conn.param c email_param) with
      | Some uid, Some email -> (
        match issue_email_verification t uid email with
        | Error _ -> Conn.redirect c error
        | Ok issued -> (
          match send issued with
          | () -> Conn.redirect c success
          | exception exn -> send_failed c exn))
      | _ -> Conn.redirect c error)

let email_verification_paw t ?(token_param = "token") ?mfa_required ~path ~success ~error () =
  Paw.Route.get path (fun c ->
      match Conn.param c token_param with
      | None -> Conn.redirect c error
      | Some token -> (
        match verify_email_completion t (Challenge.token_of_string token) with
        | result -> redirect_login_completion t c ?mfa_required ~success ~error result))

let email_login_link_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.Route.post path (fun c ->
      match Conn.param c email_param with
      | None -> Conn.redirect c error
      | Some raw_email -> (
        match Email.normalize raw_email with
        | Error _ -> Conn.redirect c error
        | Ok address ->
          let email = email_service t () in
          match Email.issue_login_link email (Email.binding address) with
          | Error _ -> Conn.redirect c error
          | Ok issued -> (
            match send issued with
            | () -> Conn.redirect c success
            | exception exn -> send_failed c exn)))

let email_login_link_paw t ?(token_param = "token") ?allow_signup ?link_verified_email ?mfa_required ~path
    ~success ~error () =
  Paw.Route.get path (fun c ->
      let email = email_service t () in
      match Conn.param c token_param with
      | None -> Conn.redirect c error
      | Some token -> (
        redirect_identity_completion t c ?mfa_required ~success ~error
          (login_with_email_link_completion t email ?current_user_id:(user_id c) ?allow_signup
             ?link_verified_email (Challenge.token_of_string token))))

let email_otp_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.Route.post path (fun c ->
      match Conn.param c email_param with
      | None -> Conn.redirect c error
      | Some raw_email -> (
        match Email.normalize raw_email with
        | Error _ -> Conn.redirect c error
        | Ok address ->
          let email = email_service t () in
          match Email.issue_otp email (Email.binding address) with
          | Error _ -> Conn.redirect c error
          | Ok issued -> (
            match send issued with
            | () -> Conn.redirect c success
            | exception exn -> send_failed c exn)))

let email_otp_paw t ?(token_param = "token") ?(code_param = "code") ?allow_signup ?link_verified_email
    ?mfa_required ~path ~success ~error () =
  Paw.Route.post path (fun c ->
      let email = email_service t () in
      match (Conn.param c token_param, Conn.param c code_param) with
      | Some token, Some code -> (
        (* throttle OTP-code guesses (IP + the OTP token id) so the low-entropy 6-digit code can't be
           brute-forced over the token's lifetime; a throttled attempt looks like a bad code (no oracle) *)
        let id = match String.index_opt token '.' with Some i -> String.sub token 0 i | None -> token in
        match Throttle.login_allowed t.rate_limit ~ip:(Conn.remote_ip c) ~account:("otp:" ^ id) with
        | Error _ -> Conn.redirect c error
        | Ok () ->
          redirect_identity_completion t c ?mfa_required ~success ~error
            (login_with_email_otp_completion t email ?current_user_id:(user_id c) ?allow_signup
               ?link_verified_email ~token:(Challenge.token_of_string token) ~code ()))
      | _ -> Conn.redirect c error)

let redirect_mfa_completion t c ~success ~error mfa_token verification =
  match verification with
  | Error _ -> Conn.redirect c error
  | Ok verification -> (
    match complete_login_step_up t (Challenge.token_of_string mfa_token) verification with
    | Error _ -> Conn.redirect c error
    | Ok (_, session) -> Conn.redirect (set_login_cookie t c session) success)

let mfa_totp_paw t ?(mfa_token_param = "mfaToken") ?(factor_param = "factor") ?(code_param = "code") ~path
    ~success ~error () =
  Paw.Route.post path (fun c ->
      match (Conn.param c mfa_token_param, Conn.param c factor_param, Conn.param c code_param) with
      | Some mfa_token, Some factor, Some code ->
        redirect_mfa_completion t c ~success ~error mfa_token (verify_totp_factor t factor ~code)
      | _ -> Conn.redirect c error)

let mfa_backup_code_paw t ?(mfa_token_param = "mfaToken") ?(user_param = "userId")
    ?(code_param = "code") ~path ~success ~error () =
  Paw.Route.post path (fun c ->
      match (Conn.param c mfa_token_param, Conn.param c user_param, Conn.param c code_param) with
      | Some mfa_token, Some uid, Some code ->
        redirect_mfa_completion t c ~success ~error mfa_token (consume_backup_code t uid ~code)
      | _ -> Conn.redirect c error)

let route_redirect c fallback requested = Conn.redirect c (Option.value requested ~default:fallback)

let current_or_state_user c state_user = match user_id c with Some _ as uid -> uid | None -> state_user

let json_error ?(status = 400) c reason =
  Conn.json ~status ~headers:[ ("Cache-Control", "no-store") ] c
    (Json.to_string (Json.Obj [ ("error", Json.String reason) ]))

let json_mfa_required c (step_up : login_step_up) =
  Conn.json ~status:409 ~headers:[ ("Cache-Control", "no-store") ] c
    (Json.to_string
       (Json.Obj
          [
            ("mfaRequired", Json.Bool true);
            ("userId", Json.String step_up.user.id);
            ("mfaToken", Json.String (Challenge.token_to_string step_up.step_up.token));
          ]))

let json_ok c fields =
  Conn.json ~headers:[ ("Cache-Control", "no-store") ] c (Json.to_string (Json.Obj fields))

let json_string key value = (key, Json.String value)
let json_bool key value = (key, Json.Bool value)
let json_opt_string key = function Some value -> [ json_string key value ] | None -> []
let req_body_json c = Json.parse_opt (Conn.req c).H.body
let json_member_string key json = Option.bind (Json.member key json) Json.to_string_opt

let json_member_list_strings key json =
  match Option.bind (Json.member key json) Json.to_list_opt with
  | None -> []
  | Some xs -> List.filter_map Json.to_string_opt xs

let nested_response json key =
  match Json.member "response" json with
  | Some response -> json_member_string key response
  | None -> None

let passkey_user_of_account (u : user) =
  let name =
    match u.username with
    | Some username -> username
    | None -> (
      match u.emails with email :: _ -> email.address | [] -> u.id)
  in
  Passkey.user ~id:u.id ~handle:u.id ~name ()

let passkey_registration_options_paw t relying_party ~path () =
  Paw.Route.get path (fun c ->
      let c = paw t () c in
      match user_id c with
      | None -> json_error ~status:401 c "Unauthorized"
      | Some uid -> (
        match
          Result.bind (find_required_user t uid) (fun user ->
              Result.map_error
                (fun e -> Login_rejected (Passkey.string_of_error e))
                (passkey_user_of_account user))
        with
        | Error e -> json_error c (string_of_error e)
        | Ok user -> (
          match begin_passkey_registration t relying_party user with
          | Error e -> json_error c (string_of_error e)
          | Ok options -> Conn.json ~headers:[ ("Cache-Control", "no-store") ] c options.json)))

let passkey_registration_finish_paw t relying_party ~path () =
  Paw.Route.post path (fun c ->
      let c = paw t () c in
      match (user_id c, req_body_json c) with
      | None, _ -> json_error ~status:401 c "Unauthorized"
      | _, None -> json_error c "Malformed JSON"
      | Some uid, Some json -> (
        match Option.map Challenge.token_of_string (json_member_string "token" json) with
        | None -> json_error c "Missing passkey token"
        | Some token -> (
          match finish_passkey_registration t relying_party ~user_id:uid ~token json with
          | Error e -> json_error c (string_of_error e)
          | Ok finished ->
            json_ok c
              [
                json_string "credentialId" finished.credential.Passkey.id;
                json_string "userId" finished.credential.user_id;
              ])))

let passkey_assertion_options_paw t relying_party ~path () =
  Paw.Route.get path (fun c ->
      let c = paw t () c in
      let user_id = user_id c in
      let allowed_credentials =
        Option.map
          (fun user_id ->
            t.store.passkeys.Passkey.list ~user_id () |> List.map (fun (credential : Passkey.credential) -> credential.id))
          user_id
      in
      match begin_passkey_assertion t ?user_id ?allowed_credentials relying_party with
      | Error e -> json_error c (string_of_error e)
      | Ok options -> Conn.json ~headers:[ ("Cache-Control", "no-store") ] c options.json)

let passkey_assertion_finish_paw t relying_party ~path () =
  Paw.Route.post path (fun c ->
      let c = paw t () c in
      match req_body_json c with
      | None -> json_error c "Malformed JSON"
      | Some json -> (
        match Option.map Challenge.token_of_string (json_member_string "token" json) with
        | None -> json_error c "Missing passkey token"
        | Some token -> (
          match finish_passkey_assertion_completion t ?current_user_id:(user_id c) relying_party ~token json with
          | Error e -> json_error c (string_of_error e)
          | Ok (Identity_step_up_required user) -> json_mfa_required c user
          | Ok (Complete_identity_login login) ->
            let c = set_login_cookie t c login.token in
            json_ok c
              [
                json_string "id" login.user.id;
                json_string "token" login.token;
                json_bool "created" login.created;
              ])))

let mfa_passkey_assertion_options_paw t relying_party ~path () =
  Paw.Route.get path (fun c ->
      let c = paw t () c in
      let user_id = user_id c in
      let allowed_credentials =
        Option.map
          (fun user_id ->
            t.store.passkeys.Passkey.list ~user_id ()
            |> List.map (fun (credential : Passkey.credential) -> credential.id))
          user_id
      in
      match begin_passkey_assertion t ?user_id ?allowed_credentials relying_party with
      | Error e -> json_error c (string_of_error e)
      | Ok options -> Conn.json ~headers:[ ("Cache-Control", "no-store") ] c options.json)

let mfa_passkey_assertion_finish_paw t relying_party ~path () =
  Paw.Route.post path (fun c ->
      let c = paw t () c in
      match req_body_json c with
      | None -> json_error c "Malformed JSON"
      | Some json -> (
        match
          ( Option.map Challenge.token_of_string (json_member_string "mfaToken" json),
            Option.map Challenge.token_of_string (json_member_string "token" json) )
        with
        | None, _ -> json_error c "Missing MFA token"
        | _, None -> json_error c "Missing passkey token"
        | Some mfa_token, Some token -> (
          match Result.bind (verify_passkey_factor t relying_party ~token json) (complete_login_step_up t mfa_token) with
          | Error e -> json_error ~status:403 c (string_of_error e)
          | Ok (user, session) ->
            let c = set_login_cookie t c session in
            json_ok c [ json_string "id" user.id; json_string "token" session ])))

let scim_bearer c =
  match Conn.req_header c "authorization" with
  | Some value ->
    let prefix = "Bearer " in
    if String.length value > String.length prefix
       && String.sub value 0 (String.length prefix) = prefix
    then Some (String.sub value (String.length prefix) (String.length value - String.length prefix))
    else None
  | None -> None

let scim_connection_for_request t c =
  match scim_bearer c with
  | None -> Error (Login_rejected "Missing SCIM bearer token")
  | Some bearer -> (
    match
      List.find_opt
        (fun connection -> Result.is_ok (Scim.authenticate connection ~bearer_token:bearer))
        (t.store.scim.Scim.list_connections ())
    with
    | None -> Error (Login_rejected "Invalid SCIM bearer token")
    | Some connection -> Ok connection)

let ensure_scim_account t (connection : Scim.connection) (incoming : Scim.user) =
  Result.bind (Scim.identity connection incoming |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
    (fun key ->
      let linked = t.store.identities.Identity.find key in
      let email = match incoming.emails with email :: _ -> Some email | [] -> None in
      let ensure_user () =
        match linked with
        | Some link -> find_required_user t link.Identity.user_id
        | None ->
          Result.bind (create_user t ~username:incoming.user_name ?email ()) (fun user ->
              Result.bind
                (attach_identity t.store.identities ~verified_at:(now ()) ~created_at:(now ())
                   ~user_id:user.id key)
                (fun _ -> Ok user))
      in
      Result.bind (ensure_user ()) (fun user ->
          Result.bind
            (Scim.membership connection ~user_id:user.id incoming
            |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
            (fun membership ->
              Result.bind
                (Result.map_error (fun e -> Store_error e)
                   (t.store.orgs.Org.upsert_membership membership))
                (fun () -> Ok user))))

(* Resolve the Fennec user behind a SCIM user (via its tenant-scoped identity key) and OFFBOARD it:
   disable the account (bumps the revocation epoch, so the gated paths reject it) AND prune its live
   session rows (so the zero-read paths reject it too). This is what an IdP deprovision / DELETE must
   do — flipping the SCIM sync row alone leaves the Fennec account Active with live sessions. The user
   may already be gone (no link); that is a no-op. Best-effort; never fails the SCIM response. *)
let scim_offboard_account t (connection : Scim.connection) (scim_user : Scim.user) =
  match Scim.identity connection scim_user with
  | Error _ -> ()
  | Ok key -> (
    match t.store.identities.Identity.find key with
    | None -> ()
    | Some link ->
      let uid = link.Identity.user_id in
      ignore (disable_user t uid);
      ignore (t.store.tokens.revoke_user uid ()))

let apply_scim_user t (connection : Scim.connection) (incoming : Scim.user) =
  let connection_id = connection.id in
  let existing = t.store.scim.Scim.find_user ~connection_id ~external_id:incoming.external_id in
  Result.bind
    (Scim.plan_user connection ~existing ~incoming
    |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
    (fun plan ->
      let persist user =
        Result.bind
          (Result.map_error (fun e -> Store_error e)
             (t.store.scim.Scim.upsert_user ~connection_id user))
          (fun () -> Result.map (fun _ -> user) (ensure_scim_account t connection user))
      in
      match plan with
      | Scim.No_user_change -> Ok incoming
      | Scim.Create_user user | Scim.Update_user { after = user; _ } -> persist user
      | Scim.Deprovision_user { after = user; _ } ->
        (* persist the synced row first (keeps the directory state), THEN offboard the account: an
           [active=false] deprovision disables the Fennec user + kills its sessions. *)
        Result.map
          (fun user -> scim_offboard_account t connection user; user)
          (persist user))

let scim_resource_path ~prefix path =
  let prefix = if String.ends_with ~suffix:"/" prefix then String.sub prefix 0 (String.length prefix - 1) else prefix in
  if path = prefix ^ "/ServiceProviderConfig" then Some `ServiceProviderConfig
  else if path = prefix ^ "/ResourceTypes" then Some `ResourceTypes
  else if path = prefix ^ "/Schemas" then Some `Schemas
  else if path = prefix ^ "/Users" then Some (`Users None)
  else if String.starts_with ~prefix:(prefix ^ "/Users/") path then
    Some (`Users (Some (String.sub path (String.length prefix + 7) (String.length path - String.length prefix - 7))))
  else if path = prefix ^ "/Groups" then Some (`Groups None)
  else if String.starts_with ~prefix:(prefix ^ "/Groups/") path then
    Some (`Groups (Some (String.sub path (String.length prefix + 8) (String.length path - String.length prefix - 8))))
  else None

let scim_paw t ~prefix () : Paw.t =
 fun c ->
  match scim_resource_path ~prefix (Conn.path c) with
  | None -> c
  | Some `ServiceProviderConfig -> Conn.json c (Json.to_string Scim.scim_service_provider_config_json)
  | Some `ResourceTypes -> Conn.json c (Json.to_string Scim.scim_resource_types_json)
  | Some `Schemas -> Conn.json c (Json.to_string Scim.scim_schemas_json)
  | Some resource -> (
    match scim_connection_for_request t c with
    | Error e -> json_error ~status:401 c (string_of_error e)
    | Ok connection -> (
      match (Conn.meth c, resource, req_body_json c) with
      | H.GET, `Users None, _ ->
        json_ok c
          [
            ( "Resources",
              Json.List
                (List.map Scim.scim_user_json
                   (t.store.scim.Scim.list_users ~connection_id:connection.id ())) );
          ]
      | H.GET, `Users (Some external_id), _ -> (
        match t.store.scim.Scim.find_user ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM user not found"
        | Some user -> Conn.json c (Json.to_string (Scim.scim_user_json user)))
      | H.POST, `Users None, Some json | H.PUT, `Users (Some _), Some json -> (
        match
          Result.bind
            (Scim.scim_user_of_json json |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
            (apply_scim_user t connection)
        with
        | Error e -> json_error c (string_of_error e)
        | Ok user -> Conn.json ~status:201 c (Json.to_string (Scim.scim_user_json user)))
      | H.PATCH, `Users (Some external_id), Some json -> (
        match t.store.scim.Scim.find_user ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM user not found"
        | Some user -> (
          match
            Result.bind
              (Scim.scim_user_patch_of_json json |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
              (fun ops ->
                Scim.apply_user_patch user ops
                |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
          with
          | Error e -> json_error c (string_of_error e)
          | Ok patched when patched.Scim.external_id <> external_id ->
            json_error c "SCIM PATCH cannot change externalId"
          | Ok patched -> (
            match apply_scim_user t connection patched with
            | Error e -> json_error c (string_of_error e)
            | Ok user -> Conn.json c (Json.to_string (Scim.scim_user_json user)))))
      | H.DELETE, `Users (Some external_id), _ ->
        (* a SCIM DELETE is a hard deprovision: offboard the Fennec account (disable + revoke sessions)
           — resolved from the stored row BEFORE we drop it — then remove the SCIM sync row. *)
        (match t.store.scim.Scim.find_user ~connection_id:connection.id ~external_id with
        | Some scim_user -> scim_offboard_account t connection scim_user
        | None -> ());
        ignore (t.store.scim.Scim.delete_user ~connection_id:connection.id ~external_id);
        Conn.text ~status:204 c ""
      | H.GET, `Groups None, _ ->
        json_ok c
          [
            ( "Resources",
              Json.List
                (List.map Scim.scim_group_json
                   (t.store.scim.Scim.list_groups ~connection_id:connection.id ())) );
          ]
      | H.GET, `Groups (Some external_id), _ -> (
        match t.store.scim.Scim.find_group ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM group not found"
        | Some group -> Conn.json c (Json.to_string (Scim.scim_group_json group)))
      | H.POST, `Groups None, Some json | H.PUT, `Groups (Some _), Some json -> (
        match Scim.scim_group_of_json json with
        | Error e -> json_error c (Scim.string_of_error e)
        | Ok group -> (
          match t.store.scim.Scim.upsert_group ~connection_id:connection.id group with
          | Error e -> json_error c e
          | Ok () -> Conn.json ~status:201 c (Json.to_string (Scim.scim_group_json group))))
      | H.PATCH, `Groups (Some external_id), Some json -> (
        match t.store.scim.Scim.find_group ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM group not found"
        | Some group -> (
          match
            Result.bind
              (Scim.scim_group_patch_of_json json |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
              (fun ops ->
                Scim.apply_group_patch group ops
                |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
          with
          | Error e -> json_error c (string_of_error e)
          | Ok patched when patched.Scim.external_id <> external_id ->
            json_error c "SCIM PATCH cannot change externalId"
          | Ok patched -> (
            match t.store.scim.Scim.upsert_group ~connection_id:connection.id patched with
            | Error e -> json_error c e
            | Ok () -> Conn.json c (Json.to_string (Scim.scim_group_json patched)))))
      | H.DELETE, `Groups (Some external_id), _ ->
        ignore (t.store.scim.Scim.delete_group ~connection_id:connection.id ~external_id);
        Conn.text ~status:204 c ""
      | _, _, None -> json_error c "Malformed JSON"
      | _ -> json_error ~status:405 c "Unsupported SCIM operation"))

let oauth_authorize_paw t ?(redirect_param = "redirect") ~path ~error provider () =
  Paw.Route.get path (fun c ->
      let oauth = OAuth.make ~challenge:(challenge_service t ()) in
      let redirect = Conn.param c redirect_param in
      match OAuth.authorize oauth ?user_id:(user_id c) ?redirect provider with
      | Error _ -> Conn.redirect c error
      | Ok issued -> Conn.redirect c issued.OAuth.url)

(* Apply a provider's optional role-map to a freshly-logged-in user. The mapped role strings are
   persisted through the same audited path as admin/SCIM role changes; a role-map failure is
   non-fatal to the login (the session is already valid) but is surfaced via the audit log inside
   set_roles_from_strings. [None] is inert. *)
let apply_role_map t role_map principal user_id =
  match role_map with
  | None -> ()
  | Some f -> ignore (set_roles_from_strings t user_id (f principal))

let oauth_callback_paw t ?(link_verified_email = true) ?role_map ~path ~success ~error provider ~exchange () =
  Paw.Route.get path (fun c ->
      let oauth = OAuth.make ~challenge:(challenge_service t ()) in
      match OAuth.parse_callback (Conn.req c).H.query_string with
      | Error _ -> Conn.redirect c error
      | Ok (OAuth.Callback_error _) -> Conn.redirect c error
      | Ok (OAuth.Code { code; state }) -> (
        match OAuth.consume_state oauth ~expected_provider:provider.OAuth.name state with
        | Error _ -> Conn.redirect c error
        | Ok state -> (
          match exchange state ~code with
          | Error _ -> Conn.redirect c error
          | Ok facts -> (
            let current_user_id = current_or_state_user c state.OAuth.user_id in
            match
              login_with_identity t ?current_user_id ~allow_signup:true ~link_verified_email
                ~strategy:("oauth:" ^ provider.name) facts
            with
            | Error _ -> Conn.redirect c error
            | Ok login ->
              apply_role_map t role_map facts login.user.id;
              route_redirect (set_login_cookie t c login.token) success state.redirect))))

(* Bounce page for the OAuth-over-DDP popup: post the JSON result to the opener (same-origin only,
   via window.location.origin) and close. credentialToken/secret are URI-safe base64, so the JSON
   embeds safely in the inline script; '<' is still escaped defensively against a future charset
   change. The opener listens for a {fennecOAuth: ...} message and calls [login {oauth}]. *)
let oauth_popup_response c (payload : Json.t) =
  let data = Json.to_string payload |> String.split_on_char '<' |> String.concat "\\u003c" in
  Conn.html c
    (Printf.sprintf
       {html|<!doctype html><html><head><meta charset="utf-8"><title>Signing in…</title></head><body>
<script>(function(){var d=%s;try{if(window.opener)window.opener.postMessage(JSON.stringify({fennecOAuth:d}),window.location.origin);}catch(e){}window.close();})();</script>
<p>You can close this window.</p></body></html>|html}
       data)

(* Like [oauth_callback_paw] but completes over DDP: resolves the user and hands a single-use
   {credentialToken, credentialSecret} pair back to the opener instead of setting a cookie and
   redirecting. Mount this when the OAuth popup should finish inside an SPA without a full-page
   reload; the SPA replays the pair through the [login {oauth}] method. *)
let oauth_callback_popup_paw t ?(link_verified_email = true) ~path provider ~exchange () =
  Paw.Route.get path (fun c ->
      let oauth = OAuth.make ~challenge:(challenge_service t ()) in
      let fail reason = oauth_popup_response c (Json.Obj [ ("error", Json.String reason) ]) in
      match OAuth.parse_callback (Conn.req c).H.query_string with
      | Error _ -> fail "invalid_callback"
      | Ok (OAuth.Callback_error _) -> fail "provider_error"
      | Ok (OAuth.Code { code; state }) -> (
        match OAuth.consume_state oauth ~expected_provider:provider.OAuth.name state with
        | Error _ -> fail "invalid_state"
        | Ok state -> (
          match exchange state ~code with
          | Error _ -> fail "exchange_failed"
          | Ok facts -> (
            let current_user_id = current_or_state_user c state.OAuth.user_id in
            match
              resolve_external_user t ?current_user_id ~allow_signup:true ~link_verified_email
                ~strategy:("oauth:" ^ provider.name) facts
            with
            | Error _ -> fail "login_rejected"
            | Ok user -> (
              match issue_oauth_credential t ~user_id:user.id ~provider:provider.name () with
              | Error _ -> fail "credential_failed"
              | Ok (credential_token, credential_secret) ->
                oauth_popup_response c
                  (Json.Obj
                     [
                       ("credentialToken", Json.String credential_token);
                       ("credentialSecret", Json.String credential_secret);
                     ]))))))

let oidc_authorize_paw t ?(redirect_param = "redirect") ~path ~error (connection : Oidc.connection) () =
  Paw.Route.get path (fun c ->
      let oidc = Oidc.make ~challenge:(challenge_service t ()) in
      let redirect = Conn.param c redirect_param in
      match Oidc.authorize oidc ?user_id:(user_id c) ?redirect connection with
      | Error _ -> Conn.redirect c error
      | Ok issued -> Conn.redirect c issued.Oidc.url)

let oidc_callback_paw t ?(link_verified_email = true) ?role_map ~path ~success ~error (connection : Oidc.connection) ~exchange () =
  Paw.Route.get path (fun c ->
      let oidc = Oidc.make ~challenge:(challenge_service t ()) in
      match Oidc.parse_callback (Conn.req c).H.query_string with
      | Error _ -> Conn.redirect c error
      | Ok (Oidc.Callback_error _) -> Conn.redirect c error
      | Ok (Oidc.Code { code; state }) -> (
        match Oidc.consume_state oidc ~expected_connection:connection.Oidc.id state with
        | Error _ -> Conn.redirect c error
        | Ok state -> (
          match exchange state ~code with
          | Error _ -> Conn.redirect c error
          | Ok principal -> (
            let current_user_id = current_or_state_user c state.Oidc.user_id in
            match
              login_with_oidc t ?current_user_id ~allow_signup:connection.allow_jit ~link_verified_email
                principal
            with
            | Error _ -> Conn.redirect c error
            | Ok login ->
              apply_role_map t role_map principal login.user.id;
              route_redirect (set_login_cookie t c login.token) success state.redirect))))

let saml_authorize_paw t ?(redirect_param = "redirect") ?signing_key ~path ~error connection () =
  Paw.Route.get path (fun c ->
      let saml = Saml.make ~challenge:(challenge_service t ()) in
      let redirect = Conn.param c redirect_param in
      match Saml.issue_request saml ?user_id:(user_id c) ?redirect connection with
      | Error _ -> Conn.redirect c error
      | Ok request -> (
        match signing_key with
        | None -> Conn.redirect c (Saml.redirect_url request)
        | Some signing_key -> (
          match Saml.signed_redirect_url request ~signing_key with
          | Ok url -> Conn.redirect c url
          | Error _ -> Conn.redirect c error)))

let saml_callback_paw t ?role_map ~path ~success ~error connection ~trusted_keys () =
  Paw.Route.post path (fun c ->
      let saml = Saml.make ~challenge:(challenge_service t ()) in
      match (Conn.param c "RelayState", Conn.param c "SAMLResponse") with
      | Some relay_state, Some saml_response -> (
        match
          Saml.consume_response saml connection ~trusted_keys
            ~relay_state:(Challenge.token_of_string relay_state) ~saml_response
        with
        | Error _ -> Conn.redirect c error
        | Ok principal -> (
          match login_with_saml t ?current_user_id:(user_id c) principal with
          | Error _ -> Conn.redirect c error
          | Ok login ->
            apply_role_map t role_map principal login.user.id;
            Conn.redirect (set_login_cookie t c login.token) success))
      | _ -> Conn.redirect c error)

let doc_get_string d k = match Bson.get d k with Some (Bson.String s) -> Some s | _ -> None
let doc_get_doc d k = match Bson.get d k with Some (Bson.Document _ as d) -> Some d | _ -> None

let doc_get_float d k = match Bson.get d k with Some v -> Bson.as_float v | _ -> None
let doc_get_int d k = Bson.get_int d k
let doc_get_bool d k = Bson.get_bool d k
let doc_get_list d k = Bson.get_list d k

let opt_float = function Some v -> Bson.as_float v | _ -> None

let id_selector id = Bson.doc [ ("_id", Bson.str id) ]
let set_doc fields = Bson.doc [ ("$set", Bson.doc fields) ]
