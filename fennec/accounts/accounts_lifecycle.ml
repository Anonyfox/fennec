(* accounts_lifecycle.ml — user / password / email / role / MFA-enrollment / org / identity-link
   lifecycle: creating and mutating the durable account state. These are the engine's write
   operations (create_user, set_username/profile, the email add/remove/replace + verification
   issue/consume, password set/change/reset-core + enrollment-core, role grants, TOTP/backup-code
   enrollment, org create/member/invite, identity link/unlink/merge) and the transactional
   account emails. The session-issuing 'completion' wrappers live in Accounts_login, which sits
   on top of this. Carved verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets
open Accounts_identity_bridge
open Accounts_runtime
open Accounts_request

let user_with_defaults ?id ?username ?email ?profile () : user =
  let id = match id with Some id -> id | None -> random_id ~bytes:12 () in
  let t = now () in
  {
    id;
    username = Option.bind username nonblank_opt;
    emails =
      (match Option.bind email nonblank_opt with
      | Some e -> [ { address = normalize_email e; verified = false } ]
      | None -> []);
    roles = [];
    profile;
    services = [];
    created_at = t;
    updated_at = t;
    auth_epoch = 0;
    status = Active;
  }

let validate_password_policy t ?email ?username password =
  match t.password_policy with
  | None -> Ok ()
  | Some policy -> (
    match Password.validate ?email ?username ~policy password with
    | Ok () -> Ok ()
    | Error errors -> Error (Login_rejected (Password.describe_errors errors)))

let primary_email user =
  match user.emails with
  | email :: _ -> Some email.address
  | [] -> None

let normalize_user (u : user) =
  {
    u with
    username = Option.bind u.username nonblank_opt;
    roles =
      (match Roles.normalize_roles (Roles.role_names u.roles) with
      | Ok roles -> roles
      | Error _ -> u.roles);
    emails =
      List.filter_map
        (fun e ->
          match nonblank_opt e.address with
          | None -> None
          | Some address -> Some { e with address = normalize_email address })
        u.emails;
  }

let first_duplicate normalized xs =
  let seen = Hashtbl.create 8 in
  List.find_opt
    (fun x ->
      let k = normalized x in
      if Hashtbl.mem seen k then true
      else (
        Hashtbl.add seen k ();
        false))
    xs

let validate_user_shape (u : user) =
  if String.trim u.id = "" then Error (Invalid_user "User id cannot be blank")
  else if u.auth_epoch < 0 then Error (Invalid_user "auth_epoch cannot be negative")
  else
    match Roles.normalize_roles (Roles.role_names u.roles) with
    | Error e -> Error (Invalid_user (Roles.string_of_error e))
    | Ok _ -> (
      match List.find_opt (fun e -> normalize_email e.address = "") u.emails with
      | Some _ -> Error (Invalid_user "Email cannot be blank")
      | None -> (
      match List.find_opt (fun (strategy, _) -> String.trim strategy = "") u.services with
      | Some _ -> Error (Invalid_user "Service name cannot be blank")
      | None -> (
        match first_duplicate (fun e -> normalize_email e.address) u.emails with
        | Some e -> Error (Duplicate_email (normalize_email e.address))
        | None -> (
          match first_duplicate (fun (strategy, _) -> String.trim strategy) u.services with
          | Some (strategy, _) -> Error (Invalid_user ("Duplicate service: " ^ String.trim strategy))
          | None -> Ok ()))))

let ensure_unique t (u : user) =
  match validate_user_shape u with
  | Error _ as e -> e
  | Ok () -> (
    match t.store.users.find_user_by_id u.id with
    | Ok (Some _) -> Error (Store_error ("duplicate user id: " ^ u.id))
    | Error _ as e -> e
    | Ok None ->
      let check_email e =
        let address = normalize_email e.address in
        match t.store.users.find_user_by_email address with
        | Ok None -> Ok ()
        | Ok (Some _) -> Error (Duplicate_email address)
        | Error _ as e -> e
      in
      let check_username name =
        match t.store.users.find_user_by_username name with
        | Ok None -> Ok ()
        | Ok (Some _) -> Error (Duplicate_username name)
        | Error _ as e -> e
      in
      let rec emails = function
        | [] -> Ok ()
        | e :: rest -> Result.bind (check_email e) (fun () -> emails rest)
      in
      Result.bind (emails u.emails) (fun () ->
          match u.username with None -> Ok () | Some name -> check_username name))


let ensure_password_identity t user =
  ignore (t.store.identities.Identity.attach ~created_at:(now ()) ~user_id:user.id (Identity.password ()))

let attach_identity identity_store ?verified_at ~created_at ~user_id key =
  match identity_store.Identity.attach ?verified_at ~created_at ~user_id key with
  | Identity.Attach link | Identity.Already_linked link -> Ok (Some link)
  | Identity.Conflict link ->
    Error (Login_rejected ("Identity already belongs to user " ^ link.Identity.user_id))

let create_user t ?id ?username ?email ?password ?profile () =
  let base = user_with_defaults ?id ?username ?email ?profile () in
  let password_hash =
    match password with
    | None -> Ok None
    | Some password -> (
      match t.password_hasher with
      | None -> Error Password_not_configured
      | Some h ->
        Result.map (fun () -> Some (h.hash ~password))
          (validate_password_policy t ?email:(primary_email base) ?username:base.username password))
  in
  Result.bind password_hash (fun password_hash ->
      Result.bind (validate_user_shape base) (fun () ->
          Result.bind (run_create_hooks t base) (fun user ->
              let user = normalize_user user in
              Result.bind (ensure_unique t user) (fun () ->
                  Result.map
                    (fun user ->
                      Option.iter (fun _ -> ensure_password_identity t user) password_hash;
                      record_audit ~target_user_id:user.id ?mechanism:(Option.map (fun _ -> Audit.Password) password_hash)
                        ~metadata:[ ("kind", "user_create") ] t (Audit.Custom "user_create") Audit.Anonymous
                        Audit.Success;
                      user)
                    (t.store.users.create_user user ~password_hash)))))

let update_existing_user t uid f =
  Result.bind (find_required_user t uid) (fun user ->
      let updated = normalize_user (f user) in
      t.store.users.update_user updated)

let set_username t uid username =
  update_existing_user t uid (fun user ->
      { user with username = Option.map normalize_username (Option.bind username nonblank_opt) })

let set_profile t uid profile = update_existing_user t uid (fun user -> { user with profile })

let role_error e = Login_rejected (Roles.string_of_error e)

let role_names_csv roles = String.concat "," (Roles.role_names roles)

let record_role_change t ?request ?actor ~target_user_id ~action ?role ~before ~after () =
  let actor = Option.value ~default:(Audit.System "accounts") actor in
  let metadata =
    [
      ("action", action);
      ("before", role_names_csv before);
      ("after", role_names_csv after);
    ]
    @ match role with None -> [] | Some role -> [ ("role", Roles.Role.name role) ]
  in
  record_audit ?request ~target_user_id ~mechanism:(Audit.Custom_mechanism "roles") ~metadata t
    Audit.Role_change actor Audit.Success

let update_roles t ?actor ?request uid ~action ?role f =
  Result.bind (find_required_user t uid) (fun user ->
      let before = user.roles in
      let roles = f before |> Roles.role_names |> Roles.normalize_roles in
      Result.bind (Result.map_error role_error roles) (fun after ->
          Result.map
            (fun updated ->
              if Roles.role_names before <> Roles.role_names after then
                record_role_change t ?request ?actor ~target_user_id:uid ~action ?role ~before ~after ();
              updated)
            (t.store.users.update_user (normalize_user { user with roles = after }))))

let set_roles t ?actor ?request uid roles =
  update_roles t ?actor ?request uid ~action:"replace" (fun _ -> roles)

let set_roles_from_strings t ?actor ?request uid roles =
  Result.bind (Result.map_error role_error (Roles.normalize_roles roles)) (set_roles t ?actor ?request uid)

let grant_role t ?actor ?request uid role =
  update_roles t ?actor ?request uid ~action:"grant" ~role (fun roles -> Roles.add role roles)

let revoke_role t ?actor ?request uid role =
  update_roles t ?actor ?request uid ~action:"revoke" ~role (fun roles -> Roles.remove role roles)

let email_key address =
  match Identity.email ~verified:true address with
  | Ok key -> Ok key
  | Error e -> Error (Login_rejected (Identity.string_of_error e))

type attached_identity =
  | Attached of Identity.link
  | Already_attached of Identity.link

let attach_identity_for_update identity_store ?verified_at ~created_at ~user_id key =
  match identity_store.Identity.attach ?verified_at ~created_at ~user_id key with
  | Identity.Attach link -> Ok (Attached link)
  | Identity.Already_linked link -> Ok (Already_attached link)
  | Identity.Conflict link ->
    Error (Login_rejected ("Identity already belongs to user " ^ link.Identity.user_id))

let rollback_attached_identity identity_store key = function
  | Already_attached _ -> ()
  | Attached link ->
    ignore (identity_store.Identity.detach ~allow_last:true ~user_id:link.Identity.user_id key)

let restore_detached_identity identity_store key link =
  ignore
    (identity_store.Identity.attach ?verified_at:link.Identity.verified_at
       ~created_at:link.Identity.created_at ~user_id:link.Identity.user_id key)

let update_user_after_attach t key attach updated =
  Result.bind attach (fun attached ->
      match t.store.users.update_user updated with
      | Ok user -> Ok user
      | Error _ as e ->
        rollback_attached_identity t.store.identities key attached;
        e)

let detach_identity_for_update identity_store ?allow_last ~user_id key =
  match identity_store.Identity.detach ?allow_last ~user_id key with
  | Identity.Detach link -> Ok (Some link)
  | Identity.Link_not_found -> Ok None
  | Identity.Reject_last_credential -> Error (Login_rejected "Cannot remove the last usable credential")

let update_user_after_detach t key detached updated =
  Result.bind detached (fun detached ->
      match t.store.users.update_user updated with
      | Ok user -> Ok user
      | Error _ as e ->
        Option.iter (restore_detached_identity t.store.identities key) detached;
        e)

let ensure_verified_email_available t uid address =
  Result.bind (email_key address) (fun key ->
      match t.store.identities.Identity.find key with
      | Some link when link.Identity.user_id <> uid ->
        Error (Login_rejected ("Email identity already belongs to user " ^ link.user_id))
      | _ -> Ok key)

let add_email t ?(verified = false) uid raw_email =
  match Email.normalize raw_email with
  | Error e -> Error (email_error e)
  | Ok address ->
    let address = Email.address_to_string address in
    Result.bind (find_required_user t uid) (fun user ->
        Result.bind
          (if verified then ensure_verified_email_available t uid address |> Result.map Option.some
           else Ok None)
          (fun key ->
            let seen = ref false in
            let emails =
              List.map
                (fun email ->
                  if normalize_email email.address = address then (
                    seen := true;
                    { address; verified = email.verified || verified })
                  else email)
                user.emails
            in
            let emails = if !seen then emails else emails @ [ { address; verified } ] in
            let updated = { user with emails } in
            match key with
            | None -> t.store.users.update_user updated
            | Some key ->
              let at = now () in
              update_user_after_attach t key
                (attach_identity_for_update t.store.identities ~verified_at:at ~created_at:at
                   ~user_id:uid key)
                updated))

let remove_email t ?allow_last uid raw_email =
  match Email.normalize raw_email with
  | Error e -> Error (email_error e)
  | Ok address ->
    let address = Email.address_to_string address in
    Result.bind (find_required_user t uid) (fun user ->
        match List.find_opt (fun email -> normalize_email email.address = address) user.emails with
        | None -> Error (Invalid_user "Email is not on this user")
        | Some email ->
          let emails = List.filter (fun email -> normalize_email email.address <> address) user.emails in
          let updated = { user with emails } in
          if not email.verified then t.store.users.update_user updated
          else
            Result.bind (email_key address) (fun key ->
                update_user_after_detach t key
                  (detach_identity_for_update t.store.identities ?allow_last ~user_id:uid key)
                  updated))

let replace_email t ?allow_last ?(verified = false) uid ~old_email ~new_email =
  match (Email.normalize old_email, Email.normalize new_email) with
  | Error e, _ | _, Error e -> Error (email_error e)
  | Ok old_address, Ok new_address ->
    let old_address = Email.address_to_string old_address in
    let new_address = Email.address_to_string new_address in
    Result.bind (find_required_user t uid) (fun user ->
        match List.find_opt (fun email -> normalize_email email.address = old_address) user.emails with
        | None -> Error (Invalid_user "Email is not on this user")
        | Some old ->
          Result.bind
            (if verified then ensure_verified_email_available t uid new_address |> Result.map Option.some
             else Ok None)
            (fun new_key ->
              let replaced = ref false in
              let emails =
                user.emails
                |> List.filter (fun email ->
                       normalize_email email.address = old_address
                       || normalize_email email.address <> new_address)
                |> List.map (fun email ->
                       if normalize_email email.address = old_address then (
                         replaced := true;
                         { address = new_address; verified })
                       else email)
              in
              let emails = if !replaced then emails else emails @ [ { address = new_address; verified } ] in
              let updated = { user with emails } in
              match (old.verified && old_address <> new_address, new_key) with
              | false, None -> t.store.users.update_user updated
              | false, Some key ->
                let at = now () in
                update_user_after_attach t key
                  (attach_identity_for_update t.store.identities ~verified_at:at ~created_at:at
                     ~user_id:uid key)
                  updated
              | true, Some new_key ->
                let at = now () in
                Result.bind
                  (attach_identity_for_update t.store.identities ~verified_at:at ~created_at:at
                     ~user_id:uid new_key)
                  (fun attached ->
                    match t.store.users.update_user updated with
                    | Error _ as e ->
                      rollback_attached_identity t.store.identities new_key attached;
                      e
                    | Ok user ->
                      Result.bind (email_key old_address) (fun old_key ->
                          match
                            detach_identity_for_update t.store.identities ~allow_last:true
                              ~user_id:uid old_key
                          with
                          | Ok _ -> Ok user
                          | Error _ as e -> e))
              | true, None ->
                Result.bind (email_key old_address) (fun key ->
                    update_user_after_detach t key
                      (detach_identity_for_update t.store.identities ?allow_last ~user_id:uid key)
                      updated)))

let set_user_status t uid status =
  Result.bind (update_existing_user t uid (fun user -> { user with status })) (fun _ ->
      Result.bind (t.store.users.bump_auth_epoch uid) (fun _ ->
          Result.bind (find_required_user t uid) (fun user ->
              record_audit ~target_user_id:uid
                ~metadata:[ ("status", string_of_user_status status) ] t (Audit.Custom "user_status")
                (Audit.User uid) Audit.Success;
              Ok user)))

let suspend_user t uid = set_user_status t uid Suspended
let disable_user t uid = set_user_status t uid Disabled
let restore_user t uid = set_user_status t uid Active
let delete_user t uid = set_user_status t uid Deleted

let set_password t uid ~password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher ->
    match t.store.users.find_user_by_id uid with
    | Error _ as e -> e
    | Ok None -> Error User_not_found
    | Ok (Some user) ->
      Result.bind
        (validate_password_policy t ?email:(primary_email user) ?username:user.username password)
        (fun () ->
          Result.map
            (fun _ ->
              ensure_password_identity t user;
              ())
            (t.store.users.set_password_hash_and_bump uid (hasher.hash ~password)))

let change_password t uid ~old_password ~new_password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher ->
    match t.store.users.find_user_by_id uid with
    | Error _ as e -> e
    | Ok None -> Error User_not_found
    | Ok (Some user) -> (
      match t.store.users.password_hash uid with
      | Error _ as e -> e
      | Ok None -> Error Password_not_configured
      | Ok (Some hash) ->
        if not (hasher.verify ~password:old_password ~hash) then Error Invalid_password
        else (
          match validate_password_policy t ?email:(primary_email user) ?username:user.username new_password with
          | Error _ as e -> e
          | Ok () ->
            Result.map
              (fun _ ->
                ensure_password_identity t user;
                record_audit ~target_user_id:uid ~mechanism:Audit.Password t Audit.Password_change
                  (Audit.User uid) Audit.Success;
                ())
              (t.store.users.set_password_hash_and_bump uid (hasher.hash ~password:new_password))))

let add_service service user =
  match service with
  | None -> user
  | Some (name, doc) ->
    let name = String.trim name in
    if name = "" then user else { user with services = (name, doc) :: List.remove_assoc name user.services }
let linked_identities t uid =
  Result.bind (find_required_user t uid) (fun _ -> Ok (t.store.identities.Identity.list ~user_id:uid ()))

let unlink_identity t ?allow_last uid key =
  Result.bind (find_required_user t uid) (fun _ ->
      match t.store.identities.Identity.detach ?allow_last ~user_id:uid key with
      | Identity.Detach link ->
        Result.map
          (fun _ ->
            record_audit ~target_user_id:uid t Audit.Identity_unlink (Audit.User uid) Audit.Success;
            link)
          (t.store.users.bump_auth_epoch uid)
      | Identity.Link_not_found -> Error (Login_rejected "Identity link not found")
      | Identity.Reject_last_credential -> Error (Login_rejected "Cannot unlink the last usable credential"))

let merge_identities t ~from_user_id ~into_user_id =
  if from_user_id = into_user_id then Error (Invalid_user "Cannot merge a user into itself")
  else
    Result.bind (find_required_user t from_user_id) (fun _ ->
        Result.bind (find_required_user t into_user_id) (fun _ ->
            match t.store.identities.Identity.merge ~from_user_id ~into_user_id with
            | Error conflicts ->
              Error
                (Login_rejected
                   ("Identity merge has "
                   ^ string_of_int (List.length conflicts)
                   ^ " conflicting credential(s)"))
            | Ok plan ->
              Result.bind (t.store.users.bump_auth_epoch from_user_id) (fun _ ->
                  Result.map
                    (fun _ ->
                      record_audit ~target_user_id:into_user_id ~metadata:[ ("from_user_id", from_user_id) ] t Audit.Identity_merge
                        (Audit.User into_user_id) Audit.Success;
                      plan)
                    (t.store.users.bump_auth_epoch into_user_id))))

let link_identity t ?(now = now) uid (facts : external_identity) =
  Result.bind (find_required_user t uid) (fun user ->
      let created_at = now () in
      let verified_at = if facts.email_verified || Identity.usable_for_login facts.key then Some created_at else None in
      Result.bind
        (attach_identity t.store.identities ?verified_at ~created_at ~user_id:uid facts.key)
        (fun linked ->
          let user = add_service facts.service user in
          Result.bind (t.store.users.update_user user) (fun _ ->
              record_audit ~target_user_id:uid t Audit.Identity_link (Audit.User uid) Audit.Success;
              Ok linked)))

let link_current_identity t ?now c facts =
  match user_id c with
  | None -> Error User_not_found
  | Some uid -> link_identity t ?now uid facts



let enroll_totp t ?issuer ?account ?label uid =
  Result.bind (find_required_user t uid) (fun _ ->
      let secret = Mfa.generate_totp_secret () in
      Result.bind
        (Result.map_error (fun e -> Login_rejected (Mfa.string_of_error e))
           (Mfa.totp ?issuer ?account ~secret ()))
        (fun totp ->
          Result.bind
            (Result.map_error (fun e -> Login_rejected (Mfa.string_of_error e))
               (Mfa.enrollment ?label ~id:(random_id ()) ~user_id:uid ~factor:Mfa.Totp
                  ~secret:(seal_mfa_secret t secret) ()))
            (fun enrollment ->
              Result.bind
                (Result.map_error (fun e -> Store_error e) (t.store.mfa.Mfa.upsert enrollment))
                (fun () ->
                  record_audit ~target_user_id:uid ~mechanism:Audit.Mfa t Audit.Mfa_enrollment
                    (Audit.User uid) Audit.Success ~metadata:[ ("status", "pending"); ("factor", "totp") ];
                  Ok { enrollment; totp; provisioning_uri = Mfa.provisioning_uri totp }))))

let mfa_enrollment_error e = Login_rejected (Mfa.string_of_error e)

let replace_mfa_enrollment t ~current next =
  match t.store.mfa.Mfa.replace_if_current ~current next with
  | Ok true -> Ok ()
  | Ok false -> Error (Login_rejected "MFA enrollment changed; retry the verification")
  | Error e -> Error (Store_error e)

let totp_of_enrollment t (enrollment : Mfa.enrollment) =
  match (enrollment.factor, enrollment.secret) with
  | Mfa.Totp, Some sealed -> (
    match unseal_mfa_secret t sealed with
    | None -> Error (Store_error "TOTP enrollment secret could not be opened")
    | Some secret -> Result.map_error mfa_enrollment_error (Mfa.totp ~secret ()))
  | Mfa.Totp, None -> Error (Store_error "TOTP enrollment is missing its secret")
  | _ -> Error (Login_rejected "MFA enrollment is not a TOTP factor")

let confirm_totp_enrollment t ?time id ~code =
  match t.store.mfa.Mfa.find id with
  | None -> Error User_not_found
  | Some enrollment when enrollment.status <> Mfa.Pending ->
    Error (Login_rejected "MFA enrollment is not pending")
  | Some current ->
    Result.bind (totp_of_enrollment t current) (fun totp ->
        Result.bind (Result.map_error mfa_enrollment_error (Mfa.verify_totp ?time totp ~code)) (fun step ->
            let confirmed_at = Option.value time ~default:(now ()) in
            let next =
              { current with status = Mfa.Active; confirmed_at = Some confirmed_at; last_step = Some step }
            in
            Result.bind (replace_mfa_enrollment t ~current next) (fun () ->
                record_audit ~target_user_id:next.user_id ~mechanism:Audit.Mfa t
                  Audit.Mfa_enrollment (Audit.User next.user_id) Audit.Success
                  ~metadata:[ ("status", "active"); ("factor", "totp") ];
                Ok next)))

let verify_totp_factor t ?time id ~code =
  match t.store.mfa.Mfa.find id with
  | None -> Error User_not_found
  | Some enrollment when enrollment.status <> Mfa.Active -> Error (Login_rejected "MFA enrollment is not active")
  | Some current ->
    Result.bind (totp_of_enrollment t current) (fun totp ->
        Result.bind
          (Result.map_error mfa_enrollment_error
             (Mfa.verify_totp ?time ?last_step:current.last_step totp ~code))
          (fun step ->
            let next = { current with last_step = Some step } in
            Result.bind (replace_mfa_enrollment t ~current next) (fun () ->
                record_audit ~target_user_id:next.user_id ~mechanism:Audit.Mfa t Audit.Mfa_step_up
                  (Audit.User next.user_id) Audit.Success ~metadata:[ ("factor", "totp") ];
                Ok { user_id = next.user_id; assurance = Mfa.assurance [ Mfa.Totp ] })))

let disable_mfa_enrollment t uid id =
  match t.store.mfa.Mfa.find id with
  | None -> Error User_not_found
  | Some enrollment when enrollment.user_id <> uid -> Error User_not_found
  | Some current ->
    let next = { current with status = Mfa.Disabled; disabled_at = Some (now ()) } in
    Result.bind (replace_mfa_enrollment t ~current next) (fun () ->
        record_audit ~target_user_id:uid ~mechanism:Audit.Mfa t Audit.Mfa_enrollment (Audit.User uid)
          Audit.Success ~metadata:[ ("status", "disabled") ];
        Ok next)

let backup_enrollment_id uid = "backup:" ^ uid

let regenerate_backup_codes t ?count ?bytes uid =
  Result.bind (find_required_user t uid) (fun _ ->
      let mfa = mfa_service t in
      Result.bind
        (Result.map_error mfa_enrollment_error (Mfa.generate_backup_codes mfa ?count ?bytes ()))
        (fun generated ->
          Result.bind
            (Result.map_error mfa_enrollment_error
               (Mfa.enrollment ~id:(backup_enrollment_id uid) ~user_id:uid ~factor:Mfa.Backup_code
                  ~status:Mfa.Active ~backup_hashes:generated.hashes ()))
            (fun enrollment ->
              Result.bind
                (Result.map_error (fun e -> Store_error e) (t.store.mfa.Mfa.upsert enrollment))
                (fun () ->
                  record_audit ~target_user_id:uid ~mechanism:Audit.Mfa t Audit.Mfa_enrollment
                    (Audit.User uid) Audit.Success ~metadata:[ ("factor", "backup_code") ];
                  Ok { enrollment; codes = generated.codes }))))

let consume_backup_code t uid ~code =
  match t.store.mfa.Mfa.find (backup_enrollment_id uid) with
  | None -> Error User_not_found
  | Some enrollment when enrollment.status <> Mfa.Active -> Error (Login_rejected "Backup codes are not active")
  | Some current ->
    let mfa = mfa_service t in
    Result.bind
      (Result.map_error mfa_enrollment_error (Mfa.consume_backup_code mfa ~hashes:current.backup_hashes ~code))
      (fun (_, remaining) ->
        let next = { current with backup_hashes = remaining } in
        Result.bind (replace_mfa_enrollment t ~current next) (fun () ->
            record_audit ~target_user_id:uid ~mechanism:Audit.Mfa t Audit.Mfa_step_up (Audit.User uid)
              Audit.Success ~metadata:[ ("factor", "backup_code") ];
            Ok { user_id = uid; assurance = Mfa.assurance [ Mfa.Backup_code ] }))
let create_org t ?(now = now) ?status ?domains ?policy ~id ~name () =
  Result.bind
    (Result.map_error (fun e -> Login_rejected (Org.string_of_error e)) (Org.org ?status ?domains ?policy ~id ~name ()))
    (fun org ->
      Result.bind (Result.map_error (fun e -> Store_error e) (t.store.orgs.Org.upsert_org org)) (fun () ->
          record_audit ~at:(now ()) ~org_id:org.id ~mechanism:Audit.Org t Audit.Org_policy_change Audit.Anonymous
            Audit.Success ~metadata:[ ("action", "create_org") ];
          Ok org))

let add_org_member t ?(now = now) ?status ?role ?external_id ~org_id ~user_id () =
  match t.store.orgs.Org.find_org org_id with
  | None -> Error (Login_rejected "Organization not found")
  | Some _ ->
    Result.bind (find_required_user t user_id) (fun _ ->
        Result.bind
          (Result.map_error (fun e -> Login_rejected (Org.string_of_error e))
             (Org.membership ~now ?status ?role ?external_id ~org_id ~user_id ()))
          (fun membership ->
            Result.bind
              (Result.map_error (fun e -> Store_error e) (t.store.orgs.Org.upsert_membership membership))
              (fun () ->
                record_audit ~at:(now ()) ~target_user_id:user_id ~org_id ~mechanism:Audit.Org t
                  Audit.Org_policy_change (Audit.User user_id) Audit.Success ~metadata:[ ("action", "add_member") ];
                Ok membership)))

let invite_hash t token = sha256_hex (t.secret ^ "\000accounts-invite\000" ^ token)

let issue_org_invite t ?(now = now) ?ttl ~org_id ~email ~role () =
  match t.store.orgs.Org.find_org org_id with
  | None -> Error (Login_rejected "Organization not found")
  | Some _ ->
    let token = random_id ~bytes:24 () in
    Result.bind
      (Result.map_error (fun e -> Login_rejected (Org.string_of_error e))
         (Org.invite ~now ?ttl ~id:(random_id ()) ~org_id ~email ~role ~token_hash:(invite_hash t token) ()))
      (fun invite ->
        Result.bind
          (Result.map_error (fun e -> Store_error e) (t.store.orgs.Org.upsert_invite invite))
          (fun () ->
            record_audit ~at:(now ()) ~org_id ~mechanism:Audit.Org t Audit.Org_policy_change Audit.Anonymous Audit.Success
              ~metadata:[ ("action", "issue_invite") ];
            Ok { invite; token }))

let accept_org_invite t ?(now = now) token ~user_id =
  Result.bind (find_required_user t user_id) (fun user ->
      let token_hash = invite_hash t token in
      match
        t.store.orgs.Org.list_invites ()
        |> List.find_opt (fun (invite : Org.invite) -> constant_eq invite.token_hash token_hash)
      with
      | None -> Error (Login_rejected "Invite not found")
      | Some invite when invite.status <> Org.Invite_pending -> Error (Login_rejected "Invite is not pending")
      | Some invite when invite.expires_at <= now () -> Error (Login_rejected "Invite has expired")
      | Some invite
        when not (List.exists (fun email -> normalize_email email.address = invite.email) user.emails) ->
        Error (Login_rejected "Invite email does not belong to this user")
      | Some invite ->
        Result.bind
          (add_org_member t ~now ~role:invite.role ~org_id:invite.org_id ~user_id ())
          (fun membership ->
            let invite = { invite with status = Org.Invite_accepted; accepted_at = Some (now ()) } in
            Result.bind
              (Result.map_error (fun e -> Store_error e) (t.store.orgs.Org.upsert_invite invite))
              (fun () -> Ok membership)))

let user_has_email user address =
  let address = Email.address_to_string address in
  List.exists (fun e -> normalize_email e.address = address) user.emails

let issue_email_verification t ?ttl uid raw_email =
  match Email.normalize raw_email with
  | Error e -> Error (email_error e)
  | Ok address -> (
    match find_required_user t uid with
    | Error _ as e -> e
    | Ok user ->
      if not (user_has_email user address) then Error (Invalid_user "Email is not on this user")
      else
        let email = email_service t ?ttl () in
        Result.map_error email_error (Email.issue_verification email (Email.binding ~user_id:uid address)))

let mark_email_verified user address =
  let address = Email.address_to_string address in
  let seen = ref false in
  let emails =
    List.map
      (fun e ->
        if normalize_email e.address = address then (
          seen := true;
          { address; verified = true })
        else e)
      user.emails
  in
  if !seen then Ok { user with emails } else Error (Invalid_user "Email is not on this user")

let verified_email_key address =
  match Identity.email ~verified:true (Email.address_to_string address) with
  | Ok key -> Ok key
  | Error e -> Error (Login_rejected (Identity.string_of_error e))

let verify_email t token =
  let email = email_service t () in
  match Email.consume_verification email token with
  | Error e -> Error (email_error e)
  | Ok record -> (
    match (record.Challenge.metadata.user_id, record.Challenge.metadata.email) with
    | None, _ -> Error (Login_rejected "Email verification did not bind a user")
    | _, None -> Error (Login_rejected "Email verification did not bind an email address")
    | Some uid, Some raw_email -> (
      match Email.normalize raw_email with
      | Error e -> Error (email_error e)
      | Ok address ->
        Result.bind (find_required_user t uid) (fun user ->
                Result.bind (mark_email_verified user address) (fun updated ->
                    Result.bind (verified_email_key address) (fun key ->
                        let created_at = now () in
                        Result.map
                          (fun user ->
                            record_audit ~target_user_id:uid ~mechanism:Audit.Email t
                              Audit.Email_verification (Audit.User uid) Audit.Success;
                            user)
                          (update_user_after_attach t key
                             (attach_identity_for_update t.store.identities ~verified_at:created_at
                                ~created_at ~user_id:uid key)
                             updated))))))

let issue_password_reset t ?ttl raw_email =
  match Email.normalize raw_email with
  | Error e -> Error (email_error e)
  | Ok address -> (
    let email = Email.address_to_string address in
    match t.store.users.find_user_by_email email with
    | Error _ as e -> e
    | Ok None -> Ok None
    | Ok (Some user) ->
      let challenges = challenge_service t ?ttl () in
      let metadata = { Challenge.empty_metadata with user_id = Some user.id; email = Some email } in
      Result.map_error
        (fun e -> Login_rejected (Challenge.string_of_error e))
        (Challenge.create challenges ~purpose:Challenge.Password_reset ~metadata ())
      |> Result.map (fun issued ->
             Some ({ token = issued.Challenge.token; record = issued.record; user } : password_reset)))

let reset_password_user t token ~password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher ->
    let challenges = challenge_service t () in
    match Challenge.consume challenges ~purpose:Challenge.Password_reset token with
    | Error e -> Error (Login_rejected (Challenge.string_of_error e))
    | Ok record -> (
      match record.Challenge.metadata.user_id with
      | None -> Error (Login_rejected "Password reset did not bind a user")
      | Some uid ->
        Result.bind (find_required_user t uid) (fun reset_user ->
            match
              validate_password_policy t ?email:(primary_email reset_user) ?username:reset_user.username
                password
            with
            | Error _ as e -> e
            | Ok () ->
              Result.bind (t.store.users.set_password_hash_and_bump uid (hasher.hash ~password)) (fun _ ->
                Result.bind (find_required_user t uid) (fun user ->
                    ensure_password_identity t user;
                    record_audit ~target_user_id:uid ~mechanism:Audit.Password t Audit.Password_reset
                      (Audit.User uid) Audit.Success;
                    Ok user)))
      )
let issue_enrollment t ?ttl uid =
  Result.bind (find_required_user t uid) (fun user ->
      match t.store.users.password_hash uid with
      | Error _ as e -> e
      | Ok (Some _) -> Error (Login_rejected "User already has a password")
      | Ok None ->
        let challenges = challenge_service t ?ttl () in
        let metadata =
          {
            Challenge.empty_metadata with
            user_id = Some uid;
            email = primary_email user;
            data = [ ("kind", Bson.str "enrollment") ];
          }
        in
        Result.map_error
          (fun e -> Login_rejected (Challenge.string_of_error e))
          (Challenge.create challenges ~purpose:Challenge.Recovery ~metadata ())
        |> Result.map (fun issued ->
               ({ token = issued.Challenge.token; record = issued.record; user } : enrollment)))

let consume_enrollment t token =
  let challenges = challenge_service t () in
  match Challenge.consume challenges ~purpose:Challenge.Recovery token with
  | Error e -> Error (Login_rejected (Challenge.string_of_error e))
  | Ok record -> (
    match (record.Challenge.metadata.user_id, List.assoc_opt "kind" record.Challenge.metadata.data) with
    | Some uid, Some (Bson.String "enrollment") -> Ok uid
    | _ -> Error (Login_rejected "Enrollment token did not bind a user"))

(* ---- delivering the account emails — Meteor's Accounts.sendVerificationEmail / sendResetPasswordEmail /
   sendEnrollmentEmail. Each issues the challenge, builds the action URL (base = FENNEC_URL, default a
   localhost dev URL; pass [~link] for a custom route), renders the configured template, and submits via
   the ambient {!Fennec_mail} transport. Errors come back as a human string. ---- *)

let mail_base_url () =
  match Sys.getenv_opt "FENNEC_URL" with Some u when String.trim u <> "" -> String.trim u | _ -> "http://localhost"

(* RFC 3986 percent-encoding of the token for a query value (challenge tokens are opaque strings) *)
let pct_encode s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> Buffer.add_char b c
      | c -> Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents b

let default_link path ~token = mail_base_url () ^ path ^ "?token=" ^ pct_encode token

let deliver t (select : Mailer.templates -> Mailer.template) ~email ~url =
  match t.email_templates with
  | None -> Error "Accounts: email templates not configured (call set_email_templates first)"
  | Some tpls -> (
    let msg = Mailer.message ~from:tpls.Mailer.from ~site_name:tpls.Mailer.site_name (select tpls) ~email ~url in
    match Fennec_mail.send msg with Ok () -> Ok () | Error e -> Error (Fennec_mail.string_of_error e))

let send_verification_email t ?(link = default_link "/verify-email") uid =
  match find_required_user t uid with
  | Error e -> Error (string_of_error e)
  | Ok user -> (
    match primary_email user with
    | None -> Error "Accounts: user has no email address to verify"
    | Some email -> (
      match issue_email_verification t ~ttl:t.config.verify_token_lifetime uid email with
      | Error e -> Error (string_of_error e)
      | Ok issued ->
        deliver t (fun tp -> tp.Mailer.verify_email) ~email ~url:(link ~token:(Challenge.token_to_string issued.Email.token))))

let send_reset_password_email t ?(link = default_link "/reset-password") email =
  match issue_password_reset t ~ttl:t.config.reset_token_lifetime email with
  | Error e -> Error (string_of_error e)
  | Ok None -> Ok () (* non-enumerating: never reveal whether the address has an account *)
  | Ok (Some pr) ->
    deliver t (fun tp -> tp.Mailer.reset_password) ~email ~url:(link ~token:(Challenge.token_to_string pr.token))

(* Meteor's Accounts.createUserVerifyingEmail: create the user, then send a verification email to that
   address in one step. The user IS created on success; delivery is best-effort (a failure does not undo
   the user — they can re-request verification), so the caller sees the [create_user] error type. *)
let create_user_verifying_email t ?id ?username ~email ?password ?profile ?link () =
  Result.map
    (fun user ->
      ignore (send_verification_email t ?link user.id);
      user)
    (create_user t ?id ?username ~email ?password ?profile ())

(* Passwordless: issue a one-time CODE bound to [email], deliver it via the [login_code] template, and
   return the challenge token to pair with the code at login. Always issues (any email — passwordless
   permits signup), so it is naturally non-enumerating. TTL defaults to 10 min — the right window for a
   login code (unlike reset/enrollment links). *)
let send_login_token_email t ?(ttl = 600.) email =
  match Email.normalize email with
  | Error e -> Error (string_of_error (email_error e))
  | Ok address -> (
    let svc = email_service t () in
    match Email.issue_otp svc ~ttl (Email.binding address) with
    | Error e -> Error (string_of_error (email_error e))
    | Ok issued ->
      Result.map
        (fun () -> Challenge.token_to_string issued.Email.token)
        (deliver t (fun tp -> tp.Mailer.login_code) ~email:(Email.address_to_string address) ~url:issued.Email.code))

let send_enrollment_email t ?(link = default_link "/enroll-account") uid =
  match issue_enrollment t ~ttl:t.config.enroll_token_lifetime uid with
  | Error e -> Error (string_of_error e)
  | Ok en -> (
    match primary_email en.user with
    | None -> Error "Accounts: user has no email address for enrollment"
    | Some email ->
      deliver t (fun tp -> tp.Mailer.enroll_account) ~email ~url:(link ~token:(Challenge.token_to_string en.token)))

let enroll_account_user t token ~password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher ->
    Result.bind (consume_enrollment t token) (fun uid ->
        Result.bind (find_required_user t uid) (fun user ->
            Result.bind
              (validate_password_policy t ?email:(primary_email user) ?username:user.username password)
              (fun () ->
                Result.bind (t.store.users.set_password_hash_and_bump uid (hasher.hash ~password)) (fun _ ->
                    Result.bind (find_required_user t uid) (fun user ->
                        ensure_password_identity t user;
                        record_audit ~target_user_id:uid ~mechanism:Audit.Password t
                          (Audit.Custom "enrollment") (Audit.User uid) Audit.Success;
                        Ok user)))))
