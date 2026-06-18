(* accounts_codec.ml — BSON (de)serialization of the account record types, over the
   {!Accounts_base} type model. Extracted from accounts.ml. *)

open Accounts_base

  let org_membership_doc_id ~org_id ~user_id = org_id ^ "\000" ^ user_id
  let scim_doc_id ~connection_id ~external_id = connection_id ^ "\000" ^ external_id

  let email_to_doc e = Bson.doc [ ("address", Bson.str e.address); ("verified", Bson.bool e.verified) ]

  let email_of_doc = function
    | Bson.Document _ as d -> (
      match (doc_get_string d "address", doc_get_bool d "verified") with
      | Some address, Some verified -> Ok { address = normalize_email address; verified }
      | _ -> Error (Store_error "Malformed Accounts email document"))
    | _ -> Error (Store_error "Malformed Accounts email document")

  let service_doc services = Bson.doc services

  let user_to_doc ?password_hash (u : user) =
    let fields =
      [
        ("_id", Bson.str u.id);
        ("id", Bson.str u.id);
        ("emails", Bson.array (List.map email_to_doc u.emails));
        ("roles", Bson.array (List.map (fun role -> Bson.str (Roles.Role.name role)) u.roles));
        ("services", service_doc u.services);
        ("createdAt", Bson.float u.created_at);
        ("updatedAt", Bson.float u.updated_at);
        ("authEpoch", Bson.int u.auth_epoch);
        ("status", Bson.str (string_of_user_status u.status));
      ]
    in
    (* [usernameLower] / [emailsLower] are derived, normalized shadow fields that the unique indexes and the
       login lookups key on: [username] stays case-preserving for display while its lowercased twin makes
       lookup + uniqueness case-insensitive. They are write-only (never decoded back into {!user}). Both are
       TOP-LEVEL (emailsLower an array of scalars) so they index AND match on every backend — a dotted
       [emails.address] resolves to nothing on minimongo/burrow, so it could neither be queried nor indexed
       there. [emailsLower] is omitted when there are no emails so the SPARSE unique index skips email-less
       users (an empty array would otherwise collide them on a shared key). *)
    let fields =
      match u.username with
      | Some username ->
        ("username", Bson.str username) :: ("usernameLower", Bson.str (normalize_username username)) :: fields
      | None -> fields
    in
    let fields =
      match u.emails with
      | [] -> fields
      | emails -> ("emailsLower", Bson.array (List.map (fun e -> Bson.str (normalize_email e.address)) emails)) :: fields
    in
    let fields = match u.profile with Some profile -> ("profile", profile) :: fields | None -> fields in
    let fields =
      match password_hash with Some hash -> ("passwordHash", Bson.str hash) :: fields | None -> fields
    in
    Bson.doc (List.rev fields)

  let set_fields doc =
    Bson.fields doc |> List.filter (fun (k, _) -> k <> "_id") |> Bson.doc

  (* Login-token row. [_id] is the session [sid]; [hashedToken] is the only secret-bearing field
     (the raw token is never stored). [session_info_of_doc] drops [hashedToken] — listings never
     surface it. *)
  let token_to_doc (s : session_info) ~hashed =
    Bson.doc
      [
        ("_id", Bson.str s.session_id);
        ("userId", Bson.str s.user_id);
        ("hashedToken", Bson.str hashed);
        ("createdAt", Bson.float s.created_at);
        ("expiresAt", Bson.float s.expires_at);
        ("lastActiveAt", Bson.float s.last_active_at);
        ("strategy", Bson.str s.strategy);
      ]

  let session_info_of_doc = function
    | Bson.Document _ as d -> (
      match
        (doc_get_string d "_id", doc_get_string d "userId", opt_float (Bson.get d "createdAt"),
         opt_float (Bson.get d "expiresAt"))
      with
      | Some session_id, Some user_id, Some created_at, Some expires_at ->
        Some
          {
            session_id;
            user_id;
            created_at;
            expires_at;
            last_active_at = Option.value ~default:created_at (opt_float (Bson.get d "lastActiveAt"));
            strategy = Option.value ~default:"" (doc_get_string d "strategy");
          }
      | _ -> None)
    | _ -> None

  let user_of_doc = function
    | Bson.Document _ as d ->
      let emails =
        match doc_get_list d "emails" with
        | None -> Ok []
        | Some xs ->
          List.fold_right
            (fun raw acc -> Result.bind (email_of_doc raw) (fun e -> Result.map (fun rest -> e :: rest) acc))
            xs (Ok [])
      in
      let roles =
        match doc_get_list d "roles" with
        | None -> Ok []
        | Some xs ->
          xs
          |> List.filter_map (function Bson.String role -> Some role | _ -> None)
          |> Roles.normalize_roles
          |> Result.map_error (fun e -> Store_error (Roles.string_of_error e))
      in
      Result.bind emails (fun emails ->
      Result.bind roles (fun roles ->
          match (doc_get_string d "_id", opt_float (Bson.get d "createdAt"), opt_float (Bson.get d "updatedAt")) with
          | Some id, Some created_at, Some updated_at ->
            Ok
              {
                id;
                username = doc_get_string d "username";
                emails;
                roles;
                profile = Bson.get d "profile";
                services = (match Bson.get d "services" with Some (Bson.Document kvs) -> kvs | _ -> []);
                created_at;
                updated_at;
                auth_epoch = Option.value ~default:0 (doc_get_int d "authEpoch");
                status =
                  (match Option.bind (doc_get_string d "status") user_status_of_string with
                  | Some status -> status
                  | None -> Active);
              }
          | _ -> Error (Store_error "Malformed Accounts user document")))
    | _ -> Error (Store_error "Malformed Accounts user document")

  let kind_name = Identity.string_of_kind
  let scope_name = Identity.string_of_scope
  let verification_name = Identity.string_of_verification

  let verification_of_string = function
    | "verified" -> Some Identity.Verified
    | "unverified" -> Some Identity.Unverified
    | _ -> None

  let split_nul s =
    match String.index_opt s '\000' with
    | None -> None
    | Some i ->
      let a = String.sub s 0 i in
      let b = String.sub s (i + 1) (String.length s - i - 1) in
      Some (a, b)

  let key_of_parts ~kind ~namespace ~subject ~verification =
    match kind with
    | Identity.Password -> Ok (Identity.password ())
    | Identity.Email ->
      let verified = verification = Some Identity.Verified in
      Result.map_error identity_error (Identity.email ~verified subject)
    | Identity.OAuth -> (
      match namespace with
      | Some provider -> Result.map_error identity_error (Identity.oauth ~provider ~subject)
      | None -> Error (Store_error "Malformed OAuth identity document"))
    | Identity.Oidc -> (
      match Option.bind namespace split_nul with
      | Some (issuer, connection) -> Result.map_error identity_error (Identity.oidc ~issuer ~connection ~subject)
      | None -> Error (Store_error "Malformed OIDC identity document"))
    | Identity.Saml -> (
      match namespace with
      | Some connection -> Result.map_error identity_error (Identity.saml ~connection ~name_id:subject ())
      | None -> Error (Store_error "Malformed SAML identity document"))
    | Identity.Passkey -> Result.map_error identity_error (Identity.passkey ~credential_id:subject ())
    | Identity.Scim -> (
      match namespace with
      | Some org_id -> Result.map_error identity_error (Identity.scim ~org_id ~external_id:subject)
      | None -> Error (Store_error "Malformed SCIM identity document"))
    | Identity.Recovery -> Result.map_error identity_error (Identity.recovery ~name:subject)

  let identity_doc_id user_id key =
    match Identity.scope key with
    | Identity.Global -> Identity.stable_key key
    | Identity.Per_user -> Identity.stable_key key ^ "\000" ^ user_id

  let identity_link_to_doc (link : Identity.link) =
    let key = link.key in
    let fields =
      [
        ("_id", Bson.str (identity_doc_id link.user_id key));
        ("stableKey", Bson.str (Identity.stable_key key));
        ("userId", Bson.str link.user_id);
        ("kind", Bson.str (kind_name (Identity.kind key)));
        ("scope", Bson.str (scope_name (Identity.scope key)));
        ("subject", Bson.str (Identity.subject key));
        ("createdAt", Bson.float link.created_at);
      ]
    in
    let fields =
      match Identity.namespace key with Some ns -> ("namespace", Bson.str ns) :: fields | None -> fields
    in
    let fields =
      match Identity.verification key with
      | Some v -> ("verification", Bson.str (verification_name v)) :: fields
      | None -> fields
    in
    let fields =
      match link.verified_at with Some t -> ("verifiedAt", Bson.float t) :: fields | None -> fields
    in
    Bson.doc (List.rev fields)

  let identity_link_of_doc = function
    | Bson.Document _ as d -> (
      match (doc_get_string d "userId", doc_get_string d "kind", doc_get_string d "subject", doc_get_float d "createdAt") with
      | Some user_id, Some kind, Some subject, Some created_at -> (
        match Identity.kind_of_string kind with
        | None -> Error (Store_error "Malformed identity kind")
        | Some kind ->
          let verification = Option.bind (doc_get_string d "verification") verification_of_string in
          Result.map
            (fun key -> Identity.link ?verified_at:(doc_get_float d "verifiedAt") ~user_id key ~created_at)
            (key_of_parts ~kind ~namespace:(doc_get_string d "namespace") ~subject ~verification))
      | _ -> Error (Store_error "Malformed identity link document"))
    | _ -> Error (Store_error "Malformed identity link document")

  let strings_of_doc_list d field =
    match doc_get_list d field with
    | None -> []
    | Some values ->
      List.filter_map (function Bson.String s -> Some s | _ -> None) values

  let passkey_to_doc (credential : Passkey.credential) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str credential.id);
           Some ("id", Bson.str credential.id);
           Some ("userId", Bson.str credential.user_id);
           Some ("userHandle", Bson.str credential.user_handle);
           Some ("publicKeyPem", Bson.str (X509.Public_key.encode_pem credential.public_key));
           Some ("signCount", Bson.int (Int32.to_int credential.sign_count));
           Some ("backupEligible", Bson.bool credential.backup_eligible);
           Some ("backedUp", Bson.bool credential.backed_up);
           Some ("transports", Bson.array (List.map Bson.str credential.transports));
           Some ("createdAt", Bson.float credential.created_at);
           Option.map (fun v -> ("lastUsedAt", Bson.float v)) credential.last_used_at;
         ])

  let passkey_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "_id",
          doc_get_string d "userId",
          doc_get_string d "userHandle",
          doc_get_string d "publicKeyPem",
          doc_get_int d "signCount",
          doc_get_bool d "backupEligible",
          doc_get_bool d "backedUp",
          doc_get_float d "createdAt" )
      with
      | ( Some id,
          Some user_id,
          Some user_handle,
          Some public_key_pem,
          Some sign_count,
          Some backup_eligible,
          Some backed_up,
          Some created_at ) -> (
        match X509.Public_key.decode_pem public_key_pem with
        | Ok public_key ->
          Ok
            {
              Passkey.id;
              user_id;
              user_handle;
              public_key;
              sign_count = Int32.of_int sign_count;
              backup_eligible;
              backed_up;
              transports = strings_of_doc_list d "transports";
              created_at;
              last_used_at = doc_get_float d "lastUsedAt";
            }
        | Error (`Msg msg) -> Error (Store_error ("Malformed passkey public key: " ^ msg)))
      | _ -> Error (Store_error "Malformed passkey credential document"))
    | _ -> Error (Store_error "Malformed passkey credential document")

  let org_status_to_string = function Org.Active -> "active" | Suspended -> "suspended" | Deleted -> "deleted"
  let org_status_of_string = function
    | "active" -> Some Org.Active
    | "suspended" -> Some Suspended
    | "deleted" -> Some Deleted
    | _ -> None

  let membership_status_to_string = function
    | Org.Invited -> "invited"
    | Active_member -> "active"
    | Disabled -> "disabled"
    | Removed -> "removed"

  let membership_status_of_string = function
    | "invited" -> Some Org.Invited
    | "active" -> Some Active_member
    | "disabled" -> Some Disabled
    | "removed" -> Some Removed
    | _ -> None

  let invite_status_to_string = function
    | Org.Invite_pending -> "pending"
    | Invite_accepted -> "accepted"
    | Invite_revoked -> "revoked"

  let invite_status_of_string = function
    | "pending" -> Some Org.Invite_pending
    | "accepted" -> Some Invite_accepted
    | "revoked" -> Some Invite_revoked
    | _ -> None

  let domain_to_doc (domain : Org.domain) =
    Bson.doc
      [
        ("name", Bson.str domain.name);
        ("verified", Bson.bool domain.verified);
        ("primary", Bson.bool domain.primary);
        ("connectionIds", Bson.array (List.map Bson.str domain.connection_ids));
      ]

  let domain_of_doc = function
    | Bson.Document _ as d -> (
      match doc_get_string d "name" with
      | Some name ->
        Ok
          {
            Org.name;
            verified = Option.value ~default:false (doc_get_bool d "verified");
            primary = Option.value ~default:false (doc_get_bool d "primary");
            connection_ids = strings_of_doc_list d "connectionIds";
          }
      | None -> Error (Store_error "Malformed org domain document"))
    | _ -> Error (Store_error "Malformed org domain document")

  let sso_policy_to_doc = function
    | Org.Sso_optional -> Bson.doc [ ("kind", Bson.str "optional") ]
    | Sso_required { connection_ids; allow_password_fallback; allow_jit } ->
      Bson.doc
        [
          ("kind", Bson.str "required");
          ("connectionIds", Bson.array (List.map Bson.str connection_ids));
          ("allowPasswordFallback", Bson.bool allow_password_fallback);
          ("allowJit", Bson.bool allow_jit);
        ]

  let sso_policy_of_doc = function
    | Some (Bson.Document _ as d) -> (
      match doc_get_string d "kind" with
      | Some "optional" -> Ok Org.Sso_optional
      | Some "required" ->
        Ok
          (Org.Sso_required
             {
               connection_ids = strings_of_doc_list d "connectionIds";
               allow_password_fallback = Option.value ~default:false (doc_get_bool d "allowPasswordFallback");
               allow_jit = Option.value ~default:false (doc_get_bool d "allowJit");
             })
      | _ -> Error (Store_error "Malformed org sso policy document"))
    | _ -> Error (Store_error "Malformed org sso policy document")

  let mfa_policy_to_string = function
    | Org.Mfa_optional -> "optional"
    | Mfa_required -> "required"
    | Phishing_resistant_mfa_required -> "phishing_resistant_required"

  let mfa_policy_of_string = function
    | "optional" -> Some Org.Mfa_optional
    | "required" -> Some Mfa_required
    | "phishing_resistant_required" -> Some Phishing_resistant_mfa_required
    | _ -> None

  let auth_policy_to_doc (policy : Org.auth_policy) =
    Bson.doc
      [
        ("sso", sso_policy_to_doc policy.sso);
        ("mfa", Bson.str (mfa_policy_to_string policy.mfa));
        ("allowPublicSignup", Bson.bool policy.allow_public_signup);
      ]

  let auth_policy_of_doc = function
    | Some (Bson.Document _ as d) ->
      Result.bind (sso_policy_of_doc (Bson.get d "sso")) (fun sso ->
          match Option.bind (doc_get_string d "mfa") mfa_policy_of_string with
          | None -> Error (Store_error "Malformed org auth policy document")
          | Some mfa ->
            Ok
              {
                Org.sso;
                mfa;
                allow_public_signup = Option.value ~default:false (doc_get_bool d "allowPublicSignup");
              })
    | _ -> Error (Store_error "Malformed org auth policy document")

  let org_to_doc (org : Org.org) =
    Bson.doc
      [
        ("_id", Bson.str org.id);
        ("id", Bson.str org.id);
        ("name", Bson.str org.name);
        ("status", Bson.str (org_status_to_string org.status));
        ("domains", Bson.array (List.map domain_to_doc org.domains));
        ("policy", auth_policy_to_doc org.policy);
      ]

  let org_of_doc = function
    | Bson.Document _ as d -> (
      match (doc_get_string d "_id", doc_get_string d "name", Option.bind (doc_get_string d "status") org_status_of_string) with
      | Some id, Some name, Some status ->
        let domains =
          match doc_get_list d "domains" with
          | None -> Ok []
          | Some values ->
            List.fold_right
              (fun value acc -> Result.bind (domain_of_doc value) (fun domain -> Result.map (fun xs -> domain :: xs) acc))
              values (Ok [])
        in
        Result.bind domains (fun domains ->
            Result.map
              (fun policy -> { Org.id; name; status; domains; policy })
              (auth_policy_of_doc (Bson.get d "policy")))
      | _ -> Error (Store_error "Malformed org document"))
    | _ -> Error (Store_error "Malformed org document")

  let membership_to_doc (membership : Org.membership) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str (org_membership_doc_id ~org_id:membership.org_id ~user_id:membership.user_id));
           Some ("orgId", Bson.str membership.org_id);
           Some ("userId", Bson.str membership.user_id);
           Some ("role", Bson.str membership.role);
           Some ("status", Bson.str (membership_status_to_string membership.status));
           Option.map (fun v -> ("externalId", Bson.str v)) membership.external_id;
           Some ("createdAt", Bson.float membership.created_at);
           Option.map (fun v -> ("updatedAt", Bson.float v)) membership.updated_at;
         ])

  let membership_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "orgId",
          doc_get_string d "userId",
          doc_get_string d "role",
          Option.bind (doc_get_string d "status") membership_status_of_string,
          doc_get_float d "createdAt" )
      with
      | Some org_id, Some user_id, Some role, Some status, Some created_at ->
        Ok
          {
            Org.org_id;
            user_id;
            role;
            status;
            external_id = doc_get_string d "externalId";
            created_at;
            updated_at = doc_get_float d "updatedAt";
          }
      | _ -> Error (Store_error "Malformed org membership document"))
    | _ -> Error (Store_error "Malformed org membership document")

  let invite_to_doc (invite : Org.invite) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str invite.id);
           Some ("orgId", Bson.str invite.org_id);
           Some ("email", Bson.str invite.email);
           Some ("role", Bson.str invite.role);
           Some ("tokenHash", Bson.str invite.token_hash);
           Some ("status", Bson.str (invite_status_to_string invite.status));
           Some ("createdAt", Bson.float invite.created_at);
           Some ("expiresAt", Bson.float invite.expires_at);
           Option.map (fun v -> ("acceptedAt", Bson.float v)) invite.accepted_at;
           Option.map (fun v -> ("revokedAt", Bson.float v)) invite.revoked_at;
         ])

  let invite_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "_id",
          doc_get_string d "orgId",
          doc_get_string d "email",
          doc_get_string d "role",
          doc_get_string d "tokenHash",
          Option.bind (doc_get_string d "status") invite_status_of_string,
          doc_get_float d "createdAt",
          doc_get_float d "expiresAt" )
      with
      | Some id, Some org_id, Some email, Some role, Some token_hash, Some status, Some created_at, Some expires_at
        ->
        Ok
          {
            Org.id;
            org_id;
            email;
            role;
            token_hash;
            status;
            created_at;
            expires_at;
            accepted_at = doc_get_float d "acceptedAt";
            revoked_at = doc_get_float d "revokedAt";
          }
      | _ -> Error (Store_error "Malformed org invite document"))
    | _ -> Error (Store_error "Malformed org invite document")

  (* delegate to the single source of factor naming in {!Accounts_mfa} *)
  let mfa_factor_to_string = Mfa.factor_to_name
  let mfa_factor_of_string = Mfa.factor_of_name

  let enrollment_status_to_string = function
    | Mfa.Pending -> "pending"
    | Active -> "active"
    | Disabled -> "disabled"

  let enrollment_status_of_string = function
    | "pending" -> Some Mfa.Pending
    | "active" -> Some Active
    | "disabled" -> Some Disabled
    | _ -> None

  let mfa_enrollment_to_doc (e : Mfa.enrollment) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str e.id);
           Some ("userId", Bson.str e.user_id);
           Some ("factor", Bson.str (mfa_factor_to_string e.factor));
           Option.map (fun v -> ("label", Bson.str v)) e.label;
           Some ("status", Bson.str (enrollment_status_to_string e.status));
           Option.map (fun v -> ("secret", Bson.str v)) e.secret;
           Some ("backupHashes", Bson.array (List.map Bson.str e.backup_hashes));
           Option.map (fun v -> ("lastStep", Bson.int64 v)) e.last_step;
           Some ("createdAt", Bson.float e.created_at);
           Option.map (fun v -> ("confirmedAt", Bson.float v)) e.confirmed_at;
           Option.map (fun v -> ("disabledAt", Bson.float v)) e.disabled_at;
         ])

  let doc_get_int64 d k =
    match Bson.get d k with
    | Some (Bson.Int64 v) -> Some v
    | Some (Bson.Int v) -> Some (Int64.of_int v)
    | _ -> None

  let mfa_enrollment_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "_id",
          doc_get_string d "userId",
          Option.bind (doc_get_string d "factor") mfa_factor_of_string,
          Option.bind (doc_get_string d "status") enrollment_status_of_string,
          doc_get_float d "createdAt" )
      with
      | Some id, Some user_id, Some factor, Some status, Some created_at ->
        Ok
          {
            Mfa.id;
            user_id;
            factor;
            label = doc_get_string d "label";
            status;
            secret = doc_get_string d "secret";
            backup_hashes = strings_of_doc_list d "backupHashes";
            last_step = doc_get_int64 d "lastStep";
            created_at;
            confirmed_at = doc_get_float d "confirmedAt";
            disabled_at = doc_get_float d "disabledAt";
          }
      | _ -> Error (Store_error "Malformed MFA enrollment document"))
    | _ -> Error (Store_error "Malformed MFA enrollment document")

  let scim_connection_to_doc (connection : Scim.connection) =
    Bson.doc
      [
        ("_id", Bson.str connection.id);
        ("orgId", Bson.str connection.org_id);
        ("tokenHash", Bson.str connection.token_hash);
        ("allowDeprovision", Bson.bool connection.allow_deprovision);
        ("defaultRole", Bson.str connection.default_role);
      ]

  let scim_connection_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "_id",
          doc_get_string d "orgId",
          doc_get_string d "tokenHash",
          doc_get_bool d "allowDeprovision",
          doc_get_string d "defaultRole" )
      with
      | Some id, Some org_id, Some token_hash, Some allow_deprovision, Some default_role ->
        Ok { Scim.id; org_id; token_hash; allow_deprovision; default_role }
      | _ -> Error (Store_error "Malformed SCIM connection document"))
    | _ -> Error (Store_error "Malformed SCIM connection document")

  let scim_user_to_doc ~connection_id (user : Scim.user) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str (scim_doc_id ~connection_id ~external_id:user.external_id));
           Some ("connectionId", Bson.str connection_id);
           Option.map (fun v -> ("id", Bson.str v)) user.id;
           Some ("externalId", Bson.str user.external_id);
           Some ("userName", Bson.str user.user_name);
           Some ("active", Bson.bool user.active);
           Some ("emails", Bson.array (List.map Bson.str user.emails));
           Option.map (fun v -> ("displayName", Bson.str v)) user.display_name;
           Some ("groups", Bson.array (List.map Bson.str user.groups));
         ])

  let scim_user_of_doc = function
    | Bson.Document _ as d -> (
      match
        ( doc_get_string d "externalId",
          doc_get_string d "userName",
          doc_get_bool d "active" )
      with
      | Some external_id, Some user_name, Some active ->
        Ok
          {
            Scim.id = doc_get_string d "id";
            external_id;
            user_name;
            active;
            emails = strings_of_doc_list d "emails";
            display_name = doc_get_string d "displayName";
            groups = strings_of_doc_list d "groups";
          }
      | _ -> Error (Store_error "Malformed SCIM user document"))
    | _ -> Error (Store_error "Malformed SCIM user document")

  let scim_group_to_doc ~connection_id (group : Scim.group) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str (scim_doc_id ~connection_id ~external_id:group.external_id));
           Some ("connectionId", Bson.str connection_id);
           Option.map (fun v -> ("id", Bson.str v)) group.id;
           Some ("externalId", Bson.str group.external_id);
           Some ("displayName", Bson.str group.display_name);
           Some ("members", Bson.array (List.map Bson.str group.members));
         ])

  let scim_group_of_doc = function
    | Bson.Document _ as d -> (
      match (doc_get_string d "externalId", doc_get_string d "displayName") with
      | Some external_id, Some display_name ->
        Ok
          {
            Scim.id = doc_get_string d "id";
            external_id;
            display_name;
            members = strings_of_doc_list d "members";
          }
      | _ -> Error (Store_error "Malformed SCIM group document"))
    | _ -> Error (Store_error "Malformed SCIM group document")

  let challenge_metadata_to_doc (m : Challenge.metadata) =
    Bson.doc
      (List.filter_map Fun.id
         [
           Option.map (fun v -> ("userId", Bson.str v)) m.user_id;
           Option.map (fun v -> ("email", Bson.str v)) m.email;
           Option.map (fun v -> ("orgId", Bson.str v)) m.org_id;
           Option.map (fun v -> ("connectionId", Bson.str v)) m.connection_id;
           Option.map (fun v -> ("redirect", Bson.str v)) m.redirect;
           Some ("data", Bson.doc m.data);
         ])

  let challenge_metadata_of_doc = function
    | Some (Bson.Document _ as d) ->
      Ok
        {
          Challenge.user_id = doc_get_string d "userId";
          email = doc_get_string d "email";
          org_id = doc_get_string d "orgId";
          connection_id = doc_get_string d "connectionId";
          redirect = doc_get_string d "redirect";
          data = (match Bson.get d "data" with Some (Bson.Document kvs) -> kvs | _ -> []);
        }
    | None -> Ok Challenge.empty_metadata
    | _ -> Error (Challenge.Store_error "Malformed challenge metadata document")

  let challenge_to_doc (r : Challenge.record) ~secret_hash =
    Bson.doc
      (List.filter_map Fun.id
         [
           Some ("_id", Bson.str r.id);
           Some ("purpose", Bson.str (Challenge.string_of_purpose r.purpose));
           Some ("secretHash", Bson.str secret_hash);
           Some ("metadata", challenge_metadata_to_doc r.metadata);
           Some ("createdAt", Bson.float r.created_at);
           Some ("expiresAt", Bson.float r.expires_at);
           Option.map (fun v -> ("consumedAt", Bson.float v)) r.consumed_at;
           Option.map (fun v -> ("revokedAt", Bson.float v)) r.revoked_at;
           Some ("attempts", Bson.int r.attempts);
           Option.map (fun v -> ("maxAttempts", Bson.int v)) r.max_attempts;
         ])

  let challenge_of_doc = function
    | Bson.Document _ as d -> (
      match (doc_get_string d "_id", Option.bind (doc_get_string d "purpose") Challenge.purpose_of_string, doc_get_float d "createdAt", doc_get_float d "expiresAt") with
      | Some id, Some purpose, Some created_at, Some expires_at ->
        Result.map
          (fun metadata ->
            {
              Challenge.id;
              purpose;
              metadata;
              created_at;
              expires_at;
              consumed_at = doc_get_float d "consumedAt";
              revoked_at = doc_get_float d "revokedAt";
              attempts = Option.value ~default:0 (doc_get_int d "attempts");
              max_attempts = doc_get_int d "maxAttempts";
            })
          (challenge_metadata_of_doc (Bson.get d "metadata"))
      | _ -> Error (Challenge.Store_error "Malformed challenge document"))
    | _ -> Error (Challenge.Store_error "Malformed challenge document")

  let secret_hash d = doc_get_string d "secretHash"

  let drop_prefix prefix s =
    let n = String.length prefix in
    if String.length s >= n && String.sub s 0 n = prefix then Some (String.sub s n (String.length s - n))
    else None

  let audit_kind_of_string = function
    | "login" -> Some Audit.Login
    | "login_failure" -> Some Audit.Login_failure
    | "logout" -> Some Audit.Logout
    | "token_resume" -> Some Audit.Token_resume
    | "password_change" -> Some Audit.Password_change
    | "password_reset" -> Some Audit.Password_reset
    | "email_verification" -> Some Audit.Email_verification
    | "email_login" -> Some Audit.Email_login
    | "passkey_registration" -> Some Audit.Passkey_registration
    | "passkey_assertion" -> Some Audit.Passkey_assertion
    | "oauth_callback" -> Some Audit.OAuth_callback
    | "oidc_callback" -> Some Audit.Oidc_callback
    | "saml_callback" -> Some Audit.Saml_callback
    | "identity_link" -> Some Audit.Identity_link
    | "identity_unlink" -> Some Audit.Identity_unlink
    | "identity_merge" -> Some Audit.Identity_merge
    | "mfa_enrollment" -> Some Audit.Mfa_enrollment
    | "mfa_step_up" -> Some Audit.Mfa_step_up
  | "recovery" -> Some Audit.Recovery
  | "scim_provision" -> Some Audit.Scim_provision
  | "scim_deprovision" -> Some Audit.Scim_deprovision
  | "role_change" -> Some Audit.Role_change
  | "org_policy_change" -> Some Audit.Org_policy_change
    | "challenge_issue" -> Some Audit.Challenge_issue
    | "challenge_consume" -> Some Audit.Challenge_consume
    | s -> Option.map (fun name -> Audit.Custom name) (drop_prefix "custom:" s)

  let audit_actor_of_string s =
    match s with
    | "anonymous" -> Some Audit.Anonymous
    | _ -> (
      match drop_prefix "user:" s with
      | Some uid -> Some (Audit.User uid)
      | None -> Option.map (fun name -> Audit.System name) (drop_prefix "system:" s))

  let audit_mechanism_of_string = function
    | "password" -> Some Audit.Password
    | "email" -> Some Audit.Email
    | "passkey" -> Some Audit.Passkey
    | "mfa" -> Some Audit.Mfa
    | "org" -> Some Audit.Org
    | "token" -> Some Audit.Token
    | "challenge" -> Some Audit.Challenge
    | s -> (
      match drop_prefix "oauth:" s with
      | Some v -> Some (Audit.OAuth v)
      | None -> (
        match drop_prefix "oidc:" s with
        | Some v -> Some (Audit.Oidc v)
        | None -> (
          match drop_prefix "saml:" s with
          | Some v -> Some (Audit.Saml v)
          | None -> (
            match drop_prefix "scim:" s with
            | Some v -> Some (Audit.Scim v)
            | None -> Option.map (fun v -> Audit.Custom_mechanism v) (drop_prefix "custom:" s)))))

  let audit_outcome_of_string = function
    | "success" -> Some Audit.Success
    | s -> Option.map (fun reason -> Audit.Failure reason) (drop_prefix "failure:" s)

  let audit_to_doc event =
    Bson.doc
      [
        ("_id", Bson.str event.Audit.id);
        ("targetUserId", Bson.str (Option.value ~default:"" event.target_user_id));
        ("orgId", Bson.str (Option.value ~default:"" event.org_id));
        ("kind", Bson.str (Audit.string_of_kind event.kind));
        ("at", Bson.float event.at);
        ("fields", Bson.doc (List.map (fun (k, v) -> (k, Bson.str v)) (Audit.to_fields event)));
      ]

  let audit_of_doc = function
    | Bson.Document _ as d -> (
      match Bson.get d "fields" with
      | Some (Bson.Document fields) ->
        let string k = match List.assoc_opt k fields with Some (Bson.String s) -> Some s | _ -> None in
        let metadata =
          List.filter_map
            (fun (k, v) ->
              match (drop_prefix "meta." k, v) with Some k, Bson.String v -> Some (k, v) | _ -> None)
            fields
        in
        (match
           ( string "id",
             Option.bind (string "at") float_of_string_opt,
             Option.bind (string "kind") audit_kind_of_string,
             Option.bind (string "actor") audit_actor_of_string,
             Option.bind (string "outcome") audit_outcome_of_string )
         with
        | Some id, Some at, Some kind, Some actor, Some outcome ->
          Some
            (Audit.event ?target_user_id:(string "target_user_id") ?org_id:(string "org_id")
               ?mechanism:(Option.bind (string "mechanism") audit_mechanism_of_string)
               ?connection_id:(string "connection_id")
               ~request:
                 (Audit.request ?request_id:(string "request_id") ?ip:(string "ip")
                    ?user_agent:(string "user_agent") ())
               ~metadata ~id ~at kind actor outcome)
        | _ -> None)
      | _ -> None)
    | _ -> None
