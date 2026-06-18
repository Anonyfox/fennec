(* accounts_collection_store.ml — the backend-blind collection store builder (one Dynamic-backed
   collection per account concern). Extracted from accounts.ml. *)

open Accounts_base
module Codec = Accounts_codec

  type collection = {
    find_one : Bson.t -> Bson.t option;
    find : Bson.t -> Bson.t list;
    insert_one : Bson.t -> (unit, string) result;
    update_one : filter:Bson.t -> update:Bson.t -> (int, string) result;
    delete_many : Bson.t -> (int, string) result;
  }

  type collections = {
    users : collection;
    tokens : collection;
    identities : collection;
    challenges : collection;
    passkeys : collection;
    orgs : collection;
    org_memberships : collection;
    org_invites : collection;
    mfa_enrollments : collection;
    scim_connections : collection;
    scim_users : collection;
    scim_groups : collection;
    audit : collection;
  }

  let with_lock mutex f =
    Mutex.lock mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

  let map_store_error = function Ok x -> Ok x | Error e -> Error (Store_error e)
  let map_challenge_error = function Ok x -> Ok x | Error e -> Error (Challenge.Store_error e)

  let user_store mutex c =
    (* Pure single-read lookups run WITHOUT [mutex]: one [find_one] is atomic on every backend on its own
       (minimongo's per-op lock / burrow's MVCC snapshot / the thread-safe mongoc pool). [mutex] exists only
       to make the multi-step read-modify-write paths below (create / update / epoch bumps) atomic; holding
       it across the login READ path is what serialized every concurrent login behind every write. *)
    let find_user filter =
      match c.users.find_one filter with
      | None -> Ok None
      | Some doc -> Result.map Option.some (Codec.user_of_doc doc)
    in
    let find_user_by_id id = find_user (id_selector id) in
    (* Indexed + case-insensitive: query the lowercased shadow fields, not a full-collection scan filtered in
       OCaml. [emailsLower] is a top-level array of normalized addresses, so a scalar selector matches any
       element on minimongo/burrow/mongod (Mongo array-element semantics). *)
    let find_user_by_email email =
      find_user (Bson.doc [ ("emailsLower", Bson.str (normalize_email email)) ])
    in
    let find_user_by_username username =
      find_user (Bson.doc [ ("usernameLower", Bson.str (normalize_username username)) ])
    in
    let find_user_by_service ~strategy ~service_id =
      find_user (Bson.doc [ ("services." ^ strategy ^ ".id", Bson.str service_id) ])
    in
    let exists_other_email id email =
      match c.users.find_one (Bson.doc [ ("emailsLower", Bson.str (normalize_email email)) ]) with
      | None -> false
      | Some doc -> doc_get_string doc "_id" <> Some id
    in
    let exists_other_username id username =
      match c.users.find_one (Bson.doc [ ("usernameLower", Bson.str (normalize_username username)) ]) with
      | None -> false
      | Some doc -> doc_get_string doc "_id" <> Some id
    in
    let create_user u ~password_hash =
      with_lock mutex (fun () ->
          match validate_user_shape u with
          | Error _ as e -> e
          | Ok () ->
            if c.users.find_one (id_selector u.id) <> None then Error (Store_error ("duplicate user id: " ^ u.id))
            else
              let duplicate_email =
                List.find_map
                  (fun e ->
                    let email = normalize_email e.address in
                    if exists_other_email u.id email then Some (Duplicate_email email) else None)
                  u.emails
              in
              match duplicate_email with
              | Some e -> Error e
              | None -> (
                match u.username with
                | Some username when exists_other_username u.id username ->
                  Error (Duplicate_username (normalize_username username))
                | _ ->
                  Result.bind (map_store_error (c.users.insert_one (Codec.user_to_doc ?password_hash u))) (fun () ->
                      Ok u)))
    in
    let update_user u =
      with_lock mutex (fun () ->
          match validate_user_shape u with
          | Error _ as e -> e
          | Ok () -> (
            match c.users.find_one (id_selector u.id) with
            | None -> Error User_not_found
            | Some existing ->
              let password_hash = doc_get_string existing "passwordHash" in
              let duplicate_email =
                List.find_map
                  (fun e ->
                    let email = normalize_email e.address in
                    if exists_other_email u.id email then Some (Duplicate_email email) else None)
                  u.emails
              in
              match duplicate_email with
              | Some e -> Error e
              | None -> (
                match u.username with
                | Some username when exists_other_username u.id username ->
                  Error (Duplicate_username (normalize_username username))
                | _ ->
                  let updated = { u with updated_at = now () } in
                  (* [emails] is always rewritten (as [] when empty), but [emailsLower] is OMITTED when empty
                     so the sparse index can skip email-less users — so when a user's last email is removed we
                     must $unset the stale shadow, or a lookup / uniqueness check would still match it. *)
                  let set = ("$set", Codec.set_fields (Codec.user_to_doc ?password_hash updated)) in
                  let update =
                    if updated.emails = [] then Bson.doc [ set; ("$unset", Bson.doc [ ("emailsLower", Bson.int 1) ]) ]
                    else Bson.doc [ set ]
                  in
                  Result.bind
                    (map_store_error (c.users.update_one ~filter:(id_selector u.id) ~update))
                    (fun n -> if n = 0 then Error User_not_found else Ok updated))))
    in
    let password_hash id =
      match c.users.find_one (id_selector id) with
      | None -> Ok None
      | Some doc -> Ok (doc_get_string doc "passwordHash")
    in
    let set_password_hash id hash =
      with_lock mutex (fun () ->
          Result.bind
            (map_store_error
               (c.users.update_one ~filter:(id_selector id) ~update:(set_doc [ ("passwordHash", Bson.str hash) ])))
            (fun n -> if n = 0 then Error User_not_found else Ok ()))
    in
    let set_password_hash_and_bump id hash =
      with_lock mutex (fun () ->
          match c.users.find_one (id_selector id) with
          | None -> Error User_not_found
          | Some doc -> (
            match Codec.user_of_doc doc with
            | Error _ as e -> e
            | Ok u ->
              let epoch = u.auth_epoch + 1 in
              Result.bind
                (map_store_error
                   (c.users.update_one ~filter:(id_selector id)
                      ~update:
                        (set_doc
                           [
                             ("passwordHash", Bson.str hash);
                             ("authEpoch", Bson.int epoch);
                             ("updatedAt", Bson.float (now ()));
                           ])))
                (fun n -> if n = 0 then Error User_not_found else Ok epoch)))
    in
    let bump_auth_epoch id =
      with_lock mutex (fun () ->
          match c.users.find_one (id_selector id) with
          | None -> Error User_not_found
          | Some doc -> (
            match Codec.user_of_doc doc with
            | Error _ as e -> e
            | Ok u ->
              let epoch = u.auth_epoch + 1 in
              Result.bind
                (map_store_error
                   (c.users.update_one ~filter:(id_selector id)
                      ~update:(set_doc [ ("authEpoch", Bson.int epoch); ("updatedAt", Bson.float (now ())) ])))
                (fun n -> if n = 0 then Error User_not_found else Ok epoch)))
    in
    { find_user_by_id; find_user_by_email; find_user_by_username; find_user_by_service; create_user; update_user; password_hash; set_password_hash; set_password_hash_and_bump; bump_auth_epoch }

  let identity_store mutex c =
    let decode_link_opt = function
      | None -> None
      | Some doc -> Result.to_option (Codec.identity_link_of_doc doc)
    in
    let decode_links docs =
      List.fold_right
        (fun doc acc ->
          match (Codec.identity_link_of_doc doc, acc) with
          | Ok link, links -> link :: links
          | Error _, links -> links)
        docs []
    in
    let find key =
      with_lock mutex (fun () ->
          match Identity.scope key with
          | Identity.Per_user -> None
          | Identity.Global -> decode_link_opt (c.identities.find_one (id_selector (Identity.stable_key key))))
    in
    let list ?user_id () =
      with_lock mutex (fun () ->
          let filter =
            match user_id with Some id -> Bson.doc [ ("userId", Bson.str id) ] | None -> Bson.doc []
          in
          decode_links (c.identities.find filter))
    in
    let attach ?verified_at ~created_at ~user_id key =
      with_lock mutex (fun () ->
          let exact =
            decode_link_opt
              (c.identities.find_one (Bson.doc [ ("_id", Bson.str (Codec.identity_doc_id user_id key)) ]))
          in
          let existing =
            match exact with
            | Some _ as found -> found
            | None ->
              if Identity.scope key = Identity.Per_user then None
              else
                decode_link_opt (c.identities.find_one (id_selector (Identity.stable_key key)))
          in
          match Identity.plan_link ?verified_at ~created_at ~user_id key ~existing with
          | Identity.Attach link as plan ->
            (match c.identities.insert_one (Codec.identity_link_to_doc link) with Ok () -> plan | Error _ -> Identity.Conflict link)
          | Identity.Already_linked _ as plan -> plan
          | Identity.Conflict _ as plan -> plan)
    in
    let detach ?allow_last ~user_id key =
      with_lock mutex (fun () ->
          let links = decode_links (c.identities.find (Bson.doc [ ("userId", Bson.str user_id) ])) in
          match Identity.plan_detach ?allow_last ~user_id key ~links with
          | Identity.Detach link as plan ->
            ignore (c.identities.delete_many (id_selector (Codec.identity_doc_id link.user_id link.key)));
            plan
          | Identity.Link_not_found as plan -> plan
          | Identity.Reject_last_credential as plan -> plan)
    in
    let merge ~from_user_id ~into_user_id =
      with_lock mutex (fun () ->
          let source = decode_links (c.identities.find (Bson.doc [ ("userId", Bson.str from_user_id) ])) in
          let target = decode_links (c.identities.find (Bson.doc [ ("userId", Bson.doc [ ("$ne", Bson.str from_user_id) ]) ])) in
          let plan = Identity.plan_merge ~from_user_id ~into_user_id ~source ~target in
          match plan.Identity.conflicts with
          | _ :: _ as conflicts -> Error conflicts
          | [] ->
            ignore (c.identities.delete_many (Bson.doc [ ("userId", Bson.str from_user_id) ]));
            List.iter (fun link -> ignore (c.identities.insert_one (Codec.identity_link_to_doc link))) plan.move;
            Ok plan)
    in
    ({ find; list; attach; detach; merge } : Identity.store)

  let active (r : Challenge.record) = r.consumed_at = None && r.revoked_at = None
  let purpose_matches wanted (r : Challenge.record) = match wanted with None -> true | Some p -> r.purpose = p

  let challenge_store mutex c =
    let insert record ~secret_hash =
      with_lock mutex (fun () ->
          if c.challenges.find_one (id_selector record.Challenge.id) <> None then
            Error (Challenge.Duplicate_id record.id)
          else map_challenge_error (c.challenges.insert_one (Codec.challenge_to_doc record ~secret_hash)))
    in
    let find id =
      with_lock mutex (fun () ->
          match c.challenges.find_one (id_selector id) with
          | None -> Ok None
          | Some doc -> Result.map Option.some (Codec.challenge_of_doc doc))
    in
    let update_record r secret_hash =
      c.challenges.update_one ~filter:(id_selector r.Challenge.id)
        ~update:(Bson.doc [ ("$set", Codec.set_fields (Codec.challenge_to_doc r ~secret_hash)) ])
    in
    let consume id purpose ~secret_hash ~now =
      with_lock mutex (fun () ->
          match c.challenges.find_one (id_selector id) with
          | None -> Error Challenge.Invalid_token
          | Some doc -> (
            match (Codec.challenge_of_doc doc, Codec.secret_hash doc) with
            | Error _ as e, _ -> e
            | Ok _, None -> Error (Challenge.Store_error "Malformed challenge secret hash")
            | Ok r, Some _ when r.purpose <> purpose -> Error Challenge.Wrong_purpose
            | Ok r, Some _ when r.revoked_at <> None -> Error Challenge.Revoked
            | Ok r, Some _ when r.consumed_at <> None -> Error Challenge.Already_consumed
            | Ok r, Some _ when now > r.expires_at -> Error Challenge.Expired
            | Ok r, Some _ when option_exists (fun max -> r.attempts >= max) r.max_attempts ->
              Error Challenge.Too_many_attempts
            | Ok r, Some hash when hash <> secret_hash ->
              let attempts = r.attempts + 1 in
              let r = { r with attempts } in
              ignore (update_record r hash);
              if option_exists (fun max -> attempts >= max) r.max_attempts then Error Challenge.Too_many_attempts
              else Error Challenge.Invalid_token
            | Ok r, Some hash ->
              let r = { r with consumed_at = Some now } in
              Result.bind (map_challenge_error (update_record r hash)) (fun n ->
                  if n = 0 then Error Challenge.Invalid_token else Ok r)))
    in
    let revoke id ~now =
      with_lock mutex (fun () ->
          match c.challenges.find_one (id_selector id) with
          | None -> Ok false
          | Some doc -> (
            match (Codec.challenge_of_doc doc, Codec.secret_hash doc) with
            | Ok r, Some hash when active r ->
              let r = { r with revoked_at = Some now } in
              Result.map (fun n -> n > 0) (map_challenge_error (update_record r hash))
            | Ok _, _ -> Ok false
            | Error _ as e, _ -> e))
    in
    let revoke_where ?purpose pred ~now =
      with_lock mutex (fun () ->
          let changed = ref 0 in
          List.iter
            (fun doc ->
              match (Codec.challenge_of_doc doc, Codec.secret_hash doc) with
              | Ok r, Some hash when active r && purpose_matches purpose r && pred r ->
                let r = { r with revoked_at = Some now } in
                (match update_record r hash with Ok n -> changed := !changed + n | Error _ -> ())
              | _ -> ())
            (c.challenges.find (Bson.doc []));
          Ok !changed)
    in
    let revoke_user ?purpose user_id ~now =
      revoke_where ?purpose (fun r -> r.Challenge.metadata.user_id = Some user_id) ~now
    in
    let revoke_email ?purpose email ~now =
      let email = normalize_email email in
      revoke_where ?purpose (fun r -> r.Challenge.metadata.email = Some email) ~now
    in
    let gc_expired ~now =
      with_lock mutex (fun () ->
          map_challenge_error
            (c.challenges.delete_many (Bson.doc [ ("expiresAt", Bson.doc [ ("$lt", Bson.float now) ]) ])))
    in
    ({ insert; find; consume; revoke; revoke_user; revoke_email; gc_expired } : Challenge.store)

  let passkey_store mutex c =
    let decode = function Ok credential -> Some credential | Error _ -> None in
    let find id =
      with_lock mutex (fun () -> Option.bind (c.passkeys.find_one (id_selector id)) (fun doc -> decode (Codec.passkey_of_doc doc)))
    in
    let list ?user_id () =
      with_lock mutex (fun () ->
          let filter = match user_id with Some uid -> Bson.doc [ ("userId", Bson.str uid) ] | None -> Bson.doc [] in
          c.passkeys.find filter |> List.filter_map (fun doc -> decode (Codec.passkey_of_doc doc)))
    in
    let insert (credential : Passkey.credential) =
      with_lock mutex (fun () ->
          if credential.Passkey.id = "" then Error "passkey credential id cannot be blank"
          else if c.passkeys.find_one (id_selector credential.id) <> None then Error "duplicate passkey credential id"
          else c.passkeys.insert_one (Codec.passkey_to_doc credential))
    in
    let update (credential : Passkey.credential) =
      with_lock mutex (fun () ->
          Result.bind
            (c.passkeys.update_one ~filter:(id_selector credential.Passkey.id)
               ~update:(Bson.doc [ ("$set", Codec.set_fields (Codec.passkey_to_doc credential)) ]))
            (fun n -> if n = 0 then Error "passkey credential not found" else Ok ()))
    in
    let delete id =
      with_lock mutex (fun () ->
          Result.map (fun n -> n > 0) (c.passkeys.delete_many (id_selector id)))
    in
    ({ find; list; insert; update; delete } : Passkey.store)

  let upsert_doc collection id doc =
    Result.bind
      (collection.update_one ~filter:(id_selector id) ~update:(Bson.doc [ ("$set", Codec.set_fields doc) ]))
      (fun n -> if n = 0 then collection.insert_one doc else Ok ())

  let org_membership_id ~org_id ~user_id = org_id ^ "\000" ^ user_id
  let scim_id ~connection_id ~external_id = connection_id ^ "\000" ^ external_id

  let org_store mutex c =
    let find_org id =
      with_lock mutex (fun () -> Option.bind (c.orgs.find_one (id_selector id)) (fun doc -> Result.to_option (Codec.org_of_doc doc)))
    in
    let list_orgs () =
      with_lock mutex (fun () ->
          c.orgs.find (Bson.doc [])
          |> List.filter_map (fun doc -> Result.to_option (Codec.org_of_doc doc))
          |> List.sort (fun (a : Org.org) (b : Org.org) -> String.compare a.id b.id))
    in
    let upsert_org (org : Org.org) =
      with_lock mutex (fun () -> upsert_doc c.orgs org.id (Codec.org_to_doc org))
    in
    let delete_org id = with_lock mutex (fun () -> Result.map (fun n -> n > 0) (c.orgs.delete_many (id_selector id))) in
    let find_membership ~org_id ~user_id =
      with_lock mutex (fun () ->
          Option.bind
            (c.org_memberships.find_one (id_selector (org_membership_id ~org_id ~user_id)))
            (fun doc -> Result.to_option (Codec.membership_of_doc doc)))
    in
    let list_memberships ?org_id ?user_id () =
      with_lock mutex (fun () ->
          c.org_memberships.find (Bson.doc [])
          |> List.filter_map (fun doc -> Result.to_option (Codec.membership_of_doc doc))
          |> List.filter (fun (m : Org.membership) ->
                 Option.fold ~none:true ~some:(String.equal m.org_id) org_id
                 && Option.fold ~none:true ~some:(String.equal m.user_id) user_id)
          |> List.sort (fun (a : Org.membership) (b : Org.membership) ->
                 String.compare (org_membership_id ~org_id:a.org_id ~user_id:a.user_id)
                   (org_membership_id ~org_id:b.org_id ~user_id:b.user_id)))
    in
    let upsert_membership (membership : Org.membership) =
      let id = org_membership_id ~org_id:membership.org_id ~user_id:membership.user_id in
      with_lock mutex (fun () -> upsert_doc c.org_memberships id (Codec.membership_to_doc membership))
    in
    let delete_membership ~org_id ~user_id =
      with_lock mutex (fun () ->
          Result.map (fun n -> n > 0) (c.org_memberships.delete_many (id_selector (org_membership_id ~org_id ~user_id))))
    in
    let find_invite id =
      with_lock mutex (fun () -> Option.bind (c.org_invites.find_one (id_selector id)) (fun doc -> Result.to_option (Codec.invite_of_doc doc)))
    in
    let list_invites ?org_id ?email () =
      let email = Option.map normalize_email email in
      with_lock mutex (fun () ->
          c.org_invites.find (Bson.doc [])
          |> List.filter_map (fun doc -> Result.to_option (Codec.invite_of_doc doc))
          |> List.filter (fun (invite : Org.invite) ->
                 Option.fold ~none:true ~some:(String.equal invite.org_id) org_id
                 && Option.fold ~none:true ~some:(String.equal invite.email) email)
          |> List.sort (fun (a : Org.invite) (b : Org.invite) -> String.compare a.id b.id))
    in
    let upsert_invite (invite : Org.invite) =
      with_lock mutex (fun () -> upsert_doc c.org_invites invite.id (Codec.invite_to_doc invite))
    in
    let delete_invite id =
      with_lock mutex (fun () -> Result.map (fun n -> n > 0) (c.org_invites.delete_many (id_selector id)))
    in
    ({ find_org; list_orgs; upsert_org; delete_org; find_membership; list_memberships; upsert_membership; delete_membership; find_invite; list_invites; upsert_invite; delete_invite } : Org.store)

  let mfa_store mutex c =
    let mfa_current_filter (e : Mfa.enrollment) =
      Bson.doc
        ([
           ("_id", Bson.str e.id);
           ("userId", Bson.str e.user_id);
           ("factor", Bson.str (Codec.mfa_factor_to_string e.factor));
           ("status", Bson.str (Codec.enrollment_status_to_string e.status));
           ("backupHashes", Bson.array (List.map Bson.str e.backup_hashes));
         ]
        @
        match e.last_step with
        | Some step -> [ ("lastStep", Bson.int64 step) ]
        | None -> [ ("lastStep", Bson.doc [ ("$exists", Bson.bool false) ]) ])
    in
    let find id =
      with_lock mutex (fun () ->
          Option.bind (c.mfa_enrollments.find_one (id_selector id)) (fun doc -> Result.to_option (Codec.mfa_enrollment_of_doc doc)))
    in
    let list ?user_id ?factor () =
      with_lock mutex (fun () ->
          c.mfa_enrollments.find (Bson.doc [])
          |> List.filter_map (fun doc -> Result.to_option (Codec.mfa_enrollment_of_doc doc))
          |> List.filter (fun (e : Mfa.enrollment) ->
                 Option.fold ~none:true ~some:(String.equal e.user_id) user_id
                 && Option.fold ~none:true ~some:(fun factor -> e.factor = factor) factor)
          |> List.sort (fun (a : Mfa.enrollment) (b : Mfa.enrollment) -> String.compare a.id b.id))
    in
    let upsert (enrollment : Mfa.enrollment) =
      with_lock mutex (fun () -> upsert_doc c.mfa_enrollments enrollment.id (Codec.mfa_enrollment_to_doc enrollment))
    in
    let replace_if_current ~current enrollment =
      with_lock mutex (fun () ->
          Result.map
            (fun n -> n > 0)
            (c.mfa_enrollments.update_one ~filter:(mfa_current_filter current)
               ~update:(Bson.doc [ ("$set", Codec.set_fields (Codec.mfa_enrollment_to_doc enrollment)) ])))
    in
    let delete id =
      with_lock mutex (fun () -> Result.map (fun n -> n > 0) (c.mfa_enrollments.delete_many (id_selector id)))
    in
    ({ find; list; upsert; replace_if_current; delete } : Mfa.store)

  let scim_store mutex c =
    let find_connection id =
      with_lock mutex (fun () ->
          Option.bind (c.scim_connections.find_one (id_selector id)) (fun doc -> Result.to_option (Codec.scim_connection_of_doc doc)))
    in
    let list_connections ?org_id () =
      with_lock mutex (fun () ->
          c.scim_connections.find (Bson.doc [])
          |> List.filter_map (fun doc -> Result.to_option (Codec.scim_connection_of_doc doc))
          |> List.filter (fun (connection : Scim.connection) ->
                 Option.fold ~none:true ~some:(String.equal connection.org_id) org_id)
          |> List.sort (fun (a : Scim.connection) (b : Scim.connection) -> String.compare a.id b.id))
    in
    let upsert_connection (connection : Scim.connection) =
      with_lock mutex (fun () -> upsert_doc c.scim_connections connection.id (Codec.scim_connection_to_doc connection))
    in
    let delete_connection id =
      with_lock mutex (fun () -> Result.map (fun n -> n > 0) (c.scim_connections.delete_many (id_selector id)))
    in
    let find_user ~connection_id ~external_id =
      with_lock mutex (fun () ->
          Option.bind
            (c.scim_users.find_one (id_selector (scim_id ~connection_id ~external_id)))
            (fun doc -> Result.to_option (Codec.scim_user_of_doc doc)))
    in
    let list_users ?connection_id () =
      with_lock mutex (fun () ->
          c.scim_users.find (Bson.doc [])
          |> List.filter
               (fun doc ->
                 Option.fold ~none:true
                   ~some:(fun id -> doc_get_string doc "connectionId" = Some id)
                   connection_id)
          |> List.filter_map (fun doc -> Result.to_option (Codec.scim_user_of_doc doc))
          |> List.sort (fun (a : Scim.user) (b : Scim.user) -> String.compare a.external_id b.external_id))
    in
    let upsert_user ~connection_id (user : Scim.user) =
      let id = scim_id ~connection_id ~external_id:user.external_id in
      with_lock mutex (fun () -> upsert_doc c.scim_users id (Codec.scim_user_to_doc ~connection_id user))
    in
    let delete_user ~connection_id ~external_id =
      with_lock mutex (fun () ->
          Result.map (fun n -> n > 0) (c.scim_users.delete_many (id_selector (scim_id ~connection_id ~external_id))))
    in
    let find_group ~connection_id ~external_id =
      with_lock mutex (fun () ->
          Option.bind
            (c.scim_groups.find_one (id_selector (scim_id ~connection_id ~external_id)))
            (fun doc -> Result.to_option (Codec.scim_group_of_doc doc)))
    in
    let list_groups ?connection_id () =
      with_lock mutex (fun () ->
          c.scim_groups.find (Bson.doc [])
          |> List.filter
               (fun doc ->
                 Option.fold ~none:true
                   ~some:(fun id -> doc_get_string doc "connectionId" = Some id)
                   connection_id)
          |> List.filter_map (fun doc -> Result.to_option (Codec.scim_group_of_doc doc))
          |> List.sort (fun (a : Scim.group) (b : Scim.group) -> String.compare a.external_id b.external_id))
    in
    let upsert_group ~connection_id (group : Scim.group) =
      let id = scim_id ~connection_id ~external_id:group.external_id in
      with_lock mutex (fun () -> upsert_doc c.scim_groups id (Codec.scim_group_to_doc ~connection_id group))
    in
    let delete_group ~connection_id ~external_id =
      with_lock mutex (fun () ->
          Result.map (fun n -> n > 0) (c.scim_groups.delete_many (id_selector (scim_id ~connection_id ~external_id))))
    in
    ({ find_connection; list_connections; upsert_connection; delete_connection; find_user; list_users; upsert_user; delete_user; find_group; list_groups; upsert_group; delete_group } : Scim.store)

  (* The login-token facet over any backend collection. Rows are keyed by [sid] ([_id]); only the
     SHA-256 of the token is stored. All deletes/finds use plain [_id]/[userId] equality selectors so
     the behavior is identical on minimongo / Burrow / mongod (no operator-coverage assumptions). *)
  let token_store mutex c =
    let record (s : session_info) ~hashed =
      with_lock mutex (fun () -> map_store_error (c.tokens.insert_one (Codec.token_to_doc s ~hashed)))
    in
    let find_live ~sid ~hashed ~now =
      with_lock mutex (fun () ->
          match c.tokens.find_one (id_selector sid) with
          | None -> Ok None
          | Some doc -> (
            match Codec.session_info_of_doc doc with
            (* constant-time hash compare (defense-in-depth: post-HMAC-verify, but free) *)
            | Some info
              when info.expires_at > now
                   && (match doc_get_string doc "hashedToken" with Some h -> constant_eq h hashed | None -> false) ->
              Ok (Some info)
            | _ -> Ok None))
    in
    let list_for_user user_id ~now =
      with_lock mutex (fun () ->
          Ok
            (c.tokens.find (Bson.doc [ ("userId", Bson.str user_id) ])
            |> List.filter_map Codec.session_info_of_doc
            |> List.filter (fun (i : session_info) -> i.expires_at > now)
            |> List.sort (fun (a : session_info) (b : session_info) -> compare b.created_at a.created_at)))
    in
    let touch ~sid ~now =
      with_lock mutex (fun () ->
          match c.tokens.update_one ~filter:(id_selector sid) ~update:(set_doc [ ("lastActiveAt", Bson.float now) ]) with
          | Ok _ -> Ok ()
          | Error e -> Error (Store_error e))
    in
    let revoke ~sid =
      with_lock mutex (fun () -> map_store_error (Result.map (fun n -> n > 0) (c.tokens.delete_many (id_selector sid))))
    in
    let delete_sid sid acc =
      match acc with
      | Error _ as e -> e
      | Ok n -> ( match c.tokens.delete_many (id_selector sid) with Ok d -> Ok (n + d) | Error e -> Error (Store_error e))
    in
    let revoke_user user_id ?keep () =
      with_lock mutex (fun () ->
          match keep with
          | None -> map_store_error (c.tokens.delete_many (Bson.doc [ ("userId", Bson.str user_id) ]))
          | Some keep ->
            c.tokens.find (Bson.doc [ ("userId", Bson.str user_id) ])
            |> List.filter_map (fun doc -> doc_get_string doc "_id")
            |> List.filter (fun sid -> sid <> keep)
            |> List.fold_left (fun acc sid -> delete_sid sid acc) (Ok 0))
    in
    let gc_expired ~now =
      with_lock mutex (fun () ->
          c.tokens.find (Bson.doc [])
          |> List.filter_map (fun doc ->
                 match Codec.session_info_of_doc doc with
                 | Some info when info.expires_at <= now -> Some info.session_id
                 | _ -> None)
          |> List.fold_left (fun acc sid -> delete_sid sid acc) (Ok 0))
    in
    ({ record; find_live; list_for_user; touch; revoke; revoke_user; gc_expired } : token_store)

  let make ?(ensure_indexes = fun () -> ()) collections =
    let mutex = Mutex.create () in
    let audit =
      let append event =
        with_lock mutex (fun () ->
            if event.Audit.id = "" then Error "audit event id cannot be blank"
            else if collections.audit.find_one (id_selector event.id) <> None then Error "duplicate audit event id"
            else collections.audit.insert_one (Codec.audit_to_doc event))
      in
      let list ~target_user_id ~org_id ~kind =
        let filter =
          Bson.doc
            (List.filter_map Fun.id
               [
                 Option.map (fun target -> ("targetUserId", Bson.str target)) target_user_id;
                 Option.map (fun org -> ("orgId", Bson.str org)) org_id;
                 Option.map (fun kind -> ("kind", Bson.str (Audit.string_of_kind kind))) kind;
               ])
        in
        with_lock mutex (fun () ->
            collections.audit.find filter
            |> List.filter_map Codec.audit_of_doc
            |> List.filter (fun event ->
                   Option.fold ~none:true ~some:(fun target -> event.Audit.target_user_id = Some target) target_user_id
                   && Option.fold ~none:true ~some:(fun org -> event.Audit.org_id = Some org) org_id
                   && Option.fold
                        ~none:true
                        ~some:(fun kind -> String.equal (Audit.string_of_kind event.Audit.kind) (Audit.string_of_kind kind))
                        kind))
      in
      Audit.store ~append ~list
    in
    {
      users = user_store mutex collections;
      tokens = token_store mutex collections;
      identities = identity_store mutex collections;
      challenges = challenge_store mutex collections;
      passkeys = passkey_store mutex collections;
      orgs = org_store mutex collections;
      mfa = mfa_store mutex collections;
      scim = scim_store mutex collections;
      audit;
      ensure_indexes;
    }
