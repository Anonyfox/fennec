(* accounts_integration_test.ml — the engine-integration + HTTP/DDP-system test suite.

   These [let%test] blocks exercise the Accounts ENGINE end-to-end (create/login/session/MFA/
   lifecycle/identity-link/role/org) and its HTTP surface (the *_paw routes, SCIM provisioning,
   OAuth/OIDC/SAML callbacks, the session paw). They build on the shared in-memory fixture —
   [test_accounts], [Store.minimongo], [memory_store], [test_active_totp], the [req_]/[post_form_]/…
   HTTP helpers — which lives in {!Accounts_native}. That fixture sits ABOVE the whole engine (it
   needs [make] plus an assembled store from {!Accounts_collection_store}), so it cannot be pushed
   down into the engine leaves (accounts_login.ml / accounts_lifecycle.ml / …) without inverting the
   dependency. Per the house rule, integration tests are the one case where a dedicated file is
   correct: they are grouped here, riding on that fixture, rather than force-split across the engine
   modules they happen to touch.

   The HTTP session-view serializers ([session_doc] / [session_paw]) live in {!Accounts_session},
   opened here so the two tests covering them see the transparent engine [t]. *)

open Accounts_base
open Accounts_native
open Accounts_session

let%test "mfa seal round-trips; a non-sealed or tampered secret fails closed (no plaintext trust)" =
  let t = test_accounts () in
  let sealed = seal_mfa_secret t "S3CRET-totp" in
  let roundtrips = unseal_mfa_secret t sealed = Some "S3CRET-totp" in
  (* a raw, never-sealed value is REJECTED rather than trusted as plaintext *)
  let raw_rejected = unseal_mfa_secret t "S3CRET-totp" = None in
  (* a tampered MAC is rejected *)
  let tamper_rejected =
    match String.split_on_char '.' sealed with
    | [ v; n; c; _mac ] -> unseal_mfa_secret t (String.concat "." [ v; n; c; "AAAA" ]) = None
    | _ -> false
  in
  roundtrips && raw_rejected && tamper_rejected


let%test "password_hasher verifies matching passwords and rejects wrong ones" =
  let h = password_hasher ~iterations:2 () in
  let hash = h.hash ~password:"pw" in
  h.verify ~password:"pw" ~hash && not (h.verify ~password:"bad" ~hash)

let%test "create_user stores a password user" =
  let a = test_accounts () in
  match create_user a ~username:"Ada" ~email:"ADA@example.com" ~password:"pw" () with
  | Ok u -> u.username = Some "Ada" && List.exists (fun e -> e.address = "ada@example.com") u.emails
  | Error _ -> false

let%test "create_user with password fails before persistence when no hasher is configured" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error Password_not_configured -> store.users.find_user_by_username "ada" = Ok None
  | _ -> false

let%test "create_user passes the initial password hash to the store atomically" =
  let backing = memory_store () in
  let backing_users = backing.users in
  let password_hash_seen = ref None in
  let set_password_called = ref false in
  let users =
    {
      backing_users with
      create_user =
        (fun user ~password_hash ->
          password_hash_seen := password_hash;
          backing_users.create_user user ~password_hash);
      set_password_hash =
        (fun _ _ ->
          set_password_called := true;
          Error (Store_error "set_password_hash must not be used during create_user"));
    }
  in
  let store =
    {
      backing with
      users;
    }
  in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Ok _ -> !password_hash_seen = Some "test$pw" && not !set_password_called
  | Error _ -> false

let%test "duplicate email rejected" =
  let a = test_accounts () in
  let _ = create_user a ~email:"ada@example.com" () in
  match create_user a ~email:"ADA@example.com" () with Error (Duplicate_email "ada@example.com") -> true | _ -> false

let%test "create hook cannot introduce a duplicate email" =
  let a = test_accounts () in
  let _ = create_user a ~email:"ada@example.com" () in
  on_create_user a (fun u -> Ok { u with emails = [ { address = "ADA@example.com"; verified = false } ] });
  match create_user a ~username:"hooked" () with Error (Duplicate_email "ada@example.com") -> true | _ -> false

let%test "create hook can repair an initially duplicate email before insertion" =
  let a = test_accounts () in
  let _ = create_user a ~email:"taken@example.com" () in
  on_create_user a (fun u -> Ok { u with emails = [ { address = "fresh@example.com"; verified = false } ] });
  match create_user a ~email:"taken@example.com" () with
  | Ok u -> List.exists (fun e -> e.address = "fresh@example.com") u.emails
  | Error _ -> false

let%test "create hook cannot leave duplicate emails on the same user" =
  let a = test_accounts () in
  on_create_user a (fun u ->
      Ok
        {
          u with
          emails =
            [
              { address = "ada@example.com"; verified = false };
              { address = "ADA@example.com"; verified = false };
            ];
        });
  match create_user a ~username:"ada" () with Error (Duplicate_email "ada@example.com") -> true | _ -> false

let%test "login_with_password issues a verifiable token" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Ok (_, token) -> verify_token a token = Ok u.id
    | Error _ -> false)

let%test "login_with_password_completion branches before issuing a token when MFA is active" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    (match login_with_password_completion a (By_username "ada") ~password:"pw" with
    | Ok (Step_up_required step_up) ->
      step_up.user.id = u.id
      && step_up.step_up.user_id = u.id
      && step_up.step_up.requirement.Mfa.level = Mfa.Multi_factor
    | _ -> false)
    && login_with_password a (By_username "ada") ~password:"pw"
       = Error (Login_rejected "MFA step-up required")

let%test "complete_login_step_up issues an MFA-bearing session" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match login_with_password_completion a (By_username "ada") ~password:"pw" with
    | Ok (Step_up_required step_up) -> (
      let verified = { user_id = u.id; assurance = Mfa.assurance ~now:(fun () -> 1_000.) [ Mfa.Totp ] } in
      match complete_login_step_up a step_up.step_up.token verified with
      | Error _ -> false
      | Ok (user, token) -> (
        let req = H.make_request ~meth:H.GET ~path:"/" ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] () in
        let c = paw a () (Conn.make req) in
        match (auth_context c, assurance c, Mfa.requirement Mfa.Multi_factor) with
        | Some ctx, Some session_assurance, Ok requirement ->
          user.id = u.id
          && verify_token a token = Ok u.id
          && ctx.strategy = "password"
          && List.exists (same_mfa_factor Mfa.Totp) ctx.factors
          && Mfa.require requirement session_assurance = Ok ()
          && complete_login_step_up a step_up.step_up.token verified
             = Error (Login_rejected (Mfa.string_of_error (Mfa.Challenge_error Challenge.Already_consumed)))
        | _ -> false))
	    | _ -> false)

let%test "complete_login_step_up keeps the challenge alive on wrong-user verification" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match (create_user a ~username:"ada" ~password:"pw" (), create_user a ~username:"grace" ~password:"pw" ()) with
  | Ok ada, Ok grace -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp ada.id));
    match login_with_password_completion a (By_username "ada") ~password:"pw" with
    | Ok (Step_up_required step_up) ->
      let wrong =
        { user_id = grace.id; assurance = Mfa.assurance ~now:(fun () -> 1_000.) [ Mfa.Totp ] }
      in
      let correct = { wrong with user_id = ada.id } in
      complete_login_step_up a step_up.step_up.token wrong = Error (Login_rejected "Invalid MFA state")
      && Result.is_ok (complete_login_step_up a step_up.step_up.token correct)
    | _ -> false)
  | _ -> false

let%test "wrong password is rejected" =
  let a = test_accounts () in
  let _ = create_user a ~username:"ada" ~password:"pw" () in
  match login_with_password a (By_username "ada") ~password:"bad" with Error Invalid_password -> true | _ -> false

let%test "set_password changes the login secret and bumps the auth epoch" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"old" with
    | Error _ -> false
    | Ok (_, old_token) ->
      set_password a u.id ~password:"new" = Ok ()
      && verify_token a old_token = Error Invalid_token
      && login_with_password a (By_username "ada") ~password:"old" = Error Invalid_password
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"new"))

let%test "set_password uses the store's atomic hash-and-bump operation" =
  let backing = memory_store () in
  let backing_users = backing.users in
  let set_password_hash_called = ref false in
  let atomic_called = ref false in
  let users =
    {
      backing_users with
      set_password_hash =
        (fun _ _ ->
          set_password_hash_called := true;
          Error (Store_error "set_password_hash must not be used during set_password"));
      set_password_hash_and_bump =
        (fun id hash ->
          atomic_called := true;
          backing_users.set_password_hash_and_bump id hash);
    }
  in
  let store =
    {
      backing with
      users;
    }
  in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"old" () with
  | Error _ -> false
  | Ok u -> set_password a u.id ~password:"new" = Ok () && !atomic_called && not !set_password_hash_called

let%test "change_password requires the old password and bumps the auth epoch" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"old" with
    | Error _ -> false
    | Ok (_, old_token) ->
      change_password a u.id ~old_password:"bad" ~new_password:"new" = Error Invalid_password
      && change_password a u.id ~old_password:"old" ~new_password:"new" = Ok ()
      && verify_token a old_token = Error Invalid_token
      && login_with_password a (By_username "ada") ~password:"old" = Error Invalid_password
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"new"))

let%test "issue_password_reset is non-enumerating for missing email" =
  let a = test_accounts () in
  issue_password_reset a "missing@example.com" = Ok None

let%test "password policy applies to create change set and reset flows" =
  let store = memory_store () in
  let a =
    make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher
      ~password_policy:Password.strict_policy ()
  in
  let weak_create = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"weak" () in
  let created = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"GoodPassword1!" () in
  match created with
  | Error _ -> false
  | Ok user ->
    Result.is_error weak_create
    && Result.is_error (set_password a user.id ~password:"weak")
    && Result.is_error (change_password a user.id ~old_password:"GoodPassword1!" ~new_password:"weak")
    &&
    match issue_password_reset a "ada@example.com" with
    | Error _ | Ok None -> false
    | Ok (Some reset) -> Result.is_error (reset_password a reset.token ~password:"weak")

let%test "reset_password consumes a single-use token and returns a fresh session" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~email:"ADA@example.com" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"old" with
    | Error _ -> false
    | Ok (_, old_token) -> (
      match issue_password_reset a "ada@example.com" with
      | Error _ | Ok None -> false
      | Ok (Some reset) -> (
        match reset_password a reset.token ~password:"new" with
        | Error _ -> false
        | Ok (updated, fresh_token) ->
          updated.id = u.id
          && verify_token a old_token = Error Invalid_token
          && verify_token a fresh_token = Ok u.id
          && (match reset_password a reset.token ~password:"again" with Error (Login_rejected _) -> true | _ -> false)
          && login_with_password a (By_username "ada") ~password:"old" = Error Invalid_password
          && Result.is_ok (login_with_password a (By_username "ada") ~password:"new"))))

let%test "reset_password_completion requires step-up when active MFA exists" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match issue_password_reset a "ada@example.com" with
    | Error _ | Ok None -> false
    | Ok (Some reset) -> (
      match reset_password_completion a reset.token ~password:"new" with
      | Ok (Step_up_required step_up) ->
        step_up.user.id = u.id
        && step_up.step_up.user_id = u.id
        && (match reset_password a reset.token ~password:"again" with Error (Login_rejected _) -> true | _ -> false)
        &&
        (match login_with_password_completion a (By_username "ada") ~password:"new" with
        | Ok (Step_up_required step_up) -> step_up.user.id = u.id
        | _ -> false)
      | _ -> false))

let%test "initial enrollment sets the first password and signs in" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    match issue_enrollment a u.id with
    | Error _ -> false
    | Ok enrollment -> (
      match enroll_account a enrollment.token ~password:"pw" with
      | Error _ -> false
      | Ok (user, token) ->
        user.id = u.id
        && verify_token a token = Ok u.id
        && issue_enrollment a u.id = Error (Login_rejected "User already has a password")
        && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw")))

let%test "enroll_account_completion requires step-up when active MFA exists" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match issue_enrollment a u.id with
    | Error _ -> false
    | Ok enrollment -> (
      match enroll_account_completion a enrollment.token ~password:"pw" with
      | Ok (Step_up_required step_up) ->
        step_up.user.id = u.id
        && step_up.step_up.user_id = u.id
        &&
        (match login_with_password_completion a (By_username "ada") ~password:"pw" with
        | Ok (Step_up_required step_up) -> step_up.user.id = u.id
        | _ -> false)
      | _ -> false))

let%test "verify_email marks the user email verified and attaches a verified identity" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ADA@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued -> (
      match verify_email a issued.Email.token with
      | Error _ -> false
      | Ok updated ->
        let key = match Identity.email ~verified:true "ada@example.com" with Ok key -> key | Error _ -> assert false in
        let email_verified =
          List.exists (fun e -> e.address = "ada@example.com" && e.verified) updated.emails
        in
        let linked =
          match store.identities.Identity.find key with Some link -> link.user_id = u.id | None -> false
        in
        updated.id = u.id && email_verified && linked))

let%test "verify_email_completion requires step-up when active MFA exists" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ADA@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued -> (
      match verify_email_completion a issued.Email.token with
      | Ok (Step_up_required step_up) ->
        step_up.user.id = u.id
        && step_up.step_up.user_id = u.id
        &&
        (match store.users.find_user_by_id u.id with
        | Ok (Some stored) -> List.exists (fun e -> e.address = "ada@example.com" && e.verified) stored.emails
        | _ -> false)
      | _ -> false))

let%test "issue_email_verification rejects an address outside the user record" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    issue_email_verification a u.id "other@example.com" = Error (Invalid_user "Email is not on this user")

let%test "email mutation helpers normalize link and protect usable credentials" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    let old_key = test_verified_email_key "ada@example.com" in
    let new_key = test_verified_email_key "ada2@example.com" in
    (match add_email a ~verified:true u.id " ADA@example.com " with
    | Error _ -> false
    | Ok user ->
      List.exists (fun email -> email.address = "ada@example.com" && email.verified) user.emails
      && (match replace_email a ~verified:true u.id ~old_email:"ada@example.com" ~new_email:"ADA2@example.com" with
         | Error _ -> false
         | Ok user ->
           List.exists (fun email -> email.address = "ada2@example.com" && email.verified) user.emails
           && Option.is_none (store.identities.Identity.find old_key)
           && Option.is_some (store.identities.Identity.find new_key)
           &&
           match remove_email a u.id "ada2@example.com" with
           | Ok user ->
             not (List.exists (fun email -> email.address = "ada2@example.com") user.emails)
             && Option.is_none (store.identities.Identity.find new_key)
           | Error _ -> false))

let%test "email mutation rejects removing the last usable credential" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued -> (
      match verify_email a issued.token with
      | Error _ -> false
      | Ok _ -> remove_email a u.id "ada@example.com" = Error (Login_rejected "Cannot remove the last usable credential")))

let%test "verified email attach rolls back when the user update fails" =
  let backing = memory_store () in
  let fail_update = ref false in
  let users =
    {
      backing.users with
      update_user =
        (fun user ->
          if !fail_update then Error (Store_error "forced update failure")
          else backing.users.update_user user);
    }
  in
  let store = { backing with users } in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    fail_update := true;
    let key = test_verified_email_key "ada@example.com" in
    add_email a ~verified:true u.id "ada@example.com" = Error (Store_error "forced update failure")
    && Option.is_none (store.identities.Identity.find key)
    &&
    match backing.users.find_user_by_id u.id with
    | Ok (Some stored) -> stored.emails = []
    | _ -> false

let%test "verified email detach rolls back when the user update fails" =
  let backing = memory_store () in
  let fail_update = ref false in
  let users =
    {
      backing.users with
      update_user =
        (fun user ->
          if !fail_update then Error (Store_error "forced update failure")
          else backing.users.update_user user);
    }
  in
  let store = { backing with users } in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued -> (
      match verify_email a issued.token with
      | Error _ -> false
      | Ok _ ->
        let key = test_verified_email_key "ada@example.com" in
        fail_update := true;
        remove_email a u.id "ada@example.com" = Error (Store_error "forced update failure")
        &&
        match store.identities.Identity.find key with
        | Some link -> link.user_id = u.id
        | None -> false))

let%test "password users get a password identity link" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match linked_identities a u.id with
    | Error _ -> false
    | Ok links -> List.exists (fun (link : Identity.link) -> Identity.kind link.key = Identity.Password) links)

let%test "unlink_identity rejects the last usable credential" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued -> (
      match verify_email a issued.token with
      | Error _ -> false
      | Ok _ ->
        let key = match Identity.email ~verified:true "ada@example.com" with Ok key -> key | Error _ -> assert false in
        unlink_identity a u.id key = Error (Login_rejected "Cannot unlink the last usable credential")))

let%test "unlink_identity allows removing one of several usable credentials and bumps epoch" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, old_token) -> (
      match issue_email_verification a u.id "ada@example.com" with
      | Error _ -> false
      | Ok issued -> (
        match verify_email a issued.token with
        | Error _ -> false
        | Ok _ ->
          let key = match Identity.email ~verified:true "ada@example.com" with Ok key -> key | Error _ -> assert false in
          let removed = Result.is_ok (unlink_identity a u.id key) in
          let unlinked =
            match linked_identities a u.id with
            | Error _ -> false
            | Ok links -> not (List.exists (fun (link : Identity.link) -> Identity.equal link.key key) links)
          in
          removed && verify_token a old_token = Error Invalid_token && unlinked)))

let%test "merge_identities moves source links and revokes both users" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match
    ( create_user a ~username:"old" ~password:"oldpw" (),
      create_user a ~username:"new" ~password:"newpw" () )
  with
  | Ok old_user, Ok new_user -> (
    let key = match Identity.oauth ~provider:"github" ~subject:"ada" with Ok key -> key | Error _ -> assert false in
    let facts = external_identity key in
    match login_with_identity a ~current_user_id:old_user.id ~strategy:"github" facts with
    | Error _ -> false
    | Ok _ -> (
      match
        ( login_with_password a (By_username "old") ~password:"oldpw",
          login_with_password a (By_username "new") ~password:"newpw" )
      with
      | Ok (_, old_token), Ok (_, new_token) -> (
        match merge_identities a ~from_user_id:old_user.id ~into_user_id:new_user.id with
        | Error _ -> false
        | Ok plan ->
          List.exists (fun (link : Identity.link) -> Identity.equal link.key key && link.user_id = new_user.id) plan.Identity.move
          && verify_token a old_token = Error Invalid_token
          && verify_token a new_token = Error Invalid_token
          &&
          match linked_identities a new_user.id with
          | Error _ -> false
          | Ok links -> List.exists (fun (link : Identity.link) -> Identity.equal link.key key) links)
      | _ -> false))
  | _ -> false

let%test "login_with_token refreshes a valid token" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) -> (
      match login_with_token a token with
      | Ok (u', fresh) -> u'.id = u.id && Result.is_ok (verify_token a fresh)
      | Error _ -> false))

let%test "login_with_token observes auth epoch revocation even on the zero-read cookie path" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:false () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      (* Bump the epoch WITHOUT revoking the session row (the token-store stays live) to isolate the
         epoch axis from per-session revocation: the zero-read [verify_token] still accepts the live
         token (it is epoch-blind by design), while [login_with_token] loads the user and rejects the
         stale epoch. Going through [logout_other_clients] would now ALSO revoke the row, which is the
         separate revocation behavior covered by the token-store tests below. *)
      Result.is_ok ((Store.users store).bump_auth_epoch u.id)
      && verify_token a token = Ok u.id
      && login_with_token a token = Error Invalid_token)

let%test "list_sessions returns one entry per live session with configurable-expiry metadata" =
  let store = memory_store () in
  let lifetime = 3600. in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~lifetime () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match
      (login_with_password a (By_username "ada") ~password:"pw", login_with_password a (By_username "ada") ~password:"pw")
    with
    | Ok _, Ok _ -> (
      match list_sessions a u.id with
      | Ok sessions ->
        List.length sessions = 2
        && List.for_all (fun (s : session_info) -> s.user_id = u.id && s.strategy = "password") sessions
        (* [expires_at] tracks the configurable [lifetime] (loginExpirationInDays equivalent). *)
        && List.for_all (fun (s : session_info) -> Float.abs (s.expires_at -. s.created_at -. lifetime) < 1.) sessions
      | Error _ -> false)
    | _ -> false)

let%test "revoke_session removes one session, leaves the others, and bumps no epoch" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match
      (login_with_password a (By_username "ada") ~password:"pw", login_with_password a (By_username "ada") ~password:"pw")
    with
    | Ok (_, t1), Ok (_, t2) -> (
      match list_sessions a u.id with
      | Ok [ (s : session_info); _ ] -> (
        match revoke_session a ~user_id:u.id ~session_id:s.session_id with
        | Ok true ->
          let v1 = Result.is_ok (verify_token a t1) in
          let v2 = Result.is_ok (verify_token a t2) in
          (* exactly one token died; the other still authenticates *)
          v1 <> v2
          && (match list_sessions a u.id with Ok rest -> List.length rest = 1 | Error _ -> false)
          (* no epoch bump: the surviving token still RESUMES (resume re-checks auth_epoch) *)
          && Result.is_ok (login_with_token a (if v1 then t1 else t2))
        | _ -> false)
      | _ -> false)
    | _ -> false)

let%test "revoke_session is scoped to the owner and refuses unknown ids" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match (create_user a ~username:"ada" ~password:"pw" (), create_user a ~username:"bob" ~password:"pw" ()) with
  | Ok ada, Ok bob -> (
    match (login_with_password a (By_username "ada") ~password:"pw", login_with_password a (By_username "bob") ~password:"pw") with
    | Ok (_, ada_token), Ok _ -> (
      match list_sessions a ada.id with
      | Ok [ (s : session_info) ] ->
        (* bob cannot revoke ada's session by id, and an unknown id is a no-op; ada's token survives *)
        revoke_session a ~user_id:bob.id ~session_id:s.session_id = Ok false
        && revoke_session a ~user_id:ada.id ~session_id:"no-such-sid" = Ok false
        && verify_token a ada_token = Ok ada.id
      | _ -> false)
    | _ -> false)
  | _ -> false

let%test "logout_other_clients revokes token rows so verify_token fails even on the zero-read path" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:false () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Ok (_, token) ->
      verify_token a token = Ok u.id
      && logout_other_clients a u.id = Ok ()
      && verify_token a token = Error Invalid_token
      && (match list_sessions a u.id with Ok [] -> true | _ -> false)
    | Error _ -> false)

let%test "token store reads rows past expiry as absent and gc_expired prunes them" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~lifetime:3600. () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Ok _ -> (
      let tokens = Store.tokens store in
      let future = now () +. 7200. (* past the 3600s lifetime *) in
      match (list_sessions a u.id, tokens.list_for_user u.id ~now:future) with
      | Ok [ _ ], Ok [] -> (
        (* a future-dated gc prunes the one expired row; a second pass finds nothing *)
        match tokens.gc_expired ~now:future with
        | Ok 1 -> ( match tokens.gc_expired ~now:future with Ok 0 -> true | _ -> false)
        | _ -> false)
      | _ -> false)
    | Error _ -> false)

let%test "non-active users cannot start or keep validated sessions" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) -> (
      match suspend_user a u.id with
      | Error _ -> false
      | Ok suspended ->
        suspended.status = Suspended
        && login_with_password a (By_username "ada") ~password:"pw"
           = Error (Login_rejected "Account is not active")
        && verify_token a token = Error Invalid_token))

let%test "restore_user re-enables login after a lifecycle block" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    Result.is_ok (disable_user a u.id)
    && login_with_password a (By_username "ada") ~password:"pw"
       = Error (Login_rejected "Account is not active")
    && (match restore_user a u.id with Ok user -> user.status = Active | Error _ -> false)
    && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw")

let%test "logout_other_clients_and_refresh invalidates old tokens and returns a usable replacement" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, old_token) -> (
      match logout_other_clients_and_refresh a u.id with
      | Error _ -> false
      | Ok (user, fresh_token) ->
        user.id = u.id && verify_token a old_token = Error Invalid_token
        && verify_token a fresh_token = Ok u.id))

let%test "custom strategy issues a normal account token" =
  let a = test_accounts () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok u ->
    register_strategy a
      { name = "ticket"; login = (fun ~credentials -> if Bson.equal credentials (Bson.str "ok") then Ok u else Error Invalid_token) };
    (match login_with_strategy a "ticket" ~credentials:(Bson.str "ok") with
    | Ok (_, token) -> verify_token a token = Ok u.id
    | Error _ -> false)

let%test "custom strategy cannot issue a token for a missing user" =
  let a = test_accounts () in
  let missing =
    {
      id = "missing";
      username = Some "missing";
      emails = [];
      roles = [];
      profile = None;
      services = [];
      created_at = 0.;
      updated_at = 0.;
      auth_epoch = 0;
      status = Active;
    }
  in
  register_strategy a { name = "stale"; login = (fun ~credentials:_ -> Ok missing) };
  login_with_strategy a "stale" ~credentials:Bson.Null = Error User_not_found

let%test "blank strategy names are rejected at registration" =
  let a = test_accounts () in
  match register_strategy a { name = " "; login = (fun ~credentials:_ -> Error Invalid_token) } with
  | () -> false
  | exception Invalid_argument _ -> true


let%test "login_with_identity creates a user when signup is allowed" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  let facts =
    external_identity key ~email:"ada@example.com" ~email_verified:true ~username:"ada"
      ~service:("github", Bson.doc [ ("id", Bson.str "ada") ])
  in
  match login_with_identity a ~identity_store ~allow_signup:true ~strategy:"github" facts with
  | Error _ -> false
  | Ok r ->
    r.created
    && r.linked <> None
    && verify_token a r.token = Ok r.user.id
    && (match identity_store.find key with Some link -> link.Identity.user_id = r.user.id | None -> false)
    && List.assoc_opt "github" r.user.services <> None

let%test "login_with_identity reuses an existing identity link" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    ignore (identity_store.attach ~created_at:1. ~user_id:user.id key);
    let facts = external_identity key in
    (match login_with_identity a ~identity_store ~strategy:"github" facts with
    | Ok r -> (not r.created) && r.linked = None && r.user.id = user.id && verify_token a r.token = Ok user.id
    | Error _ -> false)

let%test "login_with_identity_completion branches before issuing a token when MFA is active" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    ignore (store.mfa.Mfa.upsert (test_active_totp user.id));
    ignore (identity_store.attach ~created_at:1. ~user_id:user.id key);
    let facts = external_identity key in
    (match login_with_identity_completion a ~identity_store ~strategy:"github" facts with
    | Ok (Identity_step_up_required stepped) ->
      stepped.user.id = user.id
      && stepped.step_up.user_id = user.id
      && login_with_identity a ~identity_store ~strategy:"github" facts
         = Error (Login_rejected "MFA step-up required")
    | _ -> false)

let%test "login_with_identity links the current user explicitly" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.passkey ~credential_id:"cred" ()) in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    let facts = external_identity key in
    (match login_with_identity a ~identity_store ~current_user_id:user.id ~strategy:"passkey" facts with
    | Ok r ->
      (not r.created)
      && r.linked <> None
      && r.user.id = user.id
      && (match identity_store.find key with Some link -> link.Identity.user_id = user.id | None -> false)
    | Error _ -> false)

let%test "login_with_identity can auto-link a verified email when enabled" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oidc ~issuer:"https://idp.example" ~connection:"main" ~subject:"sub") in
  match create_user a ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok user ->
    let facts = external_identity key ~email:"ADA@example.com" ~email_verified:true in
    (match login_with_identity a ~identity_store ~link_verified_email:true ~strategy:"oidc" facts with
    | Ok r -> (not r.created) && r.linked <> None && r.user.id = user.id
    | Error _ -> false)

let%test "login_with_identity does not auto-link unverified email" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oidc ~issuer:"https://idp.example" ~connection:"main" ~subject:"sub") in
  match create_user a ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok _ ->
    let facts = external_identity key ~email:"ada@example.com" in
    login_with_identity a ~identity_store ~link_verified_email:true ~strategy:"oidc" facts = Error User_not_found

let%test "link_identity attaches a provider to an existing user without issuing a session" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    let facts = external_identity key ~service:("github", Bson.doc [ ("id", Bson.str "ada") ]) in
    match link_identity a ~now:(fun () -> 1_000.) user.id facts with
    | Error _ -> false
    | Ok None -> false
    | Ok (Some link) -> (
      match store.users.find_user_by_id user.id with
      | Error _ | Ok None -> false
      | Ok (Some stored) ->
        link.user_id = user.id
        && (Store.identities store).find key = Some link
        && List.assoc_opt "github" stored.services = Some (Bson.doc [ ("id", Bson.str "ada") ])))

let%test "link_current_identity uses the authenticated request user" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c =
        paw a ()
          (Conn.make
             (H.make_request ~meth:H.GET ~path:"/settings" ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] ()))
      in
      link_current_identity a c (external_identity key) <> Error User_not_found
      && Option.fold ~none:false ~some:(fun link -> link.Identity.user_id = user.id)
           ((Store.identities store).find key)
      && link_current_identity a
           (Conn.make (H.make_request ~meth:H.GET ~path:"/settings" ()))
           (external_identity key)
         = Error User_not_found)

let%test "totp helper lifecycle confirms verifies and rejects replay deterministically" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    match enroll_totp a ~issuer:"Fennec" ~account:"ada@example.com" user.id with
    | Error _ -> false
    | Ok setup ->
      let code0 = Mfa.totp_code ~time:1_000. setup.totp in
      let code1 = Mfa.totp_code ~time:1_060. setup.totp in
      (match confirm_totp_enrollment a ~time:1_000. setup.enrollment.id ~code:code0 with
      | Error _ -> false
      | Ok confirmed ->
        confirmed.status = Mfa.Active
        && confirmed.confirmed_at = Some 1_000.
        && (match verify_totp_factor a ~time:1_060. confirmed.id ~code:code1 with
           | Error _ -> false
           | Ok verification ->
             verification.user_id = user.id
             && verification.assurance.level = Mfa.Single_factor
	             && verify_totp_factor a ~time:1_060. confirmed.id ~code:code1
	                = Error (Login_rejected "MFA code was already used"))))

let%test "totp verification fails closed when MFA state changed concurrently" =
  let base = Store.minimongo () in
  let mfa = Store.mfa base in
  let cas_calls = ref 0 in
  let guarded_mfa =
    {
      mfa with
      Mfa.replace_if_current =
        (fun ~current next ->
          incr cas_calls;
          if !cas_calls = 1 then mfa.Mfa.replace_if_current ~current next else Ok false);
    }
  in
  let store = { base with mfa = guarded_mfa } in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    match enroll_totp a user.id with
    | Error _ -> false
    | Ok setup ->
      let code0 = Mfa.totp_code ~time:30. setup.totp in
      let code1 = Mfa.totp_code ~time:60. setup.totp in
      (match confirm_totp_enrollment a setup.enrollment.id ~time:30. ~code:code0 with
      | Error _ -> false
      | Ok _ ->
        verify_totp_factor a setup.enrollment.id ~time:60. ~code:code1
        = Error (Login_rejected "MFA enrollment changed; retry the verification")))

let%test "totp enrollment stores a sealed secret and rejects tampering" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    match enroll_totp a user.id with
    | Error _ -> false
    | Ok setup -> (
      match (Store.mfa store).find setup.enrollment.id with
      | None -> false
      | Some stored -> (
        match stored.secret with
        | None -> false
        | Some sealed ->
          sealed <> setup.totp.secret
          && String.starts_with ~prefix:"v1." sealed
          &&
          let code = Mfa.totp_code ~time:30. setup.totp in
          Result.is_ok (confirm_totp_enrollment a setup.enrollment.id ~time:30. ~code)
          &&
          match (Store.mfa store).find setup.enrollment.id with
          | None -> false
          | Some active ->
            let tampered = { active with secret = Some (sealed ^ "x") } in
            Result.is_ok ((Store.mfa store).upsert tampered)
            && verify_totp_factor a setup.enrollment.id ~time:60.
                 ~code:(Mfa.totp_code ~time:60. setup.totp)
               = Error (Store_error "TOTP enrollment secret could not be opened"))))

let%test "backup code helper replaces hashes and consumes each code once" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user -> (
    match regenerate_backup_codes a ~count:2 user.id with
    | Error _ -> false
    | Ok setup -> (
      match setup.codes with
      | code :: _ ->
        (match consume_backup_code a user.id ~code with
        | Error _ -> false
        | Ok verification ->
          verification.user_id = user.id
          && verification.assurance.level = Mfa.Single_factor
          && consume_backup_code a user.id ~code = Error (Login_rejected "Incorrect MFA code")
          && Option.fold ~none:false
               ~some:(fun stored -> List.length stored.Mfa.backup_hashes = 1)
               ((Store.mfa store).find setup.enrollment.id))
      | [] -> false))

let%test "organization invite helper stores only hashes and binds acceptance to the invited email" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match
    ( create_user a ~email:"ada@example.com" (),
      create_user a ~email:"grace@example.com" (),
      create_org a ~now:(fun () -> 10.) ~id:"acme" ~name:"Acme" () )
  with
  | Ok ada, Ok grace, Ok _ -> (
    match issue_org_invite a ~now:(fun () -> 20.) ~ttl:60. ~org_id:"acme" ~email:"ADA@example.com" ~role:"admin" () with
    | Error _ -> false
    | Ok issued ->
      issued.token <> issued.invite.token_hash
      && (Store.orgs store).find_invite issued.invite.id = Some issued.invite
      && accept_org_invite a ~now:(fun () -> 30.) issued.token ~user_id:grace.id
         = Error (Login_rejected "Invite email does not belong to this user")
      &&
      match accept_org_invite a ~now:(fun () -> 30.) issued.token ~user_id:ada.id with
      | Error _ -> false
      | Ok membership ->
        membership.org_id = "acme"
        && membership.user_id = ada.id
        && membership.role = "admin"
        && Option.fold ~none:false
             ~some:(fun invite ->
               invite.Org.status = Org.Invite_accepted && invite.accepted_at = Some 30.)
             ((Store.orgs store).find_invite issued.invite.id))
  | _ -> false

let%test "helper audit events never include raw invite tokens or MFA backup codes" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match
    ( create_user a ~email:"ada@example.com" (),
      create_org a ~id:"acme" ~name:"Acme" (),
      create_user a ~username:"grace" () )
  with
  | Ok ada, Ok _, Ok grace -> (
    match
      ( issue_org_invite a ~org_id:"acme" ~email:"ada@example.com" ~role:"member" (),
        regenerate_backup_codes a ~count:1 grace.id )
    with
    | Ok invite, Ok backup ->
      let _ = accept_org_invite a invite.token ~user_id:ada.id in
      let raw_values = invite.token :: backup.codes in
      let metadata_values =
        Audit.list (Store.audit store)
        |> List.concat_map (fun event -> List.map snd event.Audit.metadata)
      in
      List.for_all (fun raw -> not (List.exists (String.equal raw) metadata_values)) raw_values
    | _ -> false)
  | _ -> false

let%test "login on an unknown account is indistinguishable from a wrong password (enumeration defense)" =
  let verifies = ref 0 in
  let hasher =
    Password.{ hash = (fun ~password -> "h$" ^ password); verify = (fun ~password:_ ~hash:_ -> incr verifies; false) }
  in
  let a = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:hasher () in
  (* a login for a user that does NOT exist must (a) still invoke [verify] (the constant-work dummy) so the
     not-found path costs the same as a wrong password (no timing oracle), AND (b) under the secure default
     return the SAME ambiguous [Invalid_password] error as a wrong password (no message oracle) *)
  let r = login_with_password a (By_email "ghost@example.com") ~password:"pw" in
  r = Error Invalid_password && !verifies >= 1

let%test "on_login_failure fires (allowed=false, tagged reason) on unknown account + wrong password, not on success" =
  let seen = ref [] in
  let a = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher () in
  on_login_failure a (fun attempt -> if not attempt.allowed then seen := Option.value attempt.reason ~default:"?" :: !seen);
  let _ = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () in
  let _ = login_with_password a (By_email "nobody@example.com") ~password:"x" in (* user_not_found *)
  let _ = login_with_password a (By_email "ada@example.com") ~password:"wrong" in (* invalid_password *)
  let _ = login_with_password a (By_email "ada@example.com") ~password:"pw" in (* success ⇒ must NOT fire *)
  !seen = [ "invalid_password"; "user_not_found" ]

let%test "default token lifetimes are safe windows, not the 10-minute challenge default" =
  (* guards the footgun: enrollment invites / reset / verify links must not expire in 10 minutes *)
  default_config.reset_token_lifetime >= 86_400.
  && default_config.enroll_token_lifetime >= 7. *. 86_400.
  && default_config.verify_token_lifetime >= 86_400.

let%test "configure require_verified_email blocks login until an email is verified (mutable policy)" =
  let a = make ~secret:"accounts-test-secret-cfg" ~store:(memory_store ()) ~password_hasher:test_hasher () in
  let _ = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () in
  let ok_default = Result.is_ok (login_with_password a (By_username "ada") ~password:"pw") in
  configure a { default_config with require_verified_email = true };
  (* the email is still unverified, so the stricter policy now rejects the same credentials *)
  let blocked_strict =
    match login_with_password a (By_username "ada") ~password:"pw" with Error (Login_rejected _) -> true | _ -> false
  in
  ok_default && blocked_strict

let%test "config.ambiguous_error_messages makes unknown-account indistinguishable from wrong-password" =
  let a =
    make ~secret:"accounts-test-secret-amb" ~store:(memory_store ()) ~password_hasher:test_hasher
      ~config:{ default_config with ambiguous_error_messages = true } ()
  in
  let _ = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () in
  (* both the missing account and the wrong password return the SAME error — no enumeration via the value *)
  login_with_password a (By_email "nobody@example.com") ~password:"x" = Error Invalid_password
  && login_with_password a (By_email "ada@example.com") ~password:"wrong" = Error Invalid_password

let%test "send_verification_email renders the template and delivers via the ambient transport" =
  let has hay sub =
    let hl = String.length hay and sl = String.length sub in
    let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
    sl = 0 || go 0
  in
  let tr, sent = Fennec_mail.capture () in
  Fennec_mail.set_transport tr;
  let a = make ~secret:"accounts-test-secret-mail" ~store:(memory_store ()) ~password_hasher:test_hasher () in
  set_email_templates a (Mailer.default ~site_name:"Acme" ~from:(Fennec_mail.Address.v ~name:"Acme" "no-reply@acme.test") ());
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match send_verification_email a ~link:(fun ~token -> "https://acme.test/v/" ^ token) user.id with
    | Error _ -> false
    | Ok () -> (
      match sent () with
      | [ m ] ->
        List.exists (fun (ad : Fennec_mail.Address.t) -> ad.email = "ada@example.com") m.Fennec_mail.to_
        && m.Fennec_mail.from.email = "no-reply@acme.test"
        && has m.Fennec_mail.subject "Acme"
        && has (Option.value m.Fennec_mail.text ~default:"") "https://acme.test/v/"
        && (match m.Fennec_mail.html with Some h -> has h "https://acme.test/v/" | None -> false)
      | _ -> false))

let%test "create_user_verifying_email creates the user AND sends one verification email" =
  let tr, sent = Fennec_mail.capture () in
  Fennec_mail.set_transport tr;
  let a = make ~secret:"accounts-test-secret-cuve" ~store:(memory_store ()) ~password_hasher:test_hasher () in
  set_email_templates a (Mailer.default ~site_name:"Acme" ~from:(Fennec_mail.Address.v ~name:"Acme" "no-reply@acme.test") ());
  ( match create_user_verifying_email a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok user ->
    user.username = Some "ada"
    && ( match sent () with
       | [ m ] -> List.exists (fun (ad : Fennec_mail.Address.t) -> ad.email = "ada@example.com") m.Fennec_mail.to_
       | _ -> false ) )

let%test "before_external_login vetoes an external login before a session is minted" =
  let a = make ~secret:"accounts-test-secret" ~store:(Store.minimongo ()) ~password_hasher:test_hasher () in
  before_external_login a (fun ~strategy:_ ~identity ~user:_ -> identity.email <> Some "blocked@x.test");
  let login email =
    let key = identity_ok (Identity.oauth ~provider:"github" ~subject:email) in
    login_with_identity a ~allow_signup:true ~strategy:"github" (external_identity key ~email ~email_verified:true)
  in
  (match login "blocked@x.test" with Error (Login_rejected _) -> true | _ -> false)
  && (match login "ok@x.test" with Ok _ -> true | _ -> false)

let%test "Store.minimongo persists identity links for external login" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let key = identity_ok (Identity.oauth ~provider:"github" ~subject:"ada") in
  let facts = external_identity key ~email:"ada@example.com" ~email_verified:true in
  match login_with_identity a ~allow_signup:true ~strategy:"github" facts with
  | Error _ -> false
  | Ok first -> (
    match login_with_identity a ~strategy:"github" facts with
    | Ok second -> first.user.id = second.user.id && (Store.identities store).find key <> None
    | Error _ -> false)

let%test "register_passkey_credential persists and links the credential" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    let credential = test_passkey_credential user.id in
    (match register_passkey_credential a credential with
    | Error _ -> false
    | Ok link ->
      link.user_id = user.id
      && Option.fold ~none:false
           ~some:(fun stored -> same_passkey_credential stored credential)
           ((Store.passkeys store).find credential.id)
      &&
      match Passkey.identity credential with
      | Ok key -> (Store.identities store).find key <> None
      | Error _ -> false)

let%test "login_with_passkey_assertion persists counter update and issues a token" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    let credential = test_passkey_credential user.id in
    (match register_passkey_credential a credential with
    | Error _ -> false
    | Ok _ ->
      let updated = { credential with sign_count = 2l; last_used_at = Some 2_000. } in
      let assertion =
        {
          Passkey.credential = updated;
          user_present = true;
          user_verified = false;
          backup_eligible = false;
          backed_up = false;
        }
      in
      match login_with_passkey_assertion a assertion with
      | Error _ -> false
      | Ok login ->
        login.user.id = user.id
        && Result.is_ok (verify_token a login.token)
        && Option.fold ~none:false
             ~some:(fun stored -> same_passkey_credential stored updated)
             ((Store.passkeys store).find credential.id))

let%test "login_with_email_link consumes challenge and signs up" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let email = test_email_helper () in
  let address =
    match Email.normalize "ADA@example.com" with Ok a -> a | Error e -> failwith (Email.string_of_error e)
  in
  match Email.issue_login_link email (Email.binding address) with
  | Error _ -> false
  | Ok issued -> (
    match login_with_email_link a ~identity_store email ~allow_signup:true issued.token with
    | Error _ -> false
    | Ok r ->
      r.created
      && verify_token a r.token = Ok r.user.id
      && List.exists (fun e -> e.address = "ada@example.com") r.user.emails)

let%test "oauth_identity builds provider-scoped facts" =
  let provider =
    match
      OAuth.provider ~name:" GitHub " ~authorize_url:"https://github.test/auth" ~client_id:"client"
        ~redirect_uri:"https://app.test/cb" ()
    with
    | Ok p -> p
    | Error e -> failwith (OAuth.string_of_error e)
  in
  match oauth_identity provider ~subject:"sub" ~email:"ADA@example.com" ~email_verified:true with
  | Error _ -> false
  | Ok facts ->
    Identity.kind facts.key = Identity.OAuth
    && Identity.namespace facts.key = Some "github"
    && facts.email = Some "ada@example.com"
    && facts.email_verified

let%test "login_with_oidc delegates to identity resolver" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.oidc ~issuer:"https://idp.example" ~connection:"main" ~subject:"sub") in
  let email_key = identity_ok (Identity.email ~verified:true "ada@example.com") in
  let claims : Oidc.claims =
    {
      issuer = "https://idp.example";
      subject = "sub";
      audience = [ "client" ];
      expires_at = 2_000.;
      not_before = None;
      issued_at = None;
      nonce = Some "nonce";
      email = Some "ada@example.com";
      email_verified = Some true;
      hosted_domain = None;
      tenant = None;
      groups = [];
    }
  in
  let principal : Oidc.principal =
    {
      identity = key;
      email_identity = Some email_key;
      email = Some "ada@example.com";
      email_verified = true;
      org_id = None;
      groups = [];
      claims;
    }
  in
  match login_with_oidc a ~identity_store ~allow_signup:true principal with
  | Ok r -> r.created && verify_token a r.token = Ok r.user.id
  | Error _ -> false

let%test "login_with_saml defaults signup to principal allow_jit" =
  let a = test_accounts () in
  let identity_store = Identity.memory_store () in
  let key = identity_ok (Identity.saml ~connection:"corp" ~name_id:"ada@example.com" ()) in
  let email_key = identity_ok (Identity.email ~verified:true "ada@example.com") in
  let assertion : Saml.assertion =
    {
      issuer = "https://idp.example";
      audience = "sp";
      recipient = "https://app.test/saml/acs";
      destination = None;
      in_response_to = Some "req";
      not_before = None;
      not_on_or_after = None;
      name_id = "ada@example.com";
      name_id_format = None;
      external_id = None;
      email = Some "ada@example.com";
      attributes = [];
      session_index = None;
    }
  in
  let principal : Saml.principal =
    {
      identity = key;
      email_identity = Some email_key;
      email = Some "ada@example.com";
      allow_jit = true;
      org_id = None;
      session_index = None;
      signature_key_fingerprint = None;
      attributes = [];
      assertion;
    }
  in
  match login_with_saml a ~identity_store principal with
  | Ok r -> r.created && verify_token a r.token = Ok r.user.id
  | Error _ -> false

let%test "scim_identity uses external id and first email" =
  let connection =
    match Scim.connection ~id:"corp" ~org_id:"org" ~bearer_token:"very-secret-scim-token" () with
    | Ok c -> c
    | Error e -> failwith (Scim.string_of_error e)
  in
  let user =
    match Scim.user ~external_id:"ext-1" ~user_name:"ada" ~emails:[ "ADA@example.com" ] () with
    | Ok u -> u
    | Error e -> failwith (Scim.string_of_error e)
  in
  match scim_identity connection user with
  | Error _ -> false
  | Ok facts ->
    Identity.kind facts.key = Identity.Scim
    && facts.email = Some "ada@example.com"
    && facts.username = Some "ada"

let%test "scim_paw exposes discovery metadata without provisioning auth" =
  let app = scim_paw (test_accounts ()) ~prefix:"/scim" () in
  let get path = H.make_request ~meth:H.GET ~path () in
  let service = Paw.run app (get "/scim/ServiceProviderConfig") in
  let resource_types = Paw.run app (get "/scim/ResourceTypes") in
  let schemas = Paw.run app (get "/scim/Schemas") in
  let users = Paw.run app (get "/scim/Users") in
  service.H.status = 200
  && resource_types.H.status = 200
  && schemas.H.status = 200
  && users.H.status = 401
  &&
  match (Json.parse_opt service.H.body, Json.parse_opt resource_types.H.body, Json.parse_opt schemas.H.body) with
  | Some service, Some resource_types, Some schemas ->
    Option.is_some (Json.member "patch" service)
    && Option.is_some (Json.member "Resources" resource_types)
    && Option.is_some (Json.member "Resources" schemas)
  | _ -> false

let%test "scim_paw provisions accounts users memberships and directory state" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let org = match create_org a ~id:"acme" ~name:"Acme" () with Ok org -> org | Error _ -> failwith "org" in
  let connection =
    match Scim.connection ~id:"corp" ~org_id:org.id ~bearer_token:"very-secret-scim-token" () with
    | Ok c -> c
    | Error e -> failwith (Scim.string_of_error e)
  in
  ignore ((Store.scim store).upsert_connection connection);
  let app = scim_paw a ~prefix:"/scim" () in
  let body =
    {|{"externalId":"ext-1","userName":"ada","active":true,"emails":["ada@example.com"],"groups":["eng"]}|}
  in
  let r =
    Paw.run app
      (H.make_request ~meth:H.POST ~path:"/scim/Users"
         ~headers:[ ("authorization", "Bearer very-secret-scim-token"); ("content-type", "application/json") ]
         ~body ())
  in
  let stored_scim = (Store.scim store).find_user ~connection_id:"corp" ~external_id:"ext-1" in
  let stored_user = (Store.users store).find_user_by_email "ada@example.com" in
  r.H.status = 201
  && Option.is_some stored_scim
  &&
  match stored_user with
  | Ok (Some user) ->
    Option.is_some ((Store.orgs store).find_membership ~org_id:"acme" ~user_id:user.id)
    &&
    let key = identity_ok (Identity.scim ~org_id:"acme" ~external_id:"ext-1") in
    Option.is_some ((Store.identities store).find key)
  | _ -> false

let%test "scim_paw applies user and group PATCH operations" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  let org = match create_org a ~id:"acme" ~name:"Acme" () with Ok org -> org | Error _ -> failwith "org" in
  let connection =
    match Scim.connection ~id:"corp" ~org_id:org.id ~bearer_token:"very-secret-scim-token" () with
    | Ok c -> c
    | Error e -> failwith (Scim.string_of_error e)
  in
  ignore ((Store.scim store).upsert_connection connection);
  let app = scim_paw a ~prefix:"/scim" () in
  let headers = [ ("authorization", "Bearer very-secret-scim-token"); ("content-type", "application/json") ] in
  let _ =
    Paw.run app
      (H.make_request ~meth:H.POST ~path:"/scim/Users" ~headers
         ~body:
           {|{"externalId":"ext-1","userName":"ada","active":true,"emails":["ada@example.com"],"groups":["eng"]}|}
         ())
  in
  let _ =
    Paw.run app
      (H.make_request ~meth:H.POST ~path:"/scim/Groups" ~headers
         ~body:{|{"externalId":"eng","displayName":"Engineering","members":["ext-1"]}|} ())
  in
  let user_patch =
    Paw.run app
      (H.make_request ~meth:H.PATCH ~path:"/scim/Users/ext-1" ~headers
         ~body:
           {|{"Operations":[{"op":"replace","path":"active","value":false},{"op":"add","path":"emails","value":[{"value":"ADA2@example.com"}]}]}|}
         ())
  in
  let group_patch =
    Paw.run app
      (H.make_request ~meth:H.PATCH ~path:"/scim/Groups/eng" ~headers
         ~body:{|{"Operations":[{"op":"add","path":"members","value":[{"value":"ext-2"}]}]}|} ())
  in
  match
    ( (Store.scim store).find_user ~connection_id:"corp" ~external_id:"ext-1",
      (Store.scim store).find_group ~connection_id:"corp" ~external_id:"eng" )
  with
  | Some user, Some group ->
    user_patch.H.status = 200
    && group_patch.H.status = 200
    && not user.Scim.active
    && user.emails = [ "ada2@example.com"; "ada@example.com" ]
    && group.members = [ "ext-1"; "ext-2" ]
  | _ -> false

let%test "paw assigns user_id from signed login cookie" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c0 = Conn.make (req_ "/") in
      let c1 = Conn.text (set_login_cookie a c0 token) "ok" in
      let set_cookie = match Paw.Headers.get_all (finalize_ c1).H.headers "set-cookie" with s :: _ -> s | [] -> "" in
      let c2 = paw a () (Conn.make (req_ ~headers:[ ("Cookie", cookie_kv_ set_cookie) ] "/")) in
      user_id c2 = Some u.id)

let%test "session_doc exposes the current account without secret material" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c = paw a () (Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/me")) in
      match session_doc a c with
      | Error _ -> false
      | Ok doc -> (
        match (Bson.get_string doc "userId", Bson.get doc "user", Bson.get doc "authContext") with
        | Some uid, Some (Bson.Document user_fields), Some (Bson.Document _) ->
          uid = u.id
          && List.assoc_opt "passwordHash" user_fields = None
          && List.assoc_opt "services" user_fields = None
          && Option.fold ~none:false
               ~some:(function Bson.String username -> username = "ada" | _ -> false)
               (List.assoc_opt "username" user_fields)
        | _ -> false))

let%test "session_paw works standalone and marks the response private" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let r = Paw.run (session_paw a ~path:"/me" ()) (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/me") in
      r.H.status = 200
      && Paw.Headers.get r.H.headers "cache-control" = Some "no-store"
      && (match Bson_json.of_string r.H.body with
         | Bson.Document _ as doc -> Bson.get_string doc "userId" = Some u.id
         | _ -> false))

let%test "password_reset_request_paw sends only existing reset tokens and redirects" =
  let a = test_accounts () in
  let _ = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () in
  let sent = ref None in
  let app =
    password_reset_request_paw a ~path:"/forgot" ~success:"/sent" ~error:"/error"
      ~send:(fun reset -> sent := Some reset.token)
      ()
  in
  let existing = Paw.run app (post_form_ "/forgot" "email=ada%40example.com") in
  let missing = Paw.run app (post_form_ "/forgot" "email=missing%40example.com") in
  existing.H.status = 302 && location_ existing = Some "/sent" && Option.is_some !sent
  && missing.H.status = 302 && location_ missing = Some "/sent"

let%test "password_reset_paw sets a login cookie after consuming a reset token" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"old" () with
  | Error _ -> false
  | Ok _ -> (
    match issue_password_reset a "ada@example.com" with
    | Error _ | Ok None -> false
    | Ok (Some reset) ->
      let app = password_reset_paw a ~path:"/reset" ~success:"/" ~error:"/reset-error" () in
      let body = "token=" ^ H.percent_encode (Challenge.token_to_string reset.token) ^ "&password=new" in
      let r = Paw.run app (post_form_ "/reset" body) in
      r.H.status = 302
      && location_ r = Some "/"
      && Paw.Headers.mem r.H.headers "set-cookie"
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"new"))

let%test "password_reset_paw redirects to MFA without setting a login cookie" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match issue_password_reset a "ada@example.com" with
    | Error _ | Ok None -> false
    | Ok (Some reset) ->
      let app = password_reset_paw a ~path:"/reset" ~success:"/" ~error:"/error" ~mfa_required:"/mfa" () in
      let body = "token=" ^ H.percent_encode (Challenge.token_to_string reset.token) ^ "&password=new" in
      let r = Paw.run app (post_form_ "/reset" body) in
      r.H.status = 302 && mfa_redirect_ok_ a u.id r
      && not (Paw.Headers.mem r.H.headers "set-cookie"))

let%test "enrollment_paw sets the first password and login cookie" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    match issue_enrollment a u.id with
    | Error _ -> false
    | Ok enrollment ->
      let app = enrollment_paw a ~path:"/enroll" ~success:"/" ~error:"/error" () in
      let body = "token=" ^ H.percent_encode (Challenge.token_to_string enrollment.token) ^ "&password=pw" in
      let r = Paw.run app (post_form_ "/enroll" body) in
      r.H.status = 302
      && location_ r = Some "/"
      && Paw.Headers.mem r.H.headers "set-cookie"
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw"))

let%test "email_verification_request_paw requires a logged-in user and sends a token" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok _ -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let sent = ref None in
      let app =
        Paw.seq
          [
            paw a ();
            email_verification_request_paw a ~path:"/verify/request" ~success:"/sent" ~error:"/error"
              ~send:(fun issued -> sent := Some issued.token)
              ();
          ]
      in
      let r =
        Paw.run app
          (H.make_request ~meth:H.POST ~path:"/verify/request"
             ~headers:
               [
                 ("content-type", "application/x-www-form-urlencoded");
                 ("Cookie", a.cookie ^ "=" ^ token);
               ]
             ~body:"email=ada%40example.com" ())
      in
      r.H.status = 302 && location_ r = Some "/sent" && Option.is_some !sent)

let%test "email_verification_paw verifies email and sets a login cookie" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued ->
      let app = email_verification_paw a ~path:"/verify" ~success:"/" ~error:"/verify-error" () in
      let r =
        Paw.run app
          (H.make_request ~meth:H.GET ~path:"/verify"
             ~query_string:("token=" ^ H.percent_encode (Challenge.token_to_string issued.token))
             ())
      in
      let verified =
        match a.store.users.find_user_by_id u.id with
        | Ok (Some user) -> List.exists (fun e -> e.address = "ada@example.com" && e.verified) user.emails
        | _ -> false
      in
      r.H.status = 302
      && location_ r = Some "/"
      && Paw.Headers.mem r.H.headers "set-cookie"
      && verified)

let%test "email_verification_paw redirects to MFA without setting a login cookie" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued ->
      let app = email_verification_paw a ~path:"/verify" ~success:"/" ~error:"/error" ~mfa_required:"/mfa" () in
      let r =
        Paw.run app
          (H.make_request ~meth:H.GET ~path:"/verify"
             ~query_string:("token=" ^ H.percent_encode (Challenge.token_to_string issued.token))
             ())
      in
      r.H.status = 302 && mfa_redirect_ok_ a u.id r
      && not (Paw.Headers.mem r.H.headers "set-cookie"))

let%test "email magic-link paws issue and consume a login token" =
  let a = test_accounts () in
  let sent = ref None in
  let request =
    email_login_link_request_paw a ~path:"/email/request" ~success:"/sent" ~error:"/error"
      ~send:(fun issued -> sent := Some issued.token)
      ()
  in
  let requested = Paw.run request (post_form_ "/email/request" "email=ada%40example.com") in
  match !sent with
  | None -> false
  | Some token ->
    let consume = email_login_link_paw a ~allow_signup:true ~path:"/email/login" ~success:"/" ~error:"/error" () in
    let consumed =
      Paw.run consume
        (H.make_request ~meth:H.GET ~path:"/email/login"
           ~query_string:("token=" ^ H.percent_encode (Challenge.token_to_string token))
           ())
    in
    requested.H.status = 302
    && location_ requested = Some "/sent"
    && consumed.H.status = 302
    && location_ consumed = Some "/"
    && Paw.Headers.mem consumed.H.headers "set-cookie"

let%test "email magic-link paw redirects to MFA without setting a login cookie" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    let email = email_service a () in
    let address = match Email.normalize "ada@example.com" with Ok address -> address | Error _ -> assert false in
    (match Email.issue_login_link email (Email.binding address) with
    | Error _ -> false
    | Ok issued ->
      let consume =
        email_login_link_paw a ~allow_signup:true ~mfa_required:"/mfa" ~path:"/email/login" ~success:"/"
          ~error:"/error" ()
      in
      let r =
        Paw.run consume
          (H.make_request ~meth:H.GET ~path:"/email/login"
             ~query_string:("token=" ^ H.percent_encode (Challenge.token_to_string issued.token))
             ())
      in
      r.H.status = 302 && mfa_redirect_ok_ a u.id r
      && not (Paw.Headers.mem r.H.headers "set-cookie"))

let%test "email otp paws issue and consume one-time codes" =
  let a = test_accounts () in
  let sent = ref None in
  let request =
    email_otp_request_paw a ~path:"/otp/request" ~success:"/sent" ~error:"/error"
      ~send:(fun issued -> sent := Some (issued.token, issued.code))
      ()
  in
  let requested = Paw.run request (post_form_ "/otp/request" "email=ada%40example.com") in
  match !sent with
  | None -> false
  | Some (token, code) ->
    let consume = email_otp_paw a ~allow_signup:true ~path:"/otp/login" ~success:"/" ~error:"/error" () in
    let body = "token=" ^ H.percent_encode (Challenge.token_to_string token) ^ "&code=" ^ H.percent_encode code in
    let consumed = Paw.run consume (post_form_ "/otp/login" body) in
    requested.H.status = 302
    && location_ requested = Some "/sent"
    && consumed.H.status = 302
    && location_ consumed = Some "/"
    && Paw.Headers.mem consumed.H.headers "set-cookie"

let%test "email otp paw redirects to MFA without setting a login cookie" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    let email = email_service a () in
    let address = match Email.normalize "ada@example.com" with Ok address -> address | Error _ -> assert false in
    (match Email.issue_otp email (Email.binding address) with
    | Error _ -> false
    | Ok issued ->
      let consume =
        email_otp_paw a ~allow_signup:true ~mfa_required:"/mfa" ~path:"/otp/login" ~success:"/"
          ~error:"/error" ()
      in
      let body =
        "token=" ^ H.percent_encode (Challenge.token_to_string issued.token) ^ "&code="
        ^ H.percent_encode issued.code
      in
	      let r = Paw.run consume (post_form_ "/otp/login" body) in
	      r.H.status = 302 && mfa_redirect_ok_ a u.id r
	      && not (Paw.Headers.mem r.H.headers "set-cookie"))

let%test "mfa_totp_paw completes step-up and sets a login cookie" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match enroll_totp a user.id with
    | Error _ -> false
    | Ok setup -> (
      let initial_code = Mfa.totp_code ~time:1_000. setup.totp in
      match confirm_totp_enrollment a setup.enrollment.id ~time:1_000. ~code:initial_code with
      | Error _ -> false
      | Ok active -> (
        match login_with_password_completion a (By_username "ada") ~password:"pw" with
        | Ok (Step_up_required step_up) ->
          let code = Mfa.totp_code setup.totp in
          let body =
            "mfaToken=" ^ H.percent_encode (Challenge.token_to_string step_up.step_up.token)
            ^ "&factor=" ^ H.percent_encode active.id ^ "&code=" ^ H.percent_encode code
          in
          let r =
            Paw.run
              (mfa_totp_paw a ~path:"/mfa/totp" ~success:"/" ~error:"/mfa" ())
              (post_form_ "/mfa/totp" body)
          in
          r.H.status = 302
          && location_ r = Some "/"
          &&
          (match Paw.Headers.get_all r.H.headers "set-cookie" with
          | set_cookie :: _ ->
            let c = paw a () (Conn.make (req_ ~headers:[ ("Cookie", cookie_kv_ set_cookie) ] "/")) in
            user_id c = Some user.id
          | [] -> false)
        | _ -> false)))

let%test "mfa_backup_code_paw completes step-up and consumes one backup code" =
  let a = make ~secret:"accounts-test-secret" ~store:(Store.minimongo ()) ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match regenerate_backup_codes a ~count:1 user.id with
    | Error _ -> false
    | Ok backup -> (
      match (backup.codes, login_with_password_completion a (By_username "ada") ~password:"pw") with
      | code :: _, Ok (Step_up_required step_up) ->
        let body =
          "mfaToken=" ^ H.percent_encode (Challenge.token_to_string step_up.step_up.token)
          ^ "&userId=" ^ H.percent_encode user.id ^ "&code=" ^ H.percent_encode code
        in
        let r =
          Paw.run
            (mfa_backup_code_paw a ~path:"/mfa/backup" ~success:"/" ~error:"/mfa" ())
            (post_form_ "/mfa/backup" body)
        in
        r.H.status = 302
        && location_ r = Some "/"
        && Paw.Headers.mem r.H.headers "set-cookie"
        && consume_backup_code a user.id ~code = Error (Login_rejected "Incorrect MFA code")
      | _ -> false))

let%test "passkey registration option paw emits browser JSON for the current user" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok _ -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let app = passkey_registration_options_paw a (passkey_rp_ ()) ~path:"/passkeys/register/options" () in
      let r =
        Paw.run app
          (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/passkeys/register/options")
      in
      r.H.status = 200
      && Paw.Headers.get r.H.headers "cache-control" = Some "no-store"
      &&
      match Json.parse_opt r.H.body with
      | Some json -> Option.is_some (json_member_string "token" json) && Option.is_some (Json.member "publicKey" json)
      | None -> false)

let%test "passkey registration finish paw rejects malformed JSON cleanly" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok _ -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let app = passkey_registration_finish_paw a (passkey_rp_ ()) ~path:"/passkeys/register/finish" () in
      let r =
        Paw.run app
          (H.make_request ~meth:H.POST ~path:"/passkeys/register/finish"
             ~headers:[ ("Cookie", a.cookie ^ "=" ^ token); ("content-type", "application/json") ]
             ~body:{|{"token":"bad"}|} ())
      in
      r.H.status = 400 && Option.is_some (Json.parse_opt r.H.body))

let%test "oauth route helpers redirect to provider then resolve callback" =
  let a = test_accounts () in
  let provider = oauth_provider_ () in
  let authorize = oauth_authorize_paw a provider ~path:"/oauth/start" ~error:"/oauth/error" () in
  let start = Paw.run authorize (req_ "/oauth/start") in
  match Option.bind (location_ start) (fun url -> query_param_ url "state") with
  | None -> false
  | Some state ->
    let key = match Identity.oauth ~provider:"github" ~subject:"ada" with Ok key -> key | Error _ -> assert false in
    let callback =
      oauth_callback_paw a provider ~path:"/oauth/callback" ~success:"/" ~error:"/oauth/error"
        ~exchange:(fun (_state : OAuth.state) ~code ->
          if code = "ok" then Ok (external_identity key ~email:"ada@example.com" ~email_verified:true)
          else Error (Login_rejected "bad_code"))
        ()
    in
    let r =
      Paw.run callback
        (H.make_request ~meth:H.GET ~path:"/oauth/callback"
           ~query_string:("code=ok&state=" ^ H.percent_encode state) ())
    in
    r.H.status = 302 && location_ r = Some "/" && Paw.Headers.mem r.H.headers "set-cookie"

let%test "oauth credential handshake: issue then consume binds the user, single-use, secret-checked" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match issue_oauth_credential a ~user_id:u.id ~provider:"github" () with
    | Error _ -> false
    | Ok (ct, cs) ->
      let first = consume_oauth_credential a ~credential_token:ct ~credential_secret:cs in
      let replay = consume_oauth_credential a ~credential_token:ct ~credential_secret:cs in
      let wrong_secret =
        match issue_oauth_credential a ~user_id:u.id () with
        | Ok (ct2, _) -> consume_oauth_credential a ~credential_token:ct2 ~credential_secret:"not-the-secret"
        | Error _ -> Ok ("unexpected", None)
      in
      (* the consumed token returns the bound user + provider; a replay and a wrong secret both fail *)
      first = Ok (u.id, Some "github") && Result.is_error replay && Result.is_error wrong_secret)

let%test "oidc route helpers redirect to provider then resolve callback" =
  let a = test_accounts () in
  let connection = oidc_connection_ () in
  let authorize = oidc_authorize_paw a connection ~path:"/oidc/start" ~error:"/oidc/error" () in
  let start = Paw.run authorize (req_ "/oidc/start") in
  match Option.bind (location_ start) (fun url -> query_param_ url "state") with
  | None -> false
  | Some state ->
    let callback =
      oidc_callback_paw a connection ~path:"/oidc/callback" ~success:"/" ~error:"/oidc/error"
        ~exchange:(fun (state : Oidc.state) ~code ->
          if code <> "ok" then Error (Login_rejected "bad_code")
          else
            let claims =
              {
                Oidc.issuer = "https://idp.test";
                subject = "ada";
                audience = [ "client" ];
                expires_at = Unix.gettimeofday () +. 600.;
                not_before = None;
                issued_at = None;
                nonce = Some state.nonce;
                email = Some "ada@example.com";
                email_verified = Some true;
                hosted_domain = None;
                tenant = None;
                groups = [];
              }
            in
            Result.map_error (fun e -> Login_rejected (Oidc.string_of_error e))
              (Oidc.validate_claims connection state claims))
        ()
    in
    let r =
      Paw.run callback
        (H.make_request ~meth:H.GET ~path:"/oidc/callback"
           ~query_string:("code=ok&state=" ^ H.percent_encode state) ())
    in
    r.H.status = 302 && location_ r = Some "/" && Paw.Headers.mem r.H.headers "set-cookie"

let%test "saml route helper rejects malformed ACS callback by redirecting to error" =
  let a = test_accounts () in
  let connection =
    match
      Saml.connection ~id:"corp" ~issuer:"https://idp.test" ~sso_url:"https://idp.test/sso"
        ~entity_id:"https://app.test/sp" ~acs_url:"https://app.test/saml/acs" ()
    with
    | Ok connection -> connection
    | Error e -> failwith (Saml.string_of_error e)
  in
  let app = saml_callback_paw a connection ~trusted_keys:[] ~path:"/saml/acs" ~success:"/" ~error:"/saml/error" () in
  let r = Paw.run app (post_form_ "/saml/acs" "RelayState=x&SAMLResponse=y") in
  r.H.status = 302 && location_ r = Some "/saml/error"

let%test "paw assigns auth_context from signed login cookie" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c = paw a () (Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/")) in
      match auth_context c with
      | None -> false
      | Some ctx ->
        ctx.user_id = u.id
        && ctx.session_id <> ""
        && ctx.strategy = "password"
        && ctx.auth_epoch = u.auth_epoch
        && ctx.issued_at > 0.
        && ctx.expires_at > ctx.issued_at)

let%test "paw derives built-in MFA assurance from the login strategy" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok _ -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c = paw a () (Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/")) in
      match (assurance c, Mfa.requirement Mfa.Single_factor, Mfa.requirement Mfa.Multi_factor) with
      | Some current, Ok single, Ok multi ->
        current.level = Mfa.Single_factor
        && Result.is_ok (Mfa.require single current)
        &&
        let blocked = require_assurance multi () c in
        (match Conn.resp blocked with Some r -> r.H.status = 403 | None -> false)
      | _ -> false)

let%test "set_assurance lets verified step-up satisfy a route guard" =
  let c = Conn.make (req_ "/settings") in
  let assurance = Mfa.assurance ~now:(fun () -> 1_000.) [ Mfa.Password; Mfa.Totp ] in
  match Mfa.requirement Mfa.Multi_factor with
  | Error _ -> false
  | Ok requirement ->
    let c = set_assurance c assurance in
    let c = require_assurance requirement () c in
    Conn.resp c = None

let%test "require_org checks active membership and permissions" =
  let org =
    match Org.org ~id:"acme" ~name:"Acme" () with Ok org -> org | Error _ -> failwith "org"
  in
  let admin =
    match Org.membership ~org_id:"acme" ~user_id:"u1" ~role:"admin" () with
    | Ok membership -> membership
    | Error _ -> failwith "membership"
  in
  let member =
    match Org.membership ~org_id:"acme" ~user_id:"u2" ~role:"member" () with
    | Ok membership -> membership
    | Error _ -> failwith "membership"
  in
  (* org permissions resolve against the app's ONE code-declared policy: admin grants write, member does not *)
  let policy =
    Roles.policy
      [ Roles.role (Roles.Role.v_exn "admin") [ Roles.Permission.v_exn "write" ];
        Roles.role (Roles.Role.v_exn "member") [ Roles.Permission.v_exn "read" ] ]
  in
  let a = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher ~policy () in
  let allowed = require_org a ~permission:"write" () (set_org_context (Conn.make (req_ "/admin")) ~membership:admin org) in
  let blocked = require_org a ~permission:"write" () (set_org_context (Conn.make (req_ "/admin")) ~membership:member org) in
  Conn.resp allowed = None && match Conn.resp blocked with Some r -> r.H.status = 403 | None -> false

let%test "can_in resolves global and org-scoped roles against the one policy" =
  let policy =
    Roles.policy
      [ Roles.role (Roles.Role.v_exn "support") [ Roles.Permission.v_exn "ticket.read" ];
        Roles.role (Roles.Role.v_exn "admin") [ Roles.Permission.v_exn "billing.write" ] ]
  in
  let a = make ~secret:"accounts-test-secret" ~store:(Store.minimongo ()) ~password_hasher:test_hasher ~policy () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok user ->
    ignore (set_roles a user.id [ Roles.Role.v_exn "support" ]);
    ignore (a.store.orgs.Org.upsert_org (Result.get_ok (Org.org ~id:"acme" ~name:"Acme" ())));
    ignore (a.store.orgs.Org.upsert_membership (Result.get_ok (Org.membership ~org_id:"acme" ~user_id:user.id ~role:"admin" ())));
    let p s = Roles.Permission.v_exn s in
    (* global: the support role grants ticket.read everywhere, but not billing.write *)
    can a user.id (p "ticket.read") = Ok true
    && can a user.id (p "billing.write") = Ok false
    (* in acme: the admin membership grants billing.write AND the global support role still applies *)
    && can_in a user.id ~scope:(Org "acme") (p "billing.write") = Ok true
    && can_in a user.id ~scope:(Org "acme") (p "ticket.read") = Ok true
    (* in another org where they're not a member: only the global role applies *)
    && can_in a user.id ~scope:(Org "other") (p "billing.write") = Ok false

let%test "require_org_strategy maps tenant SSO policy into Accounts errors" =
  let policy =
    {
      Org.sso = Org.Sso_required { connection_ids = [ "corp" ]; allow_password_fallback = false; allow_jit = true };
      mfa = Org.Mfa_optional;
      allow_public_signup = false;
    }
  in
  let org =
    match Org.org ~id:"acme" ~name:"Acme" ~policy () with Ok org -> org | Error _ -> failwith "org"
  in
  require_org_strategy org (Org.Saml "corp") = Ok ()
  && match require_org_strategy org Org.Password with Error (Login_rejected _) -> true | _ -> false

let%test "app-wide roles are typed, stored on users, and mapped from external strings" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match set_roles_from_strings a ~actor:(Audit.User "admin") user.id [ "Admin"; "support"; "admin" ] with
    | Error _ -> false
    | Ok updated ->
      Roles.role_names updated.roles = [ "admin"; "support" ]
      &&
      match (Store.users store).find_user_by_id user.id with
      | Ok (Some stored) ->
        let events = Audit.list ~target_user_id:user.id ~kind:Audit.Role_change (Store.audit store) in
        Roles.role_names stored.roles = [ "admin"; "support" ]
        &&
        (match events with
        | [ event ] ->
          event.actor = Audit.User "admin"
          && List.assoc_opt "action" event.metadata = Some "replace"
          && List.assoc_opt "before" event.metadata = Some ""
          && List.assoc_opt "after" event.metadata = Some "admin,support"
        | _ -> false)
      | _ -> false)

let%test "role and permission guards deny by default and allow declared grants" =
  let admin_access = Roles.Permission.v_exn "admin.access" in
  (* the policy is declared as CODE and handed to make ONCE — guards read it, no per-call threading *)
  let policy = Roles.policy [ Roles.role Roles.Role.admin [ admin_access ] ] in
  let a = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher ~policy () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let authenticated = Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/admin") |> paw a () in
      let denied = require_permission a admin_access () authenticated in
      let anonymous = require_role a Roles.Role.admin () (Conn.make (req_ "/admin")) in
      let allowed =
        match grant_role a user.id Roles.Role.admin with
        | Error _ -> denied
        | Ok _ ->
          Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/admin")
          |> paw a ()
          |> require_permission a admin_access ()
      in
      (match (Conn.resp denied, Conn.resp anonymous, Conn.resp allowed) with
      | Some denied, Some anonymous, None -> denied.H.status = 403 && anonymous.H.status = 403
      | _ -> false))

let%test "require_user returns 401 for anonymous requests" =
  let c = require_user () (Conn.make (req_ "/private")) in
  match Conn.resp c with Some r -> r.H.status = 401 | None -> false

let%test "logout clears same-request user_id" =
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok _ -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let c = paw a () (Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/")) in
      let c = logout a c in
      user_id c = None && auth_context c = None)

let%test "epoch validation rejects old tokens when enabled" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let _ = logout_other_clients a u.id in
      verify_token a token = Error Invalid_token)

let%test "password login emits success and failure audit events" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user ->
    let _ = login_with_password a (By_username "ada") ~password:"bad" in
    let _ = login_with_password a (By_username "ada") ~password:"pw" in
    let events = Audit.list (Store.audit store) in
    List.exists
      (fun event ->
        event.Audit.kind = Audit.Login_failure && event.target_user_id = Some user.id
        && event.outcome = Audit.Failure "invalid_password")
      events
    && List.exists
         (fun event ->
           event.Audit.kind = Audit.Login && event.target_user_id = Some user.id
           && event.outcome = Audit.Success)
         events
