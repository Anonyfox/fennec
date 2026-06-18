(* Backend parity for Accounts over the embedded Burrow engine — the path that used to fall back to
   "unavailable" before the store was unified onto Backend.S. A standalone exe (Eio + a temp burrow dir,
   no mongod): set MONGO_URL=burrow://<tmp>, install the data-layer switch, then drive the PUBLIC Accounts
   API (the same calls userland makes) and assert the same outcomes the minimongo inline tests assert.
   It also exercises the unique+SPARSE username/email indexes on burrow: email-only users (no username)
   coexist, while a duplicate email is rejected. If accounts ever breaks on burrow, this fails the build. *)
module A = Fennec_accounts.Accounts

let failf fmt = Printf.ksprintf (fun s -> prerr_endline ("accounts/burrow FAIL: " ^ s); exit 1) fmt
let ok label = function Ok v -> v | Error _ -> failf "%s returned Error" label
let expect_error label = function Error _ -> () | Ok _ -> failf "%s should have failed but returned Ok" label

let () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  Mirage_crypto_rng_unix.use_default ();
  let dir =
    Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "fennec_accounts_burrow_%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (* a storage-only burrow:// URL (no authority => no wire endpoint), pointing at the temp engine dir *)
  Unix.putenv "MONGO_URL" ("burrow://" ^ dir);
  Unix.putenv "FENNEC_ACCOUNTS_SECRET" "accounts-burrow-parity-secret-0123456789";
  (* the data layer opens collections by name on this switch; accounts builds its store eagerly *)
  Fennec_mongo_dynamic.Dynamic.set_switch sw;
  (* declare the RBAC policy ONCE on the umbrella config BEFORE boot forces current () — the policy is
     construction-frozen on the instance, so it must be set before the singleton is built. This is what
     lets the no-[t] guards (Stage 3c) resolve against current ()'s policy. *)
  let guard_permission = A.Roles.Permission.v_exn "admin.access" in
  let guard_policy = A.Roles.policy [ A.Roles.role A.Roles.Role.admin [ guard_permission ] ] in
  A.start ~config:{ A.defaults with rbac = Some guard_policy } ();
  A.boot ();
  let a = A.current () in

  (* 1. password signup + login + token all resolve to the same user — on the embedded engine *)
  let user = ok "create_user(Ada)" (A.create_user a ~username:"Ada" ~email:"ADA@example.com" ~password:"pw" ()) in
  let _, token = ok "login_with_password(by email)" (A.login_with_password a (A.By_email "ada@example.com") ~password:"pw") in
  let uid = ok "verify_token" (A.verify_token a token) in
  if uid <> user.A.id then failf "verify_token resolved a different user id than create_user";
  if not (Result.is_ok (A.login_with_password a (A.By_username "ada") ~password:"pw")) then
    failf "username login failed on burrow";

  (* 2. unique email/username is enforced on burrow, case-INsensitively, via the indexed lookups on the
        lowercased shadow fields (the DB safety-net + the app check agree). A case variant of either is a dup. *)
  expect_error "duplicate email" (A.create_user a ~email:"ada@example.com" ());
  expect_error "duplicate email (case variant)" (A.create_user a ~email:"ADA@EXAMPLE.com" ());
  expect_error "duplicate username (case variant)" (A.create_user a ~username:"ADA" ~email:"someone@example.com" ());

  (* 3. SPARSE username/email indexes: several users with NO username coexist — a non-sparse unique index
        would reject the 2nd and 3rd as a shared "missing username" collision *)
  let _ = ok "create_user(email-only #1)" (A.create_user a ~email:"u1@example.com" ()) in
  let _ = ok "create_user(email-only #2)" (A.create_user a ~email:"u2@example.com" ()) in
  let _ = ok "create_user(email-only #3)" (A.create_user a ~email:"u3@example.com" ()) in

  (* 4. PER-SESSION revocation on burrow (gap (a) — the thing the old all-or-nothing epoch bump could not
        do). A fresh user logs in twice → two live rows in the accounts_tokens collection → revoke ONE by
        its session_id → that token's verify_token fails while the other still verifies, and the active-
        sessions list drops to one. validate_every_request is the default (false), so this exercises
        verify_token's UNCONDITIONAL token-store gate on the embedded engine, not the epoch path. *)
  let carol = ok "create_user(carol)" (A.create_user a ~username:"carol" ~password:"pw" ()) in
  let _, c1 = ok "carol login #1" (A.login_with_password a (A.By_username "carol") ~password:"pw") in
  let _, c2 = ok "carol login #2" (A.login_with_password a (A.By_username "carol") ~password:"pw") in
  let target =
    match ok "list_sessions(carol)" (A.list_sessions a carol.A.id) with
    | [ (s : A.session_info); _ ] -> s.session_id
    | other -> failf "expected exactly 2 live sessions for carol, got %d" (List.length other)
  in
  (match A.revoke_session a ~user_id:carol.A.id ~session_id:target with
  | Ok true -> ()
  | Ok false -> failf "revoke_session reported nothing revoked"
  | Error _ -> failf "revoke_session returned Error");
  let v1 = Result.is_ok (A.verify_token a c1) in
  let v2 = Result.is_ok (A.verify_token a c2) in
  if v1 = v2 then failf "expected exactly one of carol's two tokens to be revoked (v1=%b v2=%b)" v1 v2;
  (match A.list_sessions a carol.A.id with
  | Ok [ _ ] -> ()
  | Ok other -> failf "expected one live session after per-session revoke, got %d" (List.length other)
  | Error _ -> failf "list_sessions failed after revoke");

  (* 5. RBAC GUARD EQUIVALENCE (Stage 3c). The no-[t] guards [A.require_role] / [A.require_permission]
        read the policy + instance from current () — the SAME singleton (with [guard_policy] above) that
        authenticates every request. Prove they decide identically to the deprecated explicit-[t] forms
        ([A.Advanced.*] given current ()): one configured policy, one decision. *)
  let module H = Paw.Http in
  let req path headers = Paw.Conn.make (H.make_request ~meth:H.GET ~path ~headers ()) in
  let status_of c = match A.Conn.resp c with Some (r : H.response) -> Some r.status | None -> None in
  let dave = ok "create_user(dave)" (A.create_user a ~username:"dave" ~password:"pw" ()) in
  let _ = ok "grant_role(dave, admin)" (A.grant_role a dave.A.id A.Roles.Role.admin) in
  let _, dtok = ok "dave login" (A.login_with_password a (A.By_username "dave") ~password:"pw") in
  (* current () was built from { defaults with rbac = … }, so its cookie name is defaults.session.cookie *)
  let cookie_name = A.defaults.A.session.A.cookie in
  let authed () = req "/admin" [ ("Cookie", cookie_name ^ "=" ^ A.token_to_string dtok) ] |> A.paw a () in
  let anon () = req "/admin" [] in
  (* the no-[t] daily-driver forms, bound to current () *)
  let role_authed = status_of (A.require_role A.Roles.Role.admin () (authed ())) in
  let role_anon = status_of (A.require_role A.Roles.Role.admin () (anon ())) in
  let perm_authed = status_of (A.require_permission guard_permission () (authed ())) in
  let perm_anon = status_of (A.require_permission guard_permission () (anon ())) in
  (* the deprecated explicit-instance shims, given the SAME current () instance *)
  let dep_role_authed = status_of ((A.Advanced.require_role [@alert "-deprecated"]) a A.Roles.Role.admin () (authed ())) in
  let dep_perm_anon = status_of ((A.Advanced.require_permission [@alert "-deprecated"]) a guard_permission () (anon ())) in
  if not (role_authed = None) then failf "no-t require_role denied an admin (got %s)" (match role_authed with Some s -> string_of_int s | None -> "pass");
  if not (perm_authed = None) then failf "no-t require_permission denied a granted admin";
  if not (role_anon = Some 403) then failf "no-t require_role did not 403 an anonymous request";
  if not (perm_anon = Some 403) then failf "no-t require_permission did not 403 an anonymous request";
  (* equivalence: the no-[t] form and the explicit-[t] shim resolve to the same decision *)
  if role_authed <> dep_role_authed then failf "no-t and explicit-t require_role disagree on an admin";
  if perm_anon <> dep_perm_anon then failf "no-t and explicit-t require_permission disagree on anonymous";

  Unix.putenv "MONGO_URL" "";
  print_endline
    "accounts/burrow parity: OK (password login + token + username login + unique email + sparse usernames + \
     per-session revocation + RBAC guard equivalence)"
