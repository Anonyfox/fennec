(* Backend parity for Accounts over the embedded Burrow engine — the path that used to fall back to
   "unavailable" before the store was unified onto Backend.S. A standalone exe (Eio + a temp burrow dir,
   no mongod): set MONGO_URL=burrow://<tmp>, install the data-layer switch, then drive the PUBLIC Accounts
   API (the same calls userland makes) and assert the same outcomes the minimongo inline tests assert.
   It also exercises the unique+SPARSE username/email indexes on burrow: email-only users (no username)
   coexist, while a duplicate email is rejected. If accounts ever breaks on burrow, this fails the build. *)
module A = Fennec_server.Accounts

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
  A.boot ();
  let a = A.current () in

  (* 1. password signup + login + token all resolve to the same user — on the embedded engine *)
  let user = ok "create_user(Ada)" (A.create_user a ~username:"Ada" ~email:"ADA@example.com" ~password:"pw" ()) in
  let _, token = ok "login_with_password(by email)" (A.login_with_password a (A.By_email "ada@example.com") ~password:"pw") in
  let uid = ok "verify_token" (A.verify_token a token) in
  if uid <> user.A.id then failf "verify_token resolved a different user id than create_user";
  if not (Result.is_ok (A.login_with_password a (A.By_username "ada") ~password:"pw")) then
    failf "username login failed on burrow";

  (* 2. unique email is enforced on burrow (the DB safety-net + the app check agree) *)
  expect_error "duplicate email" (A.create_user a ~email:"ada@example.com" ());

  (* 3. SPARSE username/email indexes: several users with NO username coexist — a non-sparse unique index
        would reject the 2nd and 3rd as a shared "missing username" collision *)
  let _ = ok "create_user(email-only #1)" (A.create_user a ~email:"u1@example.com" ()) in
  let _ = ok "create_user(email-only #2)" (A.create_user a ~email:"u2@example.com" ()) in
  let _ = ok "create_user(email-only #3)" (A.create_user a ~email:"u3@example.com" ()) in

  Unix.putenv "MONGO_URL" "";
  print_endline "accounts/burrow parity: OK (password login + token + username login + unique email + sparse usernames)"
