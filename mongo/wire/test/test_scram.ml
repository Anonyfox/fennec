(* SCRAM-SHA-256 against the published RFC 7677 test vector (user "user", password "pencil"). If this
   exact exchange reproduces the RFC's server-first and server-final messages byte-for-byte, our PBKDF2 /
   HMAC / proof verification match the spec — which is precisely what a real mongosh client computes, so
   interop is guaranteed. We also check the three failure paths: wrong password, unknown user, tampered
   nonce — each must raise (and the user-facing message stays generic). *)
module S = Mongo_wire.Scram

(* RFC 7677 §5 *)
let salt_b64 = "W22ZaJ0SNY7soEsUEjb6gQ=="
let client_first = "n,,n=user,r=rOprNGfwEbeRWgbNEkqO"
let server_nonce = "%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0"
let expect_server_first = "r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096"
let client_final = "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ="
let expect_server_final = "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4="

let () =
  let salt = Base64.decode_exn salt_b64 in
  let cred = S.make_credential ~salt ~iterations:4096 ~password:"pencil" in
  let lookup = function "user" -> Some cred | _ -> None in

  (* happy path reproduces the RFC vector exactly *)
  let user, sf, st = S.server_first ~lookup ~server_nonce client_first in
  assert (user = "user");
  assert (sf = expect_server_first);
  assert (S.server_final st client_final = expect_server_final);

  (* wrong password: nonce matches but the proof fails against the stored key *)
  let bad = S.make_credential ~salt ~iterations:4096 ~password:"wrong" in
  let _, _, st_bad = S.server_first ~lookup:(function "user" -> Some bad | _ -> None) ~server_nonce client_first in
  (match S.server_final st_bad client_final with _ -> assert false | exception S.Auth_failed _ -> ());

  (* unknown user: server-first itself fails (and says only "authentication failed") *)
  (match S.server_first ~lookup:(fun _ -> None) ~server_nonce "n,,n=ghost,r=abc" with
   | _ -> assert false
   | exception S.Auth_failed msg -> assert (msg = "authentication failed"));

  (* tampered combined nonce in client-final: rejected before any password check *)
  let tampered = "c=biws,r=NOTtheNONCE,p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ=" in
  (match S.server_final st tampered with _ -> assert false | exception S.Auth_failed _ -> ());

  (* ---- client side: reproduce the RFC vector byte-for-byte, then a full round-trip ---- *)
  let bare = S.client_first_bare ~user:"user" ~nonce:"rOprNGfwEbeRWgbNEkqO" in
  assert (bare = "n=user,r=rOprNGfwEbeRWgbNEkqO");
  let cf, expected_sig = S.client_final ~password:"pencil" ~client_first_bare:bare ~server_first:expect_server_first in
  assert (cf = client_final) (* our client's proof == the RFC's client-final *);
  assert (S.verify_server_final ~expected_server_sig:expected_sig expect_server_final);

  (* drive our own server with our own client, end to end; a wrong-password client is rejected *)
  let _, sf2, st2 = S.server_first ~lookup ~server_nonce client_first in
  let cf2, exp2 = S.client_final ~password:"pencil" ~client_first_bare:bare ~server_first:sf2 in
  assert (S.verify_server_final ~expected_server_sig:exp2 (S.server_final st2 cf2));
  let cf_bad, _ = S.client_final ~password:"wrong" ~client_first_bare:bare ~server_first:sf2 in
  (match S.server_final st2 cf_bad with _ -> assert false | exception S.Auth_failed _ -> ());

  print_string "scram (SCRAM-SHA-256, RFC 7677 vector, client+server): OK\n"
