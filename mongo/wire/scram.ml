(* SCRAM-SHA-256 server side (RFC 5802 + RFC 7677) — MongoDB's default authentication mechanism.

   This is a PURE crypto state machine: it never touches a socket and never generates randomness. The
   server layer above supplies the random server nonce (and the random salt at registration time),
   drives the two steps, and wraps the messages in the saslStart/saslContinue command envelope. Keeping
   it pure means the whole of the security-critical auth logic is a handful of functions checkable
   against the published RFC 7677 test vector (which we do in the test) — if that vector passes, a real
   mongosh client interoperates.

   We never store a plaintext password: {!make_credential} runs PBKDF2-HMAC-SHA256 once and keeps only
   (salt, iterations, StoredKey, ServerKey). Verification recovers ClientKey from the client's proof and
   checks H(ClientKey) = StoredKey, then returns ServerSignature so the client can authenticate us too
   (mutual auth). A wrong password fails at the StoredKey check; a replayed/forged message fails at the
   nonce check.

   SASLprep (RFC 4013) on the password is identity for ASCII passwords (the overwhelmingly common case)
   and is not applied here — non-ASCII passwords must be pre-normalised to match what mongosh sends. *)

exception Auth_failed of string

(* ---- crypto primitives (digestif) ---------------------------------------------------------- *)

let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))
let hmac ~key s = Digestif.SHA256.(to_raw_string (hmac_string ~key s))

let xor a b = String.init (String.length a) (fun i -> Char.chr (Char.code a.[i] lxor Char.code b.[i]))

(* PBKDF2-HMAC-SHA256 with dkLen = hLen (32) — SCRAM uses exactly one output block, so T1 = U1 ^ ... ^ Uc *)
let pbkdf2 ~password ~salt ~iterations =
  let prf msg = hmac ~key:password msg in
  let u1 = prf (salt ^ "\x00\x00\x00\x01") in
  let rec loop n u acc = if n > iterations then acc else let u' = prf u in loop (n + 1) u' (xor acc u') in
  loop 2 u1 u1

(* ---- credentials --------------------------------------------------------------------------- *)

(* a user's stored SCRAM-SHA-256 verifier — no plaintext password is retained *)
type credential = { salt : string; iterations : int; stored_key : string; server_key : string }

let make_credential ~salt ~iterations ~password =
  let salted = pbkdf2 ~password ~salt ~iterations in
  let client_key = hmac ~key:salted "Client Key" in
  { salt; iterations; stored_key = sha256 client_key; server_key = hmac ~key:salted "Server Key" }

(* ---- message parsing (SCRAM attribute lists are comma-separated "k=v") ---------------------- *)

let attr_value attrs key =
  List.find_map
    (fun a ->
      if String.length a >= 2 && a.[0] = key && a.[1] = '=' then Some (String.sub a 2 (String.length a - 2)) else None)
    attrs

(* a SCRAM username escapes ',' as =2C and '=' as =3D *)
let unescape_username u =
  let b = Buffer.create (String.length u) in
  let n = String.length u in
  let rec go i =
    if i >= n then ()
    else if u.[i] = '=' && i + 2 < n && u.[i + 1] = '2' && u.[i + 2] = 'C' then (Buffer.add_char b ','; go (i + 3))
    else if u.[i] = '=' && i + 2 < n && u.[i + 1] = '3' && u.[i + 2] = 'D' then (Buffer.add_char b '='; go (i + 3))
    else (Buffer.add_char b u.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

(* parse client-first "<gs2-header>,<bare>" where bare = "n=<user>,r=<nonce>[,...]" ->
   (username, client-first-bare, client-nonce) *)
let parse_client_first s =
  match String.index_opt s ',' with
  | None -> raise (Auth_failed "malformed client-first")
  | Some c1 -> (
    match String.index_from_opt s (c1 + 1) ',' with
    | None -> raise (Auth_failed "malformed client-first")
    | Some c2 ->
      let bare = String.sub s (c2 + 1) (String.length s - c2 - 1) in
      let attrs = String.split_on_char ',' bare in
      let user = match attr_value attrs 'n' with Some u -> unescape_username u | None -> raise (Auth_failed "no username") in
      let nonce = match attr_value attrs 'r' with Some n -> n | None -> raise (Auth_failed "no client nonce") in
      (user, bare, nonce))

(* ---- the conversation ---------------------------------------------------------------------- *)

(* state carried between the two server steps *)
type t = { cred : credential; client_first_bare : string; server_first : string; combined_nonce : string }

(* step 1: from the client-first message, a credential lookup, and a fresh random server nonce, produce
   (username, server-first message, state). Raises {!Auth_failed} for an unknown user. *)
let server_first ~lookup ~server_nonce client_first =
  let user, bare, cnonce = parse_client_first client_first in
  match lookup user with
  | None -> raise (Auth_failed "authentication failed")
  | Some cred ->
    let combined = cnonce ^ server_nonce in
    let sf = Printf.sprintf "r=%s,s=%s,i=%d" combined (Base64.encode_string cred.salt) cred.iterations in
    (user, sf, { cred; client_first_bare = bare; server_first = sf; combined_nonce = combined })

(* step 2: verify the client-final message and produce the server-final message ("v=..."). Raises
   {!Auth_failed} on a nonce mismatch, unexpected channel binding, or a bad password. *)
let server_final t client_final =
  let attrs = String.split_on_char ',' client_final in
  let chan = match attr_value attrs 'c' with Some c -> c | None -> raise (Auth_failed "no channel binding") in
  let rnonce = match attr_value attrs 'r' with Some r -> r | None -> raise (Auth_failed "no nonce") in
  let proof_b64 = match attr_value attrs 'p' with Some p -> p | None -> raise (Auth_failed "no proof") in
  if not (String.equal rnonce t.combined_nonce) then raise (Auth_failed "nonce mismatch");
  if not (String.equal chan "biws") then raise (Auth_failed "unexpected channel binding");
  let client_final_without_proof = Printf.sprintf "c=%s,r=%s" chan t.combined_nonce in
  let auth_message = String.concat "," [ t.client_first_bare; t.server_first; client_final_without_proof ] in
  let client_signature = hmac ~key:t.cred.stored_key auth_message in
  let proof = try Base64.decode_exn proof_b64 with _ -> raise (Auth_failed "malformed proof") in
  if String.length proof <> String.length client_signature then raise (Auth_failed "malformed proof");
  let client_key = xor proof client_signature in
  if not (String.equal (sha256 client_key) t.cred.stored_key) then raise (Auth_failed "authentication failed");
  let server_signature = hmac ~key:t.cred.server_key auth_message in
  Printf.sprintf "v=%s" (Base64.encode_string server_signature)

(* ---- client side (a follower / tool authenticating TO a SCRAM server) ---------------------- *)

(* escape a username for a SCRAM attribute list: ',' -> =2C, '=' -> =3D (inverse of unescape_username) *)
let escape_username u =
  let b = Buffer.create (String.length u) in
  String.iter (function ',' -> Buffer.add_string b "=2C" | '=' -> Buffer.add_string b "=3D" | c -> Buffer.add_char b c) u;
  Buffer.contents b

let client_first_bare ~user ~nonce = Printf.sprintf "n=%s,r=%s" (escape_username user) nonce

(* from the password, our client-first-bare, and the server-first reply, compute (client-final message, the
   expected server-signature for mutual-auth verification). Raises {!Auth_failed} on a malformed
   server-first. Mirrors {!server_final}: same SaltedPassword -> ClientKey -> proof derivation. *)
let client_final ~password ~client_first_bare ~server_first =
  let attrs = String.split_on_char ',' server_first in
  let combined = match attr_value attrs 'r' with Some r -> r | None -> raise (Auth_failed "no server nonce") in
  let salt =
    match attr_value attrs 's' with
    | Some s -> ( try Base64.decode_exn s with _ -> raise (Auth_failed "malformed salt"))
    | None -> raise (Auth_failed "no salt")
  in
  let iterations =
    match attr_value attrs 'i' with
    | Some i -> ( try int_of_string i with _ -> raise (Auth_failed "malformed iteration count"))
    | None -> raise (Auth_failed "no iteration count")
  in
  let salted = pbkdf2 ~password ~salt ~iterations in
  let client_key = hmac ~key:salted "Client Key" in
  let stored_key = sha256 client_key in
  let cfwp = Printf.sprintf "c=biws,r=%s" combined (* client-final-without-proof *) in
  let auth_message = String.concat "," [ client_first_bare; server_first; cfwp ] in
  let proof = xor client_key (hmac ~key:stored_key auth_message) in
  let server_signature = hmac ~key:(hmac ~key:salted "Server Key") auth_message in
  (Printf.sprintf "%s,p=%s" cfwp (Base64.encode_string proof), Base64.encode_string server_signature)

(* verify the server-final "v=..." proves the server also knows the password (mutual auth) *)
let verify_server_final ~expected_server_sig server_final =
  match attr_value (String.split_on_char ',' server_final) 'v' with Some v -> String.equal v expected_server_sig | None -> false
