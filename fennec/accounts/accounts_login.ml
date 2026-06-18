(* accounts_login.ml — login orchestration: issuing + recording a session (issue_session /
   record_session / finish_login), the MFA step-up branch (has_active_mfa / login_step_up /
   complete_login_unless_mfa / complete_login_step_up), the account-enumeration timing guard, the
   whole login_with_* family (password / strategy / external identity / email link+OTP / OIDC /
   SAML / passkey / token resume) with their MFA-aware completions, the reset/enroll/verify
   session completions, the OAuth-over-DDP credential handoff, the passkey ceremony API, and
   cookie/logout/verify_token/active-session management. Sits on Accounts_lifecycle. Carved
   verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets
open Accounts_identity_bridge
open Accounts_runtime
open Accounts_request
open Accounts_lifecycle

let issue_session t ?(factors = []) ~strategy (u : user) =
  let iat = now () in
  { uid = u.id; sid = random_id (); iat; exp = iat +. t.lifetime; auth_epoch = u.auth_epoch; strategy; factors }

let session_info_of_session (s : session) : session_info =
  { session_id = s.sid; user_id = s.uid; created_at = s.iat; expires_at = s.exp; last_active_at = s.iat; strategy = s.strategy }

(* Persist the token row for a freshly minted session (only the SHA-256 of the token is stored). The
   [sid] becomes the row key, so a later [token_live] / [list_sessions] / [revoke_session] can find it. *)
let record_session t (s : session) token = t.store.tokens.record (session_info_of_session s) ~hashed:(sha256_hex token)

let finish_login t ?factors ~strategy (u : user) =
  if not (user_can_login u.status) then (
    observe_login_failure t { strategy; user = Some u; allowed = false; reason = Some "account_inactive" };
    Error (Login_rejected "Account is not active"))
  else if t.config.require_verified_email && not (List.exists (fun e -> e.verified) u.emails) then (
    observe_login_failure t { strategy; user = Some u; allowed = false; reason = Some "email_not_verified" };
    Error (Login_rejected "Email not verified"))
  else
  let attempt = { strategy; user = Some u; allowed = true; reason = None } in
  match run_login_hooks t attempt with
  | Error (Login_rejected reason as e) ->
    observe_login_failure t { attempt with allowed = false; reason = Some reason };
    record_audit ?mechanism:(mechanism_of_strategy strategy) t Audit.Login_failure Audit.Anonymous
      (Audit.Failure reason);
    Error e
  | Error _ as e -> e
  | Ok () ->
    let s = issue_session t ?factors ~strategy u in
    let token = sign t s in
    (* Fail-closed: a login that cannot persist its session record is not a durable login. Stricter
       than [append_audit]'s best-effort, because the token row is the security source of truth. *)
    (match record_session t s token with
    | Error _ as e -> e
    | Ok () ->
      observe_login t u;
      record_audit ~target_user_id:u.id ?mechanism:(mechanism_of_strategy strategy) t
        (login_audit_kind strategy) (Audit.User u.id) Audit.Success;
      Ok (u, token))

let has_active_mfa t user =
  t.store.mfa.Mfa.list ~user_id:user.id ()
  |> List.exists (fun (enrollment : Mfa.enrollment) -> enrollment.status = Mfa.Active)

let login_step_up t ~strategy user =
  match Mfa.requirement Mfa.Multi_factor with
  | Error e -> Error (Login_rejected (Mfa.string_of_error e))
  | Ok requirement -> (
    let mfa = Mfa.make ~secret:(t.secret ^ "\000accounts-mfa") ~challenge:(challenge_service t ()) in
    let data = [ ("strategy", Bson.String strategy) ] in
    match Mfa.issue_step_up mfa ~data ~user_id:user.id requirement with
    | Error e -> Error (Login_rejected (Mfa.string_of_error e))
    | Ok step_up -> Ok { user; step_up })

let complete_login_unless_mfa t ~strategy user =
  if has_active_mfa t user then Result.map (fun step_up -> Step_up_required step_up) (login_step_up t ~strategy user)
  else Result.map (fun (user, token) -> Complete_login (user, token)) (finish_login t ~strategy user)

(* Account-enumeration timing defense: when the selector matches no user, still verify the password against
   a precomputed dummy hash so a missing account costs the same PBKDF2 as a wrong password — no timing
   oracle reveals whether an email/username exists. The dummy hash is memoised per process and the verify
   result is always discarded, so a hasher mismatch (e.g. across test hashers) is harmless. *)
let _enum_guard_hash : string option Atomic.t = Atomic.make None
let enumeration_guard (hasher : password_hasher) ~password =
  let hash =
    match Atomic.get _enum_guard_hash with
    | Some h -> h
    | None ->
      let h = try hasher.hash ~password:"fennec-account-enumeration-guard" with _ -> "" in
      Atomic.set _enum_guard_hash (Some h);
      h
  in
  (try ignore (hasher.verify ~password ~hash) with _ -> ())

let login_with_password_completion t selector ~password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher -> (
    match find_by_selector t selector with
    | Error _ as e -> e
    | Ok None ->
      enumeration_guard hasher ~password;
      observe_login_failure t { strategy = "password"; user = None; allowed = false; reason = Some "user_not_found" };
      record_audit ~mechanism:Audit.Password t Audit.Login_failure Audit.Anonymous
        (Audit.Failure "user_not_found");
      Error (if t.config.ambiguous_error_messages then Invalid_password else User_not_found)
    | Ok (Some u) -> (
      match t.store.users.password_hash u.id with
      | Error _ as e -> e
      | Ok None -> Error Password_not_configured
      | Ok (Some hash) ->
        if not (hasher.verify ~password ~hash) then (
          observe_login_failure t { strategy = "password"; user = Some u; allowed = false; reason = Some "invalid_password" };
          record_audit ~target_user_id:u.id ~mechanism:Audit.Password t Audit.Login_failure
            Audit.Anonymous (Audit.Failure "invalid_password");
          Error Invalid_password)
        else complete_login_unless_mfa t ~strategy:"password" u))

let require_complete_login = function
  | Ok (Complete_login (user, token)) -> Ok (user, token)
  | Ok (Step_up_required _) -> Error (Login_rejected "MFA step-up required")
  | Error _ as e -> e

let login_with_password t selector ~password = require_complete_login (login_with_password_completion t selector ~password)

let login_with_strategy_completion t name ~credentials =
  match Hashtbl.find_opt t.strategies name with
  | None -> Error (Strategy_not_found name)
  | Some strategy -> (
    match strategy.login ~credentials with
    | Error _ as e -> e
    | Ok u -> (
      match t.store.users.find_user_by_id u.id with
      | Error _ as e -> e
      | Ok None -> Error User_not_found
      | Ok (Some u) -> complete_login_unless_mfa t ~strategy:name u))

let login_with_strategy t name ~credentials =
  require_complete_login (login_with_strategy_completion t name ~credentials)

let finish_identity_login_completion t ~strategy ~created ?linked user =
  Result.bind (complete_login_unless_mfa t ~strategy user) (function
    | Complete_login (user, token) -> Ok (Complete_identity_login { user; token; created; linked })
    | Step_up_required step_up -> Ok (Identity_step_up_required step_up))
let step_up_strategy state =
  match List.assoc_opt "strategy" state.Mfa.data with
  | Some (Bson.String strategy) when String.trim strategy <> "" -> strategy
  | _ -> "mfa"

let complete_login_step_up t token verification =
  let mfa = mfa_service t in
  Result.bind
    (Result.map_error mfa_enrollment_error (Mfa.consume_step_up mfa ~expected_user:verification.user_id token))
    (fun state ->
      let strategy = step_up_strategy state in
      if verification.user_id <> state.user_id then Error (Login_rejected "MFA verification user mismatch")
      else
        let assurance = verification.assurance in
        let factors = merge_mfa_factors [] assurance.Mfa.factors in
        let combined_factors =
          match factor_of_strategy strategy with
          | None -> factors
          | Some factor -> add_mfa_factor factors factor
        in
        let combined = Mfa.assurance ~now:(fun () -> assurance.authenticated_at) combined_factors in
        Result.bind (Result.map_error mfa_enrollment_error (Mfa.require state.requirement combined)) (fun () ->
            Result.bind (find_required_user t state.user_id) (fun user -> finish_login t ~factors ~strategy user)))
let reset_password_completion t token ~password =
  Result.bind (reset_password_user t token ~password) (complete_login_unless_mfa t ~strategy:"resetPassword")

let reset_password t token ~password =
  Result.bind (reset_password_completion t token ~password) (function
    | Complete_login (user, token) -> Ok (user, token)
    | Step_up_required _ -> Error (Login_rejected "MFA step-up required"))
let enroll_account_completion t token ~password =
  Result.bind (enroll_account_user t token ~password) (complete_login_unless_mfa t ~strategy:"enrollAccount")

let enroll_account t token ~password =
  Result.bind (enroll_account_completion t token ~password) (function
    | Complete_login (user, token) -> Ok (user, token)
    | Step_up_required _ -> Error (Login_rejected "MFA step-up required"))

let verify_email_completion t token =
  Result.bind (verify_email t token) (complete_login_unless_mfa t ~strategy:"verifyEmail")
let link_existing_identity t identity_store ~finish ?current_user_id ~now (facts : external_identity) =
  let created_at = now () in
  let verified_at = if facts.email_verified || Identity.usable_for_login facts.key then Some created_at else None in
  let attach_and_login user =
    Result.bind (attach_identity identity_store ?verified_at ~created_at ~user_id:user.id facts.key) (fun linked ->
        let user = add_service facts.service user in
        Result.bind (t.store.users.update_user user) (fun user -> finish ~created:false ?linked user))
  in
  match current_user_id with
  | Some uid -> Result.bind (find_required_user t uid) attach_and_login
  | None -> (
    match identity_store.Identity.find facts.key with
    | Some link -> Result.bind (find_required_user t link.user_id) (finish ~created:false)
    | None -> Error User_not_found)

let resolve_identity_login t ?identity_store ?current_user_id ?(allow_signup = false)
    ?(link_verified_email = false) ?(now = now) ~strategy facts ~finish =
  let identity_store = Option.value identity_store ~default:t.store.identities in
  if String.trim strategy = "" then Error (Strategy_not_found strategy)
  else
    match link_existing_identity t identity_store ~finish ?current_user_id ~now facts with
    | Ok _ as ok -> ok
    | Error (Login_rejected _ as e) -> Error e
    | Error User_not_found -> (
      let created_at = now () in
      let verified_at = if facts.email_verified || Identity.usable_for_login facts.key then Some created_at else None in
      let link_user user =
        Result.bind (attach_identity identity_store ?verified_at ~created_at ~user_id:user.id facts.key) (fun linked ->
            let user = add_service facts.service user in
            Result.bind (t.store.users.update_user user) (fun user -> finish ~created:false ?linked user))
      in
      match (link_verified_email, facts.email_verified, facts.email) with
      | true, true, Some email -> (
        match t.store.users.find_user_by_email email with
        | Error _ as e -> e
        | Ok (Some user) -> link_user user
        | Ok None when allow_signup ->
          Result.bind (create_user t ?username:facts.username ?email:facts.email ?profile:facts.profile ()) (fun user ->
              let created_user = add_service facts.service user in
              Result.bind (t.store.users.update_user created_user) (fun created_user ->
                  Result.bind
                    (attach_identity identity_store ?verified_at ~created_at ~user_id:created_user.id facts.key)
                    (fun linked -> finish ~created:true ?linked created_user)))
        | Ok None -> Error User_not_found)
      | _ when allow_signup ->
        Result.bind (create_user t ?username:facts.username ?email:facts.email ?profile:facts.profile ()) (fun user ->
            let created_user = add_service facts.service user in
            Result.bind (t.store.users.update_user created_user) (fun created_user ->
                Result.bind
                  (attach_identity identity_store ?verified_at ~created_at ~user_id:created_user.id facts.key)
                  (fun linked -> finish ~created:true ?linked created_user)))
      | _ -> Error User_not_found)
    | Error _ as e -> e

(* beforeExternalLogin veto (Meteor parity): every hook must return true for an external (OAuth/OIDC/
   SAML) login to proceed. Shared by the redirect mint chokepoint below and the OAuth-over-DDP resolve
   step so both honour the same veto with identical arguments. *)
let before_external_login_allowed t ~strategy ~identity ~user =
  List.for_all (fun h -> h ~strategy ~identity ~user) t.before_external_login_hooks

let login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    ~strategy facts =
  (* the veto fires once at the mint chokepoint with the resolved user; a false vote aborts before a
     session is issued *)
  let finish ~created ?linked user =
    if before_external_login_allowed t ~strategy ~identity:facts ~user then
      finish_identity_login_completion t ~strategy ~created ?linked user
    else Error (Login_rejected "external login rejected by before_external_login hook")
  in
  resolve_identity_login t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now ~strategy
    facts ~finish

let require_complete_identity_login = function
  | Ok (Complete_identity_login login) -> Ok login
  | Ok (Identity_step_up_required _) -> Error (Login_rejected "MFA step-up required")
  | Error _ as e -> e

let login_with_identity t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now ~strategy
    facts =
  require_complete_identity_login
    (login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
       ~strategy facts)

(* OAuth-over-DDP popup handshake (Meteor parity). A provider redirect lands on a popup callback that
   resolves the external identity to a user — running create/link/find and the beforeExternalLogin
   veto — but does NOT mint a session. The callback hands the opener SPA a single-use
   {credentialToken, credentialSecret} pair (backed by an [OAuth_credential] challenge); the SPA
   replays the pair through the [login {oauth}] DDP method, which mints the session at that point (and
   may still demand MFA step-up). Mirrors Meteor's pendingCredential: resolve at the callback, mint at
   the method — so no orphan login token is ever issued, and no secret is stored in the challenge. *)
let resolve_external_user t ?current_user_id ?(allow_signup = false) ?(link_verified_email = false) ~strategy
    facts =
  let finish ~created:_ ?linked:_ user =
    if before_external_login_allowed t ~strategy ~identity:facts ~user then Ok user
    else Error (Login_rejected "external login rejected by before_external_login hook")
  in
  resolve_identity_login t ?current_user_id ~allow_signup ~link_verified_email ~strategy facts ~finish

(* Issue the handoff. The challenge token's wire shape is "<id>.<secret>"; we surface the id as the
   public credentialToken and the secret as the credentialSecret, so an opener that only learns the
   token still cannot complete the login. Short TTL (the popup bounces in milliseconds) and a small
   attempt cap on top of the 32-byte secret. *)
let issue_oauth_credential t ~user_id ?provider () =
  let metadata =
    {
      Challenge.empty_metadata with
      user_id = Some user_id;
      data = (match provider with Some p -> [ ("provider", Bson.str p) ] | None -> []);
    }
  in
  match
    Challenge.create (challenge_service t ()) ~purpose:Challenge.OAuth_credential ~metadata ~ttl:300.
      ~max_attempts:5 ()
  with
  | Error e -> Error (Login_rejected (Challenge.string_of_error e))
  | Ok issued ->
    let full = Challenge.token_to_string issued.Challenge.token in
    let secret =
      match String.index_opt full '.' with
      | Some i -> String.sub full (i + 1) (String.length full - i - 1)
      | None -> full
    in
    Ok (issued.Challenge.record.Challenge.id, secret)

(* Consume the handoff: reassemble the challenge token, verify+expire it single-use, and return the
   bound user (plus the originating provider for the audit strategy label). *)
let consume_oauth_credential t ~credential_token ~credential_secret =
  let token = Challenge.token_of_string (credential_token ^ "." ^ credential_secret) in
  match Challenge.consume (challenge_service t ()) ~purpose:Challenge.OAuth_credential token with
  | Error e -> Error (Login_rejected (Challenge.string_of_error e))
  | Ok record -> (
    let md = record.Challenge.metadata in
    match md.Challenge.user_id with
    | None -> Error (Login_rejected "OAuth credential is missing its user binding")
    | Some user_id ->
      let provider =
        match List.assoc_opt "provider" md.Challenge.data with Some (Bson.String p) -> Some p | _ -> None
      in
      Ok (user_id, provider))

let email_identity_of_record record =
  match record.Challenge.metadata.email with
  | None -> Error (Login_rejected "Email challenge did not bind an email address")
  | Some email -> (
    match Email.normalize email with
    | Error e -> Error (email_error e)
    | Ok address -> Ok (email_identity address))

let login_with_email_record_completion t ?identity_store ?current_user_id ?allow_signup
    ?(link_verified_email = true) ?now ~strategy record =
  Result.bind (email_identity_of_record record) (fun facts ->
      login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ~link_verified_email ?now
        ~strategy facts)

let login_with_email_record t ?identity_store ?current_user_id ?allow_signup ?(link_verified_email = true)
    ?now ~strategy record =
  Result.bind (email_identity_of_record record) (fun facts ->
      login_with_identity t ?identity_store ?current_user_id ?allow_signup ~link_verified_email ?now ~strategy
        facts)

let login_with_email_link_completion t ?identity_store email ?expected ?current_user_id ?allow_signup
    ?(link_verified_email = true) ?now token =
  match Email.consume_login_link email ?expected token with
  | Error e -> Error (email_error e)
  | Ok record ->
    login_with_email_record_completion t ?identity_store ?current_user_id ?allow_signup ~link_verified_email
      ?now ~strategy:"email" record

let login_with_email_link t ?identity_store email ?expected ?current_user_id ?allow_signup
    ?(link_verified_email = true) ?now token =
  match Email.consume_login_link email ?expected token with
  | Error e -> Error (email_error e)
  | Ok record ->
    login_with_email_record t ?identity_store ?current_user_id ?allow_signup ~link_verified_email ?now
      ~strategy:"email" record

let login_with_email_otp_completion t ?identity_store email ?current_user_id ?allow_signup
    ?(link_verified_email = true) ?now ~token ~code () =
  match Email.consume_otp email ~token ~code with
  | Error e -> Error (email_error e)
  | Ok record ->
    login_with_email_record_completion t ?identity_store ?current_user_id ?allow_signup ~link_verified_email
      ?now ~strategy:"email_otp" record

let login_with_email_otp t ?identity_store email ?current_user_id ?allow_signup ?(link_verified_email = true)
    ?now ~token ~code () =
  match Email.consume_otp email ~token ~code with
  | Error e -> Error (email_error e)
  | Ok record ->
    login_with_email_record t ?identity_store ?current_user_id ?allow_signup ~link_verified_email ?now
      ~strategy:"email_otp" record

let login_with_oidc_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    principal =
  login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    ~strategy:"oidc" (oidc_identity principal)

let login_with_oidc t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now principal =
  login_with_identity t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    ~strategy:"oidc" (oidc_identity principal)

let login_with_saml_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    (principal : Saml.principal) =
  let allow_signup = match allow_signup with Some v -> v | None -> principal.allow_jit in
  login_with_identity_completion t ?identity_store ?current_user_id ~allow_signup ?link_verified_email ?now
    ~strategy:"saml" (saml_identity principal)

let login_with_saml t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    (principal : Saml.principal) =
  let allow_signup = match allow_signup with Some v -> v | None -> principal.allow_jit in
  login_with_identity t ?identity_store ?current_user_id ~allow_signup ?link_verified_email ?now
    ~strategy:"saml" (saml_identity principal)

let login_with_passkey_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    assertion =
  Result.bind (passkey_identity assertion) (fun facts ->
      login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
        ~strategy:"passkey" facts)

let login_with_passkey t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now assertion =
  Result.bind (passkey_identity assertion) (fun facts ->
      login_with_identity t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
        ~strategy:"passkey" facts)

let register_passkey_credential t credential =
  Result.bind (find_required_user t credential.Passkey.user_id) (fun _ ->
      Result.bind
        (Result.map_error (fun e -> Store_error e) (t.store.passkeys.Passkey.insert credential))
        (fun () ->
          match Passkey.identity credential with
          | Error e -> Error (Login_rejected (Passkey.string_of_error e))
          | Ok key -> (
            match
              attach_identity t.store.identities ~verified_at:credential.created_at
                ~created_at:credential.created_at ~user_id:credential.user_id key
            with
            | Error _ as e -> e
            | Ok (Some link) ->
              record_audit ~target_user_id:credential.user_id ~mechanism:Audit.Passkey t
                Audit.Passkey_registration (Audit.User credential.user_id) Audit.Success;
              Ok link
            | Ok None -> Error (Store_error "Passkey identity was not linked"))))

let login_with_passkey_assertion_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email
    ?now assertion =
  Result.bind
    (Result.map_error (fun e -> Store_error e)
       (t.store.passkeys.Passkey.update assertion.Passkey.credential))
    (fun () ->
      login_with_passkey_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
        assertion)

let login_with_passkey_assertion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    assertion =
  require_complete_identity_login
    (login_with_passkey_assertion_completion t ?identity_store ?current_user_id ?allow_signup
       ?link_verified_email ?now assertion)

let login_with_token t token =
  match verify_session t token with
  | Error _ as e -> e
  | Ok s -> (
    (* The presented resume token must still have a live row (gate revocation/early-expiry), then we
       mint + record the replacement and ROTATE: drop the old [sid] row so the active-sessions list
       stays one-per-chain and the old resume token cannot be reused. Record-before-revoke = fail-open
       on a crash (the user stays logged in via the new token rather than losing both). *)
    match Result.bind (token_live t s token) (fun () -> t.store.users.find_user_by_id s.uid) with
    | Error _ as e -> e
    | Ok None -> Error User_not_found
    | Ok (Some u) when u.auth_epoch <> s.auth_epoch -> Error Invalid_token
    | Ok (Some u) -> (
      match finish_login t ~strategy:"resume" u with
      | Ok _ as ok ->
        ignore (t.store.tokens.revoke ~sid:s.sid);
        ok
      | Error _ as e -> e))

let set_login_cookie t c ?(same_site = Cookie.Lax) ?(http_only = true) ?secure token =
  (* Secure-by-default on HTTPS (honouring X-Forwarded-Proto), exactly as the Session middleware — so the
     login cookie can't leak over plaintext http. Caller can still force it with ~secure. *)
  let secure = match secure with Some b -> b | None -> Session.forwarded_scheme c = "https" in
  Conn.set_cookie c t.cookie token ~path:t.path ~max_age:(int_of_float t.lifetime) ~secure ~http_only
    ~same_site

let logout t c =
  let uid = user_id c in
  observe_logout t uid;
  Option.iter
    (fun uid -> record_audit ~target_user_id:uid ~mechanism:Audit.Token t Audit.Logout (Audit.User uid) Audit.Success)
    uid;
  (* Revoke THIS session's row server-side (best-effort; the [sid] is present only when [paw] ran
     upstream and accepted the cookie). The cookie is always cleared regardless. *)
  (match Conn.get c session_key with Some (Some s) -> ignore (t.store.tokens.revoke ~sid:s.sid) | _ -> ());
  Conn.assign (Conn.assign (Conn.delete_cookie c ~path:t.path t.cookie) user_id_key None) session_key None

(* Bump the epoch (invalidates every signed session, current included, on the gated paths) AND prune
   all of the user's now-dead token rows so the active-sessions list reflects reality. *)
let logout_other_clients t uid =
  Result.bind (t.store.users.bump_auth_epoch uid) (fun _ ->
      Result.map (fun _ -> ()) (t.store.tokens.revoke_user uid ()))

let logout_other_clients_and_refresh t uid =
  Result.bind (t.store.users.bump_auth_epoch uid) (fun _ ->
      Result.bind (find_required_user t uid) (fun user ->
          Result.bind (finish_login t ~strategy:"resume" user) (fun (user, token) ->
              (* finish_login recorded the replacement row; prune every OTHER row for this user, keeping
                 the just-issued session so the refreshed client stays in the active-sessions list. *)
              let keep = match verify_session t token with Ok s -> Some s.sid | Error _ -> None in
              ignore (t.store.tokens.revoke_user uid ?keep ());
              Ok (user, token))))

let verify_token t token =
  match Result.bind (verify_session t token) (checked_session t) with
  | Error _ as e -> e
  | Ok s -> (
    (* [verify_token] is a token-presentation path (DDP resume validation, API bearer), so the
       revocation gate is UNCONDITIONAL here — independent of [validate_every_request]. *)
    match token_live t s token with
    | Error _ as e -> e
    | Ok () ->
      ignore (t.store.tokens.touch ~sid:s.sid ~now:(now ()));
      Ok s.uid)

(* List a user's live (non-revoked, non-expired) sessions for an "active sessions" UI. *)
let list_sessions t uid = t.store.tokens.list_for_user uid ~now:(now ())

(* Revoke one specific session by its [session_id], scoped to its owner (a user cannot revoke another
   user's session by guessing an id). Returns [true] when a live session was found and removed. No
   epoch bump — this is the per-device revocation the epoch could never do. *)
let revoke_session t ~user_id ~session_id =
  match t.store.tokens.list_for_user user_id ~now:(now ()) with
  | Error _ as e -> e
  | Ok sessions ->
    if List.exists (fun (s : session_info) -> s.session_id = session_id) sessions then
      t.store.tokens.revoke ~sid:session_id
    else Ok false

(* Revoke all of a user's sessions except [keep] (e.g. "log out my other devices" from the current
   session). Returns the number of sessions removed. *)
let revoke_other_sessions t ~user_id ~keep = t.store.tokens.revoke_user user_id ~keep ()
(* The browser WebAuthn wire shapes now live in {!Accounts_passkey} (their protocol owner); the engine
   builds the typed ceremony and renders/parses through the feature, mapping a malformed response to the
   engine's stable [Login_rejected] error. *)
let begin_passkey_registration t relying_party user =
  let passkey = Passkey.make ~challenge:(challenge_service t ()) in
  Result.map_error (fun e -> Login_rejected (Passkey.string_of_error e))
    (Passkey.begin_registration passkey relying_party user)
  |> Result.map (fun registration ->
         { registration; json = Passkey.registration_options_json registration })

let passkey_registration_response_of_json json =
  match Passkey.registration_response_of_json json with
  | Some response -> Ok response
  | None -> Error (Login_rejected "Malformed passkey registration response")

let passkey_assertion_response_of_json json =
  match Passkey.assertion_response_of_json json with
  | Some response -> Ok response
  | None -> Error (Login_rejected "Malformed passkey assertion response")

let finish_passkey_registration t relying_party ~user_id ~token json =
  Result.bind (passkey_registration_response_of_json json) (fun response ->
      let passkey = Passkey.make ~challenge:(challenge_service t ()) in
      Result.bind
        (Result.map_error (fun e -> Login_rejected (Passkey.string_of_error e))
           (Passkey.finish_registration passkey relying_party response ~token ~user_id))
        (fun credential ->
          Result.map
            (fun link -> { credential; link })
            (register_passkey_credential t credential)))

let begin_passkey_assertion t ?user_id ?allowed_credentials relying_party =
  let passkey = Passkey.make ~challenge:(challenge_service t ()) in
  Result.map_error (fun e -> Login_rejected (Passkey.string_of_error e))
    (Passkey.begin_assertion passkey ?user_id ?allowed_credentials relying_party)
  |> Result.map (fun assertion -> { assertion; json = Passkey.assertion_options_json assertion })

let finish_passkey_assertion_verified t relying_party ~token json =
  Result.bind (passkey_assertion_response_of_json json) (fun response ->
      match t.store.passkeys.Passkey.find response.Passkey.id with
      | None -> Error User_not_found
      | Some credential ->
        let passkey = Passkey.make ~challenge:(challenge_service t ()) in
        Result.map_error
          (fun e -> Login_rejected (Passkey.string_of_error e))
          (Passkey.finish_assertion passkey relying_party credential response ~token))

let verify_passkey_factor t relying_party ~token json =
  Result.bind (finish_passkey_assertion_verified t relying_party ~token json) (fun assertion ->
      Result.bind
        (Result.map_error (fun e -> Store_error e)
           (t.store.passkeys.Passkey.update assertion.Passkey.credential))
        (fun () ->
          record_audit ~target_user_id:assertion.credential.user_id ~mechanism:Audit.Passkey t
            Audit.Mfa_step_up (Audit.User assertion.credential.user_id) Audit.Success
            ~metadata:[ ("factor", "passkey") ];
          Ok { user_id = assertion.credential.user_id; assurance = Mfa.assurance [ Mfa.Passkey ] }))

let finish_passkey_assertion_completion t ?current_user_id ?allow_signup ?link_verified_email relying_party
    ~token json =
  Result.bind (finish_passkey_assertion_verified t relying_party ~token json) (fun assertion ->
      login_with_passkey_assertion_completion t ?current_user_id ?allow_signup ?link_verified_email assertion)

let finish_passkey_assertion t ?current_user_id ?allow_signup ?link_verified_email relying_party ~token json =
  require_complete_identity_login
    (finish_passkey_assertion_completion t ?current_user_id ?allow_signup ?link_verified_email relying_party
       ~token json)
