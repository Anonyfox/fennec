module Conn = Paw.Conn
module Paw = Paw
module Assigns = Paw.Assigns
module H = Paw.Http
module Cookie = Paw.Cookie
module Session = Paw.Session (* signed-cookie session middleware (moved into fennec-paw) *)
module Bson = Bson
module Bson_json = Fennec_mongo_bson_json.Bson_json
module Json = Fennec_mongo_json.Json
module Mongo_runtime = Fennec_mongo_driver.Runtime
module Backend_dyn = Fennec_mongo_dynamic.Dynamic (* the runtime-selected storage backend (minimongo / Burrow / mongod) behind one Backend.S *)
module Backend_seam = Fennec_mongo_backend (* the pure seam — for its shared [query] constructor *)
module Identity = Accounts_identity
module Challenge = Accounts_challenge
module Throttle = Accounts_rate_limit (* brute-force rate limiting for the [login]/[createUser] methods *)
module Password = Accounts_password
module Email = Accounts_email
module OAuth = Accounts_oauth
module Oidc = Accounts_oidc
module Saml = Accounts_saml
module Passkey = Accounts_passkey
module Mfa = Accounts_mfa
module Org = Accounts_org
module Scim = Accounts_scim
module Roles = Accounts_roles
module Audit = Accounts_audit

type user_id = string
type email = { address : string; verified : bool }
type user_status =
  | Active
  | Suspended
  | Disabled
  | Deleted

type user = {
  id : user_id;
  username : string option;
  emails : email list;
  roles : Roles.Role.t list;
  profile : Bson.t option;
  services : (string * Bson.t) list;
  created_at : float;
  updated_at : float;
  auth_epoch : int;
  status : user_status;
}

type selector = By_id of user_id | By_email of string | By_username of string
type token = string

let token_of_string s = s
let token_to_string t = t

type auth_context = {
  user_id : user_id;
  session_id : string;
  strategy : string;
  factors : Mfa.factor list;
  issued_at : float;
  expires_at : float;
  auth_epoch : int;
}

type org_context = {
  org : Org.org;
  membership : Org.membership option;
}

type password_hasher = Password.hasher

type error =
  | User_not_found
  | Duplicate_email of string
  | Duplicate_username of string
  | Invalid_password
  | Password_not_configured
  | Strategy_not_found of string
  | Login_rejected of string
  | Invalid_user of string
  | Invalid_token
  | Store_error of string

let string_of_error = function
  | User_not_found -> "User not found"
  | Duplicate_email e -> "Email already exists: " ^ e
  | Duplicate_username u -> "Username already exists: " ^ u
  | Invalid_password -> "Incorrect password"
  | Password_not_configured -> "Password login is not configured"
  | Strategy_not_found s -> "Login strategy not found: " ^ s
  | Login_rejected r -> r
  | Invalid_user s -> s
  | Invalid_token -> "Invalid login token"
  | Store_error s -> s

type user_store = {
  find_user_by_id : user_id -> (user option, error) result;
  find_user_by_email : string -> (user option, error) result;
  find_user_by_username : string -> (user option, error) result;
  find_user_by_service : strategy:string -> service_id:string -> (user option, error) result;
  create_user : user -> password_hash:string option -> (user, error) result;
  update_user : user -> (user, error) result;
  password_hash : user_id -> (string option, error) result;
  set_password_hash : user_id -> string -> (unit, error) result;
  set_password_hash_and_bump : user_id -> string -> (int, error) result;
  bump_auth_epoch : user_id -> (int, error) result;
}

(* Public metadata for one live row in the login-token store. The hashed token itself is never
   exposed here — only the non-secret [session_id] (the [sid] already carried in every signed
   session) and timestamps. *)
type session_info = {
  session_id : string;
  user_id : user_id;
  created_at : float;
  expires_at : float;
  last_active_at : float;
  strategy : string;
}

(* Server-side login-token store ("active sessions"). Only HASHED tokens are persisted; the [sid] is
   the row key ([_id]). This is the source of truth for per-session revocation on the gated paths
   (resume / [verify_token] / opt-in per request). Time-sensitive ops take [~now] so callers (and
   tests) control the clock; expired rows read as absent. *)
type token_store = {
  record : session_info -> hashed:string -> (unit, error) result;
  find_live : sid:string -> hashed:string -> now:float -> (session_info option, error) result;
  list_for_user : user_id -> now:float -> (session_info list, error) result;
  touch : sid:string -> now:float -> (unit, error) result;
  revoke : sid:string -> (bool, error) result;
  revoke_user : user_id -> ?keep:string -> unit -> (int, error) result;
  gc_expired : now:float -> (int, error) result;
}

type store = {
  users : user_store;
  tokens : token_store;
  identities : Identity.store;
  challenges : Challenge.store;
  passkeys : Passkey.store;
  orgs : Org.store;
  mfa : Mfa.store;
  scim : Scim.store;
  audit : Audit.store;
  ensure_indexes : unit -> unit;
}

type login_attempt = { strategy : string; user : user option; allowed : bool; reason : string option }
type strategy = { name : string; login : credentials:Bson.t -> (user, error) result }

type external_identity = {
  key : Identity.key;
  email : string option;
  email_verified : bool;
  username : string option;
  profile : Bson.t option;
  service : (string * Bson.t) option;
}

type identity_login = {
  user : user;
  token : token;
  created : bool;
  linked : Identity.link option;
}

type mfa_totp_setup = {
  enrollment : Mfa.enrollment;
  totp : Mfa.totp;
  provisioning_uri : string;
}

type mfa_backup_setup = {
  enrollment : Mfa.enrollment;
  codes : string list;
}

type mfa_verification = {
  user_id : user_id;
  assurance : Mfa.assurance;
}

type passkey_registration_options = {
  registration : Passkey.registration;
  json : string;
}

type passkey_assertion_options = {
  assertion : Passkey.assertion_challenge;
  json : string;
}

type passkey_registration_finish = {
  credential : Passkey.credential;
  link : Identity.link;
}

type org_invite = {
  invite : Org.invite;
  token : string;
}

type password_reset = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

type enrollment = {
  token : Challenge.token;
  record : Challenge.record;
  user : user;
}

type login_step_up = {
  user : user;
  step_up : Mfa.step_up;
}

type login_completion =
  | Complete_login of user * token
  | Step_up_required of login_step_up

type identity_login_completion =
  | Complete_identity_login of identity_login
  | Identity_step_up_required of login_step_up

type session = {
  uid : user_id;
  sid : string;
  iat : float;
  exp : float;
  auth_epoch : int;
  strategy : string;
  factors : Mfa.factor list;
}

(* Central authentication policy — the typed twin of Meteor's [Accounts.config({...})]. [default_config]
   is SECURE-by-default (enumeration resistance on, sane token lifetimes); an app tunes it via [make
   ~config] or {!configure}. *)
type config = {
  require_verified_email : bool;
      (* a user must have at least one verified email before any session is issued (every strategy) *)
  forbid_client_account_creation : bool;
      (* the client [createUser] DDP method is rejected; server-side [create_user] still works (server-only signup) *)
  ambiguous_error_messages : bool;
      (* password login returns ONE indistinguishable error for unknown-account vs wrong-password (the
         on_login_failure hooks + the audit log still see the real reason) — pairs with the enumeration
         timing defense to close the login enumeration oracle. ON by default (secure); set [false] for
         Meteor-style specific errors. (Signup still reports a taken username/email — a UX necessity.) *)
  auto_send_verification_email : bool;
      (* auto-send a verification email when the client [createUser] DDP method creates a user with an
         email — Meteor's [sendVerificationEmail] config. Off by default (Meteor parity). Best-effort:
         a delivery failure never blocks signup. *)
  reset_token_lifetime : float;
      (* password-reset token TTL in seconds; default 3 days (Meteor's passwordResetTokenExpirationInDays) *)
  enroll_token_lifetime : float;
      (* enrollment-invite token TTL in seconds; default 30 days (Meteor's passwordEnrollTokenExpiration) *)
  verify_token_lifetime : float;
      (* email-verification token TTL in seconds; default 3 days *)
}

let default_config =
  {
    require_verified_email = false;
    forbid_client_account_creation = false;
    ambiguous_error_messages = true;
    auto_send_verification_email = false;
    reset_token_lifetime = 3. *. 86_400.;
    enroll_token_lifetime = 30. *. 86_400.;
    verify_token_lifetime = 3. *. 86_400.;
  }

type t = {
  secret : string;
  store : store;
  password_hasher : password_hasher option;
  password_policy : Password.policy option;
  policy : Roles.policy;
      (* the app's role→permission map — code-declared, immutable, held once. [can]/[require_permission]
         read it directly so callers never thread a [~policy] argument. The single RBAC policy: org
         permission checks (see [can_in]) resolve against it too. *)
  mutable config : config;
  mutable email_templates : Accounts_mailer.templates option;
  cookie : string;
  path : string;
  lifetime : float;
  validate_every_request : bool;
  mutable validate_login_hooks : (login_attempt -> (unit, string) result) list;
  mutable create_user_hooks : (user -> (user, string) result) list;
  mutable login_hooks : (user -> unit) list;
  mutable logout_hooks : (user_id option -> unit) list;
  mutable login_failure_hooks : (login_attempt -> unit) list;
  mutable before_external_login_hooks : (strategy:string -> identity:external_identity -> user:user -> bool) list;
  strategies : (string, strategy) Hashtbl.t;
  rate_limit : Throttle.t; (* throttles the [login]/[createUser] DDP methods by client IP + selector *)
}

let user_id_key : user_id option Assigns.key = Assigns.key "fennec.accounts.user_id"
let session_key : session option Assigns.key = Assigns.key "fennec.accounts.session"
let assurance_key : Mfa.assurance option Assigns.key = Assigns.key "fennec.accounts.assurance"
let org_context_key : org_context option Assigns.key = Assigns.key "fennec.accounts.org_context"

let now () = Unix.gettimeofday ()

let secure_random (n : int) : string =
  match open_in_bin "/dev/urandom" with
  | ic -> Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> really_input_string ic n)
  | exception Sys_error msg -> failwith ("Fennec.Accounts: secure randomness unavailable (/dev/urandom): " ^ msg)

let b64e s = Base64.encode_string ~alphabet:Base64.uri_safe_alphabet ~pad:false s
let b64d s = match Base64.decode ~alphabet:Base64.uri_safe_alphabet ~pad:false s with Ok x -> Some x | Error _ -> None

let random_id ?(bytes = 18) () = b64e (secure_random bytes)
let sha256_hex s = Digestif.SHA256.(to_hex (digest_string s))
let hmac_sha256 ~key msg = Digestif.SHA256.(to_raw_string (hmac_string ~key msg))
let constant_eq a b =
  let la = String.length a and lb = String.length b in
  let diff = ref (la lxor lb) in
  let max_len = max la lb in
  for i = 0 to max_len - 1 do
    let ca = if i < la then Char.code a.[i] else 0 in
    let cb = if i < lb then Char.code b.[i] else 0 in
    diff := !diff lor (ca lxor cb)
  done;
  !diff = 0

let normalize_email s = String.lowercase_ascii (String.trim s)
let normalize_username s = String.lowercase_ascii (String.trim s)
let option_exists f = function Some x -> f x | None -> false
let nonblank_opt s = let s = String.trim s in if s = "" then None else Some s

let external_identity ?email ?(email_verified = false) ?username ?profile ?service key =
  {
    key;
    email = Option.map normalize_email (Option.bind email nonblank_opt);
    email_verified;
    username = Option.bind username nonblank_opt;
    profile;
    service =
      (match service with
      | Some (name, doc) -> Option.map (fun name -> (name, doc)) (nonblank_opt name)
      | None -> None);
  }

let identity_error e = Login_rejected (Identity.string_of_error e)
let email_error e = Login_rejected (Email.string_of_error e)

let email_identity ?username ?profile ?service address =
  let key =
    match Identity.email ~verified:true (Email.address_to_string address) with
    | Ok key -> key
    | Error e -> raise (Invalid_argument (Identity.string_of_error e))
  in
  external_identity key ~email:(Email.address_to_string address) ~email_verified:true ?username ?profile ?service

let oauth_identity ?email ?(email_verified = false) ?username ?profile ?service provider ~subject =
  match OAuth.identity provider ~subject with
  | Error (OAuth.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (OAuth.string_of_error e))
  | Ok key ->
    let service = Option.map (fun doc -> (provider.OAuth.name, doc)) service in
    Ok (external_identity key ?email ~email_verified ?username ?profile ?service)

let oidc_identity ?username ?profile ?service (principal : Oidc.principal) =
  let service = Option.map (fun doc -> ("oidc", doc)) service in
  external_identity principal.identity ?email:principal.email ~email_verified:principal.email_verified ?username
    ?profile ?service

let saml_identity ?username ?profile ?service (principal : Saml.principal) =
  let service = Option.map (fun doc -> ("saml", doc)) service in
  external_identity principal.identity ?email:principal.email ~email_verified:(Option.is_some principal.email_identity)
    ?username ?profile ?service

let passkey_identity ?service (assertion : Passkey.assertion) =
  match Passkey.identity assertion.credential with
  | Error (Passkey.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (Passkey.string_of_error e))
  | Ok key ->
    let service = Option.map (fun doc -> ("passkey", doc)) service in
    Ok (external_identity key ?service)

let scim_identity ?username ?profile ?service connection user =
  match Scim.identity connection user with
  | Error (Scim.Identity_error e) -> Error (identity_error e)
  | Error e -> Error (Login_rejected (Scim.string_of_error e))
  | Ok key ->
    let email = match user.Scim.emails with email :: _ -> Some email | [] -> None in
    let username = match username with Some _ -> username | None -> Some user.user_name in
    let service = Option.map (fun doc -> ("scim", doc)) service in
    Ok (external_identity key ?email ~email_verified:true ?username ?profile ?service)

let encode_pairs kvs =
  String.concat "&" (List.map (fun (k, v) -> H.percent_encode k ^ "=" ^ H.percent_encode v) kvs)

let int_of_string_default d s = match int_of_string_opt s with Some n -> n | None -> d
let float_of_string_default d s = match float_of_string_opt s with Some n -> n | None -> d

let password_hasher = Password.password_hasher

let challenge_service t ?ttl () =
  let secret = t.secret ^ "\000accounts-challenge" in
  match ttl with
  | None -> Challenge.make ~secret ~store:t.store.challenges ()
  | Some ttl -> Challenge.make ~secret ~store:t.store.challenges ~ttl ()

let email_service t ?ttl () =
  let challenge = challenge_service t ?ttl () in
  Email.make ~secret:(t.secret ^ "\000accounts-email") ~challenge

(* the single source of factor naming lives in {!Accounts_mfa}; these are thin aliases used by the
   session codec below *)
let mfa_factor_name = Mfa.factor_to_name
let mfa_factor_of_name = Mfa.factor_of_name

let encode_mfa_factors factors = String.concat "," (List.map mfa_factor_name factors)
let decode_mfa_factors s =
  if String.trim s = "" then []
  else String.split_on_char ',' s |> List.filter_map mfa_factor_of_name

let encode_session s =
  encode_pairs
    [
      ("uid", s.uid);
      ("sid", s.sid);
      ("iat", Printf.sprintf "%.0f" s.iat);
      ("exp", Printf.sprintf "%.0f" s.exp);
      ("epoch", string_of_int s.auth_epoch);
      ("strategy", s.strategy);
      ("factors", encode_mfa_factors s.factors);
    ]

let decode_session payload =
  let pairs = H.parse_query payload in
  let get k = List.assoc_opt k pairs in
  match (get "uid", get "sid", get "iat", get "exp", get "epoch", get "strategy") with
  | Some uid, Some sid, Some iat, Some exp, Some epoch, Some strategy ->
    Some
      {
        uid;
        sid;
        iat = float_of_string_default 0. iat;
        exp = float_of_string_default 0. exp;
        auth_epoch = int_of_string_default 0 epoch;
        strategy;
        factors = Option.value ~default:[] (Option.map decode_mfa_factors (get "factors"));
      }
  | _ -> None

let sign t s = Session.sign ~secret:t.secret (encode_session s)

let verify_session t token =
  match Option.bind (Session.verify ~secret:t.secret token) decode_session with
  | None -> Error Invalid_token
  | Some s when now () > s.exp -> Error Invalid_token
  | Some s -> Ok s

(* The server-side revocation gate: a verified session is only honored if its [sid] still has a live,
   non-expired row whose stored hash matches the presented token. This is what makes per-session
   revocation and early server-side expiry possible (consequences a/c). Consulted on the token-
   presentation paths ([verify_token], resume) always, and in [paw] only when
   [validate_every_request] — the stateless cookie fast path stays zero-read by default. *)
let token_live t (s : session) token =
  match t.store.tokens.find_live ~sid:s.sid ~hashed:(sha256_hex token) ~now:(now ()) with
  | Error _ as e -> e
  | Ok (Some _) -> Ok ()
  | Ok None -> Error Invalid_token

let make ~secret ~store ?password_hasher ?password_policy ?(policy = Roles.policy []) ?(cookie = "_fennec_login")
    ?(path = "/") ?(lifetime = 86400.) ?(validate_every_request = false) ?(config = default_config) ?rate_limit () =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Accounts.make: ~secret must be at least 16 bytes (got %d)" (String.length secret));
  if lifetime <= 0. then invalid_arg "Fennec.Accounts.make: ~lifetime must be positive";
  {
    secret;
    store;
    password_hasher;
    password_policy;
    policy;
    cookie;
    path;
    lifetime;
    validate_every_request;
    config;
    email_templates = None;
    validate_login_hooks = [];
    create_user_hooks = [];
    login_hooks = [];
    logout_hooks = [];
    login_failure_hooks = [];
    before_external_login_hooks = [];
    strategies = Hashtbl.create 8;
    (* default: Meteor's 5 attempts / 10 s, keyed by IP and account for login and by IP for createUser *)
    rate_limit = (match rate_limit with Some r -> r | None -> Throttle.make ());
  }

let native : t option Atomic.t = Atomic.make None

(* Set the authentication policy after construction — the mutable twin of Meteor's [Accounts.config]; an
   app configures the framework-native instance ([current ()]) once in its startup. *)
let configure t cfg = t.config <- cfg

(* The account-email templates (Meteor's Accounts.emailTemplates), used by the [send_*_email] verbs. None
   until {!set_email_templates}; the verbs then need only a [from] + [site_name] to deliver via the ambient
   {!Fennec_mail} transport. *)
module Mailer = Accounts_mailer

let set_email_templates t tpls = t.email_templates <- Some tpls

let validate_login_attempt t f = t.validate_login_hooks <- f :: t.validate_login_hooks
let on_create_user t f = t.create_user_hooks <- f :: t.create_user_hooks
let on_login t f = t.login_hooks <- f :: t.login_hooks
let on_logout t f = t.logout_hooks <- f :: t.logout_hooks
let on_login_failure t f = t.login_failure_hooks <- f :: t.login_failure_hooks
let before_external_login t f = t.before_external_login_hooks <- f :: t.before_external_login_hooks
let register_strategy t s =
  if String.trim s.name = "" then invalid_arg "Fennec.Accounts.register_strategy: strategy name cannot be blank";
  Hashtbl.replace t.strategies s.name s

let run_create_hooks t user =
  List.fold_left
    (fun acc hook ->
      match acc with
      | Error _ as e -> e
      | Ok u -> ( match hook u with Ok u -> Ok u | Error reason -> Error (Login_rejected reason)))
    (Ok user) (List.rev t.create_user_hooks)

let run_login_hooks t attempt =
  List.fold_left
    (fun acc hook ->
      match acc with
      | Error _ as e -> e
      | Ok () -> ( match hook attempt with Ok () -> Ok () | Error reason -> Error (Login_rejected reason)))
    (Ok ()) (List.rev t.validate_login_hooks)

let observe_login t u = List.iter (fun f -> f u) (List.rev t.login_hooks)
let observe_logout t uid = List.iter (fun f -> f uid) (List.rev t.logout_hooks)

(* fire the on_login_failure hooks with the rejected attempt (allowed = false) — the reactable twin of the
   audit Login_failure record, so an app can lock out / alert without scraping the audit log *)
let observe_login_failure t attempt = List.iter (fun f -> f attempt) (List.rev t.login_failure_hooks)

let user_id c = match Conn.get c user_id_key with Some u -> u | None -> None

let auth_context_of_session s =
  {
    user_id = s.uid;
    session_id = s.sid;
    strategy = s.strategy;
    factors = s.factors;
    issued_at = s.iat;
    expires_at = s.exp;
    auth_epoch = s.auth_epoch;
  }

let auth_context c =
  match Conn.get c session_key with
  | Some (Some s) -> Some (auth_context_of_session s)
  | Some None | None -> None

let factor_of_strategy strategy =
  match String.lowercase_ascii (String.trim strategy) with
  | "password" | "createuser" | "resetpassword" -> Some Mfa.Password
  | "verifyemail" | "email" | "email_otp" -> Some Mfa.Email
  | "oauth" -> Some Mfa.OAuth
  | "oidc" -> Some Mfa.Oidc
  | "saml" -> Some Mfa.Saml
  | "passkey" -> Some Mfa.Passkey
  | _ -> None

let same_mfa_factor a b = String.equal (mfa_factor_name a) (mfa_factor_name b)
let add_mfa_factor factors factor = if List.exists (same_mfa_factor factor) factors then factors else factors @ [ factor ]
let merge_mfa_factors a b = List.fold_left add_mfa_factor a b

let assurance_of_auth_context (ctx : auth_context) =
  let factors =
    match factor_of_strategy ctx.strategy with
    | None -> ctx.factors
    | Some factor -> add_mfa_factor ctx.factors factor
  in
  match factors with
  | [] -> None
  | _ -> Some (Mfa.assurance ~now:(fun () -> ctx.issued_at) factors)

let set_assurance c assurance = Conn.assign c assurance_key (Some assurance)

let assurance c =
  match Conn.get c assurance_key with
  | Some (Some assurance) -> Some assurance
  | Some None -> None
  | None -> Option.bind (auth_context c) assurance_of_auth_context

let require_assurance ?redirect requirement () : Paw.t =
 fun c ->
  match assurance c with
  | Some current when Result.is_ok (Mfa.require requirement current) -> c
  | _ -> (
    match redirect with
    | Some location -> Conn.text ~status:302 (Conn.set_header c "location" location) ""
    | None -> Conn.text ~status:403 c "Forbidden")

let set_org_context c ?membership org = Conn.assign c org_context_key (Some { org; membership })
let org_context c = match Conn.get c org_context_key with Some ctx -> ctx | None -> None
let org c = Option.map (fun ctx -> ctx.org) (org_context c)
let membership c = Option.bind (org_context c) (fun ctx -> ctx.membership)

let require_org_strategy org strategy =
  match Org.decide_strategy org strategy with
  | Org.Allowed -> Ok ()
  | Org.Denied reason -> Error (Login_rejected reason)
  | Org.Requires_sso ids -> Error (Login_rejected ("SSO required: " ^ String.concat ", " ids))

let reject_org c redirect =
  match redirect with
  | Some location -> Conn.text ~status:302 (Conn.set_header c "location" location) ""
  | None -> Conn.text ~status:403 c "Forbidden"

let require_org t ?redirect ?permission () : Paw.t =
 fun c ->
  match org_context c with
  | None -> reject_org c redirect
  | Some { org; membership = None } ->
    if Org.is_active_org org && permission = None then c else reject_org c redirect
  | Some { org; membership = Some membership } -> (
    match Org.require_membership org membership with
    | Error _ -> reject_org c redirect
    | Ok () -> (
      match permission with
      | None -> c
      (* the org permission is decided by the SAME code-declared policy — the membership's role must
         grant it. No separate hardcoded org RBAC. *)
      | Some permission -> (
        match (Roles.Role.v membership.Org.role, Roles.Permission.v permission) with
        | Ok r, Ok p when Roles.role_allows t.policy ~role:r ~permission:p -> c
        | _ -> reject_org c redirect)))

let current_user t c =
  match user_id c with None -> Ok None | Some uid -> t.store.users.find_user_by_id uid

let mechanism_of_strategy strategy =
  match String.lowercase_ascii (String.trim strategy) with
  | "password" | "createuser" | "resetpassword" -> Some Audit.Password
  | "verifyemail" | "email" | "email_otp" -> Some Audit.Email
  | "oauth" -> Some (Audit.OAuth "oauth")
  | "oidc" -> Some (Audit.Oidc "oidc")
  | "saml" -> Some (Audit.Saml "saml")
  | "passkey" -> Some Audit.Passkey
  | "resume" -> Some Audit.Token
  | _ -> None

let login_audit_kind strategy =
  match String.lowercase_ascii (String.trim strategy) with
  | "resume" -> Audit.Token_resume
  | "resetpassword" -> Audit.Password_reset
  | "verifyemail" -> Audit.Email_verification
  | "email" | "email_otp" -> Audit.Email_login
  | "passkey" -> Audit.Passkey_assertion
  | "oidc" -> Audit.Oidc_callback
  | "saml" -> Audit.Saml_callback
  | "oauth" -> Audit.OAuth_callback
  | _ -> Audit.Login

let string_of_user_status = function
  | Active -> "active"
  | Suspended -> "suspended"
  | Disabled -> "disabled"
  | Deleted -> "deleted"

let user_status_of_string = function
  | "active" -> Some Active
  | "suspended" -> Some Suspended
  | "disabled" -> Some Disabled
  | "deleted" -> Some Deleted
  | _ -> None

let user_can_login = function
  | Active -> true
  | Suspended | Disabled | Deleted -> false

let rec append_audit ?(attempts = 3) t event =
  match Audit.append t.store.audit event with
  | Ok () -> ()
  | Error _ when attempts > 1 ->
    let event = { event with Audit.id = random_id () } in
    append_audit ~attempts:(attempts - 1) t event
  | Error _ -> ()

let record_audit ?target_user_id ?org_id ?mechanism ?connection_id ?request ?(metadata = []) ?at t kind actor
    outcome =
  let at = Option.value at ~default:(now ()) in
  Audit.event ?target_user_id ?org_id ?mechanism ?connection_id ?request ~metadata ~id:(random_id ())
    ~at kind actor outcome
  |> append_audit t

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

let checked_session t s =
  if not t.validate_every_request then Ok s
  else
    match t.store.users.find_user_by_id s.uid with
    | Error _ as e -> e
    | Ok None -> Error Invalid_token
    | Ok (Some u) when u.auth_epoch = s.auth_epoch && user_can_login u.status -> Ok s
    | Ok (Some _) -> Error Invalid_token

let paw t () : Paw.t =
 fun c ->
  match Conn.cookie c t.cookie with
  | None -> c
  | Some token -> (
    match Result.bind (verify_session t token) (checked_session t) with
    | Error _ -> c
    (* Per-request revocation check: only when the app already opted into store-backed validation, so
       the default cookie path stays zero-read. A revoked/expired row makes the request anonymous. *)
    | Ok s when t.validate_every_request && Result.is_error (token_live t s token) -> c
    | Ok s ->
      let c = Conn.assign (Conn.assign c user_id_key (Some s.uid)) session_key (Some s) in
      let ctx = auth_context_of_session s in
      (match assurance_of_auth_context ctx with Some assurance -> set_assurance c assurance | None -> c))

let require_user ?redirect () : Paw.t =
 fun c ->
  match user_id c with
  | Some _ -> c
  | None -> (
    match redirect with
    | Some location -> Conn.text ~status:302 (Conn.set_header c "location" location) ""
    | None -> Conn.text ~status:401 c "Unauthorized")

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

let find_required_user t uid =
  match t.store.users.find_user_by_id uid with
  | Error _ as e -> e
  | Ok None -> Error User_not_found
  | Ok (Some u) -> Ok u

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

let has_role t uid role =
  Result.bind (find_required_user t uid) (fun user -> Ok (Roles.mem role user.roles))

(* The scope a permission is evaluated in: app-wide, or within an organization (where the user's active
   membership role counts on top of their global roles). The SAME code-declared {!policy} decides both —
   there is no second RBAC model. *)
type scope = Global | Org of string

let roles_in_scope t uid scope =
  Result.map
    (fun (user : user) ->
      match scope with
      | Global -> user.roles
      | Org org_id -> (
        match t.store.orgs.Org.find_membership ~org_id ~user_id:uid with
        | Some m when Org.is_active_membership m -> (
          match Roles.Role.v m.Org.role with Ok r -> user.roles @ [ r ] | Error _ -> user.roles)
        | _ -> user.roles))
    (find_required_user t uid)

let can_in t uid ~scope permission =
  Result.map (fun roles -> Roles.any_role_allows t.policy ~roles ~permission) (roles_in_scope t uid scope)

let can t uid permission = can_in t uid ~scope:Global permission

let reject_authz c redirect =
  match redirect with
  | Some location -> Conn.text ~status:302 (Conn.set_header c "location" location) ""
  | None -> Conn.text ~status:403 c "Forbidden"

let require_role t ?redirect role () : Paw.t =
 fun c ->
  match user_id c with
  | None -> reject_authz c redirect
  | Some uid -> (
    match has_role t uid role with
    | Ok true -> c
    | Ok false | Error _ -> reject_authz c redirect)

let require_permission t ?redirect permission () : Paw.t =
 fun c ->
  match user_id c with
  | None -> reject_authz c redirect
  | Some uid -> (
    match can t uid permission with
    | Ok true -> c
    | Ok false | Error _ -> reject_authz c redirect)

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

let find_by_selector t = function
  | By_id id -> t.store.users.find_user_by_id id
  | By_email e -> t.store.users.find_user_by_email (normalize_email e)
  | By_username u -> t.store.users.find_user_by_username (normalize_username u)

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

let mfa_service t =
  Mfa.make ~secret:(t.secret ^ "\000accounts-mfa") ~challenge:(challenge_service t ())

let seal_key t = hmac_sha256 ~key:t.secret "fennec.accounts.mfa.seal.v1"
let seal_prefix = "v1"

let xor_with_stream ~key ~nonce plaintext =
  let n = String.length plaintext in
  let out = Bytes.create n in
  let rec block counter offset =
    if offset < n then (
      let stream = hmac_sha256 ~key (nonce ^ "\000" ^ string_of_int counter) in
      let m = min (String.length stream) (n - offset) in
      for i = 0 to m - 1 do
        Bytes.set out (offset + i) (Char.chr (Char.code plaintext.[offset + i] lxor Char.code stream.[i]))
      done;
      block (counter + 1) (offset + m))
  in
  block 0 0;
  Bytes.unsafe_to_string out

let seal_mfa_secret t secret =
  let key = seal_key t in
  let nonce = secure_random 16 in
  let cipher = xor_with_stream ~key ~nonce secret in
  let payload = seal_prefix ^ "." ^ b64e nonce ^ "." ^ b64e cipher in
  payload ^ "." ^ b64e (hmac_sha256 ~key payload)

let unseal_mfa_secret t sealed =
  match String.split_on_char '.' sealed with
  | [ version; nonce64; cipher64; mac64 ] when version = seal_prefix -> (
    match (b64d nonce64, b64d cipher64, b64d mac64) with
    | Some nonce, Some cipher, Some mac ->
      let payload = version ^ "." ^ nonce64 ^ "." ^ cipher64 in
      if constant_eq mac (hmac_sha256 ~key:(seal_key t) payload) then
        Some (xor_with_stream ~key:(seal_key t) ~nonce cipher)
      else None
    | _ -> None)
  (* FAIL CLOSED on any non-sealed value. A TOTP secret is ALWAYS written through [seal_mfa_secret]
     (keyed by [t.secret]), so a stored value that is not a valid v1 seal is either tampered or
     foreign — trusting it as plaintext would let mere store-write access plant a known TOTP secret,
     defeating the seal's whole purpose (forging a seal requires [t.secret]). *)
  | _ -> None

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

let reset_password_completion t token ~password =
  Result.bind (reset_password_user t token ~password) (complete_login_unless_mfa t ~strategy:"resetPassword")

let reset_password t token ~password =
  Result.bind (reset_password_completion t token ~password) (function
    | Complete_login (user, token) -> Ok (user, token)
    | Step_up_required _ -> Error (Login_rejected "MFA step-up required"))

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
      | Scim.Create_user user | Scim.Update_user { after = user; _ } | Scim.Deprovision_user { after = user; _ } ->
        persist user)

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

let oauth_callback_paw t ?(link_verified_email = true) ~path ~success ~error provider ~exchange () =
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

let oidc_callback_paw t ?(link_verified_email = true) ~path ~success ~error (connection : Oidc.connection) ~exchange () =
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

let saml_callback_paw t ~path ~success ~error connection ~trusted_keys () =
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
          | Ok login -> Conn.redirect (set_login_cookie t c login.token) success))
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

