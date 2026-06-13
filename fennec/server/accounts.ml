module Conn = Fennec_paw.Conn
module Paw = Fennec_paw.Paw
module Assigns = Fennec_paw.Assigns
module H = Fennec_core.Http
module Cookie = Fennec_core.Cookie
module Bson = Bson
module Bson_json = Fennec_mongo_bson_json.Bson_json
module Json = Fennec_mongo_json.Json
module Mongo_runtime = Fennec_mongo_driver.Runtime
module Identity = Accounts_identity
module Challenge = Accounts_challenge
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

type store = {
  users : user_store;
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

type t = {
  secret : string;
  store : store;
  password_hasher : password_hasher option;
  password_policy : Password.policy option;
  cookie : string;
  path : string;
  lifetime : float;
  validate_every_request : bool;
  mutable validate_login_hooks : (login_attempt -> (unit, string) result) list;
  mutable create_user_hooks : (user -> (user, string) result) list;
  mutable login_hooks : (user -> unit) list;
  mutable logout_hooks : (user_id option -> unit) list;
  strategies : (string, strategy) Hashtbl.t;
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

let mfa_factor_name = function
  | Mfa.Password -> "password"
  | Email -> "email"
  | OAuth -> "oauth"
  | Oidc -> "oidc"
  | Saml -> "saml"
  | Passkey -> "passkey"
  | Totp -> "totp"
  | Backup_code -> "backup_code"
  | Recovery_code -> "recovery_code"

let mfa_factor_of_name = function
  | "password" -> Some Mfa.Password
  | "email" -> Some Email
  | "oauth" -> Some OAuth
  | "oidc" -> Some Oidc
  | "saml" -> Some Saml
  | "passkey" -> Some Passkey
  | "totp" -> Some Totp
  | "backup_code" -> Some Backup_code
  | "recovery_code" -> Some Recovery_code
  | _ -> None

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

let make ~secret ~store ?password_hasher ?password_policy ?(cookie = "_fennec_login") ?(path = "/")
    ?(lifetime = 86400.) ?(validate_every_request = false) () =
  if String.length secret < 16 then
    invalid_arg
      (Printf.sprintf "Fennec.Accounts.make: ~secret must be at least 16 bytes (got %d)" (String.length secret));
  if lifetime <= 0. then invalid_arg "Fennec.Accounts.make: ~lifetime must be positive";
  {
    secret;
    store;
    password_hasher;
    password_policy;
    cookie;
    path;
    lifetime;
    validate_every_request;
    validate_login_hooks = [];
    create_user_hooks = [];
    login_hooks = [];
    logout_hooks = [];
    strategies = Hashtbl.create 8;
  }

let native : t option Atomic.t = Atomic.make None

let validate_login_attempt t f = t.validate_login_hooks <- f :: t.validate_login_hooks
let on_create_user t f = t.create_user_hooks <- f :: t.create_user_hooks
let on_login t f = t.login_hooks <- f :: t.login_hooks
let on_logout t f = t.logout_hooks <- f :: t.logout_hooks
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

let require_org ?redirect ?permission ?role_allows () : Paw.t =
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
      | Some permission when Org.allows ?role_allows membership ~permission -> c
      | Some _ -> reject_org c redirect))

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

let issue t ?(factors = []) ~strategy (u : user) =
  let iat = now () in
  let s =
    { uid = u.id; sid = random_id (); iat; exp = iat +. t.lifetime; auth_epoch = u.auth_epoch; strategy; factors }
  in
  sign t s

let finish_login t ?factors ~strategy (u : user) =
  if not (user_can_login u.status) then Error (Login_rejected "Account is not active")
  else
  let attempt = { strategy; user = Some u; allowed = true; reason = None } in
  match run_login_hooks t attempt with
  | Error (Login_rejected reason as e) ->
    record_audit ?mechanism:(mechanism_of_strategy strategy) t Audit.Login_failure Audit.Anonymous
      (Audit.Failure reason);
    Error e
  | Error _ as e -> e
  | Ok () ->
    let token = issue t ?factors ~strategy u in
    observe_login t u;
    record_audit ~target_user_id:u.id ?mechanism:(mechanism_of_strategy strategy) t
      (login_audit_kind strategy) (Audit.User u.id) Audit.Success;
    Ok (u, token)

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

let can t uid ~policy permission =
  Result.bind (find_required_user t uid) (fun user -> Ok (Roles.any_role_allows policy ~roles:user.roles ~permission))

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

let require_permission t ?redirect ~policy permission () : Paw.t =
 fun c ->
  match user_id c with
  | None -> reject_authz c redirect
  | Some uid -> (
    match can t uid ~policy permission with
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

let login_with_password_completion t selector ~password =
  match t.password_hasher with
  | None -> Error Password_not_configured
  | Some hasher -> (
    match find_by_selector t selector with
    | Error _ as e -> e
    | Ok None ->
      record_audit ~mechanism:Audit.Password t Audit.Login_failure Audit.Anonymous
        (Audit.Failure "user_not_found");
      Error User_not_found
    | Ok (Some u) -> (
      match t.store.users.password_hash u.id with
      | Error _ as e -> e
      | Ok None -> Error Password_not_configured
      | Ok (Some hash) ->
        if not (hasher.verify ~password ~hash) then (
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
  | _ -> Some sealed

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

let login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
    ~strategy facts =
  resolve_identity_login t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now ~strategy
    facts ~finish:(finish_identity_login_completion t ~strategy)

let require_complete_identity_login = function
  | Ok (Complete_identity_login login) -> Ok login
  | Ok (Identity_step_up_required _) -> Error (Login_rejected "MFA step-up required")
  | Error _ as e -> e

let login_with_identity t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now ~strategy
    facts =
  require_complete_identity_login
    (login_with_identity_completion t ?identity_store ?current_user_id ?allow_signup ?link_verified_email ?now
       ~strategy facts)

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
    match t.store.users.find_user_by_id s.uid with
    | Error _ as e -> e
    | Ok None -> Error User_not_found
    | Ok (Some u) when u.auth_epoch <> s.auth_epoch -> Error Invalid_token
    | Ok (Some u) -> finish_login t ~strategy:"resume" u)

let set_login_cookie t c ?(same_site = Cookie.Lax) ?(http_only = true) ?secure token =
  Conn.set_cookie c t.cookie token ~path:t.path ~max_age:(int_of_float t.lifetime) ?secure
    ~http_only ~same_site

let logout t c =
  let uid = user_id c in
  observe_logout t uid;
  Option.iter
    (fun uid -> record_audit ~target_user_id:uid ~mechanism:Audit.Token t Audit.Logout (Audit.User uid) Audit.Success)
    uid;
  Conn.assign (Conn.assign (Conn.delete_cookie c ~path:t.path t.cookie) user_id_key None) session_key None

let logout_other_clients t uid = Result.map (fun _ -> ()) (t.store.users.bump_auth_epoch uid)

let logout_other_clients_and_refresh t uid =
  Result.bind (t.store.users.bump_auth_epoch uid) (fun _ ->
      Result.bind (find_required_user t uid) (finish_login t ~strategy:"resume"))

let verify_token t token =
  match Result.bind (verify_session t token) (checked_session t) with Ok s -> Ok s.uid | Error _ as e -> e

let send_failed c exn = Conn.text ~status:500 c ("Accounts delivery failed: " ^ Printexc.to_string exn)

let password_reset_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.post path (fun c ->
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
  Paw.post path (fun c ->
      match (Conn.param c token_param, Conn.param c password_param) with
      | Some token, Some password -> (
        match reset_password_completion t (Challenge.token_of_string token) ~password with
        | result -> redirect_login_completion t c ?mfa_required ~success ~error result)
      | _ -> Conn.redirect c error)

let enrollment_paw t ?(token_param = "token") ?(password_param = "password") ?mfa_required ~path ~success
    ~error () =
  Paw.post path (fun c ->
      match (Conn.param c token_param, Conn.param c password_param) with
      | Some token, Some password ->
        redirect_login_completion t c ?mfa_required ~success ~error
          (enroll_account_completion t (Challenge.token_of_string token) ~password)
      | _ -> Conn.redirect c error)

let email_verification_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.post path (fun c ->
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
  Paw.get path (fun c ->
      match Conn.param c token_param with
      | None -> Conn.redirect c error
      | Some token -> (
        match verify_email_completion t (Challenge.token_of_string token) with
        | result -> redirect_login_completion t c ?mfa_required ~success ~error result))

let email_login_link_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.post path (fun c ->
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
  Paw.get path (fun c ->
      let email = email_service t () in
      match Conn.param c token_param with
      | None -> Conn.redirect c error
      | Some token -> (
        redirect_identity_completion t c ?mfa_required ~success ~error
          (login_with_email_link_completion t email ?current_user_id:(user_id c) ?allow_signup
             ?link_verified_email (Challenge.token_of_string token))))

let email_otp_request_paw t ?(email_param = "email") ~path ~success ~error ~send () =
  Paw.post path (fun c ->
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
  Paw.post path (fun c ->
      let email = email_service t () in
      match (Conn.param c token_param, Conn.param c code_param) with
      | Some token, Some code -> (
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
  Paw.post path (fun c ->
      match (Conn.param c mfa_token_param, Conn.param c factor_param, Conn.param c code_param) with
      | Some mfa_token, Some factor, Some code ->
        redirect_mfa_completion t c ~success ~error mfa_token (verify_totp_factor t factor ~code)
      | _ -> Conn.redirect c error)

let mfa_backup_code_paw t ?(mfa_token_param = "mfaToken") ?(user_param = "userId")
    ?(code_param = "code") ~path ~success ~error () =
  Paw.post path (fun c ->
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
let json_list key values = (key, Json.List (List.map (fun s -> Json.String s) values))
let json_opt_string key = function Some value -> [ json_string key value ] | None -> []
let req_body_json c = Json.parse_opt (Conn.req c).H.body
let json_member_string key json = Option.bind (Json.member key json) Json.to_string_opt

let json_member_bool key json =
  match Json.member key json with
  | Some (Json.Bool b) -> Some b
  | _ -> None

let json_member_list_strings key json =
  match Option.bind (Json.member key json) Json.to_list_opt with
  | None -> []
  | Some xs -> List.filter_map Json.to_string_opt xs

let nested_response json key =
  match Json.member "response" json with
  | Some response -> json_member_string key response
  | None -> None

let passkey_registration_options_json (issued : Passkey.registration) =
  Json.to_string
    (Json.Obj
       [
         json_string "token" (Challenge.token_to_string issued.token);
         ( "publicKey",
           Json.Obj
             [
               json_string "challenge" issued.challenge;
               ( "rp",
                 Json.Obj
                   [
                     json_string "id" issued.rp.id;
                     json_string "name" issued.rp.name;
                   ] );
               ( "user",
                 Json.Obj
                   [
                     json_string "id" issued.user.handle;
                     json_string "name" issued.user.name;
                     json_string "displayName" issued.user.display_name;
                   ] );
               ( "pubKeyCredParams",
                 Json.List [ Json.Obj [ ("type", Json.String "public-key"); ("alg", Json.Number (-7.)) ] ] );
               ( "authenticatorSelection",
                 Json.Obj [ ("userVerification", Json.String (if issued.rp.user_verification then "required" else "preferred")) ] );
               json_string "attestation" "none";
             ] );
       ])

let passkey_assertion_options_json (issued : Passkey.assertion_challenge) =
  Json.to_string
    (Json.Obj
       [
         json_string "token" (Challenge.token_to_string issued.token);
         ( "publicKey",
           Json.Obj
             ([
                json_string "challenge" issued.challenge;
                json_string "rpId" issued.rp.id;
                ( "allowCredentials",
                  Json.List
                    (List.map
                       (fun id -> Json.Obj [ ("type", Json.String "public-key"); ("id", Json.String id) ])
                       issued.allowed_credentials) );
                ( "userVerification",
                  Json.String (if issued.rp.user_verification then "required" else "preferred") );
              ]
             @ json_opt_string "userId" issued.user_id) );
       ])

let begin_passkey_registration t relying_party user =
  let passkey = Passkey.make ~challenge:(challenge_service t ()) in
  Result.map_error (fun e -> Login_rejected (Passkey.string_of_error e))
    (Passkey.begin_registration passkey relying_party user)
  |> Result.map (fun registration ->
         { registration; json = passkey_registration_options_json registration })

let passkey_registration_response_of_json json =
  match
    ( json_member_string "id" json,
      json_member_string "rawId" json,
      nested_response json "clientDataJSON",
      nested_response json "attestationObject" )
  with
  | Some id, Some raw_id, Some client_data_json, Some attestation_object ->
    Ok
      {
        Passkey.id;
        raw_id;
        client_data_json;
        attestation_object;
        transports = json_member_list_strings "transports" json;
      }
  | _ -> Error (Login_rejected "Malformed passkey registration response")

let passkey_assertion_response_of_json json =
  match
    ( json_member_string "id" json,
      json_member_string "rawId" json,
      nested_response json "clientDataJSON",
      nested_response json "authenticatorData",
      nested_response json "signature" )
  with
  | Some id, Some raw_id, Some client_data_json, Some authenticator_data, Some signature ->
    Ok
      {
        Passkey.id;
        raw_id;
        client_data_json;
        authenticator_data;
        signature;
        user_handle = nested_response json "userHandle";
      }
  | _ -> Error (Login_rejected "Malformed passkey assertion response")

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
  |> Result.map (fun assertion -> { assertion; json = passkey_assertion_options_json assertion })

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
  Paw.get path (fun c ->
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
  Paw.post path (fun c ->
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
  Paw.get path (fun c ->
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
  Paw.post path (fun c ->
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
  Paw.get path (fun c ->
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
  Paw.post path (fun c ->
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

let scim_user_json (user : Scim.user) =
  Json.Obj
    ([
       json_string "externalId" user.external_id;
       json_string "userName" user.user_name;
       json_bool "active" user.active;
       json_list "emails" user.emails;
       json_list "groups" user.groups;
     ]
    @ json_opt_string "id" user.id
    @ json_opt_string "displayName" user.display_name)

let scim_group_json (group : Scim.group) =
  Json.Obj
    ([
       json_string "externalId" group.external_id;
       json_string "displayName" group.display_name;
       json_list "members" group.members;
     ]
    @ json_opt_string "id" group.id)

let json_emails json =
  match Option.bind (Json.member "emails" json) Json.to_list_opt with
  | None -> []
  | Some values ->
    List.filter_map
      (function
        | Json.String s -> Some s
        | Json.Obj _ as obj -> json_member_string "value" obj
        | _ -> None)
      values

let scim_user_of_json json =
  match (json_member_string "externalId" json, json_member_string "userName" json) with
  | Some external_id, Some user_name ->
    Scim.user ?id:(json_member_string "id" json)
      ?active:(json_member_bool "active" json)
      ~emails:(json_emails json)
      ?display_name:(json_member_string "displayName" json)
      ~groups:(json_member_list_strings "groups" json)
      ~external_id ~user_name ()
    |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e))
  | _ -> Error (Login_rejected "SCIM user requires externalId and userName")

let scim_group_of_json json =
  match (json_member_string "externalId" json, json_member_string "displayName" json) with
  | Some external_id, Some display_name ->
    Scim.group ?id:(json_member_string "id" json)
      ~members:(json_member_list_strings "members" json)
      ~external_id ~display_name ()
    |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e))
  | _ -> Error (Login_rejected "SCIM group requires externalId and displayName")

let json_member_any names json =
  List.find_map (fun key -> Json.member key json) names

let scim_patch_op_name json =
  Option.bind (json_member_any [ "op"; "Op" ] json) Json.to_string_opt
  |> Option.map (fun op -> String.lowercase_ascii (String.trim op))

let scim_patch_path json =
  Option.bind (json_member_any [ "path"; "Path" ] json) Json.to_string_opt

let scim_patch_ops json = Option.bind (json_member_any [ "Operations"; "operations" ] json) Json.to_list_opt

let scim_values_of_json field value =
  let obj_value obj =
    match json_member_string "value" obj with
    | Some value -> Ok value
    | None -> (
      match field with
      | "displayName" -> (
        match json_member_string "display" obj with
        | Some value -> Ok value
        | None -> Error (Login_rejected "SCIM PATCH object value is missing value"))
      | _ -> Error (Login_rejected "SCIM PATCH object value is missing value"))
  in
  let one = function
    | Json.String s -> Ok s
    | Json.Bool b -> Ok (string_of_bool b)
    | Json.Number n when Float.is_integer n -> Ok (string_of_int (int_of_float n))
    | Json.Obj _ as obj -> obj_value obj
    | _ -> Error (Login_rejected "SCIM PATCH value must be scalar, object, or list")
  in
  match value with
  | Json.Null -> Ok []
  | Json.List values ->
    List.fold_left
      (fun acc value ->
        Result.bind acc (fun values ->
            Result.map (fun value -> value :: values) (one value)))
      (Ok []) values
    |> Result.map List.rev
  | value -> Result.map (fun value -> [ value ]) (one value)

let scim_user_path_of_string raw =
  let path = String.lowercase_ascii (String.trim raw) in
  if path = "username" then Some (Scim.User_name, "userName")
  else if path = "active" then Some (Scim.Active, "active")
  else if path = "displayname" || path = "name.formatted" then Some (Scim.Display_name, "displayName")
  else if path = "externalid" then Some (Scim.External_id, "externalId")
  else if String.starts_with ~prefix:"emails" path then Some (Scim.Emails, "emails")
  else if String.starts_with ~prefix:"groups" path then Some (Scim.Groups, "groups")
  else None

let scim_group_path_of_string raw =
  let path = String.lowercase_ascii (String.trim raw) in
  if path = "displayname" then Some (Scim.Group_display_name, "displayName")
  else if path = "externalid" then Some (Scim.Group_external_id, "externalId")
  else if String.starts_with ~prefix:"members" path then Some (Scim.Group_members, "members")
  else None

let scim_user_patch_op op path values =
  match op with
  | "add" -> Ok (Scim.Add (path, values))
  | "replace" -> Ok (Scim.Replace (path, values))
  | "remove" -> Ok (Scim.Remove (path, values))
  | _ -> Error (Login_rejected "Unsupported SCIM PATCH op")

let scim_group_patch_op op path values =
  match op with
  | "add" -> Ok (Scim.Group_add (path, values))
  | "replace" -> Ok (Scim.Group_replace (path, values))
  | "remove" -> Ok (Scim.Group_remove (path, values))
  | _ -> Error (Login_rejected "Unsupported SCIM PATCH op")

let scim_patch_field_ops path_of_string make_op op raw_path value =
  match path_of_string raw_path with
  | None -> Ok []
  | Some (path, field) ->
    Result.bind (scim_values_of_json field value) (fun values ->
        Result.map (fun op -> [ op ]) (make_op op path values))

let scim_patch_op_list parse_one json =
  match scim_patch_ops json with
  | None -> Error (Login_rejected "SCIM PATCH requires Operations")
  | Some ops ->
    List.fold_left
      (fun acc op_json ->
        Result.bind acc (fun ops ->
            Result.map (fun parsed -> List.rev_append parsed ops) (parse_one op_json)))
      (Ok []) ops
    |> Result.map List.rev

let scim_user_patch_of_json json =
  let parse_one op_json =
    match op_json with
    | Json.Obj _ -> (
      match (scim_patch_op_name op_json, scim_patch_path op_json) with
      | None, _ -> Error (Login_rejected "SCIM PATCH operation is missing op")
      | Some op, Some raw_path -> (
        match scim_user_path_of_string raw_path with
        | None -> Error (Login_rejected "Unsupported SCIM PATCH path")
        | Some (path, field) ->
          let value = Option.value (Json.member "value" op_json) ~default:Json.Null in
          Result.bind (scim_values_of_json field value) (fun values ->
              Result.map (fun op -> [ op ]) (scim_user_patch_op op path values)))
      | Some op, None -> (
        match Json.member "value" op_json with
        | Some (Json.Obj obj_fields) ->
          List.fold_left
            (fun acc (path, value) ->
              Result.bind acc (fun ops ->
                  Result.map (fun parsed -> List.rev_append parsed ops)
                    (scim_patch_field_ops scim_user_path_of_string scim_user_patch_op op path value)))
            (Ok []) obj_fields
          |> Result.map List.rev
        | _ -> Error (Login_rejected "SCIM PATCH operation is missing path")))
    | _ -> Error (Login_rejected "Malformed SCIM PATCH operation")
  in
  scim_patch_op_list parse_one json

let scim_group_patch_of_json json =
  let parse_one op_json =
    match op_json with
    | Json.Obj _ -> (
      match (scim_patch_op_name op_json, scim_patch_path op_json) with
      | None, _ -> Error (Login_rejected "SCIM PATCH operation is missing op")
      | Some op, Some raw_path -> (
        match scim_group_path_of_string raw_path with
        | None -> Error (Login_rejected "Unsupported SCIM PATCH path")
        | Some (path, field) ->
          let value = Option.value (Json.member "value" op_json) ~default:Json.Null in
          Result.bind (scim_values_of_json field value) (fun values ->
              Result.map (fun op -> [ op ]) (scim_group_patch_op op path values)))
      | Some op, None -> (
        match Json.member "value" op_json with
        | Some (Json.Obj obj_fields) ->
          List.fold_left
            (fun acc (path, value) ->
              Result.bind acc (fun ops ->
                  Result.map (fun parsed -> List.rev_append parsed ops)
                    (scim_patch_field_ops scim_group_path_of_string scim_group_patch_op op path value)))
            (Ok []) obj_fields
          |> Result.map List.rev
        | _ -> Error (Login_rejected "SCIM PATCH operation is missing path")))
    | _ -> Error (Login_rejected "Malformed SCIM PATCH operation")
  in
  scim_patch_op_list parse_one json

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

let scim_list_response resources =
  Json.Obj
    [
      json_list "schemas" [ "urn:ietf:params:scim:api:messages:2.0:ListResponse" ];
      ("totalResults", Json.Number (float_of_int (List.length resources)));
      ("Resources", Json.List resources);
      ("startIndex", Json.Number 1.);
      ("itemsPerPage", Json.Number (float_of_int (List.length resources)));
    ]

let scim_supported enabled = Json.Obj [ ("supported", Json.Bool enabled) ]

let scim_service_provider_config_json =
  Json.Obj
    [
      json_list "schemas" [ "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig" ];
      json_string "documentationUri" "https://github.com/Anonyfox/fennec";
      ("patch", scim_supported true);
      ("bulk", scim_supported false);
      ("filter", scim_supported false);
      ("changePassword", scim_supported false);
      ("sort", scim_supported false);
      ("etag", scim_supported false);
      ( "authenticationSchemes",
        Json.List
          [
            Json.Obj
              [
                json_string "type" "oauthbearertoken";
                json_string "name" "Bearer";
                json_string "description" "Bearer token issued for this SCIM connection";
                json_string "specUri" "https://www.rfc-editor.org/rfc/rfc6750";
              ];
          ] );
    ]

let scim_resource_types_json =
  scim_list_response
    [
      Json.Obj
        [
          json_list "schemas" [ "urn:ietf:params:scim:schemas:core:2.0:ResourceType" ];
          json_string "id" "User";
          json_string "name" "User";
          json_string "endpoint" "/Users";
          json_string "schema" "urn:ietf:params:scim:schemas:core:2.0:User";
        ];
      Json.Obj
        [
          json_list "schemas" [ "urn:ietf:params:scim:schemas:core:2.0:ResourceType" ];
          json_string "id" "Group";
          json_string "name" "Group";
          json_string "endpoint" "/Groups";
          json_string "schema" "urn:ietf:params:scim:schemas:core:2.0:Group";
        ];
    ]

let scim_attribute ?(multi_valued = false) name typ =
  Json.Obj
    [
      json_string "name" name;
      json_string "type" typ;
      json_bool "multiValued" multi_valued;
      json_bool "required" false;
      json_bool "caseExact" false;
      json_string "mutability" "readWrite";
      json_string "returned" "default";
      json_string "uniqueness" "none";
    ]

let scim_schemas_json =
  scim_list_response
    [
      Json.Obj
        [
          json_string "id" "urn:ietf:params:scim:schemas:core:2.0:User";
          json_string "name" "User";
          ( "attributes",
            Json.List
              [
                scim_attribute "externalId" "string";
                scim_attribute "userName" "string";
                scim_attribute "active" "boolean";
                scim_attribute "displayName" "string";
                scim_attribute ~multi_valued:true "emails" "complex";
                scim_attribute ~multi_valued:true "groups" "complex";
              ] );
        ];
      Json.Obj
        [
          json_string "id" "urn:ietf:params:scim:schemas:core:2.0:Group";
          json_string "name" "Group";
          ( "attributes",
            Json.List
              [
                scim_attribute "externalId" "string";
                scim_attribute "displayName" "string";
                scim_attribute ~multi_valued:true "members" "complex";
              ] );
        ];
    ]

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
  | Some `ServiceProviderConfig -> Conn.json c (Json.to_string scim_service_provider_config_json)
  | Some `ResourceTypes -> Conn.json c (Json.to_string scim_resource_types_json)
  | Some `Schemas -> Conn.json c (Json.to_string scim_schemas_json)
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
                (List.map scim_user_json
                   (t.store.scim.Scim.list_users ~connection_id:connection.id ())) );
          ]
      | H.GET, `Users (Some external_id), _ -> (
        match t.store.scim.Scim.find_user ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM user not found"
        | Some user -> Conn.json c (Json.to_string (scim_user_json user)))
      | H.POST, `Users None, Some json | H.PUT, `Users (Some _), Some json -> (
        match Result.bind (scim_user_of_json json) (apply_scim_user t connection) with
        | Error e -> json_error c (string_of_error e)
        | Ok user -> Conn.json ~status:201 c (Json.to_string (scim_user_json user)))
      | H.PATCH, `Users (Some external_id), Some json -> (
        match t.store.scim.Scim.find_user ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM user not found"
        | Some user -> (
          match
            Result.bind (scim_user_patch_of_json json) (fun ops ->
                Scim.apply_user_patch user ops
                |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
          with
          | Error e -> json_error c (string_of_error e)
          | Ok patched when patched.Scim.external_id <> external_id ->
            json_error c "SCIM PATCH cannot change externalId"
          | Ok patched -> (
            match apply_scim_user t connection patched with
            | Error e -> json_error c (string_of_error e)
            | Ok user -> Conn.json c (Json.to_string (scim_user_json user)))))
      | H.DELETE, `Users (Some external_id), _ ->
        ignore (t.store.scim.Scim.delete_user ~connection_id:connection.id ~external_id);
        Conn.text ~status:204 c ""
      | H.GET, `Groups None, _ ->
        json_ok c
          [
            ( "Resources",
              Json.List
                (List.map scim_group_json
                   (t.store.scim.Scim.list_groups ~connection_id:connection.id ())) );
          ]
      | H.GET, `Groups (Some external_id), _ -> (
        match t.store.scim.Scim.find_group ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM group not found"
        | Some group -> Conn.json c (Json.to_string (scim_group_json group)))
      | H.POST, `Groups None, Some json | H.PUT, `Groups (Some _), Some json -> (
        match scim_group_of_json json with
        | Error e -> json_error c (string_of_error e)
        | Ok group -> (
          match t.store.scim.Scim.upsert_group ~connection_id:connection.id group with
          | Error e -> json_error c e
          | Ok () -> Conn.json ~status:201 c (Json.to_string (scim_group_json group))))
      | H.PATCH, `Groups (Some external_id), Some json -> (
        match t.store.scim.Scim.find_group ~connection_id:connection.id ~external_id with
        | None -> json_error ~status:404 c "SCIM group not found"
        | Some group -> (
          match
            Result.bind (scim_group_patch_of_json json) (fun ops ->
                Scim.apply_group_patch group ops
                |> Result.map_error (fun e -> Login_rejected (Scim.string_of_error e)))
          with
          | Error e -> json_error c (string_of_error e)
          | Ok patched when patched.Scim.external_id <> external_id ->
            json_error c "SCIM PATCH cannot change externalId"
          | Ok patched -> (
            match t.store.scim.Scim.upsert_group ~connection_id:connection.id patched with
            | Error e -> json_error c e
            | Ok () -> Conn.json c (Json.to_string (scim_group_json patched)))))
      | H.DELETE, `Groups (Some external_id), _ ->
        ignore (t.store.scim.Scim.delete_group ~connection_id:connection.id ~external_id);
        Conn.text ~status:204 c ""
      | _, _, None -> json_error c "Malformed JSON"
      | _ -> json_error ~status:405 c "Unsupported SCIM operation"))

let oauth_authorize_paw t ?(redirect_param = "redirect") ~path ~error provider () =
  Paw.get path (fun c ->
      let oauth = OAuth.make ~challenge:(challenge_service t ()) in
      let redirect = Conn.param c redirect_param in
      match OAuth.authorize oauth ?user_id:(user_id c) ?redirect provider with
      | Error _ -> Conn.redirect c error
      | Ok issued -> Conn.redirect c issued.OAuth.url)

let oauth_callback_paw t ?(link_verified_email = true) ~path ~success ~error provider ~exchange () =
  Paw.get path (fun c ->
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

let oidc_authorize_paw t ?(redirect_param = "redirect") ~path ~error (connection : Oidc.connection) () =
  Paw.get path (fun c ->
      let oidc = Oidc.make ~challenge:(challenge_service t ()) in
      let redirect = Conn.param c redirect_param in
      match Oidc.authorize oidc ?user_id:(user_id c) ?redirect connection with
      | Error _ -> Conn.redirect c error
      | Ok issued -> Conn.redirect c issued.Oidc.url)

let oidc_callback_paw t ?(link_verified_email = true) ~path ~success ~error (connection : Oidc.connection) ~exchange () =
  Paw.get path (fun c ->
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
  Paw.get path (fun c ->
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
  Paw.post path (fun c ->
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

module Codec = struct
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
    let fields =
      match u.username with Some username -> ("username", Bson.str username) :: fields | None -> fields
    in
    let fields = match u.profile with Some profile -> ("profile", profile) :: fields | None -> fields in
    let fields =
      match password_hash with Some hash -> ("passwordHash", Bson.str hash) :: fields | None -> fields
    in
    Bson.doc (List.rev fields)

  let set_fields doc =
    Bson.fields doc |> List.filter (fun (k, _) -> k <> "_id") |> Bson.doc

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

  let mfa_factor_to_string = function
    | Mfa.Password -> "password"
    | Email -> "email"
    | OAuth -> "oauth"
    | Oidc -> "oidc"
    | Saml -> "saml"
    | Passkey -> "passkey"
    | Totp -> "totp"
    | Backup_code -> "backup_code"
    | Recovery_code -> "recovery_code"

  let mfa_factor_of_string = function
    | "password" -> Some Mfa.Password
    | "email" -> Some Email
    | "oauth" -> Some OAuth
    | "oidc" -> Some Oidc
    | "saml" -> Some Saml
    | "passkey" -> Some Passkey
    | "totp" -> Some Totp
    | "backup_code" -> Some Backup_code
    | "recovery_code" -> Some Recovery_code
    | _ -> None

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
end

let mfa_level_to_string = function
  | Mfa.Anonymous -> "anonymous"
  | Single_factor -> "single_factor"
  | Phishing_resistant_single_factor -> "phishing_resistant_single_factor"
  | Multi_factor -> "multi_factor"
  | Phishing_resistant_multi_factor -> "phishing_resistant_multi_factor"

let opt_bson = function None -> Bson.null | Some value -> value

let public_user_to_doc (user : user) =
  let fields =
    [
      ("_id", Bson.str user.id);
      ("id", Bson.str user.id);
      ("emails", Bson.array (List.map Codec.email_to_doc user.emails));
      ("roles", Bson.array (List.map (fun role -> Bson.str (Roles.Role.name role)) user.roles));
      ("status", Bson.str (string_of_user_status user.status));
      ("createdAt", Bson.float user.created_at);
      ("updatedAt", Bson.float user.updated_at);
    ]
  in
  let fields =
    match user.username with Some username -> ("username", Bson.str username) :: fields | None -> fields
  in
  let fields = match user.profile with Some profile -> ("profile", profile) :: fields | None -> fields in
  Bson.doc (List.rev fields)

let auth_context_to_doc (ctx : auth_context) =
  Bson.doc
    [
      ("userId", Bson.str ctx.user_id);
      ("sessionId", Bson.str ctx.session_id);
      ("strategy", Bson.str ctx.strategy);
      ("factors", Bson.array (List.map (fun factor -> Bson.str (Codec.mfa_factor_to_string factor)) ctx.factors));
      ("issuedAt", Bson.float ctx.issued_at);
      ("expiresAt", Bson.float ctx.expires_at);
      ("authEpoch", Bson.int ctx.auth_epoch);
    ]

let assurance_to_doc (assurance : Mfa.assurance) =
  Bson.doc
    [
      ("level", Bson.str (mfa_level_to_string assurance.level));
      ("factors", Bson.array (List.map (fun factor -> Bson.str (Codec.mfa_factor_to_string factor)) assurance.factors));
      ("authenticatedAt", Bson.float assurance.authenticated_at);
    ]

let org_context_to_doc (ctx : org_context) =
  Bson.doc
    [
      ("org", Codec.org_to_doc ctx.org);
      ("membership", opt_bson (Option.map Codec.membership_to_doc ctx.membership));
    ]

let session_doc t c =
  Result.map
    (fun user ->
      Bson.doc
        [
          ("userId", opt_bson (Option.map (fun user -> Bson.str user.id) user));
          ("user", opt_bson (Option.map public_user_to_doc user));
          ("authContext", opt_bson (Option.map auth_context_to_doc (auth_context c)));
          ("assurance", opt_bson (Option.map assurance_to_doc (assurance c)));
          ("org", opt_bson (Option.map org_context_to_doc (org_context c)));
        ])
    (current_user t c)

let session_paw t ~path () =
  Paw.get path (fun c ->
      let c = paw t () c in
      match session_doc t c with
      | Ok doc -> Conn.json ~headers:[ ("Cache-Control", "no-store") ] c (Bson_json.to_string doc)
      | Error e ->
        Conn.json ~status:500 ~headers:[ ("Cache-Control", "no-store") ] c
          (Bson_json.to_string (Bson.doc [ ("error", Bson.str (string_of_error e)) ])))

module Collection_store = struct
  type collection = {
    find_one : Bson.t -> Bson.t option;
    find : Bson.t -> Bson.t list;
    insert_one : Bson.t -> (unit, string) result;
    update_one : filter:Bson.t -> update:Bson.t -> (int, string) result;
    delete_many : Bson.t -> (int, string) result;
  }

  type collections = {
    users : collection;
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
    let find_user filter =
      with_lock mutex (fun () ->
          match c.users.find_one filter with
          | None -> Ok None
          | Some doc -> Result.map Option.some (Codec.user_of_doc doc))
    in
    let find_user_by_id id = find_user (id_selector id) in
    let find_user_by_email email =
      let email = normalize_email email in
      with_lock mutex (fun () ->
          c.users.find (Bson.doc [])
          |> List.find_map (fun doc ->
                 match Codec.user_of_doc doc with
                 | Ok user when List.exists (fun e -> normalize_email e.address = email) user.emails ->
                   Some (Ok (Some user))
                 | Ok _ -> None
                 | Error e -> Some (Error e))
          |> Option.value ~default:(Ok None))
    in
    let find_user_by_username username =
      let username = normalize_username username in
      with_lock mutex (fun () ->
          c.users.find (Bson.doc [])
          |> List.find_map (fun doc ->
                 match Codec.user_of_doc doc with
                 | Ok user when option_exists (fun name -> normalize_username name = username) user.username ->
                   Some (Ok (Some user))
                 | Ok _ -> None
                 | Error e -> Some (Error e))
          |> Option.value ~default:(Ok None))
    in
    let find_user_by_service ~strategy ~service_id =
      find_user (Bson.doc [ ("services." ^ strategy ^ ".id", Bson.str service_id) ])
    in
    let exists_other_email id email =
      let email = normalize_email email in
      c.users.find (Bson.doc [])
      |> List.exists (fun doc ->
             match Codec.user_of_doc doc with
             | Ok user -> user.id <> id && List.exists (fun e -> normalize_email e.address = email) user.emails
             | Error _ -> false)
    in
    let exists_other_username id username =
      match c.users.find_one (Bson.doc [ ("username", Bson.str (normalize_username username)) ]) with
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
                  Result.bind
                    (map_store_error
                       (c.users.update_one ~filter:(id_selector u.id)
                          ~update:(Bson.doc [ ("$set", Codec.set_fields (Codec.user_to_doc ?password_hash updated)) ])))
                    (fun n -> if n = 0 then Error User_not_found else Ok updated))))
    in
    let password_hash id =
      with_lock mutex (fun () ->
          match c.users.find_one (id_selector id) with
          | None -> Ok None
          | Some doc -> Ok (doc_get_string doc "passwordHash"))
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
      identities = identity_store mutex collections;
      challenges = challenge_store mutex collections;
      passkeys = passkey_store mutex collections;
      orgs = org_store mutex collections;
      mfa = mfa_store mutex collections;
      scim = scim_store mutex collections;
      audit;
      ensure_indexes;
    }
end

let selector_of_bson = function
  | Bson.String s when String.trim s <> "" -> Ok (By_username s)
  | Bson.String _ -> Error "login selector cannot be blank"
  | Bson.Document _ as d -> (
    match (doc_get_string d "id", doc_get_string d "email", doc_get_string d "username") with
    | Some id, _, _ when String.trim id <> "" -> Ok (By_id id)
    | _, Some email, _ when String.trim email <> "" -> Ok (By_email email)
    | _, _, Some username when String.trim username <> "" -> Ok (By_username username)
    | _ -> Error "login selector expects id, email, or username")
  | _ -> Error "login selector expects a string or document"

let user_doc (u : user) =
  Bson.doc
    [
      ("id", Bson.str u.id);
      ("username", (match u.username with Some s -> Bson.str s | None -> Bson.Null));
      ( "emails",
        Bson.array
          (List.map
             (fun e -> Bson.doc [ ("address", Bson.str e.address); ("verified", Bson.bool e.verified) ])
             u.emails) );
      ("roles", Bson.array (List.map (fun role -> Bson.str (Roles.Role.name role)) u.roles));
      ("createdAt", Bson.float u.created_at);
      ("updatedAt", Bson.float u.updated_at);
    ]

let ddp_session_doc t user_id =
  let doc user =
    Bson.doc
      [
        ("userId", opt_bson (Option.map (fun user -> Bson.str user.id) user));
        ("user", opt_bson (Option.map user_doc user));
        ("authContext", Bson.Null);
        ("assurance", Bson.Null);
        ("org", Bson.Null);
      ]
  in
  match user_id with
  | None -> Ok (doc None)
  | Some uid -> Result.map (fun user -> doc (Some user)) (find_required_user t uid)

module Methods (R : sig
  type doc = Bson.t
  type invocation = { user_id : string option; is_simulation : bool; set_user_id : string option -> unit }
  exception Error of { code : string; reason : string }
  val methods : (string * (invocation -> doc list -> doc)) list -> unit
end) =
struct
  type invocation = R.invocation

  let bad_request reason = raise (R.Error { code = "400"; reason })
  let forbidden reason = raise (R.Error { code = "403"; reason })

  let register t =
    let mfa_doc (step_up : login_step_up) =
      Bson.doc
        [
          ("mfaRequired", Bson.Bool true);
          ("userId", Bson.str step_up.user.id);
          ("mfaToken", Bson.str (Challenge.token_to_string step_up.step_up.token));
        ]
    in
    let login_doc inv = function
      | Error e -> forbidden (string_of_error e)
      | Ok (u, token) ->
        inv.R.set_user_id (Some u.id);
        Bson.doc [ ("id", Bson.str u.id); ("token", Bson.str token) ]
    in
    let login_completion_doc inv = function
      | Error e -> forbidden (string_of_error e)
      | Ok (Complete_login (u, token)) ->
        inv.R.set_user_id (Some u.id);
        Bson.doc [ ("id", Bson.str u.id); ("token", Bson.str token) ]
      | Ok (Step_up_required step_up) -> mfa_doc step_up
    in
    let complete_step_up_doc inv mfa_token verification =
      match Result.bind verification (complete_login_step_up t (Challenge.token_of_string mfa_token)) with
      | Error e -> forbidden (string_of_error e)
      | Ok (user, token) ->
        inv.R.set_user_id (Some user.id);
        Bson.doc [ ("id", Bson.str user.id); ("token", Bson.str token) ]
    in
    let current_user_method inv = function
      | [] -> (
        match ddp_session_doc t inv.R.user_id with
        | Ok doc -> doc
        | Error e -> forbidden (string_of_error e))
      | _ -> bad_request "currentUser expects no arguments"
    in
    let create_user_method inv = function
      | [ Bson.Document _ as d ] ->
        let username = doc_get_string d "username" in
        let email = doc_get_string d "email" in
        let password = doc_get_string d "password" in
        let profile = doc_get_doc d "profile" in
        (match password with
        | None -> bad_request "createUser expects a password"
        | Some password -> (
          match create_user t ?username ?email ~password ?profile () with
          | Error e -> forbidden (string_of_error e)
          | Ok u -> (
            match finish_login t ~strategy:"createUser" u with
            | Error e -> forbidden (string_of_error e)
            | Ok (_, token) ->
              inv.R.set_user_id (Some u.id);
              Bson.doc [ ("id", Bson.str u.id); ("token", Bson.str token); ("user", user_doc u) ])))
      | _ -> bad_request "createUser expects one document argument"
    in
    let login_method inv = function
      | [ selector; Bson.String password ] -> (
        match selector_of_bson selector with
        | Error reason -> bad_request reason
        | Ok selector -> login_completion_doc inv (login_with_password_completion t selector ~password))
      | [ Bson.Document _ as d ] -> begin
        match doc_get_string d "resume" with
        | Some token -> login_doc inv (login_with_token t (token_of_string token))
        | None -> begin
          match (doc_get_string d "strategy", Bson.get d "credentials") with
          | Some strategy, Some credentials ->
            login_completion_doc inv (login_with_strategy_completion t strategy ~credentials)
          | _ -> (
            match (Bson.get d "user", doc_get_string d "password") with
            | Some selector, Some password ->
              (match selector_of_bson selector with
              | Error reason -> bad_request reason
              | Ok selector -> login_completion_doc inv (login_with_password_completion t selector ~password))
            | _ -> bad_request "login expects {user, password}, {strategy, credentials}, or {resume}")
        end
      end
      | _ -> bad_request "login expects selector/password"
    in
    let logout_method inv = function
      | [] ->
        let uid = inv.R.user_id in
        inv.R.set_user_id None;
        observe_logout t uid;
        Bson.bool true
      | _ -> bad_request "logout expects no arguments"
    in
    let logout_other_clients_method inv = function
      | [] -> (
        match inv.R.user_id with
        | None -> forbidden "Not logged in"
        | Some uid -> (
          match logout_other_clients_and_refresh t uid with
          | Ok (user, token) ->
            inv.R.set_user_id (Some user.id);
            login_doc inv (Ok (user, token))
          | Error e -> forbidden (string_of_error e)))
      | _ -> bad_request "logoutOtherClients expects no arguments"
    in
    let change_password_method inv = function
      | [ Bson.String old_password; Bson.String new_password ] -> (
        match inv.R.user_id with
        | None -> forbidden "Not logged in"
        | Some uid -> (
          match change_password t uid ~old_password ~new_password with
          | Ok () -> Bson.bool true
          | Error e -> forbidden (string_of_error e)))
      | _ -> bad_request "changePassword expects old and new password strings"
    in
    let reset_password_method inv = function
      | [ Bson.String token; Bson.String password ] -> (
        match reset_password_completion t (Challenge.token_of_string token) ~password with
        | Ok (Complete_login (user, token)) -> login_doc inv (Ok (user, token))
        | Ok (Step_up_required step_up) -> mfa_doc step_up
        | Error e -> forbidden (string_of_error e))
      | _ -> bad_request "resetPassword expects token and password strings"
    in
    let verify_email_method inv = function
      | [ Bson.String token ] -> (
        match verify_email_completion t (Challenge.token_of_string token) with
        | Ok (Complete_login (user, token)) -> login_doc inv (Ok (user, token))
        | Ok (Step_up_required step_up) -> mfa_doc step_up
        | Error e -> forbidden (string_of_error e))
      | _ -> bad_request "verifyEmail expects one token string"
    in
    let enroll_account_method inv = function
      | [ Bson.String token; Bson.String password ] -> (
        match enroll_account_completion t (Challenge.token_of_string token) ~password with
        | Ok (Complete_login (user, token)) -> login_doc inv (Ok (user, token))
        | Ok (Step_up_required step_up) -> mfa_doc step_up
        | Error e -> forbidden (string_of_error e))
      | _ -> bad_request "enrollAccount expects token and password strings"
    in
    let complete_login_step_up_method inv = function
      | [ Bson.Document _ as d ] -> (
        match doc_get_string d "mfaToken" with
        | None -> bad_request "completeLoginStepUp expects mfaToken"
        | Some mfa_token -> (
          match (doc_get_string d "totpId", doc_get_string d "code", doc_get_string d "userId", doc_get_string d "backupCode") with
          | Some totp_id, Some code, _, _ ->
            complete_step_up_doc inv mfa_token (verify_totp_factor t totp_id ~code)
          | _, _, Some user_id, Some code ->
            complete_step_up_doc inv mfa_token (consume_backup_code t user_id ~code)
          | _ -> bad_request "completeLoginStepUp expects {mfaToken, totpId, code} or {mfaToken, userId, backupCode}"))
      | _ -> bad_request "completeLoginStepUp expects one document argument"
    in
    R.methods
      [
        ("createUser", create_user_method);
        ("currentUser", current_user_method);
        ("login", login_method);
        ("logout", logout_method);
        ("logoutOtherClients", logout_other_clients_method);
        ("changePassword", change_password_method);
        ("resetPassword", reset_password_method);
        ("verifyEmail", verify_email_method);
        ("enrollAccount", enroll_account_method);
        ("completeLoginStepUp", complete_login_step_up_method);
      ]
end

let memory_user_store () =
  let users : (user_id, user) Hashtbl.t = Hashtbl.create 64 in
  let passwords : (user_id, string) Hashtbl.t = Hashtbl.create 64 in
  let m = Mutex.create () in
  let locked f = Mutex.lock m; Fun.protect ~finally:(fun () -> Mutex.unlock m) f in
  let find_unlocked (pred : user -> bool) = Hashtbl.to_seq_values users |> Seq.find pred in
  let find pred = locked (fun () -> find_unlocked pred) in
  let user_has_email email (u : user) =
    List.exists (fun e -> normalize_email e.address = email) u.emails
  in
  let user_has_username username (u : user) =
    option_exists (fun n -> normalize_username n = username) u.username
  in
  let find_user_by_id id = locked (fun () -> Ok (Hashtbl.find_opt users id)) in
  let find_user_by_email email =
    let email = normalize_email email in
    Ok (find (user_has_email email))
  in
  let find_user_by_username username =
    let username = normalize_username username in
    Ok (find (user_has_username username))
  in
  let exists_email_unlocked email =
    match find_unlocked (user_has_email (normalize_email email)) with Some _ -> true | None -> false
  in
  let exists_username_unlocked username =
    match find_unlocked (user_has_username (normalize_username username)) with Some _ -> true | None -> false
  in
  let exists_other_email_unlocked id email =
    match find_unlocked (fun u -> u.id <> id && user_has_email (normalize_email email) u) with Some _ -> true | None -> false
  in
  let exists_other_username_unlocked id username =
    match find_unlocked (fun u -> u.id <> id && user_has_username (normalize_username username) u) with
    | Some _ -> true
    | None -> false
  in
  let find_user_by_service ~strategy ~service_id =
    Ok
      (find (fun u ->
           match List.assoc_opt strategy u.services with
           | Some svc -> doc_get_string svc "id" = Some service_id
           | None -> false))
  in
  let create_user u ~password_hash =
    locked (fun () ->
        match validate_user_shape u with
        | Error _ as e -> e
        | Ok () ->
          if Hashtbl.mem users u.id then Error (Store_error ("duplicate user id: " ^ u.id))
          else
          match
            List.find_map
              (fun e ->
                let email = normalize_email e.address in
                if exists_email_unlocked email then Some (Duplicate_email email) else None)
              u.emails
          with
          | Some e -> Error e
          | None -> (
            match u.username with
            | Some username when exists_username_unlocked username ->
              Error (Duplicate_username (normalize_username username))
            | _ ->
              Hashtbl.add users u.id u;
              Option.iter (Hashtbl.replace passwords u.id) password_hash;
              Ok u))
  in
  let update_user u =
    locked (fun () ->
        match validate_user_shape u with
        | Error _ as e -> e
        | Ok () ->
          if not (Hashtbl.mem users u.id) then Error User_not_found
          else
          match
            List.find_map
              (fun e ->
                let email = normalize_email e.address in
                if exists_other_email_unlocked u.id email then Some (Duplicate_email email) else None)
              u.emails
          with
          | Some e -> Error e
          | None -> (
            match u.username with
            | Some username when exists_other_username_unlocked u.id username ->
              Error (Duplicate_username (normalize_username username))
            | _ ->
              let updated = { u with updated_at = now () } in
              Hashtbl.replace users u.id updated;
              Ok updated))
  in
  let password_hash id = locked (fun () -> Ok (Hashtbl.find_opt passwords id)) in
  let set_password_hash id hash =
    locked (fun () ->
        if Hashtbl.mem users id then (Hashtbl.replace passwords id hash; Ok ()) else Error User_not_found)
  in
  let set_password_hash_and_bump id hash =
    locked (fun () ->
        match Hashtbl.find_opt users id with
        | None -> Error User_not_found
        | Some u ->
          let epoch = u.auth_epoch + 1 in
          Hashtbl.replace passwords id hash;
          Hashtbl.replace users id { u with auth_epoch = epoch; updated_at = now () };
          Ok epoch)
  in
  let bump_auth_epoch id =
    locked (fun () ->
        match Hashtbl.find_opt users id with
        | None -> Error User_not_found
        | Some u ->
          let epoch = u.auth_epoch + 1 in
          Hashtbl.replace users id { u with auth_epoch = epoch; updated_at = now () };
          Ok epoch)
  in
  { find_user_by_id; find_user_by_email; find_user_by_username; find_user_by_service; create_user; update_user; password_hash; set_password_hash; set_password_hash_and_bump; bump_auth_epoch }

let memory_store () =
  {
    users = memory_user_store ();
    identities = Identity.memory_store ();
    challenges = Challenge.memory_store ();
    passkeys = Passkey.memory_store ();
    orgs = Org.memory_store ();
    mfa = Mfa.memory_store ();
    scim = Scim.memory_store ();
    audit = Audit.memory_store ();
    ensure_indexes = (fun () -> ());
  }

module Store = struct
  type t = store
  type user = user_store

  let unavailable ?(message = Mongo_runtime.unavailable_message ()) () =
    let store_error = Store_error message in
    let error = Error store_error in
    let string_error = Error message in
    let challenge_error = Error (Challenge.Store_error message) in
    let users =
      {
        find_user_by_id = (fun _ -> error);
        find_user_by_email = (fun _ -> error);
        find_user_by_username = (fun _ -> error);
        find_user_by_service = (fun ~strategy:_ ~service_id:_ -> error);
        create_user = (fun _ ~password_hash:_ -> error);
        update_user = (fun _ -> error);
        password_hash = (fun _ -> error);
        set_password_hash = (fun _ _ -> error);
        set_password_hash_and_bump = (fun _ _ -> error);
        bump_auth_epoch = (fun _ -> error);
      }
    in
    let identities =
      let conflict ?verified_at ~created_at ~user_id key =
        Identity.Conflict (Identity.link ?verified_at ~user_id key ~created_at)
      in
      {
        Identity.find = (fun _ -> None);
        list = (fun ?user_id:_ () -> []);
        attach = conflict;
        detach = (fun ?allow_last:_ ~user_id:_ _ -> Identity.Link_not_found);
        merge = (fun ~from_user_id:_ ~into_user_id:_ -> Ok { Identity.from_user_id = ""; into_user_id = ""; move = []; keep = []; conflicts = [] });
      }
    in
    let challenges =
      {
        Challenge.insert = (fun _ ~secret_hash:_ -> challenge_error);
        find = (fun _ -> challenge_error);
        consume = (fun _ _ ~secret_hash:_ ~now:_ -> challenge_error);
        revoke = (fun _ ~now:_ -> challenge_error);
        revoke_user = (fun ?purpose:_ _ ~now:_ -> challenge_error);
        revoke_email = (fun ?purpose:_ _ ~now:_ -> challenge_error);
        gc_expired = (fun ~now:_ -> challenge_error);
      }
    in
    let passkeys =
      {
        Passkey.find = (fun _ -> None);
        list = (fun ?user_id:_ () -> []);
        insert = (fun _ -> string_error);
        update = (fun _ -> string_error);
        delete = (fun _ -> string_error);
      }
    in
    let orgs =
      {
        Org.find_org = (fun _ -> None);
        list_orgs = (fun () -> []);
        upsert_org = (fun _ -> string_error);
        delete_org = (fun _ -> string_error);
        find_membership = (fun ~org_id:_ ~user_id:_ -> None);
        list_memberships = (fun ?org_id:_ ?user_id:_ () -> []);
        upsert_membership = (fun _ -> string_error);
        delete_membership = (fun ~org_id:_ ~user_id:_ -> string_error);
        find_invite = (fun _ -> None);
        list_invites = (fun ?org_id:_ ?email:_ () -> []);
        upsert_invite = (fun _ -> string_error);
        delete_invite = (fun _ -> string_error);
      }
    in
    let mfa =
      {
        Mfa.find = (fun _ -> None);
        list = (fun ?user_id:_ ?factor:_ () -> []);
        upsert = (fun _ -> string_error);
        replace_if_current = (fun ~current:_ _ -> string_error);
        delete = (fun _ -> string_error);
      }
    in
    let scim =
      {
        Scim.find_connection = (fun _ -> None);
        list_connections = (fun ?org_id:_ () -> []);
        upsert_connection = (fun _ -> string_error);
        delete_connection = (fun _ -> string_error);
        find_user = (fun ~connection_id:_ ~external_id:_ -> None);
        list_users = (fun ?connection_id:_ () -> []);
        upsert_user = (fun ~connection_id:_ _ -> string_error);
        delete_user = (fun ~connection_id:_ ~external_id:_ -> string_error);
        find_group = (fun ~connection_id:_ ~external_id:_ -> None);
        list_groups = (fun ?connection_id:_ () -> []);
        upsert_group = (fun ~connection_id:_ _ -> string_error);
        delete_group = (fun ~connection_id:_ ~external_id:_ -> string_error);
      }
    in
    let audit =
      Audit.store ~append:(fun _ -> string_error)
        ~list:(fun ~target_user_id:_ ~org_id:_ ~kind:_ -> [])
    in
    { users; identities; challenges; passkeys; orgs; mfa; scim; audit; ensure_indexes = (fun () -> ()) }

  let minimongo_collection () =
    let c = Minimongo.create () in
    {
      Collection_store.find_one = (fun filter -> Minimongo.find_one c ~selector:filter ());
      find = (fun filter -> Minimongo.fetch (Minimongo.find c ~selector:filter ()));
      insert_one =
        (fun doc ->
          try
            ignore (Minimongo.insert c doc);
            Ok ()
          with exn -> Error (Printexc.to_string exn));
      update_one =
        (fun ~filter ~update ->
          try Ok (Minimongo.update c ~multi:false ~upsert:false filter update)
          with exn -> Error (Printexc.to_string exn));
      delete_many =
        (fun filter ->
          try Ok (Minimongo.remove c filter) with exn -> Error (Printexc.to_string exn));
    }

  let memory = memory_store

  let minimongo () =
    Collection_store.make
      {
        users = minimongo_collection ();
        identities = minimongo_collection ();
        challenges = minimongo_collection ();
        passkeys = minimongo_collection ();
        orgs = minimongo_collection ();
        org_memberships = minimongo_collection ();
        org_invites = minimongo_collection ();
        mfa_enrollments = minimongo_collection ();
        scim_connections = minimongo_collection ();
        scim_users = minimongo_collection ();
        scim_groups = minimongo_collection ();
        audit = minimongo_collection ();
      }

  let mongo_collection db name =
    let module Coll = Fennec_mongo_driver.Collection in
    let c = Fennec_mongo_driver.Database.collection db name in
    let int_field reply k =
      match Bson.get reply k with Some v -> ( match Bson.as_float v with Some f -> int_of_float f | None -> 0) | None -> 0
    in
    {
      Collection_store.find_one = (fun filter -> Coll.find_one c ~filter ());
      find = (fun filter -> Coll.find c ~filter ());
      insert_one =
        (fun doc -> try Ok (ignore (Coll.insert_one c doc)) with exn -> Error (Printexc.to_string exn));
      update_one =
        (fun ~filter ~update ->
          try
            let reply = Coll.update_one c ~filter ~update in
            Ok (int_field reply "n")
          with exn -> Error (Printexc.to_string exn));
      delete_many =
        (fun filter ->
          try
            let reply = Coll.delete_many c ~filter in
            Ok (int_field reply "n")
          with exn -> Error (Printexc.to_string exn));
    }

  let mongo ?(prefix = "accounts") db =
    let module Coll = Fennec_mongo_driver.Collection in
    let name suffix = prefix ^ "_" ^ suffix in
    let users_c = Fennec_mongo_driver.Database.collection db (name "users") in
    let identities_c = Fennec_mongo_driver.Database.collection db (name "identities") in
    let challenges_c = Fennec_mongo_driver.Database.collection db (name "challenges") in
    let passkeys_c = Fennec_mongo_driver.Database.collection db (name "passkeys") in
    let orgs_c = Fennec_mongo_driver.Database.collection db (name "orgs") in
    let org_memberships_c = Fennec_mongo_driver.Database.collection db (name "org_memberships") in
    let org_invites_c = Fennec_mongo_driver.Database.collection db (name "org_invites") in
    let mfa_enrollments_c = Fennec_mongo_driver.Database.collection db (name "mfa_enrollments") in
    let scim_connections_c = Fennec_mongo_driver.Database.collection db (name "scim_connections") in
    let scim_users_c = Fennec_mongo_driver.Database.collection db (name "scim_users") in
    let scim_groups_c = Fennec_mongo_driver.Database.collection db (name "scim_groups") in
    let audit_c = Fennec_mongo_driver.Database.collection db (name "audit") in
    let ensure_indexes () =
      let unique_sparse = [ ("unique", Bson.bool true); ("sparse", Bson.bool true) ] in
      ignore (Coll.create_index users_c ~keys:(Bson.doc [ ("username", Bson.int 1) ]) ~opts:unique_sparse ~name:"uniq_username" ());
      ignore (Coll.create_index users_c ~keys:(Bson.doc [ ("emails.address", Bson.int 1) ]) ~opts:unique_sparse ~name:"uniq_email" ());
      ignore (Coll.create_index identities_c ~keys:(Bson.doc [ ("userId", Bson.int 1) ]) ~name:"by_user" ());
      ignore (Coll.create_index identities_c ~keys:(Bson.doc [ ("stableKey", Bson.int 1) ]) ~name:"by_stable_key" ());
      ignore (Coll.create_index challenges_c ~keys:(Bson.doc [ ("expiresAt", Bson.int 1) ]) ~name:"by_expiry" ());
      ignore (Coll.create_index challenges_c ~keys:(Bson.doc [ ("metadata.userId", Bson.int 1) ]) ~name:"by_user" ());
      ignore (Coll.create_index challenges_c ~keys:(Bson.doc [ ("metadata.email", Bson.int 1) ]) ~name:"by_email" ());
      ignore (Coll.create_index passkeys_c ~keys:(Bson.doc [ ("userId", Bson.int 1) ]) ~name:"by_user" ());
      ignore (Coll.create_index orgs_c ~keys:(Bson.doc [ ("domains.name", Bson.int 1) ]) ~name:"by_domain" ());
      ignore (Coll.create_index org_memberships_c ~keys:(Bson.doc [ ("orgId", Bson.int 1) ]) ~name:"by_org" ());
      ignore (Coll.create_index org_memberships_c ~keys:(Bson.doc [ ("userId", Bson.int 1) ]) ~name:"by_user" ());
      ignore (Coll.create_index org_invites_c ~keys:(Bson.doc [ ("orgId", Bson.int 1) ]) ~name:"by_org" ());
      ignore (Coll.create_index org_invites_c ~keys:(Bson.doc [ ("email", Bson.int 1) ]) ~name:"by_email" ());
      ignore (Coll.create_index org_invites_c ~keys:(Bson.doc [ ("expiresAt", Bson.int 1) ]) ~name:"by_expiry" ());
      ignore (Coll.create_index mfa_enrollments_c ~keys:(Bson.doc [ ("userId", Bson.int 1) ]) ~name:"by_user" ());
      ignore (Coll.create_index scim_connections_c ~keys:(Bson.doc [ ("orgId", Bson.int 1) ]) ~name:"by_org" ());
      ignore (Coll.create_index scim_users_c ~keys:(Bson.doc [ ("connectionId", Bson.int 1) ]) ~name:"by_connection" ());
      ignore (Coll.create_index scim_groups_c ~keys:(Bson.doc [ ("connectionId", Bson.int 1) ]) ~name:"by_connection" ());
      ignore (Coll.create_index audit_c ~keys:(Bson.doc [ ("targetUserId", Bson.int 1) ]) ~name:"by_target_user" ());
      ignore (Coll.create_index audit_c ~keys:(Bson.doc [ ("orgId", Bson.int 1) ]) ~name:"by_org" ());
      ignore (Coll.create_index audit_c ~keys:(Bson.doc [ ("kind", Bson.int 1) ]) ~name:"by_kind" ());
      ignore (Coll.create_index audit_c ~keys:(Bson.doc [ ("at", Bson.int (-1)) ]) ~name:"by_time" ())
    in
    Collection_store.make ~ensure_indexes
      {
        users = mongo_collection db (name "users");
        identities = mongo_collection db (name "identities");
        challenges = mongo_collection db (name "challenges");
        passkeys = mongo_collection db (name "passkeys");
        orgs = mongo_collection db (name "orgs");
        org_memberships = mongo_collection db (name "org_memberships");
        org_invites = mongo_collection db (name "org_invites");
        mfa_enrollments = mongo_collection db (name "mfa_enrollments");
        scim_connections = mongo_collection db (name "scim_connections");
        scim_users = mongo_collection db (name "scim_users");
        scim_groups = mongo_collection db (name "scim_groups");
        audit = mongo_collection db (name "audit");
      }

  let users t = t.users
  let identities t = t.identities
  let challenges t = t.challenges
  let passkeys t = t.passkeys
  let orgs t = t.orgs
  let mfa t = t.mfa
  let scim t = t.scim
  let audit t = t.audit
  let ensure_indexes t = t.ensure_indexes ()
end

let native_secret () =
  match Sys.getenv_opt "FENNEC_ACCOUNTS_SECRET" with
  | Some secret when String.length secret >= 16 -> secret
  | Some _ -> invalid_arg "FENNEC_ACCOUNTS_SECRET must be at least 16 bytes"
  | None -> "fennec-ephemeral-accounts-" ^ b64e (secure_random 24)

let native_store () =
  match Mongo_runtime.state () with
  | Missing -> Store.unavailable ()
  | Memory -> Store.minimongo ()
  | Mongo { uri; db = db_name } ->
    let client = Fennec_mongo_driver.Client.connect ~uri () in
    let db = Fennec_mongo_driver.Database.create client db_name in
    let store = Store.mongo db in
    Store.ensure_indexes store;
    store

let make_native () =
  make ~secret:(native_secret ()) ~store:(native_store ()) ~password_hasher:(password_hasher ()) ()

let current () =
  match Atomic.get native with
  | Some t -> t
  | None ->
    let t = make_native () in
    if Atomic.compare_and_set native None (Some t) then t else Option.get (Atomic.get native)

let native_paw () : Paw.t =
 fun c -> paw (current ()) () c

(* ---- inline tests ---- *)

let test_hasher =
  Password.
    {
    hash = (fun ~password -> "test$" ^ password);
    verify = (fun ~password ~hash -> hash = "test$" ^ password);
    }

let test_accounts () = make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher ()

let test_verified_email_key raw =
  match Identity.email ~verified:true raw with
  | Ok key -> key
  | Error e -> failwith (Identity.string_of_error e)

let test_active_totp user_id =
  match
    Mfa.enrollment ~now:(fun () -> 10.) ~status:Mfa.Active ~id:("mfa-" ^ user_id) ~user_id
      ~factor:Mfa.Totp ~secret:"SECRET" ~confirmed_at:10. ()
  with
  | Ok enrollment -> enrollment
  | Error e -> failwith (Mfa.string_of_error e)

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

let%test "memory_store update rejects duplicate usernames" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match (create_user a ~username:"ada" (), create_user a ~username:"bob" ()) with
  | Ok _, Ok bob -> (
    match store.users.update_user { bob with username = Some "ADA" } with Error (Duplicate_username "ada") -> true | _ -> false)
  | _ -> false

let%test "memory_store update rejects duplicate services on the same user" =
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" () with
  | Error _ -> false
  | Ok u -> (
    match
      store.users.update_user
        { u with services = [ ("github", Bson.doc [ ("id", Bson.str "1") ]); ("github", Bson.doc [ ("id", Bson.str "2") ]) ] }
    with
    | Error (Invalid_user "Duplicate service: github") -> true
    | _ -> false)

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
      logout_other_clients a u.id = Ok ()
      && verify_token a token = Ok u.id
      && login_with_token a token = Error Invalid_token)

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

let identity_ok = function Ok x -> x | Error e -> failwith (Identity.string_of_error e)

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

let%test "Store.minimongo supports password login and epoch revocation" =
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"Ada" ~email:"ADA@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match login_with_password a (By_email "ada@example.com") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      Store.ensure_indexes store;
      logout_other_clients a user.id = Ok ()
      && verify_token a token = Error Invalid_token
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw"))

let%test "Store.unavailable lets the framework boot but fails account writes clearly" =
  let store = Store.unavailable ~message:"no mongo configured" () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~email:"ada@example.com" () with
  | Error (Store_error msg) -> msg = "no mongo configured"
  | _ -> false

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

let test_passkey_credential ?(id = "cred-1") user_id =
  Mirage_crypto_rng_unix.use_default ();
  {
    Passkey.id;
    user_id;
    user_handle = "handle-" ^ user_id;
    public_key = X509.Private_key.public (X509.Private_key.generate `P256);
    sign_count = 1l;
    backup_eligible = false;
    backed_up = false;
    transports = [ "internal" ];
    created_at = 1_000.;
    last_used_at = None;
  }

let same_passkey_credential (a : Passkey.credential) (b : Passkey.credential) =
  a.id = b.id && a.user_id = b.user_id && a.user_handle = b.user_handle
  && X509.Public_key.encode_pem a.public_key = X509.Public_key.encode_pem b.public_key
  && a.sign_count = b.sign_count
  && a.backup_eligible = b.backup_eligible
  && a.backed_up = b.backed_up
  && a.transports = b.transports
  && a.created_at = b.created_at
  && a.last_used_at = b.last_used_at

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

let%test "Store.minimongo challenge facet is single-use" =
  let store = Store.minimongo () in
  let challenges =
    Challenge.make ~secret:"accounts-challenge-secret" ~store:(Store.challenges store) ~ttl:60. ()
  in
  match Challenge.create challenges ~purpose:Challenge.Email_login () with
  | Error _ -> false
  | Ok issued ->
    Result.is_ok (Challenge.consume challenges ~purpose:Challenge.Email_login issued.token)
    && Challenge.consume challenges ~purpose:Challenge.Email_login issued.token = Error Challenge.Already_consumed

let%test "Store.minimongo audit facet appends and filters events" =
  let store = Store.minimongo () in
  let audit = Store.audit store in
  let a = Audit.event ~id:"evt-1" ~at:1. ~target_user_id:"u1" ~org_id:"org" Audit.Login Audit.Anonymous Audit.Success in
  let b = Audit.event ~id:"evt-2" ~at:2. ~target_user_id:"u2" ~org_id:"org" Audit.Logout Audit.Anonymous Audit.Success in
  Audit.append audit a = Ok ()
  && Audit.append audit b = Ok ()
  && Result.is_error (Audit.append audit a)
  && Audit.list ~target_user_id:"u1" audit = [ a ]
  && Audit.list ~org_id:"org" audit = [ a; b ]
  && Audit.list ~kind:Audit.Logout audit = [ b ]

let%test "Store.minimongo org facet persists org membership and invite documents" =
  let store = Store.minimongo () in
  let orgs = Store.orgs store in
  let domain = match Org.domain ~verified:true "example.com" with Ok d -> d | Error _ -> assert false in
  let org = match Org.org ~id:"acme" ~name:"Acme" ~domains:[ domain ] () with Ok o -> o | Error _ -> assert false in
  let membership =
    match Org.membership ~now:(fun () -> 10.) ~org_id:"acme" ~user_id:"u1" ~role:"admin" () with
    | Ok membership -> membership
    | Error _ -> assert false
  in
  let invite =
    match
      Org.invite ~now:(fun () -> 20.) ~id:"inv1" ~org_id:"acme" ~email:"Ada@Example.com" ~role:"member"
        ~token_hash:"hash" ()
    with
    | Ok invite -> invite
    | Error _ -> assert false
  in
  orgs.upsert_org org = Ok ()
  && orgs.upsert_membership membership = Ok ()
  && orgs.upsert_invite invite = Ok ()
  && orgs.find_org "acme" = Some org
  && orgs.find_membership ~org_id:"acme" ~user_id:"u1" = Some membership
  && orgs.list_invites ~email:"ada@example.com" () = [ invite ]

let%test "Store.minimongo mfa facet persists enrollments" =
  let store = Store.minimongo () in
  let mfa = Store.mfa store in
  let enrollment =
    match
      Mfa.enrollment ~now:(fun () -> 10.) ~status:Mfa.Active ~id:"mfa1" ~user_id:"u1" ~factor:Mfa.Totp
        ~secret:"SECRET" ~last_step:1L ~confirmed_at:11. ()
    with
    | Ok enrollment -> enrollment
    | Error _ -> assert false
  in
  let next = { enrollment with Mfa.last_step = Some 2L } in
  let stale = { enrollment with Mfa.last_step = Some 3L } in
  mfa.upsert enrollment = Ok ()
  && mfa.find "mfa1" = Some enrollment
  && mfa.replace_if_current ~current:enrollment next = Ok true
  && mfa.replace_if_current ~current:enrollment stale = Ok false
  && mfa.find "mfa1" = Some next
  && mfa.list ~user_id:"u1" ~factor:Mfa.Totp () = [ next ]

let%test "Store.minimongo scim facet persists connection users and groups" =
  let store = Store.minimongo () in
  let scim = Store.scim store in
  let connection =
    match Scim.connection ~id:"corp" ~org_id:"acme" ~bearer_token:"very-secret-scim-token" () with
    | Ok connection -> connection
    | Error _ -> assert false
  in
  let user =
    match Scim.user ~external_id:"u1" ~user_name:"ada" ~emails:[ "Ada@example.com" ] () with
    | Ok user -> user
    | Error _ -> assert false
  in
  let group =
    match Scim.group ~external_id:"g1" ~display_name:"Admins" ~members:[ "u1" ] () with
    | Ok group -> group
    | Error _ -> assert false
  in
  scim.upsert_connection connection = Ok ()
  && scim.upsert_user ~connection_id:connection.id user = Ok ()
  && scim.upsert_group ~connection_id:connection.id group = Ok ()
  && scim.find_connection "corp" = Some connection
  && scim.find_user ~connection_id:"corp" ~external_id:"u1" = Some user
  && scim.find_group ~connection_id:"corp" ~external_id:"g1" = Some group

let test_email_helper () =
  let challenge =
    Challenge.make ~secret:"accounts-email-challenge-secret" ~store:(Challenge.memory_store ()) ()
  in
  Email.make ~secret:"accounts-email-helper-secret" ~challenge

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

let req_ ?(headers = []) path = H.make_request ~meth:H.GET ~path ~headers ()
let post_form_ path body =
  H.make_request ~meth:H.POST ~path ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body ()

let finalize_ c = Conn.apply_before_send c (Option.get (Conn.resp c))
let cookie_kv_ set_cookie =
  match String.index_opt set_cookie ';' with Some i -> String.sub set_cookie 0 i | None -> set_cookie

let location_ r = Fennec_core.Headers.get r.H.headers "location"

let url_path_ url =
  match String.index_opt url '?' with
  | None -> url
  | Some i -> String.sub url 0 i

let url_query_ url =
  match String.index_opt url '?' with
  | None -> []
  | Some i -> H.parse_query (String.sub url (i + 1) (String.length url - i - 1))

let mfa_redirect_ok_ a user_id r =
  match location_ r with
  | None -> false
  | Some url -> (
    let params = url_query_ url in
    url_path_ url = "/mfa"
    && List.assoc_opt "userId" params = Some user_id
    &&
    match List.assoc_opt "mfaToken" params with
    | None -> false
    | Some token ->
      Result.is_ok (Mfa.consume_step_up (mfa_service a) ~expected_user:user_id (Challenge.token_of_string token)))

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
      let set_cookie = match Fennec_core.Headers.get_all (finalize_ c1).H.headers "set-cookie" with s :: _ -> s | [] -> "" in
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
      && Fennec_core.Headers.get r.H.headers "cache-control" = Some "no-store"
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
      && Fennec_core.Headers.mem r.H.headers "set-cookie"
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
      && not (Fennec_core.Headers.mem r.H.headers "set-cookie"))

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
      && Fennec_core.Headers.mem r.H.headers "set-cookie"
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
      && Fennec_core.Headers.mem r.H.headers "set-cookie"
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
      && not (Fennec_core.Headers.mem r.H.headers "set-cookie"))

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
    && Fennec_core.Headers.mem consumed.H.headers "set-cookie"

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
      && not (Fennec_core.Headers.mem r.H.headers "set-cookie"))

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
    && Fennec_core.Headers.mem consumed.H.headers "set-cookie"

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
	      && not (Fennec_core.Headers.mem r.H.headers "set-cookie"))

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
          (match Fennec_core.Headers.get_all r.H.headers "set-cookie" with
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
        && Fennec_core.Headers.mem r.H.headers "set-cookie"
        && consume_backup_code a user.id ~code = Error (Login_rejected "Incorrect MFA code")
      | _ -> false))

let passkey_rp_ () =
  match Passkey.relying_party ~id:"app.test" ~name:"App" ~origins:[ "https://app.test" ] () with
  | Ok rp -> rp
  | Error e -> failwith (Passkey.string_of_error e)

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
      && Fennec_core.Headers.get r.H.headers "cache-control" = Some "no-store"
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

let query_param_ url name =
  match String.index_opt url '?' with
  | None -> None
  | Some i ->
    let query = String.sub url (i + 1) (String.length url - i - 1) in
    List.assoc_opt name (H.parse_query query)

let oauth_provider_ () =
  match
    OAuth.provider ~name:"github" ~authorize_url:"https://github.test/authorize" ~client_id:"client"
      ~redirect_uri:"https://app.test/oauth/callback" ()
  with
  | Ok provider -> provider
  | Error e -> failwith (OAuth.string_of_error e)

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
    r.H.status = 302 && location_ r = Some "/" && Fennec_core.Headers.mem r.H.headers "set-cookie"

let oidc_connection_ () =
  match
    Oidc.connection ~id:"main" ~issuer:"https://idp.test" ~authorize_url:"https://idp.test/auth"
      ~client_id:"client" ~redirect_uri:"https://app.test/oidc/callback" ~allow_jit:true ()
  with
  | Ok connection -> connection
  | Error e -> failwith (Oidc.string_of_error e)

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
    r.H.status = 302 && location_ r = Some "/" && Fennec_core.Headers.mem r.H.headers "set-cookie"

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
  let allowed = require_org ~permission:"write" () (set_org_context (Conn.make (req_ "/admin")) ~membership:admin org) in
  let blocked = require_org ~permission:"write" () (set_org_context (Conn.make (req_ "/admin")) ~membership:member org) in
  Conn.resp allowed = None && match Conn.resp blocked with Some r -> r.H.status = 403 | None -> false

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
  let a = test_accounts () in
  let admin_access = Roles.Permission.v_exn "admin.access" in
  let policy = Roles.policy [ Roles.role Roles.Role.admin [ admin_access ] ] in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, token) ->
      let authenticated = Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/admin") |> paw a () in
      let denied = require_permission a ~policy admin_access () authenticated in
      let anonymous = require_role a Roles.Role.admin () (Conn.make (req_ "/admin")) in
      let allowed =
        match grant_role a user.id Roles.Role.admin with
        | Error _ -> denied
        | Ok _ ->
          Conn.make (req_ ~headers:[ ("Cookie", a.cookie ^ "=" ^ token) ] "/admin")
          |> paw a ()
          |> require_permission a ~policy admin_access ()
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

module Test_methods_runtime = struct
  type doc = Bson.t
  type invocation = { user_id : string option; is_simulation : bool; set_user_id : string option -> unit }
  exception Error of { code : string; reason : string }
  let registered : (string * (invocation -> doc list -> doc)) list ref = ref []
  let methods xs = registered := xs @ !registered
end

module Test_methods = Methods (Test_methods_runtime)

let find_registered_ name =
  match List.assoc_opt name !(Test_methods_runtime.registered) with
  | Some f -> f
  | None -> failwith ("missing registered method: " ^ name)

let%test "methods: login rebinds the invocation user_id and returns a token" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  let _ = create_user a ~username:"ada" ~password:"pw" () in
  Test_methods.register a;
  let rebound = ref None in
  let inv = { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
  match find_registered_ "login" inv [ Bson.str "ada"; Bson.str "pw" ] with
  | Bson.Document kvs ->
    let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String a), Some b -> a = b | _ -> false in
    let token_ok =
      match List.assoc_opt "token" kvs with Some (Bson.String tok) -> Result.is_ok (verify_token a tok) | _ -> false
    in
	    id_ok && token_ok
	  | _ -> false

let%test "methods: currentUser returns the canonical safe session payload" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    Test_methods.register a;
    let inv =
      { Test_methods_runtime.user_id = Some u.id; is_simulation = false; set_user_id = (fun _ -> ()) }
    in
    (match find_registered_ "currentUser" inv [] with
    | Bson.Document _ as doc -> (
      match (Bson.get_string doc "userId", Bson.get doc "user", Bson.get doc "authContext") with
      | Some uid, Some (Bson.Document user_fields), Some Bson.Null ->
        uid = u.id && List.assoc_opt "id" user_fields = Some (Bson.String u.id)
      | _ -> false)
    | _ -> false
    | exception Test_methods_runtime.Error _ -> false)

let%test "methods: login returns MFA step-up instead of rebinding when active MFA exists" =
  Test_methods_runtime.registered := [];
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    Test_methods.register a;
    let rebound = ref None in
    let inv = { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) } in
    (match find_registered_ "login" inv [ Bson.str "ada"; Bson.str "pw" ] with
    | Bson.Document kvs ->
      !rebound = None
      && List.assoc_opt "mfaRequired" kvs = Some (Bson.Bool true)
      && List.assoc_opt "userId" kvs = Some (Bson.String u.id)
      &&
      (match List.assoc_opt "mfaToken" kvs with
      | Some (Bson.String token) ->
        Result.is_ok
          (Mfa.consume_step_up (mfa_service a) ~expected_user:u.id (Challenge.token_of_string token))
	    | _ -> false)
    | _ -> false
    | exception Test_methods_runtime.Error _ -> false)

let%test "methods: completeLoginStepUp completes TOTP and rebinds the invocation" =
  Test_methods_runtime.registered := [];
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
      | Ok active ->
        Test_methods.register a;
        let rebound = ref None in
        let inv = { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) } in
        (match find_registered_ "login" inv [ Bson.str "ada"; Bson.str "pw" ] with
        | Bson.Document kvs -> (
          match List.assoc_opt "mfaToken" kvs with
          | Some (Bson.String mfa_token) -> (
            let code = Mfa.totp_code setup.totp in
            match
              find_registered_ "completeLoginStepUp" inv
                [
                  Bson.doc
                    [
                      ("mfaToken", Bson.str mfa_token);
                      ("totpId", Bson.str active.id);
                      ("code", Bson.str code);
                    ];
                ]
            with
            | Bson.Document session ->
              !rebound = Some user.id
              &&
              (match List.assoc_opt "token" session with
              | Some (Bson.String token) -> Result.is_ok (login_with_token a token)
              | _ -> false)
            | _ -> false
            | exception Test_methods_runtime.Error _ -> false)
          | _ -> false)
        | _ -> false
        | exception Test_methods_runtime.Error _ -> false)))

let%test "methods: createUser rebinds the invocation user_id and returns a resumable token" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let rebound = ref None in
  let inv = { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
  match
    find_registered_ "createUser" inv
      [ Bson.doc [ ("username", Bson.str "ada"); ("password", Bson.str "pw") ] ]
  with
  | Bson.Document kvs ->
    let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String a), Some b -> a = b | _ -> false in
    let token_ok =
      match List.assoc_opt "token" kvs with Some (Bson.String tok) -> Result.is_ok (login_with_token a tok) | _ -> false
    in
    let user_ok = match List.assoc_opt "user" kvs with Some (Bson.Document _) -> true | _ -> false in
    id_ok && token_ok && user_ok
  | _ -> false

let%test "methods: createUser requires a password" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let inv =
    { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  match find_registered_ "createUser" inv [ Bson.doc [ ("username", Bson.str "ada") ] ] with
  | _ -> false
  | exception Test_methods_runtime.Error { code = "400"; reason = "createUser expects a password" } -> true
  | exception Test_methods_runtime.Error _ -> false

let%test "methods: login can resume an explicit token and return a replacement" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  let token =
    match create_user a ~username:"ada" ~password:"pw" () with
    | Error _ -> ""
    | Ok _ -> (
      match login_with_password a (By_username "ada") ~password:"pw" with Ok (_, token) -> token | Error _ -> "")
  in
  Test_methods.register a;
  let rebound = ref None in
  let inv = { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
  match find_registered_ "login" inv [ Bson.doc [ ("resume", Bson.str token) ] ] with
  | Bson.Document kvs ->
    let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String a), Some b -> a = b | _ -> false in
    let token_ok =
      match List.assoc_opt "token" kvs with Some (Bson.String tok) -> Result.is_ok (login_with_token a tok) | _ -> false
    in
    id_ok && token_ok
  | _ -> false

let%test "methods: malformed login selectors are bad requests" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let inv =
    { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  match find_registered_ "login" inv [ Bson.doc []; Bson.str "pw" ] with
  | _ -> false
  | exception Test_methods_runtime.Error { code = "400"; reason = "login selector expects id, email, or username" } -> true
  | exception Test_methods_runtime.Error _ -> false

let%test "methods: changePassword requires login and changes the password" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"old" () with
  | Error _ -> false
  | Ok u ->
    Test_methods.register a;
    let inv =
      { Test_methods_runtime.user_id = Some u.id; is_simulation = false; set_user_id = (fun _ -> ()) }
    in
    (match find_registered_ "changePassword" inv [ Bson.str "old"; Bson.str "new" ] with
    | Bson.Bool true -> true
    | _ -> false)
    && login_with_password a (By_username "ada") ~password:"old" = Error Invalid_password
    && Result.is_ok (login_with_password a (By_username "ada") ~password:"new")

let%test "methods: resetPassword consumes a reset token and rebinds user_id" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"old" () with
  | Error _ -> false
  | Ok u -> (
    match issue_password_reset a "ada@example.com" with
    | Error _ | Ok None -> false
    | Ok (Some reset) ->
      Test_methods.register a;
      let rebound = ref None in
      let inv =
        { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
      in
      (match
         find_registered_ "resetPassword" inv
           [ Bson.str (Challenge.token_to_string reset.token); Bson.str "new" ]
       with
      | Bson.Document kvs ->
        (match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String id), Some uid -> id = uid && uid = u.id | _ -> false)
      | _ -> false)
      && login_with_password a (By_username "ada") ~password:"old" = Error Invalid_password
      && Result.is_ok (login_with_password a (By_username "ada") ~password:"new"))

let%test "methods: verifyEmail marks verified email and rebinds user_id" =
  Test_methods_runtime.registered := [];
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match issue_email_verification a u.id "ada@example.com" with
    | Error _ -> false
    | Ok issued ->
      Test_methods.register a;
      let rebound = ref None in
      let inv =
        { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
      in
      match find_registered_ "verifyEmail" inv [ Bson.str (Challenge.token_to_string issued.token) ] with
      | Bson.Document kvs ->
        let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String id), Some uid -> id = uid && uid = u.id | _ -> false in
        let user_ok =
          match store.users.find_user_by_id u.id with
          | Ok (Some user) -> List.exists (fun e -> e.address = "ada@example.com" && e.verified) user.emails
          | _ -> false
        in
        id_ok && user_ok
      | _ -> false)

let%test "methods: logoutOtherClients returns a replacement token for the current connection" =
  Test_methods_runtime.registered := [];
  let store = memory_store () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher ~validate_every_request:true () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    match login_with_password a (By_username "ada") ~password:"pw" with
    | Error _ -> false
    | Ok (_, old_token) ->
      Test_methods.register a;
      let rebound = ref None in
      let inv =
        { Test_methods_runtime.user_id = Some u.id; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
      in
      match find_registered_ "logoutOtherClients" inv [] with
      | Bson.Document kvs ->
        let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String id), Some uid -> id = uid && uid = u.id | _ -> false in
        let token_ok =
          match List.assoc_opt "token" kvs with
          | Some (Bson.String tok) ->
            verify_token a old_token = Error Invalid_token && verify_token a (token_of_string tok) = Ok u.id
          | _ -> false
        in
        id_ok && token_ok
      | _ -> false)

let%test "methods: enrollAccount sets the first password and rebinds user_id" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~email:"ada@example.com" () with
  | Error _ -> false
  | Ok u -> (
    match issue_enrollment a u.id with
    | Error _ -> false
    | Ok enrollment ->
      Test_methods.register a;
      let rebound = ref None in
      let inv =
        { Test_methods_runtime.user_id = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
      in
      match
        find_registered_ "enrollAccount" inv
          [ Bson.str (Challenge.token_to_string enrollment.token); Bson.str "pw" ]
      with
      | Bson.Document kvs ->
        let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String id), Some uid -> id = uid && uid = u.id | _ -> false in
        id_ok && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw")
      | _ -> false)

let%test "methods: logout clears the invocation user_id" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let rebound = ref (Some "ada") in
  let inv = { Test_methods_runtime.user_id = Some "ada"; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
  match find_registered_ "logout" inv [] with Bson.Bool true -> !rebound = None | _ -> false
