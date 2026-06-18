(* accounts_methods.ml — the DDP method surface. The [Methods] functor builds the wire handlers
   (login / createUser / logout / resetPassword / verifyEmail / enrollAccount / completeLoginStepUp /
   forgotPassword / requestLoginToken / changePassword / logoutOtherClients / currentUser) over the
   engine, including the brute-force throttle, the enumeration guard, and the password/MFA step-up
   paths — all moved VERBATIM from accounts.ml. [fennec.pulse.server] instantiates
   [Accounts.Methods(R)]; the facade re-exports this functor at that same path with a byte-identical
   [R] parameter signature, so the server compiles untouched.

   The small DDP-shaped (de)serialization helpers the handlers need — [selector_of_bson],
   [selector_rate_key], [user_doc], [ddp_session_doc] (and the trivial [opt_bson]) — live here too:
   they are method support, distinct from the richer HTTP [session_doc] / [public_user_to_doc] that
   stay in the facade.

   The inline [let%test] blocks exercising these handlers ride along (the house rule): they drive the
   functor through [Test_methods = Methods(Test_methods_runtime)] and so are colocated with it. The
   shared in-memory fixture they build on ([test_accounts], [Store.minimongo], [test_active_totp], …)
   lives one layer down in {!Accounts_native}, which this module opens. *)

open Accounts_base
open Accounts_native

let opt_bson = function None -> Bson.null | Some value -> value

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

(* a stable per-account rate-limit key from a login selector. Normalized so the same account hashes
   identically whether addressed by raw or mixed-case email/username, and namespaced so an id, an
   email and a username can never collide. Derived WITHOUT touching the store, so the throttle keys
   on what the client asked for — not on whether that account exists (no enumeration oracle). *)
let selector_rate_key = function
  | By_id id -> "id:" ^ id
  | By_email e -> "email:" ^ normalize_email e
  | By_username u -> "username:" ^ normalize_username u

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
  type invocation = { user_id : string option; remote_ip : string option; is_simulation : bool; set_user_id : string option -> unit }
  exception Error of { code : string; reason : string }
  val methods : (string * (invocation -> doc list -> doc)) list -> unit
end) =
struct
  type invocation = R.invocation

  let bad_request reason = raise (R.Error { code = "400"; reason })
  let forbidden reason = raise (R.Error { code = "403"; reason })

  (* a brute-force throttle hit: DDP error code ["429"] (Meteor's 'too-many-requests'), with the
     retry window in the reason so a client can back off intelligently. Deliberately uniform whether
     or not the account exists — it pairs with the enumeration timing defense, not against it. *)
  let too_many retry =
    raise
      (R.Error
         {
           code = "429";
           reason =
             Printf.sprintf "Too many attempts. Please try again in %d second%s." retry
               (if retry = 1 then "" else "s");
         })

  let register t =
    (* charge one attempt before doing any store work, so a throttled caller costs nothing past the
       bucket check; keys on the client IP (from the invocation) and the normalized selector *)
    let throttle_login inv selector =
      match Throttle.login_allowed t.rate_limit ~ip:inv.R.remote_ip ~account:(selector_rate_key selector) with
      | Ok () -> ()
      | Error retry -> too_many retry
    in
    let throttle_create_user inv =
      match Throttle.create_user_allowed t.rate_limit ~ip:inv.R.remote_ip with
      | Ok () -> ()
      | Error retry -> too_many retry
    in
    let throttle_step_up inv token =
      (* charged BEFORE verifying the second factor — keyed on the step-up token id (+ the per-IP login
         bucket) so a fixed mfaToken can't be used to online-guess the 6-digit TOTP / backup code. The
         token TTL already bounds the window; this bounds the rate. *)
      let id = match String.index_opt token '.' with Some i -> String.sub token 0 i | None -> token in
      match Throttle.login_allowed t.rate_limit ~ip:inv.R.remote_ip ~account:("step-up:" ^ id) with
      | Ok () -> ()
      | Error retry -> too_many retry
    in
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
      | [ Bson.Document _ ] when t.config.forbid_client_account_creation ->
        forbidden "client account creation is disabled"
      | [ Bson.Document _ as d ] ->
        throttle_create_user inv;
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
              (* Meteor's Accounts.config({sendVerificationEmail}) — best-effort, never blocks signup *)
              if t.config.auto_send_verification_email then ignore (send_verification_email t u.id);
              Bson.doc [ ("id", Bson.str u.id); ("token", Bson.str token); ("user", user_doc u) ])))
      | _ -> bad_request "createUser expects one document argument"
    in
    let login_method inv = function
      | [ selector; Bson.String password ] -> (
        match selector_of_bson selector with
        | Error reason -> bad_request reason
        | Ok selector ->
          throttle_login inv selector;
          login_completion_doc inv (login_with_password_completion t selector ~password))
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
              | Ok selector ->
                throttle_login inv selector;
                login_completion_doc inv (login_with_password_completion t selector ~password))
            | _ -> (
              match (doc_get_string d "token", doc_get_string d "code") with
              | Some token, Some code ->
                (* passwordless sign-in: consume the OTP token+code → session. Throttled by IP + the OTP
                   token id like the OTP paw (S2). Step-up via passwordless isn't carried over DDP here
                   (errors); such users complete via the HTTP paw. *)
                let id = match String.index_opt token '.' with Some i -> String.sub token 0 i | None -> token in
                ( match Throttle.login_allowed t.rate_limit ~ip:inv.R.remote_ip ~account:("otp:" ^ id) with
                | Error retry -> too_many retry
                | Ok () -> (
                  match
                    login_with_email_otp t (email_service t ()) ~allow_signup:true
                      ~token:(Challenge.token_of_string token) ~code ()
                  with
                  | Error e -> forbidden (string_of_error e)
                  | Ok login ->
                    inv.R.set_user_id (Some login.user.id);
                    Bson.doc [ ("id", Bson.str login.user.id); ("token", Bson.str login.token) ] ) )
              | _ -> (
                match Bson.get d "oauth" with
                | Some (Bson.Document _ as o) -> (
                  match (doc_get_string o "credentialToken", doc_get_string o "credentialSecret") with
                  | Some credential_token, Some credential_secret -> (
                    (* OAuth-over-DDP popup handshake: replay the single-use credential issued by the
                       popup callback, then mint the session here (MFA step-up still applies). *)
                    match consume_oauth_credential t ~credential_token ~credential_secret with
                    | Error e -> forbidden (string_of_error e)
                    | Ok (user_id, provider) -> (
                      match find_required_user t user_id with
                      | Error e -> forbidden (string_of_error e)
                      | Ok user ->
                        let strategy = match provider with Some p -> "oauth:" ^ p | None -> "oauth" in
                        login_completion_doc inv (complete_login_unless_mfa t ~strategy user)))
                  | _ -> bad_request "login {oauth} expects {credentialToken, credentialSecret}")
                | _ ->
                  bad_request
                    "login expects {user, password}, {strategy, credentials}, {token, code}, {oauth}, or {resume}" ) ))
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
        | Some mfa_token ->
          throttle_step_up inv mfa_token;
          ( match (doc_get_string d "totpId", doc_get_string d "code", doc_get_string d "userId", doc_get_string d "backupCode") with
          | Some totp_id, Some code, _, _ ->
            complete_step_up_doc inv mfa_token (verify_totp_factor t totp_id ~code)
          | _, _, Some user_id, Some code ->
            complete_step_up_doc inv mfa_token (consume_backup_code t user_id ~code)
          | _ -> bad_request "completeLoginStepUp expects {mfaToken, totpId, code} or {mfaToken, userId, backupCode}" ))
      | _ -> bad_request "completeLoginStepUp expects one document argument"
    in
    let forgot_password_method inv = function
      | [ Bson.Document _ as d ] -> (
        match doc_get_string d "email" with
        | None -> bad_request "forgotPassword expects an email"
        | Some email ->
          (* throttle reset requests (IP + email) so the method can't be used to spam a mailbox;
             non-enumerating + best-effort — always returns ok, whether or not the address has an account *)
          throttle_login inv (By_email email);
          ignore (send_reset_password_email t email);
          Bson.doc [ ("ok", Bson.Bool true) ])
      | _ -> bad_request "forgotPassword expects one document argument"
    in
    let request_login_token_method inv = function
      | [ Bson.Document _ as d ] -> (
        match doc_get_string d "email" with
        | None -> bad_request "requestLoginToken expects an email"
        | Some email ->
          (* throttle (IP + email) against mailbox spam; emails a one-time code and returns the token to
             pair with it. Naturally non-enumerating (passwordless permits signup). *)
          throttle_login inv (By_email email);
          ( match send_login_token_email t email with
          | Ok token -> Bson.doc [ ("token", Bson.str token) ]
          | Error e -> forbidden e ))
      | _ -> bad_request "requestLoginToken expects one document argument"
    in
    (* the config method gate: register only the methods [Wiring.method_enabled] approves for [t].
       Today that predicate is constant-true (the umbrella config carries no method allow-list yet),
       so the full Meteor-shaped method set is installed exactly as before — the seam is here for a
       later narrowing pass without touching the functor parameter [R]. *)
    R.methods
      (List.filter
         (fun (name, _) -> Wiring.method_enabled t name)
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
           ("forgotPassword", forgot_password_method);
           ("requestLoginToken", request_login_token_method);
         ])
end


(* ---- inline tests: the DDP method handlers (driven through Test_methods) ---- *)

module Test_methods_runtime = struct
  type doc = Bson.t
  type invocation = { user_id : string option; remote_ip : string option; is_simulation : bool; set_user_id : string option -> unit }
  exception Error of { code : string; reason : string }
  let registered : (string * (invocation -> doc list -> doc)) list ref = ref []
  let methods xs = registered := xs @ !registered
end

module Test_methods = Methods (Test_methods_runtime)

let find_registered_ name =
  match List.assoc_opt name !(Test_methods_runtime.registered) with
  | Some f -> f
  | None -> failwith ("missing registered method: " ^ name)

let%test "methods: login {oauth} consumes a popup credential and mints a session" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u -> (
    Test_methods.register a;
    let rebound = ref None in
    let inv =
      { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
    in
    match issue_oauth_credential a ~user_id:u.id ~provider:"github" () with
    | Error _ -> false
    | Ok (ct, cs) -> (
      let arg = Bson.doc [ ("oauth", Bson.doc [ ("credentialToken", Bson.str ct); ("credentialSecret", Bson.str cs) ]) ] in
      match find_registered_ "login" inv [ arg ] with
      | Bson.Document kvs ->
        let id_ok =
          match (List.assoc_opt "id" kvs, !rebound) with
          | Some (Bson.String id), Some uid -> id = uid && uid = u.id
          | _ -> false
        in
        let token_ok =
          match List.assoc_opt "token" kvs with
          | Some (Bson.String tok) -> verify_token a (token_of_string tok) = Ok u.id
          | _ -> false
        in
        id_ok && token_ok
      | _ -> false))

let%test "methods: login {oauth} rejects an unknown or replayed credential" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    Test_methods.register a;
    let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) } in
    let rejected ct cs =
      let arg = Bson.doc [ ("oauth", Bson.doc [ ("credentialToken", Bson.str ct); ("credentialSecret", Bson.str cs) ]) ] in
      match find_registered_ "login" inv [ arg ] with
      | _ -> false
      | exception Test_methods_runtime.Error { code = "403"; _ } -> true
      | exception _ -> false
    in
    let unknown = rejected "no-such-token" "no-such-secret" in
    let replayed =
      match issue_oauth_credential a ~user_id:u.id () with
      | Ok (ct, cs) ->
        ignore (consume_oauth_credential a ~credential_token:ct ~credential_secret:cs);
        rejected ct cs
      | Error _ -> false
    in
    unknown && replayed

let%test "methods: login {oauth} still demands MFA step-up for an mfa-enabled user" =
  Test_methods_runtime.registered := [];
  let store = memory_store () in
  let a = make ~secret:"accounts-test-oauth-mfa" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok u ->
    ignore (store.mfa.Mfa.upsert (test_active_totp u.id));
    Test_methods.register a;
    let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) } in
    ( match issue_oauth_credential a ~user_id:u.id ~provider:"github" () with
    | Error _ -> false
    | Ok (ct, cs) -> (
      let arg = Bson.doc [ ("oauth", Bson.doc [ ("credentialToken", Bson.str ct); ("credentialSecret", Bson.str cs) ]) ] in
      match find_registered_ "login" inv [ arg ] with
      | Bson.Document kvs -> List.assoc_opt "mfaRequired" kvs = Some (Bson.Bool true)
      | _ -> false ))

let%test "oauth popup callback hands the opener a credential that completes login over DDP" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let provider = oauth_provider_ () in
  let authorize = oauth_authorize_paw a provider ~path:"/oauth/start" ~error:"/oauth/error" () in
  let start = Paw.run authorize (req_ "/oauth/start") in
  match Option.bind (location_ start) (fun url -> query_param_ url "state") with
  | None -> false
  | Some state -> (
    let key = match Identity.oauth ~provider:"github" ~subject:"ada" with Ok key -> key | Error _ -> assert false in
    let callback =
      oauth_callback_popup_paw a provider ~path:"/oauth/popup"
        ~exchange:(fun (_state : OAuth.state) ~code ->
          if code = "ok" then Ok (external_identity key ~email:"ada@example.com" ~email_verified:true)
          else Error (Login_rejected "bad_code"))
        ()
    in
    let r =
      Paw.run callback
        (H.make_request ~meth:H.GET ~path:"/oauth/popup"
           ~query_string:("code=ok&state=" ^ H.percent_encode state) ())
    in
    (* the bounce page embeds [var d={...}] and posts it to the opener; pull the flat payload out *)
    let payload =
      let body = r.H.body in
      let find sub =
        let n = String.length body and m = String.length sub in
        let rec go i = if i + m > n then None else if String.sub body i m = sub then Some i else go (i + 1) in
        go 0
      in
      match find "var d=" with
      | None -> None
      | Some k -> (
        let i = k + String.length "var d=" in
        match String.index_from_opt body i '}' with
        | Some j -> Json.parse_opt (String.sub body i (j - i + 1))
        | None -> None)
    in
    match (r.H.status, payload) with
    | 200, Some (Json.Obj kvs) -> (
      let field key = match List.assoc_opt key kvs with Some (Json.String v) -> Some v | _ -> None in
      match (field "credentialToken", field "credentialSecret") with
      | Some ct, Some cs -> (
        let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) } in
        let arg = Bson.doc [ ("oauth", Bson.doc [ ("credentialToken", Bson.str ct); ("credentialSecret", Bson.str cs) ]) ] in
        match find_registered_ "login" inv [ arg ] with
        | Bson.Document login_kvs -> (
          match List.assoc_opt "token" login_kvs with
          | Some (Bson.String tok) -> Result.is_ok (verify_token a (token_of_string tok))
          | _ -> false)
        | _ -> false)
      | _ -> false)
    | _ -> false)

let%test "methods: login rebinds the invocation user_id and returns a token" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  let _ = create_user a ~username:"ada" ~password:"pw" () in
  Test_methods.register a;
  let rebound = ref None in
  let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
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
      { Test_methods_runtime.user_id = Some u.id; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) }
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
    let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) } in
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
        let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) } in
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

let%test "methods: completeLoginStepUp throttles brute-forced second-factor codes with a 429" =
  Test_methods_runtime.registered := [];
  let store = Store.minimongo () in
  let a = make ~secret:"accounts-test-secret" ~store ~password_hasher:test_hasher () in
  match create_user a ~username:"ada" ~password:"pw" () with
  | Error _ -> false
  | Ok user -> (
    match enroll_totp a user.id with
    | Error _ -> false
    | Ok setup -> (
      let code0 = Mfa.totp_code ~time:1_000. setup.totp in
      match confirm_totp_enrollment a setup.enrollment.id ~time:1_000. ~code:code0 with
      | Error _ -> false
      | Ok active ->
        Test_methods.register a;
        let inv = { Test_methods_runtime.user_id = None; remote_ip = Some "203.0.113.20"; is_simulation = false; set_user_id = (fun _ -> ()) } in
        (match find_registered_ "login" inv [ Bson.str "ada"; Bson.str "pw" ] with
        | Bson.Document kvs -> (
          match List.assoc_opt "mfaToken" kvs with
          | Some (Bson.String mfa_token) ->
            (* hammer a FIXED mfaToken with a wrong code; the throttle must cut in with a 429 before the
               6-digit space can be exhausted within the token's TTL — and must not block the first try *)
            let attempt () =
              try
                ignore
                  (find_registered_ "completeLoginStepUp" inv
                     [ Bson.doc [ ("mfaToken", Bson.str mfa_token); ("totpId", Bson.str active.id); ("code", Bson.str "000000") ] ]);
                "ok"
              with Test_methods_runtime.Error { code; _ } -> code
            in
            let codes = List.map (fun _ -> attempt ()) [ 1; 2; 3; 4; 5 ] in
            (match codes with first :: _ -> first <> "429" | [] -> false) && List.mem "429" codes
          | _ -> false)
        | _ -> false
        | exception Test_methods_runtime.Error _ -> false)))

let%test "methods: createUser rebinds the invocation user_id and returns a resumable token" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let rebound = ref None in
  let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
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
    { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  match find_registered_ "createUser" inv [ Bson.doc [ ("username", Bson.str "ada") ] ] with
  | _ -> false
  | exception Test_methods_runtime.Error { code = "400"; reason = "createUser expects a password" } -> true
  | exception Test_methods_runtime.Error _ -> false

let%test "methods: createUser is rejected (403) when config.forbid_client_account_creation is set" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  configure a { default_config with forbid_client_account_creation = true };
  Test_methods.register a;
  let inv =
    { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  (* the client signup method is refused; server-side [create_user] is unaffected *)
  match find_registered_ "createUser" inv [ Bson.doc [ ("username", Bson.str "ada"); ("password", Bson.str "pw") ] ] with
  | _ -> false
  | exception Test_methods_runtime.Error { code = "403"; _ } -> Result.is_ok (create_user a ~username:"ada" ~password:"pw" ())
  | exception Test_methods_runtime.Error _ -> false

(* a brute-force throttle wired with a frozen clock: the default 5/10 s limit refills 0 tokens, so
   exactly five attempts pass before the bucket is empty *)
let throttled_accounts () =
  make ~secret:"accounts-test-secret" ~store:(memory_store ()) ~password_hasher:test_hasher
    ~rate_limit:(Throttle.make ~now:(fun () -> 0.) ()) ()

let%test "methods: login throttles the sixth rapid attempt with a 429 (brute-force defense)" =
  Test_methods_runtime.registered := [];
  let a = throttled_accounts () in
  let _ = create_user a ~username:"ada" ~password:"pw" () in
  Test_methods.register a;
  let inv =
    { Test_methods_runtime.user_id = None; remote_ip = Some "203.0.113.7"; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  (* each wrong-password try is a normal 403 (Invalid_password) AND spends a token; the sixth is
     refused up front with 429 before the password is ever checked *)
  let code () =
    try ignore (find_registered_ "login" inv [ Bson.str "ada"; Bson.str "wrong" ]); "ok"
    with Test_methods_runtime.Error { code; _ } -> code
  in
  List.for_all (fun _ -> code () = "403") [ 1; 2; 3; 4; 5 ] && code () = "429"

let%test "methods: createUser throttles repeated signups from one IP with a 429" =
  Test_methods_runtime.registered := [];
  let a = throttled_accounts () in
  Test_methods.register a;
  let inv =
    { Test_methods_runtime.user_id = None; remote_ip = Some "203.0.113.9"; is_simulation = false; set_user_id = (fun _ -> ()) }
  in
  let signup n =
    try
      ignore
        (find_registered_ "createUser" inv
           [ Bson.doc [ ("username", Bson.str (Printf.sprintf "u%d" n)); ("password", Bson.str "pw") ] ]);
      "ok"
    with Test_methods_runtime.Error { code; _ } -> code
  in
  List.for_all (fun n -> signup n = "ok") [ 1; 2; 3; 4; 5 ] && signup 6 = "429"

let%test "methods: login throttle is keyed by IP and account, not global" =
  Test_methods_runtime.registered := [];
  let a = throttled_accounts () in
  let _ = create_user a ~username:"ada" ~password:"pw" () in
  let _ = create_user a ~username:"bob" ~password:"pw" () in
  Test_methods.register a;
  let code ip selector pw =
    let inv = { Test_methods_runtime.user_id = None; remote_ip = Some ip; is_simulation = false; set_user_id = (fun _ -> ()) } in
    try ignore (find_registered_ "login" inv [ Bson.str selector; Bson.str pw ]); "ok"
    with Test_methods_runtime.Error { code; _ } -> code
  in
  (* exhaust ada's budget from one address *)
  List.iter (fun _ -> ignore (code "198.51.100.1" "ada" "wrong")) [ 1; 2; 3; 4; 5 ];
  (* ada from that address is now blocked, but bob from another address logs in unimpeded *)
  code "198.51.100.1" "ada" "pw" = "429" && code "198.51.100.2" "bob" "pw" = "ok"

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
  let inv = { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
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
    { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) }
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
      { Test_methods_runtime.user_id = Some u.id; remote_ip = None; is_simulation = false; set_user_id = (fun _ -> ()) }
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
        { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
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
        { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
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
        { Test_methods_runtime.user_id = Some u.id; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
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
        { Test_methods_runtime.user_id = None; remote_ip = None; is_simulation = false; set_user_id = (fun uid -> rebound := uid) }
      in
      match
        find_registered_ "enrollAccount" inv
          [ Bson.str (Challenge.token_to_string enrollment.token); Bson.str "pw" ]
      with
      | Bson.Document kvs ->
        let id_ok = match (List.assoc_opt "id" kvs, !rebound) with Some (Bson.String id), Some uid -> id = uid && uid = u.id | _ -> false in
        id_ok && Result.is_ok (login_with_password a (By_username "ada") ~password:"pw")
      | _ -> false)

let%test "methods: forgotPassword sends a reset email and is non-enumerating (always ok)" =
  Test_methods_runtime.registered := [];
  let tr, sent = Fennec_mail.capture () in
  Fennec_mail.set_transport tr;
  let a = make ~secret:"accounts-test-secret-fp" ~store:(memory_store ()) ~password_hasher:test_hasher () in
  set_email_templates a (Mailer.default ~site_name:"Acme" ~from:(Fennec_mail.Address.v "no-reply@acme.test") ());
  let _ = create_user a ~username:"ada" ~email:"ada@example.com" ~password:"pw" () in
  Test_methods.register a;
  let inv = { Test_methods_runtime.user_id = None; remote_ip = Some "203.0.113.30"; is_simulation = false; set_user_id = (fun _ -> ()) } in
  let ok email =
    match find_registered_ "forgotPassword" inv [ Bson.doc [ ("email", Bson.str email) ] ] with
    | Bson.Document kvs -> List.assoc_opt "ok" kvs = Some (Bson.Bool true)
    | _ -> false
  in
  (* a known and an unknown address BOTH return ok (no enumeration); only the known one delivers mail *)
  ok "ada@example.com"
  && ok "nobody@example.com"
  && (match sent () with
     | [ m ] -> List.exists (fun (ad : Fennec_mail.Address.t) -> ad.email = "ada@example.com") m.Fennec_mail.to_
     | _ -> false)

let%test "methods: requestLoginToken emails a code; passwordless login over DDP completes the round-trip" =
  Test_methods_runtime.registered := [];
  let tr, sent = Fennec_mail.capture () in
  Fennec_mail.set_transport tr;
  let a = make ~secret:"accounts-test-secret-pwl" ~store:(Store.minimongo ()) ~password_hasher:test_hasher () in
  set_email_templates a (Mailer.default ~site_name:"Acme" ~from:(Fennec_mail.Address.v "no-reply@acme.test") ());
  Test_methods.register a;
  let inv = { Test_methods_runtime.user_id = None; remote_ip = Some "203.0.113.40"; is_simulation = false; set_user_id = (fun _ -> ()) } in
  let first_code s =
    let n = String.length s and is_d c = c >= '0' && c <= '9' in
    let rec go i =
      if i + 6 > n then None
      else if is_d s.[i] && is_d s.[i + 1] && is_d s.[i + 2] && is_d s.[i + 3] && is_d s.[i + 4] && is_d s.[i + 5] then
        Some (String.sub s i 6)
      else go (i + 1)
    in
    go 0
  in
  ( match find_registered_ "requestLoginToken" inv [ Bson.doc [ ("email", Bson.str "ada@example.com") ] ] with
  | Bson.Document kvs -> (
    match (List.assoc_opt "token" kvs, sent ()) with
    | Some (Bson.String token), [ m ] -> (
      match first_code (Option.value m.Fennec_mail.text ~default:"") with
      | None -> false
      | Some code -> (
        match find_registered_ "login" inv [ Bson.doc [ ("token", Bson.str token); ("code", Bson.str code) ] ] with
        | Bson.Document session -> (
          match List.assoc_opt "token" session with
          | Some (Bson.String t2) -> Result.is_ok (login_with_token a t2)
          | _ -> false)
        | _ -> false
        | exception Test_methods_runtime.Error _ -> false))
    | _ -> false)
  | _ -> false
  | exception Test_methods_runtime.Error _ -> false )

let%test "methods: logout clears the invocation user_id" =
  Test_methods_runtime.registered := [];
  let a = test_accounts () in
  Test_methods.register a;
  let rebound = ref (Some "ada") in
  let inv = { Test_methods_runtime.user_id = Some "ada"; remote_ip = None; is_simulation = false; set_user_id = (fun u -> rebound := u) } in
  match find_registered_ "logout" inv [] with Bson.Bool true -> !rebound = None | _ -> false
