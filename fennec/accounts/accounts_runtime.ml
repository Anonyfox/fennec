(* accounts_runtime.ml — the engine INSTANCE plumbing that both the lifecycle and login layers
   sit on, factored out to break their mutual recursion: [make]/[configure]/[native], the
   mailer-templates re-export, the registration + running of the hook families
   (validate_login/create_user/login/logout/login_failure/before_external_login), the audit
   sink (append_audit/record_audit), and the ONE canonical strategy->audit-kind / strategy->
   mechanism mapping. Carved verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets

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

(* ---- the single source for strategy -> audit-kind / mechanism, + the audit sink ---- *)
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

