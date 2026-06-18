type kind =
  | Login
  | Login_failure
  | Logout
  | Token_resume
  | Password_change
  | Password_reset
  | Email_verification
  | Email_login
  | Passkey_registration
  | Passkey_assertion
  | OAuth_callback
  | Oidc_callback
  | Saml_callback
  | Identity_link
  | Identity_unlink
  | Identity_merge
  | Mfa_enrollment
  | Mfa_step_up
  | Recovery
  | Scim_provision
  | Scim_deprovision
  | Role_change
  | Org_policy_change
  | Challenge_issue
  | Challenge_consume
  | Custom of string

type actor =
  | Anonymous
  | User of string
  | System of string

type mechanism =
  | Password
  | Email
  | OAuth of string
  | Oidc of string
  | Saml of string
  | Passkey
  | Mfa
  | Scim of string
  | Org
  | Token
  | Challenge
  | Custom_mechanism of string

type outcome =
  | Success
  | Failure of string

type request = {
  request_id : string option;
  ip : string option;
  user_agent : string option;
}

type event = {
  id : string;
  at : float;
  kind : kind;
  actor : actor;
  target_user_id : string option;
  org_id : string option;
  mechanism : mechanism option;
  connection_id : string option;
  request : request;
  outcome : outcome;
  metadata : (string * string) list;
}

let empty_request = { request_id = None; ip = None; user_agent = None }

let clean_opt = function
  | None -> None
  | Some value ->
    let value = String.trim value in
    if value = "" then None else Some value

let request ?request_id ?ip ?user_agent () =
  {
    request_id = clean_opt request_id;
    ip = clean_opt ip;
    user_agent = clean_opt user_agent;
  }

let normalize_name raw =
  let raw = String.lowercase_ascii (String.trim raw) in
  let b = Buffer.create (String.length raw) in
  let last_sep = ref true in
  String.iter
    (fun c ->
      let valid =
        match c with
        | 'a' .. 'z' | '0' .. '9' -> Some c
        | '_' | '-' | '.' | ':' -> Some c
        | _ -> None
      in
      match valid with
      | Some c ->
        Buffer.add_char b c;
        last_sep := false
      | None when not !last_sep ->
        Buffer.add_char b '_';
        last_sep := true
      | None -> ())
    raw;
  let value = Buffer.contents b in
  let rec last_non_sep i =
    if i < 0 then -1
    else
      match value.[i] with
      | '_' | '-' | '.' | ':' -> last_non_sep (i - 1)
      | _ -> i
  in
  match last_non_sep (String.length value - 1) with
  | -1 -> ""
  | last -> String.sub value 0 (last + 1)

let normalize_code raw =
  let value = normalize_name raw in
  if value = "" then "unknown" else value

let string_of_kind = function
  | Login -> "login"
  | Login_failure -> "login_failure"
  | Logout -> "logout"
  | Token_resume -> "token_resume"
  | Password_change -> "password_change"
  | Password_reset -> "password_reset"
  | Email_verification -> "email_verification"
  | Email_login -> "email_login"
  | Passkey_registration -> "passkey_registration"
  | Passkey_assertion -> "passkey_assertion"
  | OAuth_callback -> "oauth_callback"
  | Oidc_callback -> "oidc_callback"
  | Saml_callback -> "saml_callback"
  | Identity_link -> "identity_link"
  | Identity_unlink -> "identity_unlink"
  | Identity_merge -> "identity_merge"
  | Mfa_enrollment -> "mfa_enrollment"
  | Mfa_step_up -> "mfa_step_up"
  | Recovery -> "recovery"
  | Scim_provision -> "scim_provision"
  | Scim_deprovision -> "scim_deprovision"
  | Role_change -> "role_change"
  | Org_policy_change -> "org_policy_change"
  | Challenge_issue -> "challenge_issue"
  | Challenge_consume -> "challenge_consume"
  | Custom name -> "custom:" ^ normalize_code name

let string_of_actor = function
  | Anonymous -> "anonymous"
  | User user_id -> "user:" ^ String.trim user_id
  | System name -> "system:" ^ normalize_code name

let string_of_mechanism = function
  | Password -> "password"
  | Email -> "email"
  | OAuth provider -> "oauth:" ^ normalize_code provider
  | Oidc connection -> "oidc:" ^ normalize_code connection
  | Saml connection -> "saml:" ^ normalize_code connection
  | Passkey -> "passkey"
  | Mfa -> "mfa"
  | Scim connection -> "scim:" ^ normalize_code connection
  | Org -> "org"
  | Token -> "token"
  | Challenge -> "challenge"
  | Custom_mechanism name -> "custom:" ^ normalize_code name

let string_of_outcome = function
  | Success -> "success"
  | Failure reason -> "failure:" ^ normalize_code reason

let sensitive_key key =
  let key = normalize_name key in
  let contains needle =
    let n = String.length needle in
    let k = String.length key in
    let matches_at i =
      let rec loop j =
        j = n || (String.get key (i + j) = String.get needle j && loop (j + 1))
      in
      loop 0
    in
    let rec loop i =
      i + n <= k && (matches_at i || loop (i + 1))
    in
    n > 0 && loop 0
  in
  List.exists contains
    [
      "password";
      "secret";
      "token";
      "bearer";
      "authorization";
      "cookie";
      "credential";
      "assertion";
      "samlresponse";
      "code_verifier";
      "client_secret";
      "private_key";
      "otp";
    ]

let sanitize_metadata metadata =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (key, value) ->
      let key = normalize_name key in
      if key <> "" then
        let value = if sensitive_key key then "[redacted]" else String.trim value in
        Hashtbl.replace table key value)
    metadata;
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) table []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let event ?target_user_id ?org_id ?mechanism ?connection_id ?(request = empty_request) ?(metadata = []) ~id ~at kind actor outcome =
  {
    id = String.trim id;
    at;
    kind;
    actor;
    target_user_id = clean_opt target_user_id;
    org_id = clean_opt org_id;
    mechanism;
    connection_id = clean_opt connection_id;
    request;
    outcome;
    metadata = sanitize_metadata metadata;
  }

let add_opt key value acc = match value with None -> acc | Some value -> (key, value) :: acc

let to_fields event =
  let fields =
    [
      ("outcome", string_of_outcome event.outcome);
      ("actor", string_of_actor event.actor);
      ("kind", string_of_kind event.kind);
      ("at", Printf.sprintf "%.3f" event.at);
      ("id", event.id);
    ]
  in
  let fields =
    fields
    |> add_opt "target_user_id" event.target_user_id
    |> add_opt "org_id" event.org_id
    |> add_opt "mechanism" (Option.map string_of_mechanism event.mechanism)
    |> add_opt "connection_id" event.connection_id
    |> add_opt "request_id" event.request.request_id
    |> add_opt "ip" event.request.ip
    |> add_opt "user_agent" event.request.user_agent
  in
  List.rev fields @ List.map (fun (k, v) -> ("meta." ^ k, v)) event.metadata

type store = {
  append_event : event -> (unit, string) result;
  list_events : target_user_id:string option -> org_id:string option -> kind:kind option -> event list;
}

let store ~append ~list = { append_event = append; list_events = list }

let same_kind a b = String.equal (string_of_kind a) (string_of_kind b)

let memory_store () =
  let mutex = Mutex.create () in
  let events_rev = ref [] in
  let ids = Hashtbl.create 1024 in
  let locked f =
    Mutex.lock mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f
  in
  let append event =
    locked (fun () ->
        if event.id = "" then Error "audit event id cannot be blank"
        else if Hashtbl.mem ids event.id then
          Error "duplicate audit event id"
        else (
          Hashtbl.add ids event.id ();
          events_rev := event :: !events_rev;
          Ok ()))
  in
  let list ~target_user_id ~org_id ~kind =
    locked (fun () ->
        List.rev !events_rev
        |> List.filter (fun event ->
               Option.fold ~none:true ~some:(fun target -> event.target_user_id = Some target) target_user_id
               && Option.fold ~none:true ~some:(fun org -> event.org_id = Some org) org_id
               && Option.fold ~none:true ~some:(fun kind -> same_kind event.kind kind) kind))
  in
  store ~append ~list

let append store event = store.append_event event
let list ?target_user_id ?org_id ?kind store = store.list_events ~target_user_id ~org_id ~kind

(* ---- inline tests ---- *)

let%test "string names are stable" =
  string_of_kind Login = "login"
  && string_of_kind (Custom "Password Reset!") = "custom:password_reset"
  && string_of_actor (System "SCIM Sync") = "system:scim_sync"
  && string_of_mechanism (Saml "Corp SSO") = "saml:corp_sso"
  && string_of_outcome (Failure "bad password") = "failure:bad_password"

let%test "request drops blank values" =
  request ~request_id:" req-1 " ~ip:" " ~user_agent:" Browser " ()
  = { request_id = Some "req-1"; ip = None; user_agent = Some "Browser" }

let%test "metadata redacts secret-bearing keys and keeps last value" =
  sanitize_metadata
    [
      ("access_token", "raw-token");
      ("Provider", "github");
      ("provider", "gitlab");
      ("password", "secret");
      ("note", " ok ");
    ]
  = [
      ("access_token", "[redacted]");
      ("note", "ok");
      ("password", "[redacted]");
      ("provider", "gitlab");
    ]

let%test "event sanitizes optional fields and metadata" =
  let e =
    event ~id:" evt-1 " ~at:10. ~target_user_id:" user-1 " ~org_id:" " ~mechanism:(OAuth "GitHub")
      ~metadata:[ ("refresh_token", "secret"); ("provider_subject", "123") ]
      Login (User "actor-1") Success
  in
  e.id = "evt-1"
  && e.target_user_id = Some "user-1"
  && e.org_id = None
  && e.metadata = [ ("provider_subject", "123"); ("refresh_token", "[redacted]") ]

let%test "to_fields is deterministic and excludes empty optionals" =
  let e =
    event ~id:"evt-1" ~at:1.25 ~request:(request ~request_id:"req-1" ())
      ~metadata:[ ("reason", "bad_password") ] Login_failure Anonymous (Failure "bad_password")
  in
  to_fields e
  = [
      ("id", "evt-1");
      ("at", "1.250");
      ("kind", "login_failure");
      ("actor", "anonymous");
      ("outcome", "failure:bad_password");
      ("request_id", "req-1");
      ("meta.reason", "bad_password");
    ]

let%test "memory store is append-only and rejects duplicate ids" =
  let store = memory_store () in
  let a = event ~id:"evt-1" ~at:1. Login Anonymous Success in
  let b = event ~id:"evt-2" ~at:2. Logout (User "u1") Success in
  append store a = Ok ()
  && append store b = Ok ()
  && Result.is_error (append store a)
  && list store = [ a; b ]

let%test "memory store filters by target org and kind" =
  let store = memory_store () in
  let a = event ~id:"evt-1" ~at:1. ~target_user_id:"u1" ~org_id:"o1" Login Anonymous Success in
  let b = event ~id:"evt-2" ~at:2. ~target_user_id:"u2" ~org_id:"o1" Logout Anonymous Success in
  let c = event ~id:"evt-3" ~at:3. ~target_user_id:"u1" Login Anonymous Success in
  ignore (append store a);
  ignore (append store b);
  ignore (append store c);
  list ~target_user_id:"u1" store = [ a; c ]
  && list ~org_id:"o1" store = [ a; b ]
  && list ~kind:Login store = [ a; c ]
