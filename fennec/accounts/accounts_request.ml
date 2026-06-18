(* accounts_request.ml — the per-request paw middleware + route guards: reading the current
   user_id / auth_context / assurance / org_context off the Conn, the [paw] that verifies the
   signed login cookie, and the require_user / require_role / require_permission /
   require_assurance / require_org guards plus the scope/can/can_in authorization queries.
   Carved verbatim from accounts_base.ml. *)

open Accounts_types
open Accounts_secrets

(* ---- request-local identity / auth-context / assurance / org-context ---- *)
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


(* ---- the cookie-verifying paw + checked_session + require_user ---- *)
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

(* ---- app-wide role/permission queries + their route guards ---- *)
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
