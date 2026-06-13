type error =
  | Blank of string
  | Invalid_id of string
  | Invalid_domain of string
  | Invalid_role of string
  | Invalid_policy of string
  | Domain_ambiguous of string
  | Domain_not_found of string
  | Inactive_org of string
  | Inactive_membership of string

let string_of_error = function
  | Blank field -> "Blank " ^ field
  | Invalid_id id -> "Invalid id: " ^ id
  | Invalid_domain domain -> "Invalid domain: " ^ domain
  | Invalid_role role -> "Invalid role: " ^ role
  | Invalid_policy reason -> "Invalid org policy: " ^ reason
  | Domain_ambiguous domain -> "Domain belongs to multiple organizations: " ^ domain
  | Domain_not_found domain -> "No verified organization domain found for: " ^ domain
  | Inactive_org org_id -> "Organization is not active: " ^ org_id
  | Inactive_membership user_id -> "Membership is not active for user: " ^ user_id

type org_status =
  | Active
  | Suspended
  | Deleted

type membership_status =
  | Invited
  | Active_member
  | Disabled
  | Removed

type sso_policy =
  | Sso_optional
  | Sso_required of {
      connection_ids : string list;
      allow_password_fallback : bool;
      allow_jit : bool;
    }

type mfa_policy =
  | Mfa_optional
  | Mfa_required
  | Phishing_resistant_mfa_required

type auth_policy = {
  sso : sso_policy;
  mfa : mfa_policy;
  allow_public_signup : bool;
}

let default_policy = { sso = Sso_optional; mfa = Mfa_optional; allow_public_signup = false }

type domain = {
  name : string;
  verified : bool;
  primary : bool;
  connection_ids : string list;
}

type org = {
  id : string;
  name : string;
  status : org_status;
  domains : domain list;
  policy : auth_policy;
}

type membership = {
  org_id : string;
  user_id : string;
  role : string;
  status : membership_status;
  external_id : string option;
  created_at : float;
  updated_at : float option;
}

type invite_status =
  | Invite_pending
  | Invite_accepted
  | Invite_revoked

type invite = {
  id : string;
  org_id : string;
  email : string;
  role : string;
  token_hash : string;
  status : invite_status;
  created_at : float;
  expires_at : float;
  accepted_at : float option;
  revoked_at : float option;
}

type strategy =
  | Password
  | Email
  | OAuth of string
  | Oidc of string
  | Saml of string
  | Passkey
  | Recovery

type decision =
  | Allowed
  | Denied of string
  | Requires_sso of string list

type route = {
  org : org;
  domain : domain;
}

let valid_id_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '.' -> true
  | _ -> false

let normalize_id raw =
  let id = String.lowercase_ascii (String.trim raw) in
  if id = "" then Error (Blank "id")
  else if String.for_all valid_id_char id then Ok id
  else Error (Invalid_id raw)

let valid_role_char = function
  | 'a' .. 'z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
  | _ -> false

let normalize_role raw =
  let role = String.lowercase_ascii (String.trim raw) in
  if role = "" then Error (Blank "role")
  else if String.for_all valid_role_char role then Ok role
  else Error (Invalid_role raw)

let contains pred s =
  let found = ref false in
  String.iter (fun c -> if pred c then found := true) s;
  !found

let domain_of_email_or_domain raw =
  let value = String.lowercase_ascii (String.trim raw) in
  match String.rindex_opt value '@' with
  | None -> value
  | Some i when i = String.length value - 1 -> ""
  | Some i -> String.sub value (i + 1) (String.length value - i - 1)

let normalize_domain raw =
  let domain = domain_of_email_or_domain raw in
  if domain = "" then Error (Blank "domain")
  else if String.length domain > 253 then Error (Invalid_domain raw)
  else if String.contains domain '*' || String.contains domain '/' || String.contains domain ':' then
    Error (Invalid_domain raw)
  else if not (String.contains domain '.') then Error (Invalid_domain raw)
  else if domain.[0] = '.' || domain.[String.length domain - 1] = '.' then Error (Invalid_domain raw)
  else if contains (fun c -> c <= ' ' || Char.code c > 126) domain then Error (Invalid_domain raw)
  else
    let labels = String.split_on_char '.' domain in
    let valid_label label =
      label <> "" && String.length label <= 63
      && label.[0] <> '-'
      && label.[String.length label - 1] <> '-'
      && String.for_all
           (function
             | 'a' .. 'z' | '0' .. '9' | '-' -> true
             | _ -> false)
           label
    in
    if List.for_all valid_label labels then Ok domain else Error (Invalid_domain raw)

let uniq_sorted values =
  values
  |> List.sort_uniq String.compare

let normalize_connection_ids ids =
  let rec loop acc = function
    | [] -> Ok (uniq_sorted acc)
    | id :: rest -> (
      match normalize_id id with
      | Ok id -> loop (id :: acc) rest
      | Error _ -> Error (Invalid_id id))
  in
  loop [] ids

let domain ?(verified = false) ?(primary = false) ?(connection_ids = []) raw =
  match (normalize_domain raw, normalize_connection_ids connection_ids) with
  | Ok name, Ok connection_ids -> Ok { name; verified; primary; connection_ids }
  | Error e, _ -> Error e
  | _, Error e -> Error e

let policy_with_normalized_sso = function
  | { sso = Sso_optional; _ } as p -> p
  | { sso = Sso_required { connection_ids; allow_password_fallback; allow_jit }; _ } as p ->
    { p with sso = Sso_required { connection_ids = uniq_sorted connection_ids; allow_password_fallback; allow_jit } }

let validate_policy p =
  match p.sso with
  | Sso_optional -> Ok p
  | Sso_required { connection_ids; allow_password_fallback; allow_jit } -> (
    if connection_ids = [] then Error (Invalid_policy "required SSO needs at least one connection")
    else
      match normalize_connection_ids connection_ids with
      | Ok connection_ids ->
        Ok { p with sso = Sso_required { connection_ids; allow_password_fallback; allow_jit } }
      | Error e -> Error e)

let org ?(status = Active) ?(domains : domain list = []) ?(policy = default_policy) ~id ~name () =
  match (normalize_id id, validate_policy policy) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok id, Ok policy ->
    let name = String.trim name in
    if name = "" then Error (Blank "name")
    else
      let domains = List.sort (fun (a : domain) (b : domain) -> String.compare a.name b.name) domains in
      Ok { id; name; status; domains; policy = policy_with_normalized_sso policy }

let membership ?(now = Unix.gettimeofday) ?(status = Active_member) ?(role = "member") ?external_id ~org_id ~user_id () =
  match (normalize_id org_id, normalize_role role) with
  | Error e, _ -> Error e
  | _, Error e -> Error e
  | Ok org_id, Ok role ->
    let user_id = String.trim user_id in
    if user_id = "" then Error (Blank "user_id")
    else
      let external_id =
        match Option.map String.trim external_id with
        | Some "" | None -> None
        | Some external_id -> Some external_id
      in
      Ok { org_id; user_id; role; status; external_id; created_at = now (); updated_at = None }

let normalize_email raw =
  let email = String.lowercase_ascii (String.trim raw) in
  match String.index_opt email '@' with
  | Some i when i > 0 && i < String.length email - 1 -> Ok email
  | _ -> Error (Blank "email")

let invite ?(now = Unix.gettimeofday) ?(ttl = 604800.) ?(status = Invite_pending) ?accepted_at ?revoked_at
    ~id ~org_id ~email ~role ~token_hash () =
  match (normalize_id id, normalize_id org_id, normalize_email email, normalize_role role) with
  | Error e, _, _, _ -> Error e
  | _, Error e, _, _ -> Error e
  | _, _, Error e, _ -> Error e
  | _, _, _, Error e -> Error e
  | Ok id, Ok org_id, Ok email, Ok role ->
    let token_hash = String.trim token_hash in
    if token_hash = "" then Error (Blank "token_hash")
    else if ttl <= 0. || classify_float ttl = FP_nan then Error (Invalid_policy "invite ttl must be positive")
    else
      let created_at = now () in
      Ok { id; org_id; email; role; token_hash; status; created_at; expires_at = created_at +. ttl; accepted_at; revoked_at }

let is_active_org (org : org) = org.status = Active
let is_active_membership (membership : membership) = membership.status = Active_member

let require_membership (org : org) (membership : membership) =
  if not (is_active_org org) then Error (Inactive_org org.id)
  else if membership.org_id <> org.id || not (is_active_membership membership) then Error (Inactive_membership membership.user_id)
  else Ok ()

let active_memberships ~user_id memberships =
  List.filter (fun m -> m.user_id = user_id && is_active_membership m) memberships

let route_domain orgs raw =
  match normalize_domain raw with
  | Error e -> Error e
  | Ok wanted ->
    let matches =
      List.fold_left
        (fun acc org ->
          if not (is_active_org org) then acc
          else
            List.fold_left
              (fun acc domain ->
                if domain.verified && String.equal domain.name wanted then { org; domain } :: acc else acc)
              acc org.domains)
        [] orgs
    in
    match List.rev matches with
    | [] -> Error (Domain_not_found wanted)
    | [ route ] -> Ok route
    | _ -> Error (Domain_ambiguous wanted)

let connection_ids_for_route route =
  match route.domain.connection_ids with
  | _ :: _ as ids -> ids
  | [] -> (
    match route.org.policy.sso with
    | Sso_optional -> []
    | Sso_required { connection_ids; _ } -> connection_ids)

let strategy_connection = function
  | OAuth id | Oidc id | Saml id -> Some id
  | Password | Email | Passkey | Recovery -> None

let strategy_is_sso = function
  | OAuth _ | Oidc _ | Saml _ -> true
  | Password | Email | Passkey | Recovery -> false

let decide_strategy org strategy =
  if not (is_active_org org) then Denied ("organization is not active: " ^ org.id)
  else
    match org.policy.sso with
    | Sso_optional -> Allowed
    | Sso_required { connection_ids; allow_password_fallback; _ } -> (
      match (strategy_is_sso strategy, strategy_connection strategy) with
      | true, Some id -> (
        match normalize_id id with
        | Ok id when List.exists (String.equal id) connection_ids -> Allowed
        | Ok _ -> Denied "SSO connection is not owned by this organization"
        | Error _ -> Denied "SSO connection id is invalid")
      | true, None -> Allowed
      | false, _ when allow_password_fallback -> Allowed
      | false, _ -> Requires_sso connection_ids)

type role_allows = role:string -> permission:string -> bool

let default_role_allows ~role ~permission =
  match role with
  | "owner" | "admin" -> true
  | "member" -> permission = "read" || String.starts_with ~prefix:"read:" permission
  | _ -> false

let allows ?(role_allows = default_role_allows) membership ~permission =
  is_active_membership membership && role_allows ~role:membership.role ~permission

type store = {
  find_org : string -> org option;
  list_orgs : unit -> org list;
  upsert_org : org -> (unit, string) result;
  delete_org : string -> (bool, string) result;
  find_membership : org_id:string -> user_id:string -> membership option;
  list_memberships : ?org_id:string -> ?user_id:string -> unit -> membership list;
  upsert_membership : membership -> (unit, string) result;
  delete_membership : org_id:string -> user_id:string -> (bool, string) result;
  find_invite : string -> invite option;
  list_invites : ?org_id:string -> ?email:string -> unit -> invite list;
  upsert_invite : invite -> (unit, string) result;
  delete_invite : string -> (bool, string) result;
}

let membership_key ~org_id ~user_id = org_id ^ "\000" ^ user_id

let memory_store () =
  let orgs : (string, org) Hashtbl.t = Hashtbl.create 32 in
  let memberships : (string, membership) Hashtbl.t = Hashtbl.create 128 in
  let invites : (string, invite) Hashtbl.t = Hashtbl.create 128 in
  let mutex = Mutex.create () in
  let locked f = Mutex.lock mutex; Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f in
  let sorted_by f xs = List.sort (fun a b -> String.compare (f a) (f b)) xs in
  let find_org id = locked (fun () -> Hashtbl.find_opt orgs id) in
  let list_orgs () =
    locked (fun () -> Hashtbl.to_seq_values orgs |> List.of_seq |> sorted_by (fun (o : org) -> o.id))
  in
  let upsert_org (org : org) =
    locked (fun () ->
        Hashtbl.replace orgs org.id org;
        Ok ())
  in
  let delete_org id =
    locked (fun () ->
        let existed = Hashtbl.mem orgs id in
        Hashtbl.remove orgs id;
        Ok existed)
  in
  let find_membership ~org_id ~user_id =
    locked (fun () -> Hashtbl.find_opt memberships (membership_key ~org_id ~user_id))
  in
  let list_memberships ?org_id ?user_id () =
    locked (fun () ->
        Hashtbl.to_seq_values memberships
        |> List.of_seq
        |> List.filter (fun (m : membership) ->
               Option.fold ~none:true ~some:(String.equal m.org_id) org_id
               && Option.fold ~none:true ~some:(String.equal m.user_id) user_id)
        |> sorted_by (fun (m : membership) -> membership_key ~org_id:m.org_id ~user_id:m.user_id))
  in
  let upsert_membership (m : membership) =
    locked (fun () ->
        Hashtbl.replace memberships (membership_key ~org_id:m.org_id ~user_id:m.user_id) m;
        Ok ())
  in
  let delete_membership ~org_id ~user_id =
    locked (fun () ->
        let key = membership_key ~org_id ~user_id in
        let existed = Hashtbl.mem memberships key in
        Hashtbl.remove memberships key;
        Ok existed)
  in
  let find_invite id = locked (fun () -> Hashtbl.find_opt invites id) in
  let list_invites ?org_id ?email () =
    let email = Option.map String.lowercase_ascii email in
    locked (fun () ->
        Hashtbl.to_seq_values invites
        |> List.of_seq
        |> List.filter (fun i ->
               Option.fold ~none:true ~some:(String.equal i.org_id) org_id
               && Option.fold ~none:true ~some:(String.equal i.email) email)
        |> sorted_by (fun (i : invite) -> i.id))
  in
  let upsert_invite (invite : invite) =
    locked (fun () ->
        Hashtbl.replace invites invite.id invite;
        Ok ())
  in
  let delete_invite id =
    locked (fun () ->
        let existed = Hashtbl.mem invites id in
        Hashtbl.remove invites id;
        Ok existed)
  in
  {
    find_org;
    list_orgs;
    upsert_org;
    delete_org;
    find_membership;
    list_memberships;
    upsert_membership;
    delete_membership;
    find_invite;
    list_invites;
    upsert_invite;
    delete_invite;
  }

(* ---- inline tests ---- *)

let ok = function Ok x -> x | Error e -> failwith (string_of_error e)

let test_domain ?verified ?primary ?connection_ids name = ok (domain ?verified ?primary ?connection_ids name)
let test_org ?status ?domains ?policy id = ok (org ?status ?domains ?policy ~id ~name:("Org " ^ id) ())

let%test "normalize_id is conservative ascii" =
  normalize_id " Acme.Main-1 " = Ok "acme.main-1"
  && Result.is_error (normalize_id "")
  && Result.is_error (normalize_id "acme/main")

let%test "normalize_domain accepts email addresses and rejects unsafe routing input" =
  normalize_domain "Ada@Example.COM" = Ok "example.com"
  && Result.is_error (normalize_domain "*.example.com")
  && Result.is_error (normalize_domain "https://example.com")
  && Result.is_error (normalize_domain "localhost")

let%test "domain normalizes connection ids" =
  match domain ~verified:true ~connection_ids:[ "SAML"; "saml"; "OIDC" ] "Example.COM" with
  | Ok d -> d.name = "example.com" && d.connection_ids = [ "oidc"; "saml" ]
  | Error _ -> false

let%test "org rejects required sso without connections" =
  let policy =
    { default_policy with sso = Sso_required { connection_ids = []; allow_password_fallback = false; allow_jit = true } }
  in
  Result.is_error (org ~policy ~id:"acme" ~name:"Acme" ())

let%test "route_domain ignores unverified and inactive orgs" =
  let inactive = test_org ~status:Suspended ~domains:[ test_domain ~verified:true "example.com" ] "old" in
  let pending = test_org ~domains:[ test_domain "example.com" ] "pending" in
  let active = test_org ~domains:[ test_domain ~verified:true "example.com" ] "acme" in
  match route_domain [ inactive; pending; active ] "ada@example.com" with
  | Ok route -> route.org.id = "acme" && route.domain.name = "example.com"
  | Error _ -> false

let%test "route_domain reports ambiguous verified domains" =
  let a = test_org ~domains:[ test_domain ~verified:true "example.com" ] "a" in
  let b = test_org ~domains:[ test_domain ~verified:true "example.com" ] "b" in
  route_domain [ a; b ] "example.com" = Error (Domain_ambiguous "example.com")

let%test "domain route connection ids prefer domain then policy" =
  let policy =
    { default_policy with
      sso = Sso_required { connection_ids = [ "saml-default" ]; allow_password_fallback = false; allow_jit = true };
    }
  in
  let org =
    test_org ~policy
      ~domains:[ test_domain ~verified:true ~connection_ids:[ "saml-domain" ] "example.com" ]
      "acme"
  in
  match route_domain [ org ] "example.com" with
  | Ok route -> connection_ids_for_route route = [ "saml-domain" ]
  | Error _ -> false

let%test "required sso blocks local auth but allows owned connection" =
  let policy =
    { default_policy with
      sso = Sso_required { connection_ids = [ "saml-acme" ]; allow_password_fallback = false; allow_jit = false };
    }
  in
  let org = test_org ~policy "acme" in
  decide_strategy org Password = Requires_sso [ "saml-acme" ]
  && decide_strategy org (Saml "saml-acme") = Allowed
  && decide_strategy org (Saml "other") = Denied "SSO connection is not owned by this organization"

let%test "password fallback is explicit" =
  let policy =
    { default_policy with
      sso = Sso_required { connection_ids = [ "saml-acme" ]; allow_password_fallback = true; allow_jit = false };
    }
  in
  decide_strategy (test_org ~policy "acme") Password = Allowed

let%test "membership normalizes role and active lookup is user-scoped" =
  let m1 = ok (membership ~now:(fun () -> 10.) ~role:" Admin " ~org_id:"Acme" ~user_id:"u1" ()) in
  let m2 = ok (membership ~status:Disabled ~org_id:"beta" ~user_id:"u1" ()) in
  let m3 = ok (membership ~org_id:"gamma" ~user_id:"u2" ()) in
  m1.org_id = "acme" && m1.role = "admin" && active_memberships ~user_id:"u1" [ m1; m2; m3 ] = [ m1 ]

let%test "require_membership checks org and membership activity" =
  let org = test_org "acme" in
  let ok_member = ok (membership ~org_id:"acme" ~user_id:"u1" ()) in
  let wrong_org = ok (membership ~org_id:"beta" ~user_id:"u1" ()) in
  require_membership org ok_member = Ok ()
  && require_membership org wrong_org = Error (Inactive_membership "u1")
  && require_membership { org with status = Suspended } ok_member = Error (Inactive_org "acme")

let%test "default rbac hook is small and overrideable" =
  let owner = ok (membership ~role:"owner" ~org_id:"acme" ~user_id:"u1" ()) in
  let member = ok (membership ~role:"member" ~org_id:"acme" ~user_id:"u2" ()) in
  let disabled = ok (membership ~status:Disabled ~role:"owner" ~org_id:"acme" ~user_id:"u3" ()) in
  allows owner ~permission:"billing:write"
  && allows member ~permission:"read:project"
  && not (allows member ~permission:"billing:write")
  && not (allows disabled ~permission:"billing:write")
  && allows ~role_allows:(fun ~role:_ ~permission -> permission = "custom") member ~permission:"custom"

let%test "memory_store persists org memberships and invites deterministically" =
  let store = memory_store () in
  let org = ok (org ~id:"acme" ~name:"Acme" ()) in
  let membership = ok (membership ~org_id:"acme" ~user_id:"u1" ~role:"admin" ()) in
  let invite =
    ok
      (invite ~now:(fun () -> 10.) ~id:"inv1" ~org_id:"acme" ~email:"A@Example.com" ~role:"member"
         ~token_hash:"hash" ())
  in
  store.upsert_org org = Ok ()
  && store.upsert_membership membership = Ok ()
  && store.upsert_invite invite = Ok ()
  && store.find_org "acme" = Some org
  && store.find_membership ~org_id:"acme" ~user_id:"u1" = Some membership
  && store.list_invites ~email:"a@example.com" () = [ invite ]
