(** Organizations, memberships, domains, and tenant auth policy.

    This module provides storage-neutral B2B identity primitives. It models organizations,
    memberships, verified domains, SSO routing, tenant login policy, and small RBAC hooks. It does
    not persist records, create users, merge identities, or perform SCIM mutations.

    {[
      let acme =
        Result.get_ok
          (Accounts_org.org ~id:"acme" ~name:"Acme"
             ~domains:[ Result.get_ok (Accounts_org.domain ~verified:true "acme.example") ]
             ())
      in
      (* route a sign-in email to its tenant, then check the requested strategy *)
      match Accounts_org.route_domain [ acme ] "ada@acme.example" with
      | Ok route -> Accounts_org.decide_strategy route.org (Accounts_org.Oidc "google")
      | Error _ -> Accounts_org.Denied "unknown tenant"
    ]} *)

(** Constructor and policy errors. *)
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

(** Human-readable error text. *)
val string_of_error : error -> string

(** Organization lifecycle state. *)
type org_status =
  | Active
  | Suspended
  | Deleted

(** Membership lifecycle state. *)
type membership_status =
  | Invited
  | Active_member
  | Disabled
  | Removed

(** Tenant SSO policy.

    [Sso_required] may list one or more owned connection ids. [allow_password_fallback] exists for
    controlled break-glass and migration windows; normal enterprise deployments should keep it
    false. [allow_jit] describes whether a verified SSO principal may create a membership on first
    login. *)
type sso_policy =
  | Sso_optional
  | Sso_required of {
      connection_ids : string list;
      allow_password_fallback : bool;
      allow_jit : bool;
    }

(** Tenant MFA policy. *)
type mfa_policy =
  | Mfa_optional
  | Mfa_required
  | Phishing_resistant_mfa_required

(** Tenant auth policy. *)
type auth_policy = {
  sso : sso_policy;
  mfa : mfa_policy;
  allow_public_signup : bool;
}

(** Default policy: local auth allowed, no tenant-level MFA requirement, no public signup. *)
val default_policy : auth_policy

(** Verified or pending domain ownership. *)
type domain = {
  name : string;
  verified : bool;
  primary : bool;
  connection_ids : string list;
}

(** Organization record. *)
type org = {
  id : string;
  name : string;
  status : org_status;
  domains : domain list;
  policy : auth_policy;
}

(** Organization membership. *)
type membership = {
  org_id : string;
  user_id : string;
  role : string;
  status : membership_status;
  external_id : string option;
  created_at : float;
  updated_at : float option;
}

(** Invitation lifecycle state. *)
type invite_status =
  | Invite_pending
  | Invite_accepted
  | Invite_revoked

(** Organization invite.

    [token_hash] stores a digest of the user-visible invite token, never the raw token. *)
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

(** Auth strategies checked against tenant policy. *)
type strategy =
  | Password
  | Email
  | OAuth of string
  | Oidc of string
  | Saml of string
  | Passkey
  | Recovery

(** Login policy decision. *)
type decision =
  | Allowed
  | Denied of string
  | Requires_sso of string list

(** Domain route result. *)
type route = {
  org : org;
  domain : domain;
}

(** Normalize an organization, connection, or namespace id.

    Ids are trimmed, lowercased, and limited to conservative ASCII identifier characters. *)
val normalize_id : string -> (string, error) result

(** Normalize and validate a role name. *)
val normalize_role : string -> (string, error) result

(** Normalize an email/domain routing suffix.

    Domains are lowercased, reject wildcards and URL-like input, and require at least one dot. *)
val normalize_domain : string -> (string, error) result

(** Build a domain ownership record. *)
val domain :
  ?verified:bool -> ?primary:bool -> ?connection_ids:string list -> string -> (domain, error) result

(** Build an organization record. *)
val org :
  ?status:org_status -> ?domains:domain list -> ?policy:auth_policy -> id:string -> name:string -> unit -> (org, error) result

(** Build a membership record. *)
val membership :
  ?now:(unit -> float) ->
  ?status:membership_status ->
  ?role:string ->
  ?external_id:string ->
  org_id:string ->
  user_id:string ->
  unit ->
  (membership, error) result

(** Build an organization invite. [token_hash] must already be a storage hash, not a raw token. *)
val invite :
  ?now:(unit -> float) ->
  ?ttl:float ->
  ?status:invite_status ->
  ?accepted_at:float ->
  ?revoked_at:float ->
  id:string ->
  org_id:string ->
  email:string ->
  role:string ->
  token_hash:string ->
  unit ->
  (invite, error) result

(** [true] when the organization can admit interactive users. *)
val is_active_org : org -> bool

(** [true] when the membership grants tenant access. *)
val is_active_membership : membership -> bool

(** Check that [membership] belongs to [org] and is active. *)
val require_membership : org -> membership -> (unit, error) result

(** Find all active memberships for [user_id]. *)
val active_memberships : user_id:string -> membership list -> membership list

(** Route a normalized or raw email address/domain to one active verified organization domain.

    Unverified domains are ignored. Multiple active orgs with the same verified domain return
    [Domain_ambiguous] so callers can ask the user to choose instead of silently picking a tenant. *)
val route_domain : org list -> string -> (route, error) result

(** Connection ids allowed for a routed domain.

    Domain-specific connection ids take precedence. If absent and the org requires SSO, the tenant
    policy connection ids are returned. *)
val connection_ids_for_route : route -> string list

(** Check whether a login strategy is allowed for an org policy.

    This does not check passwords, tokens, or provider assertions. It only answers whether the
    already-routed tenant policy permits the strategy. *)
val decide_strategy : org -> strategy -> decision

(** A small RBAC hook. Applications can pass their own role-to-permission predicate. *)
type role_allows = role:string -> permission:string -> bool

(** Default role rules: owner/admin allow every permission, member allows read, invited/disabled
    memberships allow nothing through {!allows}. *)
val default_role_allows : role_allows

(** Check a permission for an active membership. *)
val allows : ?role_allows:role_allows -> membership -> permission:string -> bool

(** Organization persistence.

    The store keeps orgs, memberships, and invites together because tenant login routing, RBAC, and
    invitation acceptance need consistent access to all three. *)
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

(** Mutex-guarded in-memory org store for tests and local prototypes. *)
val memory_store : unit -> store
