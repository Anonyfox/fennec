(** Typed, string-backed account roles and permissions.

    Roles are application-owned authorization names. Fennec validates, normalizes, stores, and checks
    them, but it does not make ["admin"] or any other role magic. Persisted values remain plain
    strings for Mongo, SSO, SCIM, audit, and JSON boundaries; application code can use {!Role.t} and
    {!Permission.t} handles so typos are concentrated at declaration sites instead of repeated across
    routes.

    {[
      let admin = Accounts_roles.Role.admin in
      let billing_read = Accounts_roles.Permission.v_exn "billing.read" in
      let policy = Accounts_roles.policy [ Accounts_roles.role admin [ billing_read ] ] in
      Accounts_roles.role_allows policy ~role:admin ~permission:billing_read
    ]} *)

(** Constructor and policy errors. *)
type error =
  | Blank of string
  | Invalid_name of string

(** Human-readable error text. *)
val string_of_error : error -> string

(** A normalized account role. *)
module Role : sig
  type t

  (** Build a role from a wire/storage/admin value. Names are trimmed, lowercased, and limited to
      conservative identifier characters: letters, digits, [_], [-], [.], and [:]. *)
  val v : string -> (t, error) result

  (** Build a role or raise [Invalid_argument]. Intended for top-level app declarations. *)
  val v_exn : string -> t

  (** Canonical storage/wire name. *)
  val name : t -> string

  (** Ordinary convenience role values. They are not privileged unless the app policy says so. *)
  val admin : t
  val owner : t
  val member : t
end

(** A normalized permission name. *)
module Permission : sig
  type t

  (** Build a permission from a wire/storage/admin value. The same conservative name rules as roles
      apply. Dot or colon namespaces are conventional but not required. *)
  val v : string -> (t, error) result

  (** Build a permission or raise [Invalid_argument]. Intended for top-level app declarations. *)
  val v_exn : string -> t

  (** Canonical storage/wire name. *)
  val name : t -> string
end

(** A role definition grants permissions. *)
type definition

(** Define one role's permission set. Duplicate permissions are ignored. *)
val role : Role.t -> Permission.t list -> definition

(** Immutable authorization policy. Missing roles and permissions deny by default. *)
type policy

(** Build a policy from role definitions. Later definitions for the same role replace earlier ones,
    which keeps generated/userland assembly deterministic. *)
val policy : definition list -> policy

(** An empty policy grants no permissions. *)
val empty_policy : policy

(** Check whether [role] grants [permission]. *)
val role_allows : policy -> role:Role.t -> permission:Permission.t -> bool

(** Check whether any role in [roles] grants [permission]. *)
val any_role_allows : policy -> roles:Role.t list -> permission:Permission.t -> bool

(** Canonicalize and deduplicate role lists for storage. *)
val normalize_roles : string list -> (Role.t list, error) result

(** Convert typed roles to canonical storage names. *)
val role_names : Role.t list -> string list

(** Add [role] to a role set. *)
val add : Role.t -> Role.t list -> Role.t list

(** Remove [role] from a role set. *)
val remove : Role.t -> Role.t list -> Role.t list

(** True when the set contains [role]. *)
val mem : Role.t -> Role.t list -> bool
