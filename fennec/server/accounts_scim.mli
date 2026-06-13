(** SCIM directory provisioning primitives.

    SCIM is provisioning, not login. This module models directory connections, users, groups, PATCH
    operations, external-id identity evidence, and idempotent sync plans. It deliberately does not
    expose HTTP endpoints, persist records, issue sessions, merge users, or revoke sessions.

    {[
      let conn =
        Result.get_ok
          (Accounts_scim.connection ~id:"acme-scim" ~org_id:"acme" ~bearer_token ())
      in
      (* after authenticating the bearer token, plan an idempotent provisioning change *)
      let incoming =
        Result.get_ok (Accounts_scim.user ~external_id:"okta-123" ~user_name:"ada" ())
      in
      match Accounts_scim.plan_user conn ~existing:None ~incoming with
      | Ok (Accounts_scim.Create_user u) -> store.upsert_user ~connection_id:conn.id u
      | Ok _ -> Ok ()
      | Error e -> Error (Accounts_scim.string_of_error e)
    ]} *)

module Identity = Accounts_identity
module Org = Accounts_org

(** Constructor, auth, and patch errors. *)
type error =
  | Invalid_connection of string
  | Invalid_token
  | Invalid_user of string
  | Invalid_group of string
  | Invalid_patch of string
  | Identity_error of Identity.error
  | Org_error of Org.error

(** Human-readable error text. *)
val string_of_error : error -> string

(** SCIM tenant connection.

    [token_hash] is the SHA-256 hash of the bearer token. Store only this hash. *)
type connection = {
  id : string;
  org_id : string;
  token_hash : string;
  allow_deprovision : bool;
  default_role : string;
}

(** Build and validate a SCIM connection.

    [bearer_token] must be high entropy and is immediately hashed. *)
val connection :
  ?allow_deprovision:bool ->
  ?default_role:string ->
  id:string ->
  org_id:string ->
  bearer_token:string ->
  unit ->
  (connection, error) result

(** Check a bearer token against a connection hash in constant time. *)
val authenticate : connection -> bearer_token:string -> (unit, error) result

(** SCIM user resource normalized for account provisioning. *)
type user = {
  id : string option;
  external_id : string;
  user_name : string;
  active : bool;
  emails : string list;
  display_name : string option;
  groups : string list;
}

(** Build and validate a user resource.

    [external_id] is required because it is the stable enterprise identity key. [emails] are
    normalized and deduplicated. [groups] are normalized ids. *)
val user :
  ?id:string ->
  ?active:bool ->
  ?emails:string list ->
  ?display_name:string ->
  ?groups:string list ->
  external_id:string ->
  user_name:string ->
  unit ->
  (user, error) result

(** SCIM group resource normalized for account provisioning. *)
type group = {
  id : string option;
  external_id : string;
  display_name : string;
  members : string list;
}

(** Build and validate a group resource.

    [members] are SCIM user external ids, not global user ids. *)
val group :
  ?id:string -> ?members:string list -> external_id:string -> display_name:string -> unit -> (group, error) result

(** Build the canonical SCIM identity key for a user inside a tenant. *)
val identity : connection -> user -> (Identity.key, error) result

(** Build the org membership represented by a provisioned user. *)
val membership : ?now:(unit -> float) -> connection -> user_id:string -> user -> (Org.membership, error) result

(** User PATCH fields supported by the provisioning core. *)
type user_path =
  | User_name
  | Active
  | Emails
  | Display_name
  | Groups
  | External_id

(** PATCH operation. *)
type patch_op =
  | Add of user_path * string list
  | Replace of user_path * string list
  | Remove of user_path * string list

(** Apply normalized SCIM-style PATCH operations to a user resource.

    List fields use set semantics. Scalar fields require exactly one value for [Add]/[Replace] and
    may be cleared by [Remove]. Removing [External_id] is rejected because it is the identity key. *)
val apply_user_patch : user -> patch_op list -> (user, error) result

(** Group PATCH fields supported by the provisioning core. *)
type group_path =
  | Group_display_name
  | Group_members
  | Group_external_id

(** Group PATCH operation. *)
type group_patch_op =
  | Group_add of group_path * string list
  | Group_replace of group_path * string list
  | Group_remove of group_path * string list

(** Apply normalized SCIM-style PATCH operations to a group resource.

    [Group_members] uses set semantics. [Group_display_name] requires exactly one value and cannot
    be removed. Removing [Group_external_id] is rejected because it is the identity key. *)
val apply_group_patch : group -> group_patch_op list -> (group, error) result

(** Idempotent user sync plan. *)
type user_plan =
  | No_user_change
  | Create_user of user
  | Update_user of {
      before : user;
      after : user;
    }
  | Deprovision_user of {
      before : user;
      after : user;
    }

(** Compare current stored resource state with an incoming SCIM user. *)
val plan_user : connection -> existing:user option -> incoming:user -> (user_plan, error) result

(** Idempotent group sync plan. *)
type group_plan =
  | No_group_change
  | Create_group of group
  | Update_group of {
      before : group;
      after : group;
    }
  | Delete_group of group

(** Compare current stored group state with an incoming SCIM group. *)
val plan_group : existing:group option -> incoming:group option -> (group_plan, error) result

(** User external ids to add/remove when a group membership list changes. *)
type membership_delta = {
  add : string list;
  remove : string list;
}

(** Compute deterministic group membership delta by external id. *)
val membership_delta : before:group -> after:group -> membership_delta

(** SCIM directory persistence.

    Connections, users, and groups are kept together because a provisioning request normally needs
    to authenticate the connection, plan user changes, and update group membership from one tenant
    namespace. *)
type store = {
  find_connection : string -> connection option;
  list_connections : ?org_id:string -> unit -> connection list;
  upsert_connection : connection -> (unit, string) result;
  delete_connection : string -> (bool, string) result;
  find_user : connection_id:string -> external_id:string -> user option;
  list_users : ?connection_id:string -> unit -> user list;
  upsert_user : connection_id:string -> user -> (unit, string) result;
  delete_user : connection_id:string -> external_id:string -> (bool, string) result;
  find_group : connection_id:string -> external_id:string -> group option;
  list_groups : ?connection_id:string -> unit -> group list;
  upsert_group : connection_id:string -> group -> (unit, string) result;
  delete_group : connection_id:string -> external_id:string -> (bool, string) result;
}

(** Mutex-guarded in-memory SCIM store. *)
val memory_store : unit -> store
