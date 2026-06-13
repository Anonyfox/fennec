(** Canonical identity keys shared by every Accounts login mechanism.

    Accounts supports users with many credentials: local password, verified emails, OAuth/OIDC/SAML
    subjects, passkeys, SCIM external ids, MFA/recovery methods, and future provider-specific
    adapters. This module normalizes those external/local facts into typed, stable keys so each
    mechanism does not invent its own uniqueness and merge rules.

    The module is deliberately persistence-neutral. Stores decide how to attach, detach, and merge
    keys atomically, but all stores should persist {!stable_key} for globally scoped identities.

    {[
      let store = Accounts_identity.memory_store () in
      match Accounts_identity.oauth ~provider:"github" ~subject:"12345" with
      | Error e -> prerr_endline (Accounts_identity.string_of_error e)
      | Ok key -> (
          match store.attach ~created_at:(Unix.gettimeofday ()) ~user_id:"user-1" key with
          | Accounts_identity.Attach _ | Accounts_identity.Already_linked _ -> ()
          | Accounts_identity.Conflict _ -> prerr_endline "already linked to another user")
    ]} *)

(** Identity kind. *)
type kind =
  | Password
  | Email
  | OAuth
  | Oidc
  | Saml
  | Passkey
  | Scim
  | Recovery

(** Whether an identity is globally unique or only meaningful inside one user record.

    [Global] keys can be used for lookup and uniqueness constraints. [Per_user] keys describe local
    credential presence and must not be indexed as globally unique across users. *)
type scope = Global | Per_user

(** Verification state for identities whose ownership can be unverified at first.

    Verification is evidence on top of an identity, not a uniqueness component. For example,
    verified and unverified email claims for the same normalized address share the same
    {!stable_key}, but only the verified claim is safe auto-link evidence. *)
type verification = Verified | Unverified

(** A normalized identity key.

    Values are opaque so call sites cannot accidentally compare provider-specific raw strings. Use
    constructors such as {!email}, {!oauth}, {!oidc}, {!passkey}, and {!scim}. *)
type key

(** A key attached to a user. Stores may use this shape directly or map it into their native row or
    document format. *)
type link = {
  user_id : string;
  key : key;
  created_at : float;
  verified_at : float option;
}

(** Build a link for [user_id]. [verified_at] should be set when the key was verified by the
    mechanism that produced it. *)
val link : ?verified_at:float -> user_id:string -> key -> created_at:float -> link

(** Store-side attach plan for one identity key. *)
type link_plan =
  | Attach of link
  | Already_linked of link
  | Conflict of link

(** Plan attaching [key] to [user_id].

    [existing] is the current global lookup result for {!stable_key}, when one exists. Global keys
    linked to another user produce [Conflict]. Per-user keys are local credential facts and do not
    conflict with another user's per-user key. *)
val plan_link :
  ?verified_at:float -> created_at:float -> user_id:string -> key -> existing:link option -> link_plan

(** [true] when a key can be used as login or recovery evidence. Unverified email is not usable. *)
val usable_for_login : key -> bool

(** Store-side detach plan. *)
type detach_plan =
  | Detach of link
  | Link_not_found
  | Reject_last_credential

(** Plan detaching [key] from [user_id].

    When [allow_last:false] (the default), detaching the last usable credential is rejected. *)
val plan_detach : ?allow_last:bool -> user_id:string -> key -> links:link list -> detach_plan

(** Conflict encountered while planning a user merge. *)
type merge_conflict = {
  key : key;
  source : link;
  existing : link;
}

(** Store-side merge plan from one user into another. *)
type merge_plan = {
  from_user_id : string;
  into_user_id : string;
  move : link list;
  keep : link list;
  conflicts : merge_conflict list;
}

(** Plan rewriting [source] links from [from_user_id] to [into_user_id].

    Links already present on the target are reported in [keep]. Global keys owned by a third user or
    by a non-target user are reported as [conflicts]. The returned plan is deterministic by
    {!stable_key}. *)
val plan_merge :
  from_user_id:string -> into_user_id:string -> source:link list -> target:link list -> merge_plan

(** Store contract for identity links.

    Implementations must make [attach], [detach], and [merge] atomic with their uniqueness checks.
    [find] is a global-key lookup by {!stable_key}; use [list] to inspect a user's per-user links. *)
type store = {
  find : key -> link option;
  list : ?user_id:string -> unit -> link list;
  attach : ?verified_at:float -> created_at:float -> user_id:string -> key -> link_plan;
  detach : ?allow_last:bool -> user_id:string -> key -> detach_plan;
  merge : from_user_id:string -> into_user_id:string -> (merge_plan, merge_conflict list) result;
}

(** A process-local, mutex-guarded identity-link store for tests, examples, and single-process
    prototypes. *)
val memory_store : unit -> store

(** Constructor/normalization errors. *)
type error =
  | Blank of string
  | Invalid_email of string
  | Invalid_name of string

(** Human-readable error text. *)
val string_of_error : error -> string

(** Stable wire/debug name for an identity kind. *)
val string_of_kind : kind -> string

(** Parse a kind name produced by {!string_of_kind}. *)
val kind_of_string : string -> kind option

(** Stable wire/debug name for a scope. *)
val string_of_scope : scope -> string

(** Stable wire/debug name for a verification state. *)
val string_of_verification : verification -> string

(** Local password credential presence. This is [Per_user], not a global lookup key. *)
val password : unit -> key

(** Email identity. Email addresses are trimmed and lowercased. Only [verified:true] email keys are
    safe evidence for automatic account linking. *)
val email : verified:bool -> string -> (key, error) result

(** OAuth identity, normalized by provider name and provider subject. *)
val oauth : provider:string -> subject:string -> (key, error) result

(** OIDC identity, normalized by issuer, connection/client id, and subject. *)
val oidc : issuer:string -> connection:string -> subject:string -> (key, error) result

(** SAML identity, normalized by connection id and NameID.

    When [external_id] is present it is used as the subject because it is typically more stable than
    display-style NameID values. *)
val saml : connection:string -> name_id:string -> ?external_id:string -> unit -> (key, error) result

(** Passkey/WebAuthn credential identity.

    Credential ids are globally unique. [user_handle] is accepted for validation symmetry with
    WebAuthn ceremonies but does not participate in the identity key. Store richer passkey records
    outside this canonical lookup key. *)
val passkey : credential_id:string -> ?user_handle:string -> unit -> (key, error) result

(** SCIM directory identity, normalized by organization id and SCIM external id. *)
val scim : org_id:string -> external_id:string -> (key, error) result

(** Recovery credential presence. This is [Per_user], not a global lookup key. *)
val recovery : name:string -> (key, error) result

(** Identity kind. *)
val kind : key -> kind

(** Identity scope. *)
val scope : key -> scope

(** Provider/namespace component, when the kind has one. *)
val namespace : key -> string option

(** Normalized subject component. *)
val subject : key -> string

(** Verification state, when the kind has one. *)
val verification : key -> verification option

(** Stable ASCII key suitable for database uniqueness/index fields.

    The encoding is length-framed, so arbitrary provider strings cannot collide through separator
    characters. It is intended for storage and comparison, not for human display. Verification state
    is intentionally not part of this key. *)
val stable_key : key -> string

(** Short human/debug label. Do not parse it. Use {!stable_key} for storage. *)
val describe : key -> string

(** Exact normalized identity equality. *)
val equal : key -> key -> bool

(** Total ordering by {!stable_key}. *)
val compare : key -> key -> int

(** [true] when two email keys are the same verified address.

    This is the only built-in cross-identity auto-link signal. Provider profile emails should be
    converted to email keys first and must be verified by the provider before this returns [true]. *)
val same_verified_email : key -> key -> bool

(** [true] when [key] is a verified email identity. *)
val is_verified_email : key -> bool
