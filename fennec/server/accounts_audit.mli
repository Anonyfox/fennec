(** Account and identity audit events.

    This module defines the shared append-only security event vocabulary for Accounts. It is
    storage-neutral and transport-neutral: auth modules build events, stores append them, and UI or
    export code can query them later. Events must never contain passwords, raw tokens, provider
    access tokens, bearer secrets, or challenge secrets.

    {[
      let store = Accounts_audit.memory_store () in
      let e =
        Accounts_audit.event ~id:"evt-1" ~at:(Unix.gettimeofday ())
          ~target_user_id:"user-1"
          Accounts_audit.Login (Accounts_audit.User "actor-1") Accounts_audit.Success
      in
      ignore (Accounts_audit.append store e);
      Accounts_audit.list ~target_user_id:"user-1" store
    ]} *)

(** Audit event kind. *)
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

(** Actor that caused an event. *)
type actor =
  | Anonymous
  | User of string
  | System of string

(** Account mechanism involved in an event. *)
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

(** Event outcome. Failure reasons should be stable machine-readable codes. *)
type outcome =
  | Success
  | Failure of string

(** Request facts that are safe to store. *)
type request = {
  request_id : string option;
  ip : string option;
  user_agent : string option;
}

(** Empty request context. *)
val empty_request : request

(** Build a request context. Blank values are dropped. *)
val request : ?request_id:string -> ?ip:string -> ?user_agent:string -> unit -> request

(** Append-only audit event. *)
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

(** Build and sanitize an event.

    [id] should be globally unique in the backing store. [metadata] keys are normalized and values
    are redacted when the key name looks secret-bearing. Blank optional values are dropped. *)
val event :
  ?target_user_id:string ->
  ?org_id:string ->
  ?mechanism:mechanism ->
  ?connection_id:string ->
  ?request:request ->
  ?metadata:(string * string) list ->
  id:string ->
  at:float ->
  kind ->
  actor ->
  outcome ->
  event

(** Stable string names for event fields. *)
val string_of_kind : kind -> string
val string_of_actor : actor -> string
val string_of_mechanism : mechanism -> string
val string_of_outcome : outcome -> string

(** [true] when a metadata key should never store its raw value. *)
val sensitive_key : string -> bool

(** Sanitized metadata, sorted by key and deduplicated by last value. *)
val sanitize_metadata : (string * string) list -> (string * string) list

(** Deterministic key/value representation for logs, tests, and simple stores.

    This is not a JSON serializer. It is a compact stable projection that excludes empty optional
    fields and contains only sanitized metadata. *)
val to_fields : event -> (string * string) list

(** Append-only audit store. *)
type store

(** Build an audit store from append/list operations. This is used by the Mongo/Minimongo Accounts
    store; most applications should get it through {!Fennec.Accounts.Store}. *)
val store :
  append:(event -> (unit, string) result) ->
  list:(target_user_id:string option -> org_id:string option -> kind:kind option -> event list) ->
  store

(** Mutex-guarded in-memory append-only store for tests and small deployments. Events are returned
    in append order. *)
val memory_store : unit -> store

(** Append one event. Duplicate event ids are rejected. *)
val append : store -> event -> (unit, string) result

(** List events in append order, optionally filtered by target user, organization, and kind. *)
val list : ?target_user_id:string -> ?org_id:string -> ?kind:kind -> store -> event list
