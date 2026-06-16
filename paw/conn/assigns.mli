(** Type-safe, request-scoped key/value storage for a connection (Phoenix's
    [conn.assigns], but typed via [Type.Id] — zero deps, no casts).

    Mint a typed {!key} once (its identity is the [Type.Id], not the name), then {!set} and
    {!get} are checked against the key's type — the recovered value comes back at its real
    type with no annotation or cast:

    {[
      let current_user : User.t key = key "current_user"

      let store = set empty current_user user in
      match get store current_user with
      | Some u -> (* u : User.t *) u.name
      | None -> "anonymous"
    ]}

    Conns wrap this behind {!Conn.assign} / {!Conn.get}; use this module directly only when
    holding the store outside a conn. *)

(** A type-safe request-scoped key/value store (backed by a heterogeneous map). *)
type t

(** A key carrying the type of its value. *)
type 'a key

(** An empty assigns store. The starting value for a fresh conn. *)
val empty : t

(** Mint a fresh typed key. [name] is for debugging only; identity is by
    [Type.Id], so equal names still yield distinct keys. *)
val key : string -> 'a key

(** The debug name of a key. *)
val name : 'a key -> string

(** Set/replace the binding for a key. *)
val set : t -> 'a key -> 'a -> t

(** Typed lookup — [Some v] only when the key matches, recovering [v]'s type. *)
val get : t -> 'a key -> 'a option

(** Whether a key is bound. *)
val mem : t -> 'a key -> bool

(** Get or [Invalid_argument] — for keys an upstream paw guarantees. *)
val get_exn : t -> 'a key -> 'a
