(** Type-safe, request-scoped key/value storage for a connection (Phoenix's
    [conn.assigns], but typed via [Type.Id] — zero deps, no casts). *)

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
