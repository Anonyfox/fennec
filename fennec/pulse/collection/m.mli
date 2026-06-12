(** Typed modifiers over field handles — the $-operators with the strings taken out of app code.
    Combine with {!all} (grouped by operator); {!raw} keeps the full surface reachable. *)

type t

val set : 'a Codec.field -> 'a -> t
val unset : 'a Codec.field -> t
val inc : int Codec.field -> int -> t
val inc_f : float Codec.field -> float -> t
val push : 'a list Codec.field -> 'a -> t
val add_to_set : 'a list Codec.field -> 'a -> t
val pull : 'a list Codec.field -> 'a -> t
val all : t list -> t
val raw : Bson.t -> t
val to_bson : t -> Bson.t
