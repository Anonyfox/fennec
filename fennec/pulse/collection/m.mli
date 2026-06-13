(** Typed modifiers over field handles — the $-operators with the strings taken out of app code.
    Combine with {!all} (grouped by operator); {!raw} keeps the full surface reachable.

    {[ let _ = T.update tasks ~where:[%q id = x] [%set status = "done"]   (* the [%set] DSL *)
       let _ = M.(all [ set Fields.status "done"; inc Fields.n 1; push Fields.tags "t" ]) ]}
*)

type t

val set : 'a Codec.field -> 'a -> t
val unset : 'a Codec.field -> t
val inc : int Codec.field -> int -> t
val inc_f : float Codec.field -> float -> t
val push : 'a list Codec.field -> 'a -> t
val add_to_set : 'a list Codec.field -> 'a -> t
val pull : 'a list Codec.field -> 'a -> t
val pull_all : 'a list Codec.field -> 'a list -> t
val pop_first : _ list Codec.field -> t
val pop_last : _ list Codec.field -> t

(** Set the field to [v] only if [v] is less/greater than the current value ($min/$max). *)
val min : 'a Codec.field -> 'a -> t
val max : 'a Codec.field -> 'a -> t

val mul : int Codec.field -> int -> t
val mul_f : float Codec.field -> float -> t

(** Set only on an upsert-insert ($setOnInsert). *)
val set_on_insert : 'a Codec.field -> 'a -> t

(** Rename a field to another field of the same type ($rename). *)
val rename : 'a Codec.field -> to_:'a Codec.field -> t

val all : t list -> t
val raw : Bson.t -> t
val to_bson : t -> Bson.t
