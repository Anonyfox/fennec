(** Declarative indexes over field handles — declared with the collection, ensured at boot. *)

type t

val asc : _ Codec.field -> t
val desc : _ Codec.field -> t
val compound : t list -> t
val unique : t -> t

(** The Mongo key spec ([{ field: 1|-1, … }]). *)
val keys_bson : t -> Bson.t

val is_unique : t -> bool
