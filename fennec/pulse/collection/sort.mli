(** Typed sort keys over field handles — [Sort.by [ asc Fields.title; desc Fields.created ]]; a
    renamed field is a compile error. Order is the list order. *)

type t

val asc : _ Codec.field -> t
val desc : _ Codec.field -> t
val by : t list -> t
val raw : Bson.t -> t
val to_bson : t -> Bson.t
