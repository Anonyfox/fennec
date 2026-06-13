(** Typed sort keys over field handles — [Sort.by [ asc Fields.title; desc Fields.created ]]; a
    renamed field is a compile error. Order is the list order. 

    {[ let _ = Task.find ~sort:[%sort priority desc, title asc] ()   (* the [%sort] DSL *)
       let _ = Sort.(by [ desc Fields.priority; asc Fields.title ])  (* explicit form *) ]}
*)

type t

val asc : _ Codec.field -> t
val desc : _ Codec.field -> t
val by : t list -> t
val raw : Bson.t -> t
val to_bson : t -> Bson.t
