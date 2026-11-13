(** Typed sort keys over field handles — [Sort.by [ asc Fields.title; desc Fields.created ]]; a
    renamed field is a compile error. Order is the list order.

    {[ let _ = Task.find ~sort:[%sort priority desc, title asc] ()   (* the [%sort] DSL *)
       let _ = Sort.(by [ desc Fields.priority; asc Fields.title ])  (* explicit form *) ]}
*)

(** A sort specification. *)
type t

(** Sort by the field, ascending. *)
val asc : _ Sift.field -> t

(** Sort by the field, descending. *)
val desc : _ Sift.field -> t

(** Combine sort keys, in priority order. *)
val by : t list -> t

(** An untyped sort document, as is (the escape hatch). *)
val raw : Bson.t -> t

(** The Mongo sort document. *)
val to_bson : t -> Bson.t
