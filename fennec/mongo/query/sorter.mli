(** Compile a sort spec ([{field: 1|-1, …}]) into a document comparator and apply it. Reuses the
    matcher's value comparison; missing fields sort before present ones; an empty spec keeps input
    order. Stable. *)

(** [of_spec spec] is a comparator [doc -> doc -> int] for [spec]; an empty/absent spec yields the
    constant-[0] comparator (which preserves input order under a stable sort). *)
val of_spec : Bson.t -> Bson.t -> Bson.t -> int

(** [sort spec docs] stably sorts [docs] by [spec]; returns [docs] unchanged for an empty spec. *)
val sort : Bson.t -> Bson.t list -> Bson.t list
