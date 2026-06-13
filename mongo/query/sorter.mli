(** Compile a sort spec ([{field: 1|-1, …}]) into a document comparator and apply it. Reuses the
    matcher's value comparison; missing fields sort before present ones; an empty spec keeps input
    order. Stable.

    {[
      (* how Minimongo's [windowed] orders matched documents *)
      let spec = Bson.Document [ ("age", Int (-1)); ("name", Int 1) ] in
      let ordered = Sorter.sort spec docs in
      (* the bare comparator, e.g. to merge sorted lists in the observe engine *)
      let cmp = Sorter.of_spec spec in
      let _ : int = cmp doc_a doc_b
    ]} *)

(** [of_spec spec] is a comparator [doc -> doc -> int] for [spec]; an empty/absent spec yields the
    constant-[0] comparator (which preserves input order under a stable sort). *)
val of_spec : Bson.t -> Bson.t -> Bson.t -> int

(** [sort spec docs] stably sorts [docs] by [spec]; returns [docs] unchanged for an empty spec. *)
val sort : Bson.t -> Bson.t list -> Bson.t list
