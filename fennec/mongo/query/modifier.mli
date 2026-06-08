(** Update-modifier engine: applies a Mongo update document to an in-memory BSON document. Pure.

    Supported operators: [$set] [$unset] [$inc] [$mul] [$min] [$max] [$rename] [$setOnInsert]
    (insert-time only) [$push] (with [$each]/[$position]/[$sort]/[$slice]) [$addToSet] (+ [$each])
    [$pull] [$pullAll] [$pop] [$bit] ([and]/[or]/[xor]). Dotted paths target nested fields. A
    modifier with no [$]-operators is a full replacement (preserving [_id]). Not handled here:
    [$currentDate] (needs a clock — the framework layer rewrites it to a [$set] with the timestamp)
    and the positional array operators [$] / [$[]] / [$[<id>]] (need the query selector /
    arrayFilters).

    Edge cases worth knowing: [$inc]/[$mul] promote across [Int]/[Int64]/[Float] and leave a
    present-but-non-numeric field {e unchanged} (they never overwrite it); [$mul] of a missing field
    yields zero, [$inc] of a missing field yields the increment. [$min]/[$max] compare with the
    total {!Bson.compare}, so they work across types. [$pull] removes by operator predicate when its
    argument is an operator document (e.g. [{$gt:5}]) and by structural equality otherwise (so you
    {e can} pull a whole sub-document by value). An unknown [$]-operator is ignored. *)

(** [apply ?insert doc modifier] applies [modifier] to [doc]. With [~insert:true], [$setOnInsert]
    is also applied (use it only when seeding a document on upsert). A non-operator [modifier]
    replaces the document wholesale, preserving its [_id]. *)
val apply : ?insert:bool -> Bson.t -> Bson.t -> Bson.t

(** [is_operator_doc d] — whether [d] is an update {e modifier} document (has at least one
    [$]-prefixed key) as opposed to a replacement document. Useful to a caller deciding how to treat
    an update before applying it. *)
val is_operator_doc : Bson.t -> bool

(** [set doc "a.b" v] sets the (possibly dotted) path to [v], creating intermediate documents — the
    direct, single-field form of [$set] (no operator-document round-trip). *)
val set : Bson.t -> string -> Bson.t -> Bson.t

(** [unset doc "a.b"] removes the (possibly dotted) path — the direct form of [$unset]. *)
val unset : Bson.t -> string -> Bson.t
