(** Update-modifier engine: applies a Mongo update document to an in-memory BSON document. Pure.

    Supported operators: [$set] [$unset] [$inc] [$mul] [$min] [$max] [$rename] [$setOnInsert]
    (insert-time only) [$push] (+ [$each]) [$addToSet] (+ [$each]) [$pull] (value or sub-selector)
    [$pullAll] [$pop]. Dotted paths target nested fields. A modifier with no [$]-operators is a full
    replacement (preserving [_id]). *)

(** [apply ?insert doc modifier] applies [modifier] to [doc]. With [~insert:true], [$setOnInsert]
    is also applied (use it only when seeding a document on upsert). A non-operator [modifier]
    replaces the document wholesale, preserving its [_id]. *)
val apply : ?insert:bool -> Bson.t -> Bson.t -> Bson.t
