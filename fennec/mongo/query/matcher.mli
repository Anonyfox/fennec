(** Selector matching — the pure heart of Minimongo. Supports the comparison, membership, array,
    and element operators ([$eq] [$ne] [$exists] [$in] [$nin] [$gt] [$gte] [$lt] [$lte] [$all]
    [$size] [$elemMatch] [$not] [$type] [$mod]) and top-level [$and]/[$or]/[$nor], over dotted
    paths. [$regex] and [$where] are intentionally omitted so the module stays Stdlib-only (and
    JavaScript-safe).

    Two rules follow MongoDB/minimongo: equality and range comparison are {e type-scoped} — a
    number query never matches a string (range ops compare same-type only; equality uses
    {!Bson.equal}, which does treat [Int]/[Int64]/[Float] as numerically equal); and a scalar
    predicate against an {e array} field matches if the array as a whole or any element matches (so
    [{tags:"a"}] matches a document whose [tags] is [["a","b"]]). [$type] additionally understands
    the ["number"] umbrella, and [$exists] treats [false]/[0]/[null] as "must not exist". *)

(** [get_path d "a.b.c"] walks nested documents by dotted path; [None] if any segment is missing or
    an intermediate value is not a document. *)
val get_path : Bson.t -> string -> Bson.t option

(** [doc_matches selector d] — does document [d] satisfy [selector]? Handles top-level
    [$and]/[$or]/[$nor], dotted-path field conditions, operator documents, and implicit equality,
    with an implicit AND over all field conditions. An unknown operator never hides a document. *)
val doc_matches : Bson.t -> Bson.t -> bool
