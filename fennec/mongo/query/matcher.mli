(** Selector matching — the pure heart of Minimongo. Supports the comparison, membership, array,
    and element operators ([$eq] [$ne] [$exists] [$in] [$nin] [$gt] [$gte] [$lt] [$lte] [$all]
    [$size] [$elemMatch] [$not] [$type] [$mod]) and top-level [$and]/[$or]/[$nor], over dotted
    paths. [$regex] and [$where] are intentionally omitted so the module stays Stdlib-only (and
    JavaScript-safe). *)

(** A value comparison usable for range operators {e and} sorting: [Some c] (like [compare]), or
    [None] when the two values aren't comparable (e.g. a number vs a string). Numbers — including
    [Date] — compare numerically; strings and bools compare naturally. *)
val bson_cmp : Bson.t -> Bson.t -> int option

(** [get_path d "a.b.c"] walks nested documents by dotted path; [None] if any segment is missing or
    an intermediate value is not a document. *)
val get_path : Bson.t -> string -> Bson.t option

(** [doc_matches selector d] — does document [d] satisfy [selector]? Handles top-level
    [$and]/[$or]/[$nor], dotted-path field conditions, operator documents, and implicit equality,
    with an implicit AND over all field conditions. An unknown operator never hides a document. *)
val doc_matches : Bson.t -> Bson.t -> bool

(** Whether a value should be treated as a sub-selector (a [Document]) rather than an equality
    target — the heuristic [$pull] uses to decide between matching and removing-equal. *)
val is_selector_like : Bson.t -> bool
