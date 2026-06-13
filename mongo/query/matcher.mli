(** Selector matching — the pure heart of Minimongo. Supports the comparison, membership, array,
    element, evaluation, and bitwise operators ([$eq] [$ne] [$exists] [$in] [$nin] [$gt] [$gte]
    [$lt] [$lte] [$all] [$size] [$elemMatch] [$not] [$type] [$mod] [$regex] [$bitsAllSet]
    [$bitsAllClear] [$bitsAnySet] [$bitsAnyClear]) and top-level [$and]/[$or]/[$nor], over dotted
    paths. [$regex] is backed by the pure [Re] library (PCRE-ish, with [$options] [i]/[m]/[s]);
    [$where] (arbitrary JavaScript) and [$jsonSchema] are intentionally omitted.

    Two rules follow MongoDB/minimongo: equality and range comparison are {e type-scoped} — a
    number query never matches a string (range ops compare same-type only; equality uses
    {!Bson.equal}, which does treat [Int]/[Int64]/[Float] as numerically equal); and a scalar
    predicate against an {e array} field matches if the array as a whole or any element matches (so
    [{tags:"a"}] matches a document whose [tags] is [["a","b"]]). [$type] additionally understands
    the ["number"] umbrella, and [$exists] treats [false]/[0]/[null] as "must not exist".

    {[
      (* how Minimongo's [find] filters a collection (see {!Minimongo.matched}) *)
      let selector =
        Bson.Document
          [ ("age", Document [ ("$gte", Int 18) ]);
            ("tags", String "vip");                  (* matches a tags array too *)
            ("$or", Array [ Document [ ("vip", Bool true) ];
                            Document [ ("score", Document [ ("$gt", Int 90) ]) ] ]) ]
      in
      let hits = List.filter (Matcher.doc_matches selector) docs in
      let city = Matcher.get_path doc "address.city"   (* dotted-path access *)
    ]} *)

(** [get_path d "a.b.c"] walks nested documents by dotted path; [None] if any segment is missing or
    an intermediate value is not a document. *)
val get_path : Bson.t -> string -> Bson.t option

(** The MongoDB type name of a value — ["int"], ["long"], ["double"], ["string"], ["object"],
    ["array"], ["bool"], ["date"], ["objectId"], etc. (used by [$type] and aggregation's [$type]). *)
val type_name : Bson.t -> string

(** [doc_matches selector d] — does document [d] satisfy [selector]? Handles top-level
    [$and]/[$or]/[$nor], dotted-path field conditions, operator documents, and implicit equality,
    with an implicit AND over all field conditions. An unknown operator never hides a document. *)
val doc_matches : Bson.t -> Bson.t -> bool
