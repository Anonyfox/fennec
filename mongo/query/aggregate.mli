(** The aggregation pipeline over a document list — every stage is a pure transform, so the whole
    engine runs in memory with no storage layer (server and browser alike). [$lookup] and
    [$unionWith] resolve a foreign collection's documents through a caller-supplied hook.

    {[
      (* how Minimongo's [aggregate] runs a pipeline over a collection (see {!Minimongo.aggregate}) *)
      let pipeline =
        [ Bson.Document [ ("$match", Document [ ("active", Bool true) ]) ];
          Bson.Document [ ("$group", Document [ ("_id", String "$city");
                                                ("total", Document [ ("$sum", String "$amount") ]) ]) ];
          Bson.Document [ ("$sort", Document [ ("total", Int (-1)) ]) ] ]
      in
      let result = Aggregate.run ~lookup:(fun name -> foreign name) pipeline docs
    ]} *)

(** [run ?lookup pipeline docs] runs [pipeline] (a list of stage documents, e.g. [{$match: …}],
    [{$group: …}]) over [docs] and returns the result documents.

    Supported stages: [$match] [$project] [$addFields]/[$set] [$unset] [$sort] [$limit] [$skip]
    [$count] [$unwind] [$group] [$sortByCount] [$sample] [$replaceRoot]/[$replaceWith] [$lookup]
    [$unionWith] [$facet] [$bucket]. Field/computed expressions use {!Expr}. [lookup name] supplies
    a foreign collection's documents for [$lookup]/[$unionWith] (default: none). [$sample] is a
    deterministic head sample (the pure engine has no RNG); an unknown stage passes its input
    through unchanged.

    Note: [$project]/[$addFields] use {!Expr} for computed fields and have their own inclusion/
    exclusion handling — they do {e not} share {!Projection}'s find-projection path-tree semantics. *)
val run : ?lookup:(string -> Bson.t list) -> Bson.t list -> Bson.t list -> Bson.t list

(** [group spec docs] is the [$group] stage in isolation: group [docs] by the [_id] expression in
    [spec] and apply its accumulator fields, preserving first-seen group order. *)
val group : Bson.t -> Bson.t list -> Bson.t list
