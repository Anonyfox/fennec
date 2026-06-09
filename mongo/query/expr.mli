(** Aggregation expression evaluator — evaluates a MongoDB aggregation expression against a document
    to a {!Bson.t}. Pure. Supports field paths ([$a.b]), system variables ([$$ROOT]/[$$CURRENT]) and
    user variables ([$$x], bound by [$map]/[$filter]), literals, and a broad subset of the operator
    expressions, plus the [$group] accumulators.

    Normally reached through {!Aggregate.run} (pipeline stages call it for computed fields); call
    [eval] directly only to evaluate an expression standalone. *)

(** Aggregation truthiness: [false], [0], [Null] (and a missing value) are falsy; all else truthy. *)
val truthy : Bson.t -> bool

(** [eval ?vars expr doc] evaluates [expr] against [doc]. Operator expressions supported: arithmetic
    ([$add] [$subtract] [$multiply] [$divide] [$mod] [$abs] [$ceil] [$floor] [$round]), comparison
    ([$eq] [$ne] [$gt] [$gte] [$lt] [$lte] [$cmp]), boolean ([$and] [$or] [$not]), conditional
    ([$cond] [$ifNull] [$switch]), string ([$concat] [$toLower] [$toUpper] [$strLenCp] [$split]
    [$substr]), array ([$size] [$isArray] [$arrayElemAt] [$concatArrays] [$reverseArray] [$in]
    [$filter] [$map]), and type/conversion ([$type] [$toString] [$toInt] [$toDouble] [$toBool]
    [$literal]). [vars] seeds user variables; an unknown operator yields [Null]. *)
val eval : ?vars:(string * Bson.t) list -> Bson.t -> Bson.t -> Bson.t

(** [accumulate acc docs] evaluates a [$group] accumulator over one group's documents. Supported:
    [{$sum: e}], [{$avg: e}], [{$min: e}], [{$max: e}], [{$first: e}], [{$last: e}], [{$push: e}],
    [{$addToSet: e}], [{$count: {}}]. *)
val accumulate : Bson.t -> Bson.t list -> Bson.t
