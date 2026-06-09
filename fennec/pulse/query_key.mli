(** A stable identity string for a cursor's query — the key the observe multiplexer (RX9) shares one
    backend observe under. Canonicalized so a selector's key ORDER doesn't matter (which only widens
    sharing); two genuinely different queries always map to different keys, so distinct subscriptions
    never collapse onto a single observe. *)

(** [canon b] is the canonical serialization of [b]: Document keys are sorted (so key order is
    irrelevant), arrays keep their order (positional operands like [$and] / [$or]). *)
val canon : Bson.t -> string

(** [of_query ~collection q] is the multiplexer key for a cursor over [collection] running query [q].
    Same (collection, query) ⇒ same key ⇒ one shared backend observe. *)
val of_query : collection:string -> Backend.query -> string
