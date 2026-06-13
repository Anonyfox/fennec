(** Declarative indexes over field handles — declared once with the model, reconciled at boot
    ({!declare} + the runtime's [ensure_indexes]). A renamed field is a compile error here too.
    The NAME encodes the spec under an [fx_] prefix so reconcile is name-based and only
    fennec-managed indexes are ever auto-dropped. *)

type t

(** Ascending / descending single-key index. *)
val asc : _ Codec.field -> t
val desc : _ Codec.field -> t

(** A compound index from several single keys ([compound [ asc a; desc b ]]). *)
val compound : t list -> t

(** Mark unique (enforced by mongod AND, for parity, by the in-memory engine). *)
val unique : t -> t

(** Register a collection's indexes by name (used by {!Def.index}; the runtime reconciles them at
    boot). Prefer {!Def.index} which supplies the name. *)
val register : string -> t list -> unit

(** The declared indexes for a collection name (the runtime's reconcile reads this). *)
val for_collection : string -> t list

(** The deterministic fennec index name (encodes fields+directions+unique). *)
val name : t -> string

(** Whether a backend index name is fennec-managed (safe to auto-drop when undeclared). *)
val is_fennec_name : string -> bool

val is_unique : t -> bool

(** The Mongo key spec ([{ field: 1|-1, … }]). *)
val keys_bson : t -> Bson.t
