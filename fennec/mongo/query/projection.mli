(** Field projection — include/exclude with [_id] handling, over first-segment dotted paths. Takes
    the projection spec as a BSON document (Minimongo's [fields] / [projection] option). *)

(** A compiled projection: an include-list (+ whether to keep [_id]), an exclude-list (+ keep [_id]),
    or no projection at all. *)
type t = No_projection | Include of string list * bool | Exclude of string list * bool

(** Compile a projection spec document into a {!t}: [{a:1; b:1}] includes those fields, [{a:0}]
    excludes, and [_id] is kept unless explicitly set to [0]/[false]. A non-document or empty spec
    yields {!No_projection}. *)
val of_fields : Bson.t -> t

(** [apply proj d] keeps only the fields [proj] selects (or drops the excluded ones). *)
val apply : t -> Bson.t -> Bson.t

(** [cleared proj names] filters a list of unset/cleared field names down to those the projection
    would actually surface — so unsetting a hidden field is not reported to a client. *)
val cleared : t -> string list -> string list
