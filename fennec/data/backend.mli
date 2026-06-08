(** The storage backend the reactive surface needs — CRUD over BSON documents (with query options)
    plus field-level live deltas. This is the seam between the reactive layer and the data engine:
    any module satisfying {!S} can host the full Meteor-style surface. The in-memory minimongo
    backend ({!Mini}) is here; a native libmongoc backend is a later addition behind {!S}. *)

(** A live-observation handle; call [stop] to detach. *)
type handle = { stop : unit -> unit }

(** A query — a selector plus the options that shape the result set. One shared type across
    backends. *)
type query = {
  selector : Bson.t;
  sort : Bson.t;
  skip : int;
  limit : int;
  fields : Bson.t;  (** projection spec *)
}

(** [query ?selector ?sort ?skip ?limit ?fields ()] builds a {!query}; each part defaults to
    empty / zero / unbounded. *)
val query :
  ?selector:Bson.t -> ?sort:Bson.t -> ?skip:int -> ?limit:int -> ?fields:Bson.t -> unit -> query

(** What a storage backend must provide. *)
module type S = sig
  (** A collection handle in this backend. *)
  type collection

  (** [insert c d] inserts [d] and returns its [_id]. *)
  val insert : collection -> Bson.t -> string

  (** [update c ~multi ~upsert selector modifier] applies the modifier to matching documents and
      returns the number affected. *)
  val update : collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int

  (** [remove c selector] removes matching documents and returns the count. *)
  val remove : collection -> Bson.t -> int

  (** The documents matching the query (windowed by skip/limit, with the projection applied). *)
  val find : collection -> query -> Bson.t list

  (** The first matching document, or [None]. *)
  val find_one : collection -> query -> Bson.t option

  (** The number of documents matching the selector. *)
  val count : collection -> Bson.t -> int

  (** [aggregate c pipeline] runs the aggregation pipeline (a list of stage documents, e.g.
      [[doc [ "$match", … ]; doc [ "$group", … ]]]) and returns the result documents. One-shot, not
      reactive — the output rows are computed, not stored collection documents. *)
  val aggregate : collection -> Bson.t list -> Bson.t list

  (** Field-level live observation: [added id fields], [changed id changed_fields cleared_names],
      [removed id], honoring the query's selector and projection. *)
  val observe_changes :
    collection ->
    query ->
    added:(string -> Bson.t -> unit) ->
    changed:(string -> Bson.t -> string list -> unit) ->
    removed:(string -> unit) ->
    handle
end

(** The in-memory minimongo backend — the default for dev and test. *)
module Mini : S with type collection = Minimongo.t
