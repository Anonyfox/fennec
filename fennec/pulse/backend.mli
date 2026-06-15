(** The storage seam the reactive surface needs — CRUD over BSON documents (with query options) plus
    field-level live deltas. It now lives in the mongo package ({!Fennec_mongo_backend}); this is a thin
    re-export (type {e equations}, so the types are identical, not merely structurally equal) so the
    reactive layer keeps consuming it as [Fennec_pulse.Backend]:

    {[ module R = Fennec_pulse.Reactive.Make (Backend.Mini)
       let q = Backend.query ~selector:(Bson.doc [ "done", Bson.Bool false ]) ~limit:20 () ]} *)

(** A live-observation handle; call [stop] to detach. *)
type handle = Fennec_mongo_backend.handle = { stop : unit -> unit }

(** A query — a selector plus the options that shape the result set. One shared type across backends. *)
type query = Fennec_mongo_backend.query = {
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

(** What a storage backend must provide — the one seam (see {!Fennec_mongo_backend.S}). *)
module type S = Fennec_mongo_backend.S

(** The in-memory minimongo backend — the default for dev and test. *)
module Mini : S with type collection = Minimongo.t
