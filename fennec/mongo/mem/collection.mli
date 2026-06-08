(** In-memory MongoDB collection — the [_id]-keyed store, mutations, cursors, and a reactive observe
    engine. A mutation synchronously emits a change event (the "simulated change stream"); the
    observe engines recompute off those events with the pure matcher/diff core. No Eio, no polling,
    no systhreads. Pure, so it cross-compiles to JavaScript — and it is the default backend for dev
    and test. *)

(** A document — a BSON value (in practice a [Document]). *)
type doc = Bson.t

(** The kind of mutation a change event describes. *)
type change_op = Insert | Update | Remove

(** A simulated change-stream event: the op, the document [id], and the full document after
    ([new_doc], [None] on remove) and before ([old_doc], [None] on insert) the mutation. *)
type change = {
  op : change_op;
  id : string;
  new_doc : doc option;
  old_doc : doc option;
}

(** A raw change-stream subscriber. *)
type observer = change -> unit

(** A collection: an [_id → document] store with stable insertion order, an id generator, and a set
    of observers. *)
type t

(** [create ?gen_id ()] — a fresh empty collection. [gen_id] mints the [_id] for inserts that omit
    one (default: {!Query.Id.random_id}). *)
val create : ?gen_id:(unit -> string) -> unit -> t

(** A live subscription handle; call [stop] to detach. *)
type handle = { stop : unit -> unit }

(** [watch t f] subscribes [f] to the raw insert/update/remove change stream (before any
    selector/projection). Returns a {!handle}. The reactive cursor engines are built on this. *)
val watch : t -> observer -> handle

(** {2 Mutations} (each emits a change event) *)

(** [insert t d] inserts [d], minting an [_id] if absent, and returns the id. *)
val insert : t -> doc -> string

(** [update t ?multi ?upsert selector modifier] applies [modifier] to documents matching [selector]
    and returns the number affected. [~multi:true] updates all matches (default: the first only);
    [~upsert:true] inserts a seeded document when none match. *)
val update : t -> ?multi:bool -> ?upsert:bool -> doc -> doc -> int

(** [remove t selector] removes all documents matching [selector]; returns the number removed. *)
val remove : t -> doc -> int

(** {2 Cursors & queries} *)

(** A query over a collection: selector + sort/skip/limit/projection. Lazy — evaluated by
    {!fetch}/{!count}/{!observe_changes}/etc. *)
type cursor

(** [find t ?selector ?sort ?skip ?limit ?fields ()] builds a cursor. [limit = 0] is unbounded;
    [fields] is a projection spec ([{a:1}] / [{a:0}]). *)
val find :
  t ->
  ?selector:doc ->
  ?sort:doc ->
  ?skip:int ->
  ?limit:int ->
  ?fields:doc ->
  unit ->
  cursor

(** The matching documents in order, windowed by skip/limit, with the projection applied. *)
val fetch : cursor -> doc list

(** How many documents match the cursor's selector (ignores skip/limit/projection). *)
val count : cursor -> int

(** [for_each cur f] applies [f] to each fetched document. *)
val for_each : cursor -> (doc -> unit) -> unit

(** [map cur f] maps [f] over the fetched documents. *)
val map : cursor -> (doc -> 'a) -> 'a list

(** The first matching document (after sort/skip), or [None]. *)
val find_one : cursor -> doc option

(** {2 Reactive observation} *)

(** [observe_changes cur ?added ?changed ?removed ()] — field-level, unordered membership tracking.
    Fires [added id fields] for each doc in the initial window, then live: [added]/[removed] as docs
    enter/leave the selector, [changed id changed_fields cleared_names] as winning fields change.
    Honors selector + projection on live deltas; skip/limit affect only the initial snapshot. *)
val observe_changes :
  cursor ->
  ?added:(string -> doc -> unit) ->
  ?changed:(string -> doc -> string list -> unit) ->
  ?removed:(string -> unit) ->
  unit ->
  handle

(** [observe cur ?added ?changed ?removed ()] — document-level. Recomputes the ordered window and
    diffs on each change, so sort/skip/limit are honored and callbacks receive full documents
    ([changed new old]). *)
val observe :
  cursor ->
  ?added:(doc -> unit) ->
  ?changed:(doc -> doc -> unit) ->
  ?removed:(doc -> unit) ->
  unit ->
  handle
