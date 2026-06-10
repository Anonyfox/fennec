(** In-memory MongoDB — a [Minimongo.t] is one collection: the [_id]-keyed store, mutations,
    cursors, and a reactive observe engine. A mutation emits a change event (the "simulated change
    stream"); the observe engines recompute off those events with the pure matcher/diff core. No Eio,
    no polling, no systhreads. Cross-compiles to JavaScript — and it is the default backend for dev
    and test.

    This module is the {b front door}: build selectors, update documents, and projections as plain
    {!Bson.t} values and pass them to [find]/[update]/[aggregate]. The [Query.*] modules
    ([Matcher]/[Modifier]/[Projection]/[Sorter]/[Aggregate]/[Expr]/[Geo]) are the underlying engine
    and are only needed for advanced or standalone (collection-less) use.

    {b Thread-safety:} every operation is safe to call from any OCaml 5 domain. Reads snapshot and
    mutations commit under a per-collection lock; change events are delivered through {!Fanout}
    OUTSIDE all locks, in commit order, by one drainer at a time — so observers see a linearized
    stream, may re-entrantly mutate the collection, and a slow/suspending observer blocks nothing but
    its own delivery. Two caveats follow from "delivery happens outside the lock": under concurrent
    writers a mutation may return before its event is delivered (the active drainer delivers it), and
    a custom [gen_id] must be pure/non-blocking (it runs under the lock). On a single domain — and
    compiled to JavaScript — behavior is exactly the old synchronous delivery.

    Insert is O(1) and store lookups are total, so a re-entrant observer that mutates the collection
    during a notification can never raise. *)

(** The ordered event fan-out the change stream rides on — reusable wherever the same
    commit-ordered, deliver-outside-locks discipline is needed (the framework's observe multiplexer
    uses it too). *)
module Fanout : module type of Fanout

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

(** [on_drained t k] runs [k] once every change event committed {e so far} has been delivered to the
    observers — the write-fence primitive (fires immediately when idle; see {!Fanout.on_drained}). *)
val on_drained : t -> (unit -> unit) -> unit

(** {2 Mutations} (each emits a change event) *)

(** [insert t d] inserts [d], minting an [_id] if absent, and returns the id. *)
val insert : t -> doc -> string

(** [update t ?multi ?upsert selector modifier] applies [modifier] to documents matching [selector]
    and returns the number affected. [~multi:true] updates all matches (default: the first only);
    [~upsert:true] inserts a document seeded from the selector's plain-equality fields (embedded
    documents kept; operator expressions dropped) when none match. *)
val update : t -> ?multi:bool -> ?upsert:bool -> doc -> doc -> int

(** [remove t selector] removes all documents matching [selector]; returns the number removed. *)
val remove : t -> doc -> int

(** [remove_id t id] removes the single document whose id is [id] — an O(1) hash delete (plus one
    order-list compaction), skipping the O(n) selector scan [remove] runs. Returns whether a document
    was present. *)
val remove_id : t -> string -> bool

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

(** Whether {e no} document matches the cursor's selector (short-circuits). *)
val is_empty : cursor -> bool

(** [for_each cur f] applies [f] to each fetched document. *)
val for_each : cursor -> (doc -> unit) -> unit

(** [map cur f] maps [f] over the fetched documents. *)
val map : cursor -> (doc -> 'a) -> 'a list

(** The first document of a cursor (after sort/skip), or [None]. *)
val first : cursor -> doc option

(** [find_one t ?selector ?sort ?skip ?fields ()] — the collection-level [findOne]: the first
    document matching [selector] (after [sort]/[skip]), or [None]. *)
val find_one :
  t -> ?selector:doc -> ?sort:doc -> ?skip:int -> ?fields:doc -> unit -> doc option

(** [aggregate ?lookup t pipeline] runs an aggregation {!Query.Aggregate} pipeline over the
    collection's documents (in insertion order). [lookup name] supplies a foreign collection's
    documents for the [$lookup] / [$unionWith] stages (default: none). *)
val aggregate : ?lookup:(string -> doc list) -> t -> Bson.t list -> doc list

(** [distinct t ~key ?selector ()] — the distinct values of [key] across documents matching
    [selector]. Array values are unwrapped (distinct over an array field yields its elements);
    results are deduped by BSON equality, in first-seen order. *)
val distinct : t -> key:string -> ?selector:doc -> unit -> doc list

(** {2 Reactive observation} *)

(** [observe_changes cur ?added ?changed ?removed ()] — field-level, unordered membership tracking.
    Fires [added id fields] for each doc in the initial window, then live: [added]/[removed] as docs
    enter/leave the result set, [changed id changed_fields cleared_names] as winning fields change.
    Honors selector + projection on live deltas — and a WINDOWED cursor (sort + skip/limit) maintains
    its window live: a doc entering displaces the boundary doc ([added] + [removed]), one leaving
    promotes the next, and the tracked set never exceeds the window. Relevance is exact (a change to
    a skipped-prefix doc that shifts the window is caught via its pre-image). Costs: an un-windowed
    delta is O(fields of the one changed doc); a windowed delta that can affect the window
    re-snapshots + diffs — O(M log M) over the M matching docs; everything else is O(1) — including,
    via the boundary short-circuit, the dominant miss case of a full skip-less window (a matching doc
    sorting strictly below the last window doc cannot enter, so a hot leaderboard's losing writes
    cost one match + one compare). This is the incremental path — prefer it where positional
    ordering callbacks are not needed. *)
val observe_changes :
  cursor ->
  ?added:(string -> doc -> unit) ->
  ?changed:(string -> doc -> string list -> unit) ->
  ?removed:(string -> unit) ->
  unit ->
  handle

(** [observe cur ?added ?changed ?removed ()] — document-level. Recomputes the ordered window and
    diffs on each change, so sort/skip/limit are honored and callbacks receive full documents
    ([changed new old]). Heavier than {!observe_changes}; use it only when order matters. *)
val observe :
  cursor ->
  ?added:(doc -> unit) ->
  ?changed:(doc -> doc -> unit) ->
  ?removed:(doc -> unit) ->
  unit ->
  handle
