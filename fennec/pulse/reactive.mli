(** The reactive Meteor-style surface as a functor over a storage {!Backend.S} — one surface,
    instantiated on the in-memory engine ({!Mini}, pure → also JS) and, later, a native driver. The
    abstract collection type lets a single suite prove feature parity across backends.

    Reactivity is callback-driven (the backend's [observe_changes]); there is no Tracker here — a
    client supplies its own reactive graph (e.g. Fur signals) on top of the live deltas. *)

(** A document — a BSON value (in practice a [Document]). *)
type doc = Bson.t

(** A live-observation handle; call [stop] to detach. *)
type live_handle = { stop : unit -> unit }

(** One field-level change emitted by a publication's live query — the fennec-internal {e Beat} of
    the Pulse. A DDP session lowers each beat onto the wire ([Fennec_ddp.Message]). This is distinct
    from the Meteor-compatible {!Collection.observe_changes} callbacks, which stay
    [added]/[changed]/[removed]; a beat is the same change reified as a value. *)
type beat =
  | Added of { collection : string; id : string; fields : (string * doc) list }
  | Changed of {
      collection : string;
      id : string;
      fields : (string * doc) list;
      cleared : string list;
    }
  | Removed of { collection : string; id : string }

(** The full reactive surface produced by {!Make}. *)
module type REACTIVE = sig
  (** The backend collection type this instance is built on. *)
  type backend_collection

  (** Raised by methods and by denied client writes: a [code] and a [reason] message. *)
  exception Error of { code : string; reason : string }

  (** The context a method runs in: the calling user (if any) and whether this is a client-side
      latency-compensation simulation. *)
  type invocation = { user_id : string option; is_simulation : bool }

  (** A server method: [invocation -> args -> result]. *)
  type method_handler = invocation -> doc list -> doc

  (** Register named methods (RPC handlers). *)
  val methods : (string * method_handler) list -> unit

  (** [call ?user_id name args] invokes a registered method, returning its result (or raising
      {!Error} if absent). *)
  val call : ?user_id:string option -> string -> doc list -> doc

  (** [apply ?user_id ?is_simulation name args] is [call] with explicit invocation flags. *)
  val apply : ?user_id:string option -> ?is_simulation:bool -> string -> doc list -> doc

  (** How a collection mints [_id]s: 17-char strings ([STRING]) or 24-hex ObjectIds ([MONGO]). *)
  type id_generation = STRING | MONGO

  (** MongoDB ObjectIds (24-char hex). *)
  module ObjectID : sig
    (** An ObjectId, as its hex string. *)
    type t = string

    (** [make ?hex ()] validates and returns [hex], or mints a fresh ObjectId. *)
    val make : ?hex:string -> unit -> t

    (** Whether a string is a valid 24-hex ObjectId. *)
    val is_valid : string -> bool

    (** The hex string of an ObjectId. *)
    val to_hex_string : t -> string

    (** ObjectId equality. *)
    val equals : t -> t -> bool
  end

  (** A reactive collection over the backend. *)
  module Collection : sig
    (** A collection handle. *)
    type t

    (** A lazy query over a collection. *)
    type cursor

    (** [create ?id_generation ?transform ?name backend] wraps a backend collection. [transform] is
        applied to user-visible documents (not to stored ones). *)
    val create :
      ?id_generation:id_generation ->
      ?transform:(doc -> doc) ->
      ?name:string ->
      backend_collection ->
      t

    (** The collection's name. *)
    val name : t -> string

    (** [forget name] drops a named collection from this instance's [$lookup] registry — reclaim it
        for a dynamically-named (e.g. per-tenant) collection so it isn't pinned for the process
        lifetime. A no-op for an unregistered name. *)
    val forget : string -> unit

    (** [insert c d] inserts [d] (minting an [_id] per [id_generation] if absent) and returns it. *)
    val insert : t -> doc -> string

    (** [find c ?selector ?sort ?skip ?limit ?fields ?transform ()] builds a cursor. [transform]
        overrides the collection transform for this cursor ([Some None] disables it). *)
    val find :
      t ->
      ?selector:doc ->
      ?sort:doc ->
      ?skip:int ->
      ?limit:int ->
      ?fields:doc ->
      ?transform:(doc -> doc) option ->
      unit ->
      cursor

    (** The cursor's documents (windowed + projected, with the transform applied). *)
    val fetch : cursor -> doc list

    (** [map cur f] maps over the cursor's documents. *)
    val map : cursor -> (doc -> 'a) -> 'a list

    (** [for_each cur f] iterates the cursor's documents. *)
    val for_each : cursor -> (doc -> unit) -> unit

    (** The first document matching the selector (after sort), or [None]. *)
    val find_one : t -> ?selector:doc -> ?sort:doc -> ?fields:doc -> unit -> doc option

    (** The number of documents matching the selector (ignores skip/limit). *)
    val count : t -> ?selector:doc -> unit -> int

    (** [aggregate c pipeline] runs the aggregation pipeline (a list of stage documents) and returns
        the result documents. One-shot, not a reactive cursor; rows are computed, so the collection
        transform is NOT applied. *)
    val aggregate : t -> doc list -> doc list

    (** [distinct c ~key ?selector ()] — the distinct values of [key] across documents matching
        [selector] (array values unwrapped, deduped). Untransformed. *)
    val distinct : t -> key:string -> ?selector:doc -> unit -> doc list

    (** [update c ?multi ?upsert selector modifier] — the number affected. *)
    val update : t -> ?multi:bool -> ?upsert:bool -> doc -> doc -> int

    (** The result of an {!upsert}: documents affected, and the inserted [_id] if it inserted. *)
    type upsert_result = { number_affected : int; inserted_id : string option }

    (** [upsert c ?multi selector modifier] updates or inserts, reporting whether it inserted (it
        detects a true insert by snapshotting, since a Mongo upsert-insert reports nModified=0). *)
    val upsert : t -> ?multi:bool -> doc -> doc -> upsert_result

    (** [remove c selector] — the number removed. *)
    val remove : t -> doc -> int

    (** Field-level live observation over a cursor: [added id fields], [changed id fields cleared],
        [removed id]. *)
    val observe_changes :
      cursor ->
      ?added:(string -> doc -> unit) ->
      ?changed:(string -> doc -> string list -> unit) ->
      ?removed:(string -> unit) ->
      unit ->
      live_handle

    (** Document-level ordered observation: re-fetches the sorted window and diffs, so sort/skip/
        limit are honored and callbacks receive whole documents (with positional [added_at]/
        [removed_at]). *)
    val observe :
      cursor ->
      ?added:(doc -> unit) ->
      ?changed:(doc -> doc -> unit) ->
      ?removed:(doc -> unit) ->
      ?added_at:(doc -> int -> string option -> unit) ->
      ?removed_at:(doc -> int -> unit) ->
      unit ->
      live_handle

    (** [allow c ?insert ?update ?remove ()] adds client-write allow rules (secures the collection;
        a write is permitted if some allow rule passes and no deny rule matches). *)
    val allow :
      t ->
      ?insert:(string option -> doc -> bool) ->
      ?update:(string option -> doc -> bool) ->
      ?remove:(string option -> doc -> bool) ->
      unit ->
      unit

    (** [deny c ?insert ?update ?remove ()] adds client-write deny rules (a matching deny overrides
        any allow). *)
    val deny :
      t ->
      ?insert:(string option -> doc -> bool) ->
      ?update:(string option -> doc -> bool) ->
      ?remove:(string option -> doc -> bool) ->
      unit ->
      unit

    (** [insert_from_client ?user_id c d] inserts subject to the allow/deny rules, raising {!Error}
        ["403"] if denied (or if the collection has no allow rules). *)
    val insert_from_client : ?user_id:string option -> t -> doc -> string
  end

  (** What a publication returns: one cursor or several (a multi-collection publication). *)
  type cursor_kind = Cursor of Collection.cursor | Cursors of Collection.cursor list

  (** [cursor coll ?selector ?sort ?skip ?limit ?fields ()] is a convenience cursor builder for
      publications. *)
  val cursor :
    Collection.t ->
    ?selector:doc ->
    ?sort:doc ->
    ?skip:int ->
    ?limit:int ->
    ?fields:doc ->
    unit ->
    Collection.cursor

  (** [publish name f] registers a publication: [f params] yields the cursor(s) to feed subscribers,
      where [params] are the subscription's arguments (the client's [subscribe ~params]). Return a
      cursor whose selector uses [params] for a parameterized publication. *)
  val publish : string -> (doc list -> cursor_kind) -> unit

  (** A live subscription: read the merged documents (all, or per collection), the collection names,
      readiness, and a [stop]. *)
  type subscription = {
    documents : unit -> (string * doc) list;
    documents_of : string -> (string * doc) list;
    collections : unit -> string list;
    is_ready : unit -> bool;
    stop : unit -> unit;
  }

  (** [subscribe name] is a server-side {e snapshot / inspection} helper, NOT the live transport.
      It runs the named publication with EMPTY params into a per-collection merge box and returns a
      {!subscription} whose [documents]/[documents_of] are snapshot getters (current merged state) and
      whose [is_ready] is always [true]. It does not stream and cannot carry subscription arguments.
      For anything real — parameterized subscriptions, live per-document deltas, the DDP session —
      use {!run_publication} ([~params], delta-driven, multiplexer-shared); that is what a session
      feeds its sink from (DATAFLOW.md §5b). Reach for [subscribe] only to snapshot a no-arg
      publication in-process (e.g. a test or an admin inspector). *)
  val subscribe : string -> subscription

  (** The names of the registered publications (e.g. to wire a transport). *)
  val publications : unit -> string list

  (** The names of the registered methods. *)
  val method_names : unit -> string list

  (** [run_publication name ~params ~on] runs the named publication's cursors (built from [params] —
      the subscription's arguments), delivering each field-level observe delta to [on] as a {!beat}
      (the [collection] is per document), and returns a handle that stops every cursor. This is the
      delta-driven entry a DDP session feeds its sink from — unlike {!subscribe} it keeps no merge
      box. Each cursor is backed by a SHARED backend observe (the multiplexer): every subscription
      with the same (collection, query) rides one observe, yet [on] still receives the full initial
      state synchronously before this returns — a fresh observe replays it, a late joiner gets the
      current state replayed — so emitting [ready] after return stays correct. An unknown publication
      yields a no-op handle. *)
  val run_publication : string -> params:doc list -> on:(beat -> unit) -> live_handle

  (** Number of active SHARED backend observes — the observe multiplexer collapses every subscription
      with the same (collection, query) onto one, so this counts distinct live queries, not
      subscriptions. An operational scalability gauge. *)
  val live_query_count : unit -> int

  (** Pure EJSON structural operations. *)
  module EJSON : sig
    (** Value equality; [key_order_sensitive] (default false) controls document field-order. *)
    val equals : ?key_order_sensitive:bool -> doc -> doc -> bool

    (** A deep copy. *)
    val clone : doc -> doc
  end
end

(** Build the reactive surface over a storage backend. *)
module Make (B : Backend.S) : REACTIVE with type backend_collection = B.collection

(** The in-memory instance — pure, also compiles to JavaScript; the default for dev and test. *)
module Mini : REACTIVE with type backend_collection = Minimongo.t
