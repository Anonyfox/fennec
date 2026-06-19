(** Pulse live data for Fur components.

    This is the Fur binding over a {!Merge_store}: reactive queries whose signals recompute as a
    subscribed server collection changes. Pure over Fur's signals → native (tests/SSR) and
    browser. The DDP WebSocket client and [subscribe] feed the store; this module is the read side.

    Use Pulse live data for server-backed, cross-client, realtime collections such as task lists,
    chat messages, notifications, or collaborative records. Use plain {!Fur.signal} for local
    browser UI state such as counters, toggles, and input drafts.

    The DDP client owns a {!t} and feeds its {!store}; components read the reactive side:
    {[ let live = Live.create () in
       (* the socket layer routes deltas into [Live.store live] (a Merge_store) *)
       let tasks = Live.find_c live Task.collection ~sort:[%sort title asc] () in
       (* [tasks : Task.t array Fur.signal] — recomputes as the collection changes *) ]} *)

(** A reactive client cache: a merge store plus the per-collection Fur signals that drive
    {!find} and {!aggregate}. *)
type t

(** A fresh client cache. *)
val create : unit -> t

(** The underlying {!Merge_store} — feed it DDP deltas (the WebSocket client does this). *)
val store : t -> Merge_store.t

(** Install the recompute scheduler — how a store change reaches the reactive signals. Default:
    immediate (native/SSR/tests keep synchronous semantics). The browser client installs a
    frame-batched scheduler so a delta burst costs one recompute per collection per frame. Per-signal
    dedup is built in; the scheduler only decides WHEN the batch runs. *)
val set_scheduler : ((unit -> unit) -> unit) -> unit

(** The TYPED reactive read over a collection declaration — the same live signal as {!find},
    decoded at the boundary with the skip policy (malformed docs skipped, warned once per id; the
    UI never crashes on foreign garbage). [~where] is a clause list read as AND. *)
val find_c :
  t -> 'a Def.t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'a array Fur.signal

(** The PROJECTED typed live read — the projection's inferred object type, decoded from the cache
    slice (malformed rows skipped, warned once). [name] is the collection (use [Def.name def]). *)
val find_p :
  t -> string -> 'o Projection.t -> ?where:Filter.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'o array Fur.signal

(** [find t name ?selector ?sort ?skip ?limit ?fields ()] is a Fur signal of the matching documents
    that recomputes whenever collection [name] changes. Read it with {!Fur.get} inside a component;
    the underlying watch is torn down on the component's cleanup.

    @deprecated The raw [Bson.t] selector/sort/fields bypass the typed DSL: a malformed selector or a
    renamed field is a silent runtime mismatch, and the result is untyped [Bson.t]. Prefer {!find_c}
    (the typed collection read) or {!find_p} (typed projection) with the [%q] selector DSL and
    {!Filter} / {!Sort} — they decode at the boundary and compile-check field names + value types.
    The sanctioned raw escape, when you truly need a hand-built selector, is [Filter.raw bson]
    threaded through {!find_c} ([~where:[ Filter.raw … ]]), which keeps the result typed. *)
val find :
  t ->
  string ->
  ?selector:Bson.t ->
  ?sort:Bson.t ->
  ?skip:int ->
  ?limit:int ->
  ?fields:Bson.t ->
  unit ->
  Bson.t array Fur.signal
[@@deprecated
  "Use the typed find_c/find_p with the [%q] selector DSL + Filter/Sort (Filter.raw is the \
   sanctioned escape). The raw Bson.t selector bypasses field-name/type checking and returns untyped \
   Bson.t."]

(** [aggregate t name pipeline] is a Fur signal of the aggregation result over collection [name];
    [$lookup] / [$unionWith] join across the client's other collections, and the signal recomputes
    when the primary collection {e or any referenced foreign collection} changes. Note the inherent
    asymmetry with the server: the client joins over the {e subscribed subset} of a foreign collection
    (what's in the local cache), whereas the server joins over the full collection — so a client-side
    join sees only the foreign documents the client has also subscribed to. *)
val aggregate : t -> string -> Bson.t list -> Bson.t array Fur.signal
