(** Pulse live data for Fur components.

    This is the Fur binding over a {!Merge_store}: reactive queries whose signals recompute as a
    subscribed server collection changes. Pure over Fur's signals → native (tests/SSR) and
    browser. The DDP WebSocket client and [subscribe] feed the store; this module is the read side.

    Use Pulse live data for server-backed, cross-client, realtime collections such as task lists,
    chat messages, notifications, or collaborative records. Use plain {!Fur.signal} for local
    browser UI state such as counters, toggles, and input drafts. *)

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

(** [find t name ?selector ?sort ?skip ?limit ?fields ()] is a Fur signal of the matching documents
    that recomputes whenever collection [name] changes. Read it with {!Fur.get} inside a component;
    the underlying watch is torn down on the component's cleanup.

    Choose this when the UI should follow live server data. For a local counter or button-only
    widget, create a {!Fur.signal} instead. *)
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

(** [aggregate t name pipeline] is a Fur signal of the aggregation result over collection [name];
    [$lookup] / [$unionWith] join across the client's other collections, and the signal recomputes
    when the primary collection {e or any referenced foreign collection} changes. Note the inherent
    asymmetry with the server: the client joins over the {e subscribed subset} of a foreign collection
    (what's in the local cache), whereas the server joins over the full collection — so a client-side
    join sees only the foreign documents the client has also subscribed to. *)
val aggregate : t -> string -> Bson.t list -> Bson.t array Fur.signal
