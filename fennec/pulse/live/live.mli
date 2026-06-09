(** The Fur binding over a {!Merge_store}: a reactive [find] whose signal recomputes as the merged
    collection changes. Pure over Fur's signals → native (tests/SSR) and browser. The DDP WebSocket
    client and [subscribe] (which feed the store) are a later js_of_ocaml addition; this is the read
    side. *)

(** A reactive client cache: a merge store plus the per-collection Fur signals that drive {!find}. *)
type t

(** A fresh client cache. *)
val create : unit -> t

(** The underlying {!Merge_store} — feed it DDP deltas (the WebSocket client does this). *)
val store : t -> Merge_store.t

(** [find t name ?selector ?sort ?skip ?limit ?fields ()] is a Fur signal of the matching documents
    that recomputes whenever collection [name] changes. Read it with {!Fur.get} inside a component;
    the underlying watch is torn down on the component's cleanup. *)
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
