(** The browser DDP client (Js_of_ocaml): dials the server's [/websocket], runs the DDP handshake,
    and feeds sub-tagged deltas into a live merge store. {!find} is the reactive Fur query over the
    merged data; {!call} invokes a server method (the data it changes flows back through the open
    subscription as a normal delta). Browser-only — the native build is a stub: no socket, an empty
    store, subscriptions that never become ready. *)

(** A live connection to the DDP server. *)
type t

(** A live subscription handle. [ready] is a Fur signal — [false] until the server confirms the
    subscription ([ready], or [nosub] when it ends/fails), then [true] — so a component can render a
    loading state until the data is present. [stop] releases this handle; subscriptions are deduped +
    refcounted by (name, params), so the underlying server subscription is torn down only when the
    LAST holder stops (which also clears that subscription's documents from the store). *)
type subscription = { ready : bool Fur.signal; stop : unit -> unit }

(** [connect ?path ()] opens a WebSocket to [path] (default [/websocket]) on the current origin and
    sends [connect]. Returns immediately; data arrives asynchronously into {!find}. *)
val connect : ?path:string -> unit -> t

(** [publish ~name ?collection f] registers, for SERVER-SIDE SSR only (the browser ignores it), a
    publication's initial-document fetcher. During SSR a component's {!subscribe}/{!use_subscribe}
    runs it, renders with the data, and embeds it via Fur's seed — so the browser hydrates
    flicker-free and the live subscription then re-confirms + streams. [collection] is the collection
    the documents belong to (and that {!find} queries); it defaults to [name] and travels in the seed
    payload, so set it when a publication's name differs from its collection. Register it where the
    publication is set up (e.g. in [Fennec.serve ~on_start]); a fetch that needs Eio (real mongo)
    degrades to a loading client. *)
val publish : name:string -> ?collection:string -> (Bson.t list -> Bson.t list) -> unit

(** [subscribe t ~name ?params ()] starts (or, for an identical [name]+[params], joins) the named
    publication; its documents stream into the merge store and become visible through {!find}.
    Returns a {!subscription} — read [ready] for the loading state, call [stop] to release. *)
val subscribe : t -> name:string -> ?params:Bson.t list -> unit -> subscription

(** [use_subscribe t ~name ?params ()] is {!subscribe} bound to the calling Fur component's
    lifecycle: it subscribes now and auto-[stop]s on the component's cleanup, returning just the
    [ready] signal — the everyday call inside a component
    ([if Fur.get (use_subscribe …) then … else loading]). *)
val use_subscribe : t -> name:string -> ?params:Bson.t list -> unit -> bool Fur.signal

(** [call t ~name ?params ()] invokes a server method (fire-and-forget; any data it changes returns
    via an open subscription). *)
val call : t -> name:string -> ?params:Bson.t list -> unit -> unit

(** [find t name ?selector ?sort ?skip ?limit ?fields ()] is a Fur signal of the matching documents
    that recomputes as the server pushes changes. Read it with {!Fur.get} inside a component. *)
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

(** [aggregate t name pipeline] is a Fur signal of the aggregation result over collection [name],
    recomputing as the server pushes changes; [$lookup] / [$unionWith] join across the client's other
    subscribed collections (the local cache is a real multi-collection store). Read it with
    {!Fur.get} inside a component. *)
val aggregate : t -> string -> Bson.t list -> Bson.t array Fur.signal
