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

(** [connect ?path ?persist ()] opens a WebSocket to [path] (default [/websocket]) on the current
    origin and sends [connect]. Returns immediately; data arrives asynchronously into {!find}.
    [persist] (a storage namespace, usually the app name) turns on PWA-grade persistence: each
    subscription's data snapshots to local storage (debounced + at every [ready]) and restores on
    the next boot as a seed — warm data instantly, even fully offline, reconciled by the next live
    [ready] (quiescence prunes what died) — and the WRITE OUTBOX survives reloads: buffered methods
    re-issue with fresh ids and their original seeds, their stubs replaying byte-identically
    ({!Fennec_pulse_method.Method.stub_replay} + the deterministic seed streams). Call
    {!purge_storage} on logout/user-switch (auto-purged on a server-pushed identity change).
    [chrome] controls the built-in status indicator (offline / saving… (N) / update-available, a
    small CSS-variable-styleable element): ON by default in PWA mode (persistence active), [false]
    to opt out, [true] to force it on without persistence. *)
val connect : ?path:string -> ?persist:string -> ?chrome:bool -> unit -> t

(** [purge_storage t] wipes this client's persisted namespace (snapshots + outbox) — the
    identity-change hook: call it on logout/user-switch so one user's cache never leaks to the
    next. A no-op without [?persist] and on SSR/native. *)
val purge_storage : t -> unit

(** [close t] tears the client down: it stops the auto-reconnect loop and shuts the live socket, so a
    finished client (page teardown, a throwaway SPA-route client) doesn't keep a reconnect timer
    firing forever. Idempotent. On the SSR/native client there is no socket, so it is a no-op. *)
val close : t -> unit

(** The live connection state, as a Fur signal — OFFLINE MODE is built in, this is just the
    affordance hook ("reconnecting…", a disabled save button). [`Connected]: socket open and the
    heartbeat healthy. [`Connecting]: a dial in progress. [`Waiting]: offline, backing off until the
    next attempt. While not connected the app keeps WORKING: {!find} renders the cache, stubs apply
    instantly, and method calls buffer in order ({!pending_writes}) — the reconnect handshake
    resubscribes, heals the cache (resync + quiescence), and flushes the buffer, all automatically.
    Silent network death (no FIN) is detected by a DDP heartbeat within ~25s. Scope: the running
    page — buffers don't survive a reload. On SSR/native this is pinned [`Connected] (the first
    paint assumes connectivity). *)
val status : t -> [ `Connected | `Connecting | `Waiting ] Fur.signal

(** How many method calls are currently buffered/unacknowledged (0 = everything flushed and
    confirmed) — drive a "saving… (N)" affordance. Pinned [0] on SSR/native. *)
val pending_writes : t -> int Fur.signal

(** [publish ~name f] registers, for SERVER-SIDE SSR only (the browser ignores it), a publication's
    initial-document fetcher. [f params] returns the documents GROUPED BY collection —
    [[ (collection, docs); … ]] — so a publication that feeds several collections seeds them all
    (the common case is a single group, [[ (coll, docs) ]]); each group's collection travels in the
    seed payload, so [find] and the live deltas line up regardless of the publication's name. During
    SSR a component's {!subscribe}/{!use_subscribe} runs it, renders with the data, and embeds it via
    Fur's seed for flicker-free browser hydration. Register it where the publication is set up (e.g.
    in [Fennec.serve ~on_start]); a fetch that needs Eio (real mongo) degrades to a loading client. *)
val publish : name:string -> (Bson.t list -> (string * Bson.t list) list) -> unit

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

(** [call_result t ~name ?params ()] invokes a method and returns a Fur signal of its outcome: [None]
    while in flight, then [Some (Ok value)] or [Some (Error (code, reason))] — so a rejected method
    (e.g. a method's [403]) surfaces to the UI instead of failing silently. {!call} is the
    fire-and-forget form (the data a method changes still flows back via the open subscription). *)
val call_result : t -> name:string -> ?params:Bson.t list -> unit -> (Bson.t, string * string) result option Fur.signal

(** [call_m t m args] invokes a TYPED method ({!Method.t} — the same shared value
    the server attached its handler to, so the name/args/result cannot drift). Arguments encode
    through the method's codec; the outcome signal resolves to the DECODED result ([None] in flight,
    [Some (Ok v)] / [Some (Error (code, reason))]; a result the codec rejects surfaces as
    [Error ("client-decode", …)]). If the method declares a [?stub], it runs immediately against the
    client cache as an optimistic simulation (latency compensation): the UI updates now, and the
    server's [updated] replaces the simulation with truth — including converging insert [_id]s via
    the call's random seed. On the SSR/native client this sends nothing and stays [None]. *)
val call_m : t -> ('a, 'r) Method.t -> 'a -> ('r, string * string) result option Fur.signal

(** The TYPED live query over a collection declaration: the same reactive signal as {!find},
    decoded at the boundary (malformed docs skipped + warned once — the UI never crashes on foreign
    garbage); [~where] is a typed clause list read as AND. The component-facing read of the typed
    collection layer. *)
val find_c :
  t -> 'a Def.t -> ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'a array Fur.signal

(** The PROJECTED typed live query: [find_p client Task.collection [%fields title; done_] ()] yields
    [< title : string; done_ : bool > array] — Meteor's [{ fields: {…} }], type-checked. The
    projected object exposes ONLY the chosen fields (a projected-away field is a compile error, not
    [undefined]); the server ships only those fields too. *)
val find_p :
  t -> 'a Def.t -> 'o Proj.t -> ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'o array Fur.signal

(** The ambient page connection recorded by {!connect} — the per-model {!Collection} views read
    through it so day-to-day code threads no [client]. Raises if [connect] hasn't run. *)
val default : unit -> t

(** Bind a collection once, then query with NO [client]/[collection] threading — Meteor's
    [Tasks.find(...)]. Reads only (writes go through methods, by decree); reactive (Fur signals over
    the live cache — live in the browser, SSR-seeded server-side):
    {[ open Task
       module Tasks = Ddp_client.Collection (Task)
       let open_ = Tasks.find ~where:[%q status = "doing"] ~sort:[%sort priority desc] () ]} *)
module Collection (M : sig
  type doc
  val collection : doc Def.t
end) : sig
  val find :
    ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> M.doc array Fur.signal
  val project :
    'o Proj.t -> ?where:Q.t list -> ?sort:Sort.t -> ?skip:int -> ?limit:int -> unit -> 'o array Fur.signal
end

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
