(** The native live-query engine: ONE MongoDB change stream per collection, fanned out in-process to
    per-query views that keep a result cache and emit field-level deltas — reusing the pure {!Query}
    semantics so it behaves identically to the in-memory minimongo observe. A polling guard caps the
    open-stream count under the pool size. Requires an ambient Eio switch ({!set_switch}) and a
    replica set (see {!Server}).

    {[
      set_switch sw;                       (* once at startup, under the app's Eio switch *)
      let q = query coll ~selector:(Bson.doc [ ("active", Bson.bool true) ]) in
      let h =
        observe_changes q
          ~added:(fun id doc -> register id doc)
          ~removed:(fun id -> forget id)
          ()
      in
      (* … later … *) h.stop ()
    ]} *)

(** A live-observation handle; call [stop] to detach. *)
type handle = { stop : unit -> unit }

(** A deferred live query over a collection. *)
type query

(** [query ?selector ?sort ?skip ?limit ?fields coll] builds a live query. *)
val query :
  ?selector:Bson.t -> ?sort:Bson.t -> ?skip:int -> ?limit:int -> ?fields:Bson.t -> Collection.t -> query

(** Set the ambient Eio switch the change-stream / poller daemons fork into. Call once at startup
    (e.g. in [Fennec.serve ~on_start]); the daemons live for the switch's lifetime. *)
val set_switch : Eio.Switch.t -> unit

(** [observe_changes q ?added ?changed ?removed ()] — field-level, unordered membership tracking.
    Existing documents are replayed as [added] synchronously before returning (so a consumer that
    signals readiness right after sees the initial set first); subsequent changes arrive as
    [added]/[changed id changed_fields cleared_names]/[removed]. *)
val observe_changes :
  query ->
  ?added:(string -> Bson.t -> unit) ->
  ?changed:(string -> Bson.t -> string list -> unit) ->
  ?removed:(string -> unit) ->
  unit ->
  handle

(** Ordered/positional observe: re-fetch + ordered diff on each relevant change, emitting
    [added_before]/[changed]/[moved_before]/[removed]. *)
val observe :
  query ->
  added_before:(string -> Bson.t -> string option -> unit) ->
  changed:(string -> Bson.t -> string list -> unit) ->
  moved_before:(string -> string option -> unit) ->
  removed:(string -> unit) ->
  unit ->
  handle

(** Cap on concurrently-open change streams (default 200); collections observed beyond it fall back
    to polling so the connection pool is never exhausted. *)
val set_collection_stream_budget : int -> unit

(** Poll interval (seconds, default 0.25) for collections on the polling fallback. *)
val set_poll_interval : float -> unit

(** Total change streams ever opened (instrumentation). *)
val streams_opened_total : unit -> int

(** Currently-open change streams. *)
val live_streams : unit -> int

(** Collections currently on the polling fallback. *)
val polled_collections : unit -> int

(** Reset the opened/polled counters. *)
val reset_stats : unit -> unit
