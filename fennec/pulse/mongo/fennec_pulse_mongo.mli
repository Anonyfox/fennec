(** Native MongoDB backend — a {!Fennec_pulse.Backend.S} over the libmongoc driver
    ({!Fennec_mongo_driver}), so the whole reactive / DDP / realtime stack runs over a real mongod
    with no other change (it is the same seam {!Fennec_pulse.Backend.Mini} implements in memory):
    [Reactive.Make (Fennec_pulse_mongo)].

    Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
    calling fiber. {!observe_changes} uses real CHANGE STREAMS (via {!Fennec_mongo_driver.Live} — one
    stream per collection, fanned out to per-query views), not polling — so it needs a replica set
    (the managed mongod is launched as one by {!Fennec_mongo_driver.Server}; a production cluster
    already is one). Native only. *)

(** Whether the native driver was compiled in. [false] on a build where libmongoc was unavailable
    (the FFI degraded to stubs); then {!connect} and every op raise [Failure]. Most apps should NOT
    branch on this — use {!Dynamic.from_env}, which consumes the global Mongo URL and selects the
    in-memory engine only for the [":memory:"] sentinel. *)
val available : unit -> bool

(** A connection — a thread-safe client pool. *)
type connection

(** [connect uri] initializes the driver and opens a pool to [uri] (e.g.
    ["mongodb://127.0.0.1:27017"]). @raise Failure if the native driver is not {!available}. *)
val connect : string -> connection

(** A collection handle. *)
type collection

(** [collection ?poll ~sw conn ~db ~name] is a handle on [db.name]. [sw] is the server's long-lived
    switch — {!Fennec_mongo_driver.Live}'s change-stream daemons fork into it, so a collection that
    will ever be observed (i.e. backs a publication/subscription) must be created under it, in
    [Fennec.serve ~on_start]. [poll] tunes the fallback poll interval (seconds) used only for
    collections beyond the open-stream budget; the change-stream path needs no interval. *)
val collection : ?poll:float -> sw:Eio.Switch.t -> connection -> db:string -> name:string -> collection

include Fennec_pulse.Backend.S with type collection := collection

(** A runtime-selectable backend — in-memory or this native driver behind one
    {!Fennec_pulse.Backend.S}, so an app chooses at boot from the global framework Mongo state
    ([MONGO_URL] or explicit [":memory:"]) with no type change downstream:
    [Reactive.Make (Fennec_pulse_mongo.Dynamic)]. *)
module Dynamic : sig
  (** A collection that is either in-memory or mongo-backed. *)
  type collection

  (** [mem store] wraps an in-memory Minimongo collection. *)
  val mem : Minimongo.t -> collection

  (** [real ?poll ~sw conn ~db ~name] wraps a mongo-backed collection (same arguments as the
      top-level {!val:collection}). *)
  val real : ?poll:float -> sw:Eio.Switch.t -> connection -> db:string -> name:string -> collection

  (** The environment variable the fennec CLI uses to hand an app its database. [fennec dev]
      auto-starts/adopts a local MongoDB when possible; [fennec test] sets [":memory:"] by default
      and [fennec test --mongo] supplies a per-suite real Mongo URL. Value: ["MONGO_URL"]. *)
  val mongo_url_env : string

  (** [from_env ?poll ~sw ~db ~name ()] — the one call an app needs to consume the global Mongo URL:
      a fresh in-memory {!mem} collection for explicit [":memory:"], a {!real} collection for a
      real URI, or a collection whose operations fail clearly when no [MONGO_URL] is configured.
      Build it in [Fennec.serve ~on_start] (the captured [sw] drives Live's change-stream daemons). *)
  val from_env : ?poll:float -> sw:Eio.Switch.t -> db:string -> name:string -> unit -> collection

  include Fennec_pulse.Backend.S with type collection := collection
end
