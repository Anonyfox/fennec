(** Native MongoDB backend — a {!Fennec_data.Backend.S} over the statically-linked libmongoc driver,
    so the whole reactive / DDP / realtime stack runs over a real mongod with no other change (it is
    the same seam {!Fennec_data.Backend.Mini} implements in memory): [Reactive.Make (Fennec_data_mongo)].

    Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
    calling fiber. {!observe_changes} polls and diffs (it works against a standalone mongod; change
    streams — which need a replica set — are a later optimization). Native only. *)

(** Whether the native driver was compiled in. [false] on a build where libmongoc was unavailable
    (the FFI degraded to stubs); then {!connect} and every op raise [Failure]. Most apps should NOT
    branch on this — use {!Dynamic.from_env}, which selects the in-memory engine when no database is
    configured, so the degrade is a config decision, not a runtime check. *)
val available : unit -> bool

(** A connection — a thread-safe client pool. *)
type connection

(** [connect uri] initializes the driver and opens a pool to [uri] (e.g.
    ["mongodb://127.0.0.1:27017"]). @raise Failure if the native driver is not {!available}. *)
val connect : string -> connection

(** A collection handle. It also carries the Eio context {!observe_changes} needs. *)
type collection

(** [collection ?poll ~sw ~sleep conn ~db ~name] is a handle on [db.name]. The [sw]/[sleep] it
    captures are used ONLY by {!observe_changes} — which forks its polling loop into [sw] and waits
    [poll] seconds (default 0.5) between polls via [sleep] ([~sleep:(Eio.Time.sleep clock)]). So any
    collection that will ever be observed (i.e. backs a publication/subscription) must be created
    under the server's switch — in [Fennec.serve ~on_start] — not at module top level. *)
val collection :
  ?poll:float -> sw:Eio.Switch.t -> sleep:(float -> unit) -> connection -> db:string -> name:string -> collection

include Fennec_data.Backend.S with type collection := collection

(** A runtime-selectable backend — in-memory or this native driver behind one
    {!Fennec_data.Backend.S}, so an app chooses at boot (real mongo when configured, else
    [:memory:]) with no type change downstream: [Reactive.Make (Fennec_data_mongo.Dynamic)]. *)
module Dynamic : sig
  (** A collection that is either in-memory or mongo-backed. *)
  type collection

  (** [mem store] wraps an in-memory Minimongo collection. *)
  val mem : Minimongo.t -> collection

  (** [real ?poll ~sw ~sleep conn ~db ~name] wraps a mongo-backed collection (same arguments as the
      top-level {!val:collection}). *)
  val real :
    ?poll:float -> sw:Eio.Switch.t -> sleep:(float -> unit) -> connection -> db:string -> name:string -> collection

  (** The environment variable the fennec CLI uses to hand an app its database: [fennec dev --mongo]
      and [fennec test --mongo] launch a managed mongod and export its URL here. Value: ["MONGO_URL"]. *)
  val mongo_url_env : string

  (** [from_env ?poll ~sw ~sleep ~db ~name ()] — the one call an app needs to "use real mongo when
      it's there": a {!real} collection when {!mongo_url_env} is set (as under [fennec dev --mongo]),
      else a fresh in-memory {!mem} one — so app code carries no config branch. Build it in
      [Fennec.serve ~on_start] (the captured [sw]/[sleep] drive {!observe_changes}' polling loop). *)
  val from_env :
    ?poll:float -> sw:Eio.Switch.t -> sleep:(float -> unit) -> db:string -> name:string -> unit -> collection

  include Fennec_data.Backend.S with type collection := collection
end
