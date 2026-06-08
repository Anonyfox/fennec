(** Native MongoDB backend — a {!Fennec_data.Backend.S} over the statically-linked libmongoc driver,
    so the whole reactive / DDP / realtime stack runs over a real mongod with no other change (it is
    the same seam {!Fennec_data.Backend.Mini} implements in memory): [Reactive.Make (Fennec_data_mongo)].

    Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
    calling fiber. {!observe_changes} polls and diffs (it works against a standalone mongod; change
    streams — which need a replica set — are a later optimization). Native only. *)

(** Whether the native driver was compiled in (else use {!Fennec_data.Backend.Mini}). *)
val available : unit -> bool

(** A connection — a thread-safe client pool. *)
type connection

(** [connect uri] initializes the driver and opens a pool to [uri] (e.g.
    ["mongodb://127.0.0.1:27017"]). *)
val connect : string -> connection

(** A collection handle. It also carries the Eio context {!observe_changes} needs. *)
type collection

(** [collection ?poll ~sw ~sleep conn ~db ~name] is a handle on [db.name]. {!observe_changes} forks
    its polling loop into [sw] and waits [poll] seconds (default 0.5) between polls via [sleep] —
    pass [~sleep:(Eio.Time.sleep clock)]. *)
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

  include Fennec_data.Backend.S with type collection := collection
end
