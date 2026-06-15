(** Native MongoDB backend — a {!Fennec_pulse.Backend.S} over the libmongoc driver
    ({!Fennec_mongo_driver}), so the whole reactive / DDP / realtime stack runs over a real mongod
    with no other change (it is the same seam {!Fennec_pulse.Backend.Mini} implements in memory):
    [Reactive.Make (Fennec_pulse_mongo)].

    Every blocking driver call runs in an Eio systhread, so a mongo round-trip suspends only the
    calling fiber. {!observe_changes} uses real CHANGE STREAMS (via {!Fennec_mongo_driver.Live} — one
    stream per collection, fanned out to per-query views), not polling — so it needs a replica set
    (the managed mongod is launched as one by {!Fennec_mongo_driver.Server}; a production cluster
    already is one). Native only.

    Apps rarely name this module: the {!Fennec_pulse_app} facade wraps the reactive stack over
    {!Dynamic} and consumes the global Mongo URL for you. The raw seam, if you build it by hand:

    {[ module R = Fennec_pulse.Reactive.Make (Fennec_pulse_mongo.Dynamic)

       (* inside Fennec.serve ~on_start, where [sw] drives the change-stream daemons *)
       let backend = Fennec_pulse_mongo.Dynamic.from_env ~sw ~db:"app" ~name:"tasks" ()
       let tasks = R.Collection.create ~name:"tasks" backend ]} *)

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

(** {1 mongosh / driver exposure}

    Open a MongoDB wire-protocol endpoint in front of the embedded Burrow engine, so any real client —
    mongosh, Compass, an official driver — connects exactly as it would to a hosted mongod and runs
    ad-hoc queries against the app's live embedded data. It is the wire protocol over TCP (optionally
    TLS), not HTTP — which is precisely why unmodified mongosh works.

    The endpoint shares the same per-directory engine cache as the [:embedded:] backend, so it sees the
    app's data and its writes go through the same group-committing writer (no concurrent-writer hazard).
    It is secure by default: loopback bind, SCRAM-SHA-256 required whenever any user is configured, no
    [$where]/JS (so no server-side code injection), a capped message size, and optional read-only / TLS.

    One line in [Fennec.serve ~on_start] (where [sw] is the server's long-lived switch):
    {[ Fennec_pulse_mongo.expose ~sw ~net:(Eio.Stdenv.net env)
         ~users:[ Fennec_pulse_mongo.wire_user ~user:"admin" ~password:secret ] () ]}
    then [mongosh "mongodb://admin:secret@127.0.0.1:27017"]. *)

(** A principal allowed to authenticate over the wire endpoint (SCRAM-SHA-256). *)
type wire_user

val wire_user : user:string -> password:string -> wire_user
(** [wire_user ~user ~password] is a credential for {!expose}. The password is turned into a SCRAM
    verifier at startup and never retained in plaintext. *)

val expose :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  ?addr:Eio.Net.Sockaddr.stream ->
  ?base_dir:string ->
  ?users:wire_user list ->
  ?require_auth:bool ->
  ?read_only:bool ->
  ?tls:Tls.Config.server ->
  ?max_message_bytes:int ->
  ?max_connections:int ->
  unit ->
  unit
(** [expose ~sw ~net () ] starts the wire endpoint as a background fiber under [sw] (returns once bound;
    a bind failure raises). Defaults: [addr] = loopback [:27017]; [base_dir] = ["./.fennec/burrow"]
    (must match the app's [:embedded:] base so they share engines); [users] = none; [require_auth] =
    [true] iff any user is given (set [~require_auth:false] only for a trusted localhost socket);
    [read_only] = [false]; [max_message_bytes] = 48 MB; [max_connections] = 1000. Raises
    [Invalid_argument] if [require_auth] holds with no users. *)

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
