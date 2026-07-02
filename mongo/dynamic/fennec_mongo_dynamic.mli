(* fennec-mongo.dynamic — the native data plane behind ONE curated surface. Everything here is
   server-only (it links libmongoc, LMDB, and TLS; the pure seam it implements is fennec-mongo.backend):

   - the native DRIVER backend over a real mongod (the flat ops below, on {!connection}/{!collection});
   - the embedded BURROW backend + the runtime selector {!Dynamic} (one [Backend.S] picked by MONGO_URL:
     [:memory:] / [burrow://] / [mongodb://], no type change downstream);
   - the transparent transaction context {!Tx} (workflow atomicity across all three backends);
   - the mongosh WIRE ENDPOINT {!expose} (SCRAM auth, RBAC {!Role}, TLS, audit, change streams) and its
     zero-config form {!boot} / {!expose_from_env} driven by MONGO_URL alone;
   - the wire CLIENT {!Wire_client} + the replication transport {!Replication} (a [Burrow.Replica]
     follower pulling a remote source's oplog).

   Anything not listed here (per-op helpers, engine registries, SASL plumbing) is internal. *)

(* ---- the native driver backend (a real mongod over libmongoc) ---- *)

val available : unit -> bool
(** Whether the native libmongoc driver was compiled in ([false] = the stub build; use [:memory:]). *)

type connection = Fennec_mongo_driver.Client.t

val connect : string -> connection
(** Connect a pooled client to a [mongodb://] URI. *)

type collection = Fennec_mongo_driver.Collection.t

val collection : ?poll:float -> sw:Eio.Switch.t -> connection -> db:string -> name:string -> collection
(** A named collection; [sw] hosts the live change-stream daemons, [?poll] their fallback interval. *)

(** The [Backend.S] operations on a native collection (selectors / updates / pipelines are plain
    {!Bson.t} — the shapes you'd type in the shell): *)

val insert : collection -> Bson.t -> string
val update : collection -> multi:bool -> upsert:bool -> Bson.t -> Bson.t -> int
val remove : collection -> Bson.t -> int
val find : collection -> Fennec_mongo_backend.query -> Bson.t list
val find_one : collection -> Fennec_mongo_backend.query -> Bson.t option
val count : collection -> Bson.t -> int
val aggregate : collection -> ?lookup:(string -> Bson.t list) -> Bson.t list -> Bson.t list
val distinct : collection -> string -> Bson.t -> Bson.t list

val observe_changes :
  collection ->
  Fennec_mongo_backend.query ->
  added:(string -> Bson.t -> unit) ->
  changed:(string -> Bson.t -> string list -> unit) ->
  removed:(string -> unit) ->
  Fennec_mongo_backend.handle
(** Live per-query deltas over ONE real change stream per collection (initial set replays synchronously). *)

val fence : collection -> (unit -> unit) -> unit
val ensure_index : collection -> name:string -> keys:Bson.t -> unique:bool -> sparse:bool -> unit
val drop_index : collection -> name:string -> unit
val index_names : collection -> string list
val ensure_validator : collection -> Bson.t option -> unit

(* ---- the runtime-selectable backend + the transaction context ---- *)

module Dynamic = Adapters.Dynamic
(** ONE [Backend.S] over all three storage tiers, selected at boot by MONGO_URL ([:memory:] /
    [burrow://] / [mongodb://]) with no type change downstream. [set_switch] installs the ambient Eio
    switch so collections open by name. *)

module Tx = Adapters.Tx
(** The transparent transaction context: [Tx.run f] executes [f] in one transaction — commit on return,
    rollback on raise, read-your-writes throughout, nested runs join the outer one — natively on every
    backend (minimongo snapshots / a Burrow parent txn / a Mongo session). [Tx.on_commit] defers a thunk
    to the outermost commit (the exactly-once effects seam). *)

(* ---- the mongosh wire endpoint (the embedded engine over the MongoDB wire protocol) ---- *)

module Role = Adapters.Role
(** A database-scoped grant for a wire user: [Read db] / [Read_write db] / [Root] ([db = "*"] = all). *)

type wire_user = Adapters.wire_user

val wire_user : user:string -> password:string -> ?roles:Role.t list -> unit -> wire_user
(** A SCRAM-SHA-256 wire login; [?roles] defaults to [[Role.Root]]. The plaintext password is never
    retained (a verifier is derived once at {!expose}). *)

module Audit = Wire_server.Audit
(** The security audit event types (auth ok / failed + authz denials) an [~audit] sink receives. *)

val expose :
  sw:Eio.Switch.t ->
  net:'a Eio.Net.t ->
  ?addr:Eio.Net.Sockaddr.stream ->
  ?base_dir:string ->
  ?users:wire_user list ->
  ?require_auth:bool ->
  ?read_only:bool ->
  ?tls:Tls.Config.server ->
  ?max_message_bytes:int ->
  ?max_connections:int ->
  ?audit:(Audit.event -> unit) ->
  ?slow_ms:int ->
  unit ->
  unit
(** Front the embedded Burrow engine with a MongoDB wire-protocol listener, so real mongosh / Compass /
    drivers connect as to a hosted mongod — sharing the SAME engine cache as the [burrow://] backend
    (live data, one group-committing writer). Secure by default: loopback bind, SCRAM required when any
    user is set, no [$where]/JS, capped message size; [?read_only], [?tls], [?audit], and a slow-command
    log ([?slow_ms], ms threshold) opt in. Binding is best-effort: a port clash logs and never takes the
    app down. *)

val expose_from_env : sw:Eio.Switch.t -> net:'a Eio.Net.t -> unit -> unit
(** The zero-config form: a [burrow://] MONGO_URL WITH an authority opens the endpoint there (SCRAM iff
    the URL carries [user:pass], read-only iff [?readonly]); any other backend or a bare path = no
    endpoint. MONGO_URL alone decides. *)

val boot : sw:Eio.Switch.t -> net:'a Eio.Net.t -> unit -> unit
(** The one data-layer boot [Fennec.serve] runs: installs the ambient switch ({!Dynamic.set_switch})
    and calls {!expose_from_env}. *)

val command_access : string -> [ `Read | `Write | `Exempt ]
(** Classify a wire command's per-database authorization requirement — the gate the endpoint enforces;
    exposed for tooling + tests. *)

val change_event : db:string -> Bson.t -> Bson.t
(** Format a raw oplog entry ([{lsn; op; ns; id; o?}]) as a MongoDB change-stream event (resume token =
    the LSN) — the pure formatting behind the wire [$changeStream]; exposed for tooling + tests. *)

(* ---- the wire client + the replication transport ---- *)

module Wire_client = Wire_client
(** A minimal MongoDB wire CLIENT (connect / run one command / SCRAM auth) — see [wire_client.mli]. *)

module Replication = Replication
(** The wire-backed oplog transport for a [Burrow.Replica] follower — see [replication.mli]. *)
