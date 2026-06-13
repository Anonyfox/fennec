(** Deterministic single-node replica-set lifecycle — what makes change streams "just work" locally.
    Launches [mongod --replSet], waits until it answers, initiates the set if never initiated, then
    waits until the node is PRIMARY. Every step polls an explicit condition with a bounded timeout
    (no fixed sleeps); the config uses [127.0.0.1:<port>], never a hostname.

    {[
      (* A hermetic throwaway replica set for a test — primary on a free port, wiped on exit. *)
      Eio_main.run @@ fun env ->
      with_ephemeral ~env (fun srv ->
          let coll = Collection.create (client srv) ~db:"test" ~name:"items" in
          ignore (Collection.insert_one coll (Bson.doc [ ("v", Bson.int 1) ])))
    ]} *)

(** A managed (or adopted) mongod. *)
type t

(** [start ~env ~sw ?port ?dbpath ?replset ?reuse ?ephemeral ()] brings up a single-node replica set
    and returns once the node is PRIMARY. [~reuse:true] (default) adopts a mongod already answering
    on [port] instead of spawning a second one (and then [stop] leaves it running). [~ephemeral:true]
    marks the data dir to be wiped on [stop]. *)
val start :
  env:Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  ?port:int ->
  ?dbpath:string ->
  ?replset:string ->
  ?reuse:bool ->
  ?ephemeral:bool ->
  unit ->
  t

(** Graceful shutdown (a [shutdown] command, then SIGTERM, then await); wipes the data dir if the
    instance was ephemeral. A no-op for an adopted (reused) instance. *)
val stop : t -> unit

(** The connected client. *)
val client : t -> Client.t

(** The connection URI (with [directConnection=true]). *)
val uri_of : t -> string

(** The port the node listens on. *)
val port : t -> int

(** [ensure_replica_set ~port ~replset client] initiates the set if it has never been initiated
    (idempotent — a no-op on an already-initiated node). *)
val ensure_replica_set : port:int -> replset:string -> Client.t -> unit

(** [with_replica_set ~env ?port ?dbpath ?replset ?reuse f] starts (or reuses) a local set, runs [f]
    on the managed instance, and tears everything down afterwards. *)
val with_replica_set :
  env:Eio_unix.Stdenv.base ->
  ?port:int ->
  ?dbpath:string ->
  ?replset:string ->
  ?reuse:bool ->
  (t -> 'a) ->
  'a

(** [with_ephemeral ~env ?port ?replset f] runs [f] against a hermetic throwaway set: a private
    mongod on a free port with a tmpfs data dir, wiped on exit — safe to run many in parallel. An
    at_exit backstop reaps it even if the process exits without unwinding (a stray [exit]), so an
    orphaned mongod is impossible. Ideal for tests. *)
val with_ephemeral : env:Eio_unix.Stdenv.base -> ?port:int -> ?replset:string -> (t -> 'a) -> 'a
