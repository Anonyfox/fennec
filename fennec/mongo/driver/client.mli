(** A connection handle: a thread-safe libmongoc client pool plus the URI it was built from. Cheap to
    share across fibers — every operation checks a client out of the pool for the call's duration. *)

(** A pooled connection. [pool] is the libmongoc client pool; [uri] is the connection string. *)
type t = { pool : Fennec_mongo_ffi.Mongo_ffi.pool; uri : string }

(** The default local URI: [directConnection=true] (talk to one node, skip replica-set discovery)
    and [maxPoolSize=256] — an open change stream holds a pooled client for its whole lifetime, so
    the concurrent-stream ceiling is the pool size (the {!Live} multiplexer keeps it one-per-
    collection, not one-per-subscriber). *)
val default_uri : string

(** [connect ?uri ()] initializes the driver and opens a pool to [uri] (default {!default_uri}). *)
val connect : ?uri:string -> unit -> t

(** [ping t ~db] — whether the server answers a [ping] on [db]. Never raises. *)
val ping : t -> db:string -> bool

(** [command t ~db cmd] runs a raw command document and returns the reply.
    @raise Failure on a driver or command error. *)
val command : t -> db:string -> Bson.t -> Bson.t

(** [drop_database t ~db] drops the whole database (collections + indexes). Idempotent. *)
val drop_database : t -> db:string -> unit
