(** A database-scoped handle: connect a {!Client.t} once, name a database, and reach collections
    without repeating the db name. A local managed mongod and a remote cluster look identical here —
    only the URI handed to {!Client.connect} differs. *)

(** A database: a client plus a name. *)
type t = { client : Client.t; name : string }

(** [create client name] names a database on [client]. *)
val create : Client.t -> string -> t

(** [collection t name] is a handle on [name] in this database. *)
val collection : t -> string -> Collection.t

(** [command t cmd] runs a command against this database. *)
val command : t -> Bson.t -> Bson.t

(** Drop the whole database. Prefer {!Collection.clear}/{!Collection.drop} for a per-collection
    clean slate. *)
val drop : t -> unit
