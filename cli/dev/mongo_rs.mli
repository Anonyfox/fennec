(** [--mongo]: launch a managed single-node MongoDB replica set (change streams require one) and
    point the app at it via [MONGO_URL]. The process is spawned + reaped by the pure-Unix
    {!Fennec_mongo_mongod.Mongod} lifecycle; the set is initiated and brought to PRIMARY via the
    driver. An absent or failed mongod degrades to the in-memory backend (leaves [MONGO_URL] unset).

    @return the managed process (the caller tracks / reaps it), or [None] when degraded. *)
val launch : unit -> Fennec_mongo_mongod.Mongod.t option
