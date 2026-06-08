(* A connection handle: a thread-safe libmongoc client pool plus the URI it was built from. Cheap to
   share across fibers — every operation checks a client out of the pool for the duration of the call
   (see the C stubs). *)

module Mongo_ffi = Fennec_mongo_ffi.Mongo_ffi
module Bson_json = Fennec_mongo_bson_json.Bson_json

type t = { pool : Mongo_ffi.pool; uri : string }

(* directConnection talks to a single node without waiting for replica-set discovery — exactly what
   we want for a local single-node set.

   maxPoolSize matters here: an open change stream checks a client out of the pool and HOLDS it for
   the stream's entire lifetime (the blocking getMore loop), unlike find/insert/command which
   pop+push per call. So the number of *concurrent* change streams must stay below maxPoolSize, with
   headroom for writes — otherwise the (pool_size+1)th stream blocks forever in
   mongoc_client_pool_pop and starves everything, including inserts. libmongoc's default is 100; we
   raise it and, crucially, the Live multiplexer keeps the live-stream count at one-per-distinct-
   collection rather than one-per-subscriber. *)
let default_uri = "mongodb://127.0.0.1:27017/?directConnection=true&maxPoolSize=256"

let connect ?(uri = default_uri) () =
  Mongo_ffi.init ();
  { pool = Mongo_ffi.pool_new uri; uri }

let ping t ~db = Internal.run (fun () -> Mongo_ffi.ping t.pool db)

let command t ~db cmd =
  Internal.run (fun () -> Mongo_ffi.command t.pool db (Bson_json.to_string cmd)) |> Bson_json.of_string

(* Drop an entire database (all its collections + indexes). Idempotent: the server returns ok on a
   database that does not exist. *)
let drop_database t ~db = ignore (command t ~db (Bson.Document [ ("dropDatabase", Bson.Int 1) ]))
