(* Raw externals over libmongoc. The C boundary speaks extended-JSON strings in
   both directions (libbson's bson_init_from_json / bson_as_relaxed_extended_json
   do the translation), which keeps this layer tiny and lets the OCaml side own a
   real BSON value type. A native bson<->value bridge is the perf follow-up.

   Handles are opaque GC-managed custom blocks with C finalizers:
     - [pool] owns a thread-safe mongoc_client_pool_t (+ its uri)
     - [change_stream] holds a client checked out of the pool for its lifetime

   Every function below blocks on network I/O and is meant to be called from an Eio systhread (the
   safe layer wraps it). They raise Failure on driver error. When the native driver was not built
   (see config/discover.ml — no cmake, unsupported OS), [available] is false and every other call
   raises a clear error; the in-memory backend is used instead. *)

type pool
type change_stream

external init : unit -> unit = "ocaml_mongo_init"

(* whether the native libmongoc driver was compiled in (false → the stub build; use :memory:) *)
external available : unit -> bool = "ocaml_mongo_available"

external pool_new : string -> pool = "ocaml_mongo_pool_new"

(* db -> reachable? (runs {ping:1}; never raises) *)
external ping : pool -> string -> bool = "ocaml_mongo_ping"

(* db -> command_json -> reply_json *)
external command : pool -> string -> string -> string = "ocaml_mongo_command"

(* db -> coll -> filter_json -> opts_json -> json-array-of-docs *)
external find : pool -> string -> string -> string -> string -> string
  = "ocaml_mongo_find"

(* db -> coll -> pipeline_json(array) -> opts_json -> json-array-of-docs *)
external aggregate : pool -> string -> string -> string -> string -> string
  = "ocaml_mongo_aggregate"

(* db -> coll -> document_json -> reply_json *)
external insert_one : pool -> string -> string -> string -> string
  = "ocaml_mongo_insert_one"

(* db -> coll -> filter_json -> update_json -> reply_json *)
external update_one : pool -> string -> string -> string -> string -> string
  = "ocaml_mongo_update_one"

(* db -> coll -> filter_json -> reply_json *)
external delete_one : pool -> string -> string -> string -> string
  = "ocaml_mongo_delete_one"

(* db -> coll -> pipeline_json(array) -> opts_json -> stream *)
external watch_open : pool -> string -> string -> string -> string -> change_stream
  = "ocaml_mongo_watch_open"

(* blocks up to the stream's maxAwaitTimeMS; None on timeout, Some json on event *)
external watch_next : change_stream -> string option = "ocaml_mongo_watch_next"
external watch_close : change_stream -> unit = "ocaml_mongo_watch_close"
