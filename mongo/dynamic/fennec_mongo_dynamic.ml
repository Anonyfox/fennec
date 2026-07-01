(* The concrete, NATIVE storage backends + the runtime MONGO_URL selector + the mongosh wire endpoint.
   {!Adapters} provides the native driver backend, the embedded Burrow backend (with its engine
   registry), the {!Adapters.Dynamic} selector over MONGO_URL, and [expose]/[expose_from_env]; flattened
   here. Server-only — it links libmongoc (driver) + LMDB (burrow) + TLS, so it never ships to JS. The
   pure seam it implements lives in fennec-mongo.backend. *)

include Adapters

(* classify a wire command's per-database authorization requirement ([`Read]/[`Write]/[`Exempt]) — the
   gate the endpoint enforces; exposed for tooling + tests. *)
let command_access = Wire_server.access_of
