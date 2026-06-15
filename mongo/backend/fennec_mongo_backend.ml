(* The storage backend layer, owned by the mongo package. {!Seam} is the one signature every layer
   above consumes (Backend.S + query); {!Adapters} provides the concrete backends (minimongo / native
   driver / embedded Burrow), the runtime {!Adapters.Dynamic} selector over MONGO_URL, and the mongosh
   wire endpoint. This module flattens both so consumers see [Fennec_mongo_backend.S],
   [Fennec_mongo_backend.Dynamic], [Fennec_mongo_backend.expose], … directly. *)

include Seam
include Adapters
