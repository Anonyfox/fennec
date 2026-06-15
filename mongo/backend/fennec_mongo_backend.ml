(* The storage SEAM — the one signature (Backend.S + query) every layer above consumes, plus the
   in-memory minimongo backend ({!Mini}). PURE (bson + minimongo only), so this layer cross-compiles to
   JavaScript alongside the reactive client; the native backends (driver / embedded Burrow), the runtime
   selector, and the mongosh wire endpoint live in the separate native lib fennec-mongo.dynamic. *)

include Seam
