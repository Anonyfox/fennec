(* Planner — choose an access {!Plan.t} for a selector (+ sort). Pure: it reads only the index
   descriptors and the query shape, never the data, so it is unit-testable with no disk. Today it
   recognizes [_id] equality (a point lookup) and otherwise falls back to a collection scan; secondary
   index selection (equality / range / sort-via-index) lands with the indexing phase. *)

val plan : Catalog.index list -> selector:Bson.t -> sort:Bson.t -> Plan.t
