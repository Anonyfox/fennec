(* Plan — the access path the planner chose for a query. The executor interprets it into a candidate
   document stream that the residual matcher then filters. Pure data (depends only on Bson); index
   range/point variants land with the indexing phase. *)

type t =
  | Id_point of Bson.t  (** [_id] equality — a single point lookup on the clustered record store *)
  | Collection_scan  (** full scan in [_id] order; the residual matcher filters *)
