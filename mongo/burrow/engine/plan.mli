(* Plan — the access path the planner chose for a query. The executor interprets it into a candidate
   document stream that the residual matcher then filters. Pure data (depends only on Bson). *)

type t =
  | Id_point of Bson.t  (** [_id] equality — a single point lookup on the clustered record store *)
  | Index_scan of {
      index : string;  (** the index name; the executor resolves it on the collection *)
      lo : string option;  (** encoded inclusive lower bound to seek to (None = from the start) *)
      excl_lo : bool;  (** drop entries whose field value equals [lo]'s ([$gt]) *)
      hi : string option;  (** encoded upper bound (None = to the end) *)
      excl_hi : bool;  (** exclusive upper ([$lt]) vs inclusive ([$lte] / equality) *)
    }  (** a range/point scan over a secondary index; bounds are on the first index field *)
  | Collection_scan  (** full scan in [_id] order; the residual matcher filters *)
