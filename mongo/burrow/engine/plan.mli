(* Plan — the access path the planner chose for a query. The executor interprets it into a candidate
   document stream that the residual matcher then filters. Pure data (depends only on Bson). *)

type range = {
  lo : string option;  (** encoded inclusive lower bound to seek to (None = from the start) *)
  excl_lo : bool;  (** drop entries whose key equals [lo] ([$gt]) *)
  hi : string option;  (** encoded upper bound (None = to the end) *)
  excl_hi : bool;  (** exclusive upper ([$lt]) vs inclusive ([$lte] / equality / prefix) *)
}
(** A contiguous key range over an index. Equality is [lo = hi]; a compound bound concatenates the
    leading equality fields into the prefix; an open side is [None]. *)

type t =
  | Id_point of Bson.t  (** [_id] equality — a single point lookup on the clustered record store *)
  | Index_scan of {
      index : string;  (** the index name; the executor resolves it on the collection *)
      ranges : range list;  (** scanned and unioned (deduped by record key) — one each for [$in] values *)
      sorted : bool;  (** the scan order already satisfies the query sort, so the executor skips sorting *)
      reverse : bool;  (** scan back-to-front (LAST->PREV) — an ascending index serving a descending
                           sort (or vice versa); only paired with a single full range *)
    }
  | Index_union of (string * range list) list
      (** union (deduped by record key) of per-index scans — one entry per [$or] clause when every
          clause is index-served; the residual matcher then applies the full selector *)
  | Collection_scan  (** full scan in [_id] order; the residual matcher filters *)
