type range = {
  lo : string option;
  excl_lo : bool;
  hi : string option;
  excl_hi : bool;
}

type t =
  | Id_point of Bson.t
  | Index_scan of { index : string; ranges : range list; sorted : bool }
  | Index_union of (string * range list) list
  | Collection_scan
