type range = {
  lo : string option;
  excl_lo : bool;
  hi : string option;
  excl_hi : bool;
}

type t =
  | Id_point of Bson.t
  | Index_scan of { index : string; ranges : range list; sorted : bool }
  | Collection_scan
