type t =
  | Id_point of Bson.t
  | Index_scan of {
      index : string;
      lo : string option;
      excl_lo : bool;
      hi : string option;
      excl_hi : bool;
    }
  | Collection_scan
