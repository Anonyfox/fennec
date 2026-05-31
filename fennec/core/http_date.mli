(** HTTP-date (RFC 7231 §7.1.1.1): format and parse. Times are UNIX epoch seconds
    (UTC). Pure (uses [Unix.gmtime] for formatting). *)

(** Format epoch seconds as an IMF-fixdate, e.g. ["Sun, 06 Nov 1994 08:49:37 GMT"]. *)
val format : float -> string

(** Parse any of the three HTTP-date forms (IMF-fixdate, RFC 850, asctime) into
    epoch seconds. Returns [None] on anything unparseable. *)
val parse : string -> float option
