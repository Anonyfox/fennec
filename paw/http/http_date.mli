(** HTTP-date (RFC 7231 §7.1.1.1): format and parse. Times are UNIX epoch seconds
    (UTC). Pure (uses [Unix.gmtime] for formatting).

    {[
      let now = Http_date.format (Unix.time ()) in       (* for a Date / Last-Modified header *)
      match Http_date.parse "Sun, 06 Nov 1994 08:49:37 GMT" with
      | Some epoch -> ignore epoch                        (* e.g. compare an If-Modified-Since *)
      | None -> ()                                        (* unparseable date *)
    ]} *)

(** Format epoch seconds as an IMF-fixdate, e.g. ["Sun, 06 Nov 1994 08:49:37 GMT"]. *)
val format : float -> string

(** Like {!format} but caches the result for the current whole second (lock-free, multicore-safe).
    Use for the per-response [Date] header on a busy server — it formats ~once/second instead of
    once/response. Equivalent to [format] truncated to the second. *)
val format_cached : float -> string

(** Parse any of the three HTTP-date forms (IMF-fixdate, RFC 850, asctime) into
    epoch seconds. Returns [None] on anything unparseable. *)
val parse : string -> float option
