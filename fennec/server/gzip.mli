(** gzip + zlib-wrapped deflate for HTTP [Content-Encoding], on real zlib.
    One-shot whole-string compression.

    {[
      let encoded = Gzip.gzip body in
      (* serve with header ("Content-Encoding", "gzip") *)
    ]} *)

(** gzip-encode (RFC 1952 container) — for [Content-Encoding: gzip]. *)
val gzip : ?level:int -> string -> string

(** zlib-wrapped deflate — for [Content-Encoding: deflate]. *)
val deflate : ?level:int -> string -> string
