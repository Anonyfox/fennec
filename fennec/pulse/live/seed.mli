(** The SSR live-data seed payload: a publication's documents grouped BY the collection they belong to
    (a publication may feed several), serialized into Fur's seed <script> by the SSR server and read
    back by the browser client. Carrying the collection name(s) makes hydration robust when a
    publication's name differs from its collection, or feeds multiple collections — the browser's
    [publish] is a no-op, so the collections can only travel via this payload. Pure → native + JS. *)

(** [encode groups] is the wire string embedded in the page — one [(collection, docs)] group per
    collection the publication feeds. *)
val encode : (string * Bson.t list) list -> string

(** [decode s] reads the [(collection, docs)] groups back; an empty list on a malformed/legacy/absent
    payload. *)
val decode : string -> (string * Bson.t list) list
