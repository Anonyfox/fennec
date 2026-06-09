(** The SSR live-data seed payload: a publication's documents plus the COLLECTION they belong to,
    serialized into Fur's seed <script> by the SSR server and read back by the browser client.
    Carrying the collection makes hydration robust when a publication's name differs from its
    collection — the browser's [publish] is a no-op, so the collection can only travel via this
    payload, not be re-derived client-side. Pure → native + JS. *)

(** [encode ~collection docs] is the wire string embedded in the page (the collection rides with its
    documents). *)
val encode : collection:string -> Bson.t list -> string

(** [decode s] reads back [Some (collection, docs)], or [None] on a malformed/legacy payload. *)
val decode : string -> (string * Bson.t list) option
