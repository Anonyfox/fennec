(** Spec-exact BSON binary codec for the MongoDB wire protocol — byte-for-byte what mongosh and every
    official driver send and expect. A {e separate} codec from {!Burrow_binary} (the on-disk record
    format): the wire is decoded by real driver code, so [Binary] is the raw payload + subtype byte and
    every type matches the spec exactly. Pure and bounds-safe — a malformed length raises
    [Invalid_argument]/{!Unsupported} rather than over-reading or over-allocating. *)

(** Raised for a BSON value this codec does not (yet) serialise to the wire — currently only
    [Decimal128] (the 16-byte BID form is unimplemented). The server maps this to a normal Mongo error
    reply, so a hostile or exotic document never corrupts the stream or crashes the connection. *)
exception Unsupported of string

val encode : Bson.t -> string
(** [encode (Document fields)] is its spec BSON bytes. Raises [Invalid_argument] on a non-[Document]
    top-level value, {!Unsupported} on a value with no wire encoding. *)

val decode : string -> Bson.t
(** [decode bytes] is the [Document] encoded by [bytes] (read from offset 0). *)

val encode_doc : (string * Bson.t) list -> string
(** [encode_doc fields] is the spec BSON bytes of [Document fields]. *)

val decode_doc : string -> (string * Bson.t) list
(** [decode_doc bytes] is the ordered fields of the document encoded by [bytes]. *)

val doc_len : string -> int -> int
(** [doc_len s off] is the total byte length of the BSON document whose leading int32 length is at
    [off] (i.e. the int32 stored there). *)

val read_doc_at : string -> int -> Bson.t * int
(** [read_doc_at s off] decodes the BSON document whose leading int32 length is at [off] and returns it
    paired with the offset just past it — for walking the back-to-back documents in an OP_MSG frame. *)
