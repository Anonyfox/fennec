(* Burrow's record binary codec — [Bson.t] documents <-> compact bytes, plus a zero-copy field reader.
   See the .ml header for the on-disk format (BSON-derived; [Binary] and [Decimal128] keep their string
   form for lossless round-trip). Pure: no disk, no FFI. *)

(** Encode a top-level [Document] to its record bytes. Raises [Invalid_argument] on any other value. *)
val encode : Bson.t -> string

(** Decode record bytes back to a [Document]. *)
val decode : string -> Bson.t

(** Field-list encode/decode, avoiding the [Document] wrapper. *)
val encode_doc : (string * Bson.t) list -> string

val decode_doc : string -> (string * Bson.t) list

(** [find_field record name] decodes ONLY the named top-level field, skipping every other field by its
    size — no [Bson.t] is allocated for values that aren't read. [None] if absent. *)
val find_field : string -> string -> Bson.t option
