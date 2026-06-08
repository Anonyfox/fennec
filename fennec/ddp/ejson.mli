(** EJSON wire codec — {!Bson.t} ⇄ {!Json.t}, the JSON projection used on the DDP wire. Implements
    the four EJSON escape objects ([{$date}], [{$binary}], [{$type,$value}], [{$escape}]).

    Numbers are IEEE-754 doubles on the wire: an integral value below {!Json.int_cutoff} round-trips
    as [Int], otherwise as [Float] — so the [Int]/[Float] distinction and [Int64] magnitudes beyond
    2^53 are {e not} preserved (the numeric value is, modulo double precision). All other types
    round-trip exactly, including documents shaped like a marker (they are [{$escape}]-wrapped). *)

(** Encode a BSON value to its EJSON JSON projection. *)
val of_bson : Bson.t -> Json.t

(** Decode an EJSON JSON value back to BSON. *)
val to_bson : Json.t -> Bson.t

(** Encode a document's fields as a JSON object (wrapping it in [$escape] if it would otherwise be
    misread as a marker). *)
val doc_to_json : (string * Bson.t) list -> Json.t

(** Decode a JSON value to a document's fields ([[]] if it is not an object). *)
val json_to_doc : Json.t -> (string * Bson.t) list

(** [encode b] is the EJSON string of [b]. *)
val encode : Bson.t -> string

(** [decode s] parses an EJSON string back to BSON. *)
val decode : string -> Bson.t
