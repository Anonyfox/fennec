(** EJSON wire codec — {!Bson.t} ⇄ {!Json.t}, the JSON projection used on the DDP wire. Implements
    the four EJSON escape objects ([{$date}], [{$binary}], [{$type,$value}], [{$escape}]); numbers
    are doubles on the wire and decode to [Int] when integral. Round-trips losslessly both ways. *)

(** Encode a BSON value to its EJSON JSON projection. *)
val of_bson : Bson.t -> Json.t

(** Decode an EJSON JSON value back to BSON. *)
val to_bson : Json.t -> Bson.t

(** Encode a document's fields as a JSON object (wrapping it in [$escape] if it would otherwise be
    misread as a marker). *)
val doc_to_json : (string * Bson.t) list -> Json.t

(** Decode a JSON value to a document's fields ([[]] if it is not an object). *)
val json_to_doc : Json.t -> (string * Bson.t) list

(** Whether a JSON object structurally matches an EJSON marker (and so needs escaping). *)
val looks_like_marker : Json.t -> bool

(** [encode b] is the EJSON string of [b]. *)
val encode : Bson.t -> string

(** [decode s] parses an EJSON string back to BSON. *)
val decode : string -> Bson.t
