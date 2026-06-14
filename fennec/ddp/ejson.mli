(** EJSON wire codec — {!Bson.t} ⇄ {!Json.t}, the JSON projection used on the DDP wire. Implements
    the four EJSON escape objects ([{$date}], [{$binary}], [{$type,$value}], [{$escape}]).

    Plain numbers are IEEE-754 doubles on the wire: an integral value below {!Json.int_cutoff}
    round-trips as [Int], otherwise as [Float] — so the [Int]/[Float] distinction is {e not}
    preserved (the value is, and {!Bson.equal} is cross-type numeric, so this never surfaces as a
    spurious delta). [Int64] {e is} preserved exactly — type and full 64-bit magnitude — via a
    [{$type:"Int64",$value:"<decimal>"}] marker (a string, since a JS double would lose both). All
    other types round-trip exactly, including documents shaped like a marker (they are
    [{$escape}]-wrapped).

    {[
      (* a BSON value to its EJSON wire string and back *)
      let wire = Ejson.encode (Bson.Date 1718236800000L) in
      (* {"$date":1718236800000} *)
      let back = Ejson.decode wire in
      assert (Bson.equal back (Bson.Date 1718236800000L))
    ]} *)

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
