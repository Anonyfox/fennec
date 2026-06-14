(* Memcomparable index-key encoding: [String.compare (encode a) (encode b)] has the same sign as
   [Bson.compare a b] over the indexable scalar domain. Pure; see the .ml for the byte layout. *)

exception Unsupported of string
(** Raised by {!encode} on composite / non-indexable types (Document / Array / Binary / Timestamp /
    Regex / Code / Decimal128) — these are not index-key material. *)

val encode : Bson.t -> string
(** Order-preserving encoding of an indexable scalar. Prefix-free (variable-length strings are escaped
    and terminated), so concatenations stay order-correct. *)

val encode_fields : Bson.t list -> string
(** Compound-key prefix: per-field {!encode} concatenated, all ascending. *)
