(* A relaxed-JSON parser into {!Bson.t} — the HTTP wire form (plain JSON, no extended $oid/$date). The
   recursive-descent state stays private inside {!of_string}. *)

(** Parse a plain-JSON string (RFC 8259) to a {!Bson.t} tree — a JSON integer becomes [Int], a
    fractional/exponent number [Float]. The shape-directed decode then reads it. [Error msg] on a
    parse failure. *)
val of_string : string -> (Bson.t, string) result
