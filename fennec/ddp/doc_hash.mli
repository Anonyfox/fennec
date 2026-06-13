(** The content hash behind delta resync (v2): both sides fingerprint a document's fields the same
    way (canonical key order, MD5/12hex — a sync fingerprint, not a security boundary), so a
    resubscribing client can declare what it holds and receive only the difference. Pure;
    bit-identical native and js_of_ocaml.

    {[
      (* both sides fingerprint the same fields, so the client can declare its cache *)
      let h = Doc_hash.fields [ ("name", Bson.String "Ada"); ("age", Bson.Int 36) ] in
      (* order-insensitive: this yields the same hash *)
      assert (h = Doc_hash.fields [ ("age", Bson.Int 36); ("name", Bson.String "Ada") ])
    ]} *)

(** Fingerprint of a doc's fields (assoc-order-insensitive). *)
val fields : (string * Bson.t) list -> string
