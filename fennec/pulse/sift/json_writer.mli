(* Relaxed (plain) JSON encode, shape-directed — the wire form an HTTP API / browser speaks (strings,
   numbers, [], {}), distinct from the {!Bson_json} bridge's extended JSON ($oid/$date). The buffer
   helpers stay private. *)

(** Encode a value to a plain-JSON string straight from the shape (a non-finite float → [null]; the
    [TBson] escape emits its {!Bson.t} as plain JSON). *)
val encode_json : 'a Shape.shape -> 'a -> string
