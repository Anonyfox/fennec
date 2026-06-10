(* The content hash behind delta resync (v2): both sides fingerprint a document's FIELDS (the _id
   travels separately) the same way, so a resubscribing client can tell the server what it already
   holds and receive only the difference. Canonical: Document keys sorted recursively (field order is
   storage noise), arrays positional; MD5 over that, truncated to 12 hex — a sync fingerprint, not a
   security boundary. Pure (bson only), bit-identical native and js_of_ocaml. *)

let rec canon (b : Bson.t) : string =
  match b with
  | Bson.Document kvs ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) kvs in
      "{" ^ String.concat "," (List.map (fun (k, v) -> k ^ ":" ^ canon v) sorted) ^ "}"
  | Bson.Array xs -> "[" ^ String.concat "," (List.map canon xs) ^ "]"
  | other -> Bson.to_string other

(* the fingerprint of a doc's FIELDS (an assoc list, order-insensitive) *)
let fields (kvs : (string * Bson.t) list) : string =
  String.sub (Digest.to_hex (Digest.string (canon (Bson.Document kvs)))) 0 12
