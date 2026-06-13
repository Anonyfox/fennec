(* The content hash behind delta resync (v2): both sides fingerprint a document's FIELDS (the _id
   travels separately) the same way, so a resubscribing client can tell the server what it already
   holds and receive only the difference. Canonical: Document keys sorted recursively (field order is
   storage noise), arrays positional; MD5 over that, truncated to 12 hex — a sync fingerprint, not a
   security boundary. Pure (bson only), bit-identical native and js_of_ocaml. *)

(* write the canonical form into [buf] — one Buffer instead of a tree of [^]/[concat] allocations;
   byte-identical to the old string form, so the MD5 fingerprint is unchanged *)
let rec canon_buf buf (b : Bson.t) =
  match b with
  | Bson.Document kvs ->
      let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) kvs in
      Buffer.add_char buf '{';
      List.iteri
        (fun i (k, v) ->
          if i > 0 then Buffer.add_char buf ',';
          Buffer.add_string buf k;
          Buffer.add_char buf ':';
          canon_buf buf v)
        sorted;
      Buffer.add_char buf '}'
  | Bson.Array xs ->
      Buffer.add_char buf '[';
      List.iteri (fun i v -> if i > 0 then Buffer.add_char buf ','; canon_buf buf v) xs;
      Buffer.add_char buf ']'
  | other -> Buffer.add_string buf (Bson.to_string other)

(* the fingerprint of a doc's FIELDS (an assoc list, order-insensitive) — MD5 straight over the
   canonical buffer, skipping the intermediate canon string *)
let fields (kvs : (string * Bson.t) list) : string =
  let buf = Buffer.create 64 in
  canon_buf buf (Bson.Document kvs);
  String.sub (Digest.to_hex (Digest.string (Buffer.contents buf))) 0 12
