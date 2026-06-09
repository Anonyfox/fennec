(* The SSR live-data seed payload. The server embeds a publication's documents PLUS the collection
   they belong to into Fur's seed <script>; the browser client reads it back to pre-populate the
   merge store before the socket opens (flicker-free hydration).

   Carrying the collection is what makes hydration robust under multi-collection apps: the browser's
   [publish] is a no-op, so it cannot re-derive which collection a publication feeds — if we keyed
   only by publication name (the old [collection_of_name name] convention), a publication whose name
   differs from its collection would seed into the wrong collection and [find]/live deltas (which use
   the REAL collection) would silently miss it. The collection therefore travels in the payload. *)

module BJ = Fennec_mongo_bson_json.Bson_json

(* [encode ~collection docs] → the wire string embedded in the page. Shape: {"c": collection, "d":
   [docs]} as extended-JSON, so the collection rides alongside its documents. *)
let encode ~collection (docs : Bson.t list) : string =
  BJ.to_string (Bson.Document [ ("c", Bson.String collection); ("d", Bson.Array docs) ])

(* [decode s] reads back [Some (collection, docs)], or [None] if the payload is malformed/legacy. *)
let decode (s : string) : (string * Bson.t list) option =
  match BJ.of_string_opt s with
  | Some (Bson.Document kvs) -> (
      match (List.assoc_opt "c" kvs, List.assoc_opt "d" kvs) with
      | Some (Bson.String c), Some (Bson.Array docs) -> Some (c, docs)
      | _ -> None)
  | _ -> None
