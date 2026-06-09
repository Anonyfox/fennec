(* The SSR live-data seed payload. The server embeds a publication's documents — GROUPED BY the
   collection they belong to (a publication may feed several) — into Fur's seed <script>; the browser
   client reads it back to pre-populate the merge store before the socket opens (flicker-free
   hydration).

   Carrying the collection name(s) is what makes hydration robust: the browser's [publish] is a no-op,
   so it cannot re-derive which collection(s) a publication feeds. If we keyed only by publication
   name, a publication whose name differs from its collection — or one feeding multiple collections —
   would seed into the wrong place and [find]/live deltas (which use the REAL collection) would miss
   it. So the collections ride in the payload, one {c; d} group each. *)

module BJ = Fennec_mongo_bson_json.Bson_json

let group_of (collection, docs) = Bson.Document [ ("c", Bson.String collection); ("d", Bson.Array docs) ]

let group_to = function
  | Bson.Document kvs -> (
      match (List.assoc_opt "c" kvs, List.assoc_opt "d" kvs) with
      | Some (Bson.String c), Some (Bson.Array docs) -> Some (c, docs)
      | _ -> None)
  | _ -> None

(* [encode groups] → the wire string embedded in the page: a JSON array of {"c": collection, "d":
   [docs]} groups (extended-JSON), one per collection the publication feeds. *)
let encode (groups : (string * Bson.t list) list) : string =
  BJ.to_string (Bson.Array (List.map group_of groups))

(* [decode s] reads the groups back; an empty list on a malformed/legacy/absent payload. *)
let decode (s : string) : (string * Bson.t list) list =
  match BJ.of_string_opt s with Some (Bson.Array groups) -> List.filter_map group_to groups | _ -> []
