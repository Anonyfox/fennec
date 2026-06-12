(* Typed selectors over field handles — functions, not a parser: each combinator compiles to the
   same Bson selector the engine already executes, with field names and value encodings coming from
   the declaration (a renamed field is a compile error, not a silently-empty query). [raw] keeps
   the full Mongo operator surface reachable. *)

type t = (string * Bson.t) list

let op name f op_ v = [ (Codec.field_name f, Bson.doc [ (op_, Codec.field_enc f v) ]) ] [@@inline] [@@warning "-27"]
let eq f v = [ (Codec.field_name f, Codec.field_enc f v) ]
let ne f v = op "ne" f "$ne" v
let lt f v = op "lt" f "$lt" v
let lte f v = op "lte" f "$lte" v
let gt f v = op "gt" f "$gt" v
let gte f v = op "gte" f "$gte" v
let in_ f vs = [ (Codec.field_name f, Bson.doc [ ("$in", Bson.Array (List.map (Codec.field_enc f) vs)) ]) ]
let exists f b = [ (Codec.field_name f, Bson.doc [ ("$exists", Bson.Bool b) ]) ]
let has f v = [ (Codec.field_name f, Codec.field_elem_enc f v) ] (* array membership *)
let all qs = List.concat qs
let any qs = [ ("$or", Bson.Array (List.map (fun q -> Bson.Document q) qs)) ]
let raw (b : Bson.t) : t = match b with Bson.Document kvs -> kvs | _ -> []
let to_bson (q : t) : Bson.t = Bson.Document q
