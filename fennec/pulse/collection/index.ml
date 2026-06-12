(* Declarative indexes over field handles — declared once with the collection, ensured at boot
   (mongod createIndex; declared parity on the in-memory engine). *)

type key = string * [ `Asc | `Desc ]
type t = { keys : key list; unique : bool }

let asc f = { keys = [ (Codec.field_name f, `Asc) ]; unique = false }
let desc f = { keys = [ (Codec.field_name f, `Desc) ]; unique = false }
let compound keys = { keys = List.concat_map (fun i -> i.keys) keys; unique = false }
let unique i = { i with unique = true }

let keys_bson i =
  Bson.Document (List.map (fun (n, dir) -> (n, Bson.int (match dir with `Asc -> 1 | `Desc -> -1))) i.keys)
let is_unique i = i.unique
