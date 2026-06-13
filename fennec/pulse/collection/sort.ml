(* Typed sort keys over field handles — replaces the stringly [~sort:(Bson.doc [("title",1)])] with
   [Sort.[ asc Fields.title; desc Fields.created ]]: a renamed field is a compile error, order is
   the list order. Compiles to the Mongo sort document the engine already executes. *)

type t = (string * int) list

let asc f = [ (Codec.field_name f, 1) ]
let desc f = [ (Codec.field_name f, -1) ]
let by keys = List.concat keys
let raw (b : Bson.t) : t = match b with Bson.Document kvs -> List.filter_map (function k, Bson.Int n -> Some (k, n) | _ -> None) kvs | _ -> []
let to_bson (s : t) : Bson.t = Bson.Document (List.map (fun (k, n) -> (k, Bson.int n)) s)
