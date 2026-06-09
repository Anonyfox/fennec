(* The deterministic subscription key (name + params), shared by the SSR collector and the client
   session so the inline-hydration payload lines up with what the client looks up. Dependency-free
   string key. Pure → native + JS. *)

let rec bkey (b : Bson.t) : string =
  match b with
  | Bson.Null -> "null"
  | Bson.Bool x -> if x then "t" else "f"
  | Bson.Int n -> string_of_int n
  | Bson.Int64 n -> Int64.to_string n
  | Bson.Float f -> string_of_float f
  | Bson.String s -> "\"" ^ s ^ "\""
  | Bson.Object_id s -> "oid:" ^ s
  | Bson.Date d -> "d:" ^ Int64.to_string d
  | Bson.Document kvs -> "{" ^ String.concat "," (List.map (fun (k, v) -> k ^ ":" ^ bkey v) kvs) ^ "}"
  | Bson.Array xs -> "[" ^ String.concat "," (List.map bkey xs) ^ "]"
  | _ -> "?"

let key (name : string) (params : Bson.t list) : string =
  name ^ "|" ^ String.concat "," (List.map bkey params)

(* the collection a publication feeds, by convention name -> name (override at the find call site
   when a publication feeds a differently-named collection) *)
let collection_of_name (name : string) : string = name
