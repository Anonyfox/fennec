(* EJSON wire codec: Bson.t (the in-memory value) <-> Json.t (the on-the-wire JSON projection).
   Implements the DDP/EJSON spec's four escape objects:

     Date         {"$date": <ms since epoch>}
     Binary       {"$binary": "<base64>"}
     custom type  {"$type": "<name>", "$value": <ejson>}     (ObjectID is "oid")
     escape       {"$escape": <obj>}   (a literal object shaped like a marker)

   Numbers are JS doubles on the wire; decode picks Int when integral. Escaping is minimal and
   shape-exact — we wrap only an object that would otherwise be misread as a marker — matching
   Meteor so data round-trips both ways. *)

(* a JSON object that structurally matches one of the EJSON markers, hence would be
   misinterpreted on decode unless escaped *)
let looks_like_marker (j : Json.t) : bool =
  match j with
  | Json.Obj [ ("$date", Json.Number _) ] -> true
  | Json.Obj [ ("$binary", Json.String _) ] -> true
  | Json.Obj [ ("$escape", _) ] -> true
  | Json.Obj [ ("$type", Json.String _); ("$value", _) ] -> true
  | _ -> false

let rec of_bson (b : Bson.t) : Json.t =
  match b with
  | Bson.Null -> Json.Null
  | Bson.Bool x -> Json.Bool x
  | Bson.Int n -> Json.Number (float_of_int n)
  | Bson.Int64 n -> Json.Number (Int64.to_float n)
  | Bson.Float f -> Json.Number f
  | Bson.String s -> Json.String s
  | Bson.Array xs -> Json.List (List.map of_bson xs)
  | Bson.Document kvs -> of_doc kvs
  | Bson.Object_id hex -> Json.Obj [ ("$type", Json.String "oid"); ("$value", Json.String hex) ]
  | Bson.Date ms -> Json.Obj [ ("$date", Json.Number (Int64.to_float ms)) ]
  | Bson.Binary { base64; _ } -> Json.Obj [ ("$binary", Json.String base64) ]
  | Bson.Decimal128 s -> Json.Obj [ ("$type", Json.String "Decimal"); ("$value", Json.String s) ]
  | Bson.Regex { pattern; options } ->
      Json.Obj
        [ ("$type", Json.String "Regex");
          ("$value", Json.List [ Json.String pattern; Json.String options ]) ]
  | Bson.Code s -> Json.Obj [ ("$type", Json.String "Code"); ("$value", Json.String s) ]
  | Bson.Code_with_scope (s, kvs) ->
      Json.Obj
        [ ("$type", Json.String "CodeScope");
          ("$value", Json.List [ Json.String s; of_doc kvs ]) ]
  | Bson.Timestamp { t; i } ->
      Json.Obj
        [ ("$type", Json.String "Timestamp");
          ("$value", Json.List [ Json.Number (float_of_int t); Json.Number (float_of_int i) ]) ]
  | Bson.Symbol s -> Json.String s
  | Bson.Min_key -> Json.Obj [ ("$type", Json.String "MinKey"); ("$value", Json.Number 0.) ]
  | Bson.Max_key -> Json.Obj [ ("$type", Json.String "MaxKey"); ("$value", Json.Number 0.) ]

and of_doc kvs =
  let inner = Json.Obj (List.map (fun (k, v) -> (k, of_bson v)) kvs) in
  if looks_like_marker inner then Json.Obj [ ("$escape", inner) ] else inner

let rec to_bson (j : Json.t) : Bson.t =
  match j with
  | Json.Null -> Bson.Null
  | Json.Bool b -> Bson.Bool b
  | Json.Number f ->
      if Float.is_integer f && Float.abs f < 1e15 then Bson.Int (int_of_float f)
      else Bson.Float f
  | Json.String s -> Bson.String s
  | Json.List xs -> Bson.Array (List.map to_bson xs)
  | Json.Obj kvs -> of_obj kvs

and of_obj kvs =
  match kvs with
  | [ ("$date", Json.Number ms) ] -> Bson.Date (Int64.of_float ms)
  | [ ("$binary", Json.String b64) ] -> Bson.Binary { subtype = "00"; base64 = b64 }
  | [ ("$escape", Json.Obj inner) ] ->
      (* the object is literal: decode its values but do NOT treat it as a marker *)
      Bson.Document (List.map (fun (k, v) -> (k, to_bson v)) inner)
  | [ ("$escape", other) ] -> to_bson other
  | [ ("$type", Json.String ty); ("$value", v) ]
  | [ ("$value", v); ("$type", Json.String ty) ] -> of_typed ty v
  | _ -> Bson.Document (List.map (fun (k, v) -> (k, to_bson v)) kvs)

and of_typed ty v =
  match (ty, v) with
  | "oid", Json.String hex -> Bson.Object_id hex
  | "Decimal", Json.String s -> Bson.Decimal128 s
  | "Code", Json.String s -> Bson.Code s
  | "Regex", Json.List [ Json.String p; Json.String o ] ->
      Bson.Regex { pattern = p; options = o }
  | "CodeScope", Json.List [ Json.String s; Json.Obj kvs ] ->
      Bson.Code_with_scope (s, List.map (fun (k, x) -> (k, to_bson x)) kvs)
  | "Timestamp", Json.List [ Json.Number t; Json.Number i ] ->
      Bson.Timestamp { t = int_of_float t; i = int_of_float i }
  | "MinKey", _ -> Bson.Min_key
  | "MaxKey", _ -> Bson.Max_key
  | _ -> Bson.Document [ ("$type", Bson.String ty); ("$value", to_bson v) ]

(* convenience: a document <-> a JSON object (fields are always documents) *)
let doc_to_json (kvs : (string * Bson.t) list) : Json.t = of_doc kvs

let json_to_doc (j : Json.t) : (string * Bson.t) list =
  match to_bson j with Bson.Document kvs -> kvs | _ -> []

(* string entry points *)
let encode (b : Bson.t) : string = Json.to_string (of_bson b)
let decode (s : string) : Bson.t = to_bson (Json.parse s)
