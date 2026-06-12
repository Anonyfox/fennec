(* $jsonSchema, rendered from Codec's neutral [view] — the structural half of validation pushed
   into the DATABASE: mongod rejects foreign writes that violate the declared shape, and minimongo
   enforces the identical rule in-engine. Renderable refinements ride their hints (min_len →
   minLength, pattern, enum, bounds, items…); arbitrary checks (H_none) are app-side-only, by
   design. Objects deliberately do NOT set additionalProperties — legacy fields must keep their
   docs writable (evolution tolerance); requiredness comes from the field specs (an [opt] field is
   simply not in [required]). Pure rendering; installation rides the backend boot path. *)

let doc = Bson.doc
let str = Bson.str

(* merge one hint into a schema document *)
let apply_hint (h : Codec.hint) (kvs : (string * Bson.t) list) : (string * Bson.t) list =
  match h with
  | Codec.H_none -> kvs
  | H_min_len n -> ("minLength", Bson.int n) :: kvs
  | H_max_len n -> ("maxLength", Bson.int n) :: kvs
  | H_pattern p -> ("pattern", str p) :: kvs
  | H_enum vs -> ("enum", Bson.Array (List.map str vs)) :: kvs
  | H_min x -> ("minimum", Bson.Float x) :: kvs
  | H_max x -> ("maximum", Bson.Float x) :: kvs
  | H_multiple_of x -> ("multipleOf", Bson.Float x) :: kvs
  | H_min_items n -> ("minItems", Bson.int n) :: kvs
  | H_max_items n -> ("maxItems", Bson.int n) :: kvs
  | H_unique_items -> ("uniqueItems", Bson.Bool true) :: kvs

let bson_type names =
  match names with [ one ] -> ("bsonType", str one) | many -> ("bsonType", Bson.Array (List.map str many))

let rec render (v : Codec.view) : (string * Bson.t) list =
  match v with
  | Codec.V_string -> [ bson_type [ "string" ] ]
  | V_int -> [ bson_type [ "int"; "long"; "double" ] ] (* EJSON/driver reality: ints may land wide *)
  | V_float -> [ bson_type [ "double"; "int"; "long" ] ]
  | V_bool -> [ bson_type [ "bool" ] ]
  | V_date -> [ bson_type [ "date" ] ]
  | V_id -> [ bson_type [ "objectId"; "string" ] ]
  | V_bson -> [] (* the dynamic escape hatch: anything *)
  | V_unit -> [ bson_type [ "null" ] ]
  | V_list el -> [ bson_type [ "array" ]; ("items", doc (render el)) ]
  | V_option el -> render el (* optionality = absence from [required], not a type union *)
  | V_map el -> [ bson_type [ "object" ]; ("additionalProperties", doc (render el)) ]
  | V_check (h, inner) -> apply_hint h (render inner)
  | V_obj fields -> render_obj fields
  | V_variant (tag, cases) ->
      [ ("oneOf",
         Bson.Array
           (List.map
              (fun (name, fields) ->
                let case_fields = (tag, true, Codec.V_check (Codec.H_enum [ name ], Codec.V_string)) :: fields in
                doc (render_obj case_fields))
              cases)) ]

and render_obj fields =
  let required =
    List.filter_map (fun (n, req, _) -> if req then Some (str n) else None) fields
  in
  let properties = List.map (fun (n, _, fv) -> (n, doc (render fv))) fields in
  bson_type [ "object" ]
  :: (("properties", doc properties)
     :: (if required = [] then [] else [ ("required", Bson.Array required) ]))

let json_schema (view : Codec.view) : Bson.t = doc (render view)

(* the validator document mongod's create/collMod expects *)
let validator (c : 'a Codec.t) : Bson.t = doc [ ("$jsonSchema", json_schema (Codec.view c)) ]
