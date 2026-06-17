(* json_writer.ml — relaxed-JSON encode, shape-directed, no Bson.t tree (SIFT axis A: a second format).

   The bson_json bridge emits CANONICAL extended JSON ($numberInt / $oid / $date …) — lossless for
   BSON interchange (mongosh), but not what an HTTP API sends. This emits plain, relaxed JSON (strings,
   numbers, [], {}) straight into a buffer from the typed value — what a browser/API client expects —
   skipping the Bson.t tree the bridge builds. It round-trips: [of_json_string c (encode_json c v)]
   reads it back (of_json accepts relaxed JSON; an id is a string, a date a number, which decode's
   coercions accept). Non-finite floats can't be represented in plain JSON → emitted as null (rare;
   finite is the default). *)

open Shape

let escape_string (b : Buffer.t) (s : string) : unit =
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | '\b' -> Buffer.add_string b "\\b"
      | '\012' -> Buffer.add_string b "\\f"
      | c when Char.code c < 0x20 -> Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"'

let float_str f = if Float.is_finite f then Printf.sprintf "%.17g" f else "null"

(* relaxed JSON for the [TBson] escape hatch *)
let rec bson_json (b : Buffer.t) (v : Bson.t) : unit =
  match v with
  | Bson.Null | Bson.Min_key | Bson.Max_key -> Buffer.add_string b "null"
  | Bson.Bool x -> Buffer.add_string b (if x then "true" else "false")
  | Bson.Int n -> Buffer.add_string b (string_of_int n)
  | Bson.Int64 n -> Buffer.add_string b (Int64.to_string n)
  | Bson.Float f -> Buffer.add_string b (float_str f)
  | Bson.Date ms -> Buffer.add_string b (Int64.to_string ms)
  | Bson.String s | Bson.Code s | Bson.Symbol s | Bson.Object_id s | Bson.Decimal128 s -> escape_string b s
  | Bson.Document kvs -> bson_obj b kvs
  | Bson.Array xs -> Buffer.add_char b '['; List.iteri (fun i x -> if i > 0 then Buffer.add_char b ','; bson_json b x) xs; Buffer.add_char b ']'
  | Bson.Binary { base64; _ } -> escape_string b base64
  | Bson.Regex { pattern; _ } -> escape_string b pattern
  | Bson.Timestamp _ -> Buffer.add_string b "0"
  | Bson.Code_with_scope (c, _) -> escape_string b c

and bson_obj b kvs =
  Buffer.add_char b '{';
  List.iteri (fun i (k, v) -> if i > 0 then Buffer.add_char b ','; escape_string b k; Buffer.add_char b ':'; bson_json b v) kvs;
  Buffer.add_char b '}'

let rec write_json : type a. a shape -> a -> Buffer.t -> unit =
 fun shape v b ->
  match shape with
  | TString -> escape_string b v
  | TInt -> Buffer.add_string b (string_of_int v)
  | TFloat _ -> Buffer.add_string b (float_str v)
  | TBool -> Buffer.add_string b (if v then "true" else "false")
  | TDate -> Buffer.add_string b (Int64.to_string v)
  | TId -> escape_string b v
  | TBson -> bson_json b v
  | TUnit -> Buffer.add_string b "null"
  | TList el -> Buffer.add_char b '['; write_list el v b 0; Buffer.add_char b ']'
  | TOption el -> ( match v with Some x -> write_json el x b | None -> Buffer.add_string b "null")
  | TMap el -> write_map el v b
  | TCheck (_, _, _, inner) -> write_json inner v b
  | TNorm (f, inner) -> write_json inner (f v) b
  | TConv (inj, _, inner) -> write_json inner (inj v) b
  | TCoerce inner -> write_json inner v b
  | TLazy l -> write_json (Lazy.force l) v b
  | TObj o -> write_record o.members v b
  | TVariant { tag; cases } -> write_variant tag cases v b

and write_list : type a. a shape -> a list -> Buffer.t -> int -> unit =
 fun el xs b i -> match xs with [] -> () | x :: tl -> (if i > 0 then Buffer.add_char b ','); write_json el x b; write_list el tl b (i + 1)

and write_map : type a. a shape -> (string * a) list -> Buffer.t -> unit =
 fun el kvs b ->
  Buffer.add_char b '{';
  List.iteri (fun i (k, x) -> if i > 0 then Buffer.add_char b ','; escape_string b k; Buffer.add_char b ':'; write_json el x b) kvs;
  Buffer.add_char b '}'

and write_record : type r. r bound_field list -> r -> Buffer.t -> unit =
 fun members r b ->
  Buffer.add_char b '{';
  let first = ref true in
  List.iter
    (fun (Bound_field f) ->
      let v = f.get r in
      if not (f.omit v) then ((if !first then first := false else Buffer.add_char b ','); escape_string b f.name; Buffer.add_char b ':'; write_json f.shape v b))
    members;
  Buffer.add_char b '}'

and write_variant : type r. string -> r case list -> r -> Buffer.t -> unit =
 fun tag cases v b ->
  let rec go = function
    | [] -> invalid_arg "Sift: variant value matches no declared case"
    | Case c :: rest -> (
        match c.project v with
        | Some a ->
            Buffer.add_char b '{';
            escape_string b tag;
            Buffer.add_char b ':';
            escape_string b c.name;
            List.iter (fun (Bound_field f) -> let fv = f.get a in if not (f.omit fv) then (Buffer.add_char b ','; escape_string b f.name; Buffer.add_char b ':'; write_json f.shape fv b)) c.body.members;
            Buffer.add_char b '}'
        | None -> go rest)
  in
  go cases

let encode_json (shape : 'a shape) (v : 'a) : string =
  let b = Buffer.create 256 in
  write_json shape v b;
  Buffer.contents b
