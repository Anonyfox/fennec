(* value_bson.ml — the BSON adapter for Sift's neutral {!Value}. The ONE Bson-coupled module of the
   format-agnostic core: it is what moves to the Mongo-owning layer (the [sift.bson] plugin) when Sift
   goes dependency-free. {!Value} itself never mentions Bson.

   LOSSLESS: the rich {!Value} model mirrors BSON's value space 1:1 (a date is a [Date], an objectid an
   [Id], a decimal a [Decimal], …), so [of_bson]/[to_bson] round-trip every value both ways — Value is
   a faithful, neutral replacement for {!Bson.t}. (The only narrowings are two near-mythical corners: a
   non-generic Binary SUBTYPE collapses to generic [Bytes], and the deprecated Code-with-scope drops
   its scope — neither appears in real document data.) *)

let rec of_bson : Bson.t -> Value.t = function
  | Bson.Null -> Value.Null
  | Bson.Bool b -> Value.Bool b
  | Bson.Int n -> Value.Int n
  | Bson.Int64 n -> Value.Int64 n
  | Bson.Float f -> Value.Float f
  | Bson.String s -> Value.String s
  | Bson.Document kvs -> Value.Assoc (List.map (fun (k, v) -> (k, of_bson v)) kvs)
  | Bson.Array xs -> Value.List (List.map of_bson xs)
  | Bson.Object_id s -> Value.Id s
  | Bson.Date ms -> Value.Date ms
  | Bson.Timestamp { t; i } -> Value.Timestamp (t, i)
  | Bson.Binary { base64; _ } -> Value.Bytes (try Base64.decode_exn base64 with _ -> "")
  | Bson.Regex { pattern; options } -> Value.Regex (pattern, options)
  | Bson.Decimal128 s -> Value.Decimal s
  | Bson.Code s -> Value.Code s
  | Bson.Code_with_scope (s, _) -> Value.Code s
  | Bson.Symbol s -> Value.Symbol s
  | Bson.Min_key -> Value.Min
  | Bson.Max_key -> Value.Max

let rec to_bson : Value.t -> Bson.t = function
  | Value.Null -> Bson.Null
  | Value.Bool b -> Bson.Bool b
  | Value.Int n -> Bson.Int n
  | Value.Int64 n -> Bson.Int64 n
  | Value.Float f -> Bson.Float f
  | Value.String s -> Bson.String s
  | Value.Bytes b -> Bson.Binary { subtype = "00"; base64 = Base64.encode_string b }
  | Value.List xs -> Bson.Array (List.map to_bson xs)
  | Value.Assoc kvs -> Bson.Document (List.map (fun (k, v) -> (k, to_bson v)) kvs)
  | Value.Date ms -> Bson.Date ms
  | Value.Id s -> Bson.Object_id s
  | Value.Decimal s -> Bson.Decimal128 s
  | Value.Timestamp (t, i) -> Bson.Timestamp { t; i }
  | Value.Regex (pattern, options) -> Bson.Regex { pattern; options }
  | Value.Symbol s -> Bson.Symbol s
  | Value.Code s -> Bson.Code s
  | Value.Min -> Bson.Min_key
  | Value.Max -> Bson.Max_key
