(* value_bson.ml — the BSON adapter for Sift's neutral {!Value}. The ONE Bson-coupled new module of
   the format-agnostic refactor: it is what moves to the Mongo-owning layer when Sift goes
   dependency-free. {!Value} itself never mentions Bson.

   Best-effort BY DESIGN. The lean data model keeps only the common denominator, so BSON's rich types
   collapse to their nearest neutral form on the way in — Object_id / Decimal128 / Code / Symbol /
   Regex → String, Date / Timestamp → Int, Binary → Bytes, Min/Max_key → Null. The SHAPED path never
   relies on round-tripping these: a shape reconstructs the precise BSON type itself on encode (a
   [TDate] writes a [Date], a [TId] a hex [Object_id]). This adapter is for DYNAMIC/escape values and
   raw interop only, where the exact tag is not knowable from a type.

   On the way out the model maps to canonical BSON: an [Int] is a 32-or-64-bit [Int], [Bytes] a
   generic-subtype [Binary]. [to_bson] then [of_bson] is the identity on neutral values; the reverse
   is identity only on the non-exotic BSON subset (which is exactly the shaped data Sift produces). *)

let rec of_bson : Bson.t -> Value.t = function
  | Bson.Null -> Value.Null
  | Bson.Bool b -> Value.Bool b
  | Bson.Int n -> Value.Int n
  | Bson.Int64 n -> Value.Int (Int64.to_int n)
  | Bson.Float f -> Value.Float f
  | Bson.String s -> Value.String s
  | Bson.Document kvs -> Value.Assoc (List.map (fun (k, v) -> (k, of_bson v)) kvs)
  | Bson.Array xs -> Value.List (List.map of_bson xs)
  | Bson.Object_id s -> Value.String s
  | Bson.Date ms -> Value.Int (Int64.to_int ms)
  | Bson.Timestamp { t; _ } -> Value.Int t
  | Bson.Binary { base64; _ } -> Value.Bytes (try Base64.decode_exn base64 with _ -> "")
  | Bson.Regex { pattern; _ } -> Value.String pattern
  | Bson.Decimal128 s -> Value.String s
  | Bson.Code s -> Value.String s
  | Bson.Code_with_scope (s, _) -> Value.String s
  | Bson.Symbol s -> Value.String s
  | Bson.Min_key -> Value.Null
  | Bson.Max_key -> Value.Null

let rec to_bson : Value.t -> Bson.t = function
  | Value.Null -> Bson.Null
  | Value.Bool b -> Bson.Bool b
  | Value.Int n -> Bson.Int n
  | Value.Float f -> Bson.Float f
  | Value.String s -> Bson.String s
  | Value.Bytes b -> Bson.Binary { subtype = "00"; base64 = Base64.encode_string b }
  | Value.List xs -> Bson.Array (List.map to_bson xs)
  | Value.Assoc kvs -> Bson.Document (List.map (fun (k, v) -> (k, to_bson v)) kvs)
