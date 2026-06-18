(* value_format.ml — the {!Value} instance of the {!Serde} abstraction: the neutral data model as a
   first-class serde format. With the RICH Value model the projection is LOSSLESS — a [TDate] writes a
   [Value.Date], a [TId] a [Value.Id] (not a stringly approximation) — while decode still accepts the
   BSON engine's coercions (an integral number read as a date/int, a string as an id) so
   {!Sift.of_value} accepts exactly what {!Sift.decode} does, including JSON-parsed values. The [TDyn]
   escape is identity (the source IS already a neutral Value). Fully Bson-free — this is what
   {!Sift.to_value}/{!Sift.of_value} drive, with no BSON anywhere. *)

(* [from_string]: re-read a submitted String as a neutral leaf of the inner shape — Value-native (no
   Bson), mirroring the engine's coerce; the output is always a simple leaf, which decode then reads. *)
let rec coerce_value : type a. a Shape.shape -> string -> Value.t =
 fun shape s ->
  let open Shape in
  match shape with
  | TInt -> ( match int_of_string_opt (String.trim s) with Some i -> Value.Int i | None -> Value.String s)
  | TFloat _ -> ( match float_of_string_opt (String.trim s) with Some f -> Value.Float f | None -> Value.String s)
  | TBool -> Value.Bool (match String.lowercase_ascii (String.trim s) with "true" | "1" | "on" | "yes" -> true | _ -> false)
  | TDate -> ( match Int64.of_string_opt (String.trim s) with Some ms -> Value.Date ms | None -> Value.String s)
  | TCheck (_, _, _, inner) | TNorm (_, inner) | TCoerce inner -> coerce_value inner s
  | _ -> Value.String s

module Reader : Serde.READER with type src = Value.t = struct
  type src = Value.t

  let describe : src -> string = function
    | Value.Null -> "null"
    | Value.Bool _ -> "bool"
    | Value.Int _ -> "int"
    | Value.Int64 _ -> "int64"
    | Value.Float _ -> "float"
    | Value.String _ -> "string"
    | Value.Bytes _ -> "bytes"
    | Value.List _ -> "array"
    | Value.Assoc _ -> "document"
    | Value.Date _ -> "date"
    | Value.Id _ -> "id"
    | Value.Decimal _ -> "decimal"
    | Value.Timestamp _ -> "timestamp"
    | Value.Regex _ -> "regex"
    | Value.Symbol _ -> "symbol"
    | Value.Code _ -> "code"
    | Value.Min -> "min"
    | Value.Max -> "max"

  let is_null = function Value.Null -> true | _ -> false
  let to_bool = function Value.Bool b -> Some b | _ -> None
  let to_int = function Value.Int n -> Some n | Value.Float f when Float.is_integer f -> Some (int_of_float f) | _ -> None
  let to_float = function Value.Float f -> Some f | Value.Int n -> Some (float_of_int n) | _ -> None
  let to_string = function Value.String s -> Some s | _ -> None
  let to_id = function Value.Id s | Value.String s -> Some s | _ -> None

  let to_date = function
    | Value.Date d -> Some d
    | Value.Int n -> Some (Int64.of_int n)
    | Value.Int64 n -> Some n
    | Value.Float f when Float.is_integer f -> Some (Int64.of_float f)
    | _ -> None

  let to_dyn v = v (* the escape is already a neutral Value — identity, no Bson *)
  let to_list = function Value.List xs -> Some xs | _ -> None
  let to_assoc = function Value.Assoc kvs -> Some kvs | _ -> None

  let coerce_leaf : type a. a Shape.shape -> string -> src = coerce_value
end

module Writer : Serde.WRITER with type out = Value.t = struct
  type out = Value.t

  let null = Value.Null
  let bool b = Value.Bool b
  let int n = Value.Int n
  let float f = Value.Float f
  let string s = Value.String s
  let id s = Value.Id s
  let date d = Value.Date d
  let dyn b = b (* the escape is already a neutral Value — identity, no Bson *)
  let list xs = Value.List xs
  let assoc kvs = Value.Assoc kvs
end
