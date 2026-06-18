(* value_format.ml — the {!Value} instance of the {!Format} abstraction: the neutral data model as a
   first-class serde format. Its coercions MIRROR the BSON engine (an integral [Float] read as an
   [Int], an [Int] read as a [Float] / date, a [String] as an id) so {!Sift.of_value} accepts exactly
   what {!Sift.decode} does. The [TBson] escape goes through {!Value_bson} — lossy for exotic BSON,
   which the neutral model cannot represent anyway. This is what {!Sift.to_value}/{!Sift.of_value}
   drive, with no BSON hop. *)

module Reader : Serde.READER with type src = Value.t = struct
  type src = Value.t

  let describe : src -> string = function
    | Value.Null -> "null"
    | Value.Bool _ -> "bool"
    | Value.Int _ -> "int"
    | Value.Float _ -> "float"
    | Value.String _ -> "string"
    | Value.Bytes _ -> "bytes"
    | Value.List _ -> "array"
    | Value.Assoc _ -> "document"

  let is_null = function Value.Null -> true | _ -> false
  let to_bool = function Value.Bool b -> Some b | _ -> None
  let to_int = function Value.Int n -> Some n | Value.Float f when Float.is_integer f -> Some (int_of_float f) | _ -> None
  let to_float = function Value.Float f -> Some f | Value.Int n -> Some (float_of_int n) | _ -> None
  let to_string = function Value.String s -> Some s | _ -> None
  let to_id = function Value.String s -> Some s | _ -> None
  let to_date = function Value.Int n -> Some (Int64.of_int n) | Value.Float f when Float.is_integer f -> Some (Int64.of_float f) | _ -> None
  let to_bson v = Value_bson.to_bson v
  let to_list = function Value.List xs -> Some xs | _ -> None
  let to_assoc = function Value.Assoc kvs -> Some kvs | _ -> None

  (* reuse the engine's leaf coercion, bridged to neutral — the coerced output is always a simple
     leaf (Int/Float/Bool/String), which the bridge carries faithfully *)
  let coerce_leaf : type a. a Shape.shape -> string -> src = fun inner s -> Value_bson.of_bson (Engine.coerce_string inner s)
end

module Writer : Serde.WRITER with type out = Value.t = struct
  type out = Value.t

  let null = Value.Null
  let bool b = Value.Bool b
  let int n = Value.Int n
  let float f = Value.Float f
  let string s = Value.String s
  let id s = Value.String s (* the neutral model has no ObjectId; an id is a string *)
  let date d = Value.Int (Int64.to_int d)
  let bson b = Value_bson.of_bson b
  let list xs = Value.List xs
  let assoc kvs = Value.Assoc kvs
end
