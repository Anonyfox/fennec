(* Extended-JSON codec for [Bson.t] — the bridge to the C driver, which speaks MongoDB canonical/
   relaxed extended JSON. Kept out of [bson.ml] so the value type stays minimal.

   Writes emit CANONICAL extended JSON: every typed number is wrapped as a string ($numberInt /
   $numberLong / $numberDouble), so precision survives regardless of how the JSON layer represents
   numbers, and Decimal128 is simply {$numberDecimal: "..."} — a string libbson interprets, so no
   decimal128 binary math lives here. Reads accept canonical AND relaxed forms (wrapped or bare),
   and anything unmodelled degrades to a plain document rather than being dropped. *)

module J = Fennec_mongo_json.Json
open Bson

(* canonical $numberDouble payload: special tokens for non-finite, else a round-trippable decimal *)
let double_string f =
  if Float.is_nan f then "NaN"
  else if f = Float.infinity then "Infinity"
  else if f = Float.neg_infinity then "-Infinity"
  else Printf.sprintf "%.17g" f

let in_int32 n = n >= -2147483648 && n <= 2147483647

(* ---- Bson.t -> canonical extended-JSON AST ------------------------------- *)

let rec to_json : t -> J.t = function
  | Null -> J.Null
  | Bool b -> J.Bool b
  | Int n -> J.Obj [ ((if in_int32 n then "$numberInt" else "$numberLong"), J.String (string_of_int n)) ]
  | Int64 n -> J.Obj [ ("$numberLong", J.String (Int64.to_string n)) ]
  | Float f -> J.Obj [ ("$numberDouble", J.String (double_string f)) ]
  | String s -> J.String s
  | Document kvs -> J.Obj (List.map (fun (k, v) -> (k, to_json v)) kvs)
  | Array xs -> J.List (List.map to_json xs)
  | Object_id s -> J.Obj [ ("$oid", J.String s) ]
  | Date ms -> J.Obj [ ("$date", J.Obj [ ("$numberLong", J.String (Int64.to_string ms)) ]) ]
  | Timestamp { t; i } ->
      J.Obj [ ("$timestamp", J.Obj [ ("t", J.Number (float_of_int t)); ("i", J.Number (float_of_int i)) ]) ]
  | Binary { subtype; base64 } ->
      J.Obj [ ("$binary", J.Obj [ ("base64", J.String base64); ("subType", J.String subtype) ]) ]
  | Regex { pattern; options } ->
      J.Obj [ ("$regularExpression", J.Obj [ ("pattern", J.String pattern); ("options", J.String options) ]) ]
  | Decimal128 s -> J.Obj [ ("$numberDecimal", J.String s) ]
  | Code c -> J.Obj [ ("$code", J.String c) ]
  | Code_with_scope (c, kvs) ->
      J.Obj [ ("$code", J.String c); ("$scope", J.Obj (List.map (fun (k, v) -> (k, to_json v)) kvs)) ]
  | Symbol s -> J.Obj [ ("$symbol", J.String s) ]
  | Min_key -> J.Obj [ ("$minKey", J.Number 1.) ]
  | Max_key -> J.Obj [ ("$maxKey", J.Number 1.) ]

(* ---- extended-JSON AST -> Bson.t (canonical or relaxed) ------------------ *)

let int64_of_json = function
  | J.String s -> ( match Int64.of_string_opt s with Some n -> n | None -> 0L)
  | J.Number f -> Int64.of_float f
  | _ -> 0L

let int_of_json = function
  | J.String s -> ( match int_of_string_opt s with Some n -> n | None -> 0)
  | J.Number f -> int_of_float f
  | _ -> 0

let float_of_ejson_string = function
  | "Infinity" -> Float.infinity
  | "-Infinity" -> Float.neg_infinity
  | "NaN" -> Float.nan
  | s -> ( match float_of_string_opt s with Some f -> f | None -> 0.)

let float_of_json = function
  | J.String s -> float_of_ejson_string s
  | J.Number f -> f
  | _ -> 0.

let rec of_json : J.t -> t = function
  | J.Null -> Null
  | J.Bool b -> Bool b
  | J.Number f -> if Float.is_integer f && Float.abs f < J.int_cutoff then Int (int_of_float f) else Float f
  | J.String s -> String s
  | J.List xs -> Array (List.map of_json xs)
  | J.Obj kvs -> of_assoc kvs

and of_assoc kvs =
  match kvs with
  | [ ("$oid", J.String s) ] -> Object_id s
  | [ ("$date", J.Obj [ ("$numberLong", n) ]) ] -> Date (int64_of_json n)
  | [ ("$date", J.Number ms) ] -> Date (Int64.of_float ms)
  | [ ("$numberLong", n) ] -> Int64 (int64_of_json n)
  | [ ("$numberInt", n) ] -> Int (int_of_json n)
  | [ ("$numberDouble", n) ] -> Float (float_of_json n)
  | [ ("$numberDecimal", J.String s) ] -> Decimal128 s
  | [ ("$symbol", J.String s) ] -> Symbol s
  | [ ("$timestamp", J.Obj ts) ] ->
      let g k = match List.assoc_opt k ts with Some v -> int_of_json v | None -> 0 in
      Timestamp { t = g "t"; i = g "i" }
  | [ ("$minKey", _) ] -> Min_key
  | [ ("$maxKey", _) ] -> Max_key
  | [ ("$binary", J.Obj b) ] ->
      let g k = match List.assoc_opt k b with Some (J.String s) -> s | _ -> "" in
      Binary { base64 = g "base64"; subtype = g "subType" }
  | [ ("$binary", J.String data); ("$type", J.String st) ] -> Binary { base64 = data; subtype = st }
  | [ ("$regularExpression", J.Obj r) ] ->
      let g k = match List.assoc_opt k r with Some (J.String s) -> s | _ -> "" in
      Regex { pattern = g "pattern"; options = g "options" }
  | [ ("$regex", J.String p); ("$options", J.String o) ] -> Regex { pattern = p; options = o }
  | [ ("$code", J.String c) ] -> Code c
  | [ ("$code", J.String c); ("$scope", J.Obj s) ] ->
      Code_with_scope (c, List.map (fun (k, v) -> (k, of_json v)) s)
  | _ -> Document (List.map (fun (k, v) -> (k, of_json v)) kvs)

(* ---- string interop (what the C driver exchanges) ------------------------ *)

let to_string b = J.to_string (to_json b)
let of_string s = of_json (J.parse s)
let of_string_opt s = match J.parse_opt s with Some j -> Some (of_json j) | None -> None
let list_of_string s = match J.parse s with J.List xs -> List.map of_json xs | other -> [ of_json other ]
