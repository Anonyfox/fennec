(* Extended-JSON codec: round-trips (by numeric-aware Bson.equal), the canonical wire forms, relaxed
   parsing, non-finite floats, find-style arrays, and malformed-input handling. *)

module BJ = Fennec_mongo_bson_json.Bson_json
module B = Bson

let rt b = BJ.of_string (BJ.to_string b)
let eq b = B.equal b (rt b)

let%test "round-trips scalars, including numeric type/precision" =
  List.for_all eq
    [ B.Null; B.Bool true; B.Bool false;
      B.Int 0; B.Int 42; B.Int (-7); B.Int 2147483647; B.Int (-2147483648);
      B.Int 5_000_000_000 (* > int32 → int64 on the wire, still numeric-equal *);
      B.Int64 0L; B.Int64 9223372036854775807L; B.Int64 (-9223372036854775808L);
      B.Float 0.0; B.Float 4.5; B.Float (-3.25); B.Float 1e300;
      B.String ""; B.String "h\195\169llo \240\159\166\138 \"q\" \n\ttab";
      B.Object_id (String.make 24 'a');
      B.Date 0L; B.Date 1700000000000L;
      B.Decimal128 "1.5"; B.Decimal128 "-0.00010";
      B.Code "function(){}"; B.Symbol "sym"; B.Min_key; B.Max_key ]

let%test "round-trips composite + special types" =
  List.for_all eq
    [ B.doc [ ("a", B.int 1); ("b", B.str "x"); ("c", B.array [ B.int 1; B.bool true; B.Null ]) ];
      B.doc [ ("nested", B.doc [ ("deep", B.array [ B.doc [ ("k", B.int 9) ] ]) ]) ];
      B.array []; B.doc [];
      B.Timestamp { t = 1700000000; i = 3 };
      B.Binary { subtype = "00"; base64 = "aGVsbG8=" };
      B.Regex { pattern = "^ab.*"; options = "i" };
      B.Code_with_scope ("return x", [ ("x", B.int 5) ]) ]

let%test "non-finite floats survive via canonical tokens" =
  (match rt (B.Float infinity) with B.Float f -> f = infinity | _ -> false)
  && (match rt (B.Float neg_infinity) with B.Float f -> f = neg_infinity | _ -> false)
  && match rt (B.Float nan) with B.Float f -> Float.is_nan f | _ -> false

let%test "emits canonical wrapped forms exactly" =
  BJ.to_string (B.int 42) = {|{"$numberInt":"42"}|}
  && BJ.to_string (B.Int64 7L) = {|{"$numberLong":"7"}|}
  && BJ.to_string (B.Decimal128 "1.5") = {|{"$numberDecimal":"1.5"}|}
  && BJ.to_string (B.Object_id "abc") = {|{"$oid":"abc"}|}
  && BJ.to_string (B.str "hi") = {|"hi"|}

let%test "parses relaxed forms (bare numbers, bare $numberInt, bare $date)" =
  (match BJ.of_string "42" with B.Int 42 -> true | _ -> false)
  && (match BJ.of_string "4.5" with B.Float f -> f = 4.5 | _ -> false)
  && (match BJ.of_string {|{"$numberInt":42}|} with B.Int 42 -> true | _ -> false)
  && match BJ.of_string {|{"$date":1700000000000}|} with B.Date d -> d = 1700000000000L | _ -> false

let%test "list_of_string reads a find-style array of documents" =
  match BJ.list_of_string {|[{"$oid":"a"},{"x":{"$numberInt":"1"}}]|} with
  | [ B.Object_id "a"; B.Document [ ("x", B.Int 1) ] ] -> true
  | _ -> false

let%test "of_string_opt returns None on malformed input, Some on valid" =
  BJ.of_string_opt "{bad" = None
  && BJ.of_string_opt "42 garbage" = None
  && match BJ.of_string_opt "{}" with Some (B.Document []) -> true | _ -> false

let () = exit (Fennec_hunt_unit.run ())
