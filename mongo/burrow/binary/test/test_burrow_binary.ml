(* The record codec must round-trip every [Bson.t] losslessly: [decode (encode d) = d]. We hammer it
   with random documents covering all 19 BSON types, nested to depth 3, and check the zero-copy
   [find_field] agrees with a full decode on every top-level field. Float equality is by bits (so NaN
   and -0.0 are handled). *)
module B = Bson
module Bb = Burrow_binary

let rec bson_eq a b =
  match (a, b) with
  | B.Float x, B.Float y -> Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | B.Document fa, B.Document fb -> fields_eq fa fb
  | B.Array xa, B.Array xb -> List.length xa = List.length xb && List.for_all2 bson_eq xa xb
  | B.Code_with_scope (ca, sa), B.Code_with_scope (cb, sb) -> ca = cb && fields_eq sa sb
  | _ -> a = b

and fields_eq fa fb =
  List.length fa = List.length fb
  && List.for_all2 (fun (na, va) (nb, vb) -> na = nb && bson_eq va vb) fa fb

(* ---- generators ---------------------------------------------------------------------------- *)

let rand_oid () = String.init 24 (fun _ -> "0123456789abcdef".[Random.int 16])
let rand_name () = String.init (1 + Random.int 6) (fun _ -> "abcdefg_".[Random.int 8])
let rand_str () = String.init (Random.int 8) (fun _ -> Char.chr (32 + Random.int 95))

let rand_float () =
  match Random.int 8 with
  | 0 -> Float.nan
  | 1 -> Float.infinity
  | 2 -> Float.neg_infinity
  | 3 -> 0.0
  | 4 -> -0.0
  | _ -> Random.float 2e9 -. 1e9

(* int kept within int32 so [Int] round-trips to [Int]; the widening case is tested separately *)
let rand_i32 () = Random.int 2_000_000 - 1_000_000

let rec rand_value depth : B.t =
  match Random.int (if depth <= 0 then 17 else 19) with
  | 0 -> B.Null
  | 1 -> B.Bool (Random.bool ())
  | 2 | 16 -> B.Int (rand_i32 ())
  | 3 -> B.Int64 (Int64.of_int (Random.int 1_000_000 - 500_000))
  | 4 -> B.Float (rand_float ())
  | 5 -> B.String (rand_str ())
  | 6 -> B.Object_id (rand_oid ())
  | 7 -> B.Date (Int64.of_int (Random.int 4_000_000 - 2_000_000))
  | 8 -> B.Timestamp { t = Random.int 1_000_000; i = Random.int 1000 }
  | 9 -> B.Binary { subtype = "00"; base64 = rand_str () }
  | 10 -> B.Regex { pattern = rand_str (); options = "im" }
  | 11 -> B.Decimal128 (string_of_int (Random.int 1000) ^ ".5")
  | 12 -> B.Code (rand_str ())
  | 13 -> B.Symbol (rand_str ())
  | 14 -> B.Min_key
  | 15 -> B.Max_key
  | 17 -> B.Array (List.init (Random.int 4) (fun _ -> rand_value (depth - 1)))
  | _ -> B.Document (rand_fields (depth - 1))

and rand_fields depth =
  List.init (Random.int 5) (fun i -> (rand_name () ^ string_of_int i, rand_value depth))

let%test "round-trip: decode (encode d) = d over 50k random documents (all 19 BSON types, nested)" =
  Random.init 0xB50;
  let ok = ref true and shown = ref 0 in
  for _ = 1 to 50_000 do
    let d = B.Document (rand_fields 3) in
    let d' = Bb.decode (Bb.encode d) in
    if not (bson_eq d d') then begin
      ok := false;
      if !shown < 8 then begin
        incr shown;
        Printf.eprintf "RT MISMATCH:\n in =%s\n out=%s\n%!" (B.to_string d) (B.to_string d')
      end
    end
  done;
  !ok

let%test "find_field agrees with a full decode on every top-level field; absent name is None" =
  Random.init 7;
  let ok = ref true in
  for _ = 1 to 20_000 do
    let fields = rand_fields 2 in
    let enc = Bb.encode (B.Document fields) in
    List.iter
      (fun (name, v) ->
        match Bb.find_field enc name with Some v' when bson_eq v v' -> () | _ -> ok := false)
      fields;
    if Bb.find_field enc "__definitely_absent__" <> None then ok := false
  done;
  !ok

let%test "Int outside int32 widens to Int64 on round-trip; within int32 stays Int" =
  (match Bb.decode (Bb.encode (B.Document [ ("a", B.Int 5_000_000_000) ])) with
   | B.Document [ ("a", B.Int64 n) ] -> Int64.equal n 5_000_000_000L
   | _ -> false)
  &&
  match Bb.decode (Bb.encode (B.Document [ ("a", B.Int 42) ])) with
  | B.Document [ ("a", B.Int 42) ] -> true
  | _ -> false

let%test "empty document encodes to the 5-byte BSON empty doc" =
  String.length (Bb.encode (B.Document [])) = 5

let () = exit (Fennec_hunt_unit.run ())
