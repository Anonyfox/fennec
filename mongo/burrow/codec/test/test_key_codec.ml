(* Evidence for the memcomparable key codec: over 200k random scalar pairs, the byte order of the
   encoded keys must match Bson.compare exactly. Counterexamples are printed. *)
module B = Bson
module Key_codec = Burrow_codec.Key_codec

let sign n = if n < 0 then -1 else if n > 0 then 1 else 0

let rand_float () =
  match Random.int 12 with
  | 0 -> Float.nan
  | 1 -> Float.infinity
  | 2 -> Float.neg_infinity
  | 3 -> 0.0
  | 4 -> -0.0
  | 5 -> float_of_int (Random.int 21 - 10) (* small integral — collides with Int/Int64 *)
  | 6 -> float_of_int (Random.int 2001 - 1000) +. 0.5 (* fractional *)
  | 7 -> 1e300 *. (if Random.bool () then 1. else -1.)
  | 8 -> 1e-300 *. (if Random.bool () then 1. else -1.)
  | _ -> Random.float 2000. -. 1000.

let rand_int64 () =
  match Random.int 5 with
  | 0 -> Int64.of_int (Random.int 21 - 10)
  | 1 -> Int64.max_int
  | 2 -> Int64.min_int
  | 3 -> 0L
  | _ -> Int64.of_int (Random.int 4000001 - 2000000)

let rand_string () =
  if Random.int 5 = 0 then String.init (Random.int 6) (fun _ -> Char.chr (Random.int 256))
  else String.init (Random.int 5) (fun _ -> "abc".[Random.int 3])

let rand_oid () = String.init 24 (fun _ -> "0123456789abcdef".[Random.int 16])

let rand_scalar () : B.t =
  match Random.int 13 with
  | 0 -> B.Null
  | 1 -> B.Min_key
  | 2 -> B.Max_key
  | 3 -> B.Bool (Random.bool ())
  | 4 | 5 -> B.Int (Random.int 21 - 10)
  | 6 -> B.Int64 (rand_int64 ())
  | 7 | 8 -> B.Float (rand_float ())
  | 9 | 10 -> B.String (rand_string ())
  | 11 -> B.Date (Int64.of_int (Random.int 4000001 - 2000000))
  | _ -> B.Object_id (rand_oid ())

let%test "memcomparable key codec is order-preserving vs Bson.compare (200k random scalar pairs)" =
  Random.init 0x5eed;
  let ok = ref true and shown = ref 0 in
  for _ = 1 to 200_000 do
    let a = rand_scalar () and b = rand_scalar () in
    let kc = sign (String.compare (Key_codec.encode a) (Key_codec.encode b)) in
    let bc = sign (B.compare a b) in
    if kc <> bc then begin
      ok := false;
      if !shown < 12 then begin
        incr shown;
        Printf.eprintf "MISMATCH  a=%-20s b=%-20s  key=%2d  bson=%2d\n%!" (B.to_string a) (B.to_string b) kc bc
      end
    end
  done;
  !ok

(* with prefix-free strings, concatenated index entries (field ++ _id) must order field-major *)
let%test "compound order: encode_fields orders by first field, then second (200k random pairs)" =
  Random.init 0xC0DE;
  let pair () = (rand_scalar (), rand_scalar ()) in
  let cmp2 (a1, a2) (b1, b2) = match sign (B.compare a1 b1) with 0 -> sign (B.compare a2 b2) | s -> s in
  let ok = ref true in
  for _ = 1 to 200_000 do
    let x = pair () and y = pair () in
    (* only meaningful when each field is individually encodable (skip composite draws) *)
    match
      ( Key_codec.encode_fields [ fst x; snd x ],
        Key_codec.encode_fields [ fst y; snd y ] )
    with
    | kx, ky -> if sign (String.compare kx ky) <> cmp2 x y then ok := false
    | exception Key_codec.Unsupported _ -> ()
  done;
  !ok

let%test "cross-type number equality: Int 5 = Int64 5 = Float 5.0 produce the same key" =
  Key_codec.encode (B.Int 5) = Key_codec.encode (B.Int64 5L)
  && Key_codec.encode (B.Int64 5L) = Key_codec.encode (B.Float 5.0)

let%test "type bracketing: a Number key < a String key < an ObjectId key" =
  Key_codec.encode (B.Float 1e300) < Key_codec.encode (B.String "")
  && Key_codec.encode (B.String "zzz") < Key_codec.encode (B.Object_id (String.make 24 '0'))

let () = exit (Fennec_hunt_unit.run ())
