(* Memcomparable (order-preserving) key encoding for index keys.

   Contract: [String.compare (encode a) (encode b)] has the SAME SIGN as [Bson.compare a b], for the
   indexable scalar domain (Null/Bool/Int/Int64/Float/String/Symbol/Date/ObjectId/Min_key/Max_key).
   Composite / rare types (Document/Array/Binary/Timestamp/Regex/Code/Decimal128) are not index-key
   material and raise {!Unsupported}.

   Shape: a 1-byte TYPE TAG (= Bson's [type_rank], so different types order by rank just on the first
   byte), followed by an order-preserving encoding of the value within its type. Numbers reduce to
   their float value (as Bson.compare does), encoded as a sortable big-endian double; NaN sorts below
   all numbers. Variable-length strings are 0x00-escaped and terminated so every encoding is
   prefix-free; {!encode_fields} concatenates them for compound (ascending) keys. Descending order
   (byte inversion) layers in the Index module. *)

exception Unsupported of string

(* = Bson.type_rank, inlined so the codec depends only on the public value type *)
let tag : Bson.t -> int = function
  | Min_key -> 0
  | Null -> 1
  | Int _ | Int64 _ | Float _ -> 2
  | String _ | Symbol _ -> 3
  | Document _ -> 4
  | Array _ -> 5
  | Binary _ -> 6
  | Object_id _ -> 7
  | Bool _ -> 8
  | Date _ -> 9
  | Timestamp _ -> 10
  | Regex _ -> 11
  | Code _ | Code_with_scope _ -> 12
  | Decimal128 _ -> 13
  | Max_key -> 14

let add_be64 b (x : int64) =
  for i = 7 downto 0 do
    Buffer.add_char b (Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical x (8 * i)) 0xFFL)))
  done

(* IEEE-754 double -> order-preserving unsigned 64-bit: negatives flip all bits, non-negatives set
   the sign bit. Then unsigned big-endian byte order == numeric order. (NaN handled by the caller.) *)
let sortable_float (f : float) : int64 =
  (* Bson.compare treats -0.0 and +0.0 as EQUAL (Stdlib.compare does), so normalize the sign of zero
     before encoding — [+. 0.0] turns -0.0 into +0.0 per IEEE and leaves every other value alone. *)
  let f = f +. 0.0 in
  let bits = Int64.bits_of_float f in
  if Int64.compare bits 0L < 0 (* sign bit set => f is negative *) then Int64.lognot bits
  else Int64.logor bits Int64.min_int

let num_as_float : Bson.t -> float = function
  | Int n -> float_of_int n
  | Int64 n -> Int64.to_float n
  | Float f -> f
  | _ -> 0.

(* Variable-length strings are escaped (0x00 -> 0x00 0xFF) and terminated (0x00 0x00) so the encoding
   is prefix-free: this is what makes concatenation (the _id suffix in an index entry, and compound
   keys) order-correct, since the 0x00 0x00 terminator sorts below any real continuation byte. *)
let add_escaped b s =
  String.iter
    (fun c ->
      if c = '\000' then (
        Buffer.add_char b '\000';
        Buffer.add_char b '\255')
      else Buffer.add_char b c)
    s;
  Buffer.add_char b '\000';
  Buffer.add_char b '\000'

let encode (v : Bson.t) : string =
  let b = Buffer.create 24 in
  Buffer.add_char b (Char.chr (tag v));
  (match v with
   | Min_key | Null | Max_key -> ()
   | Bool x -> Buffer.add_char b (if x then '\001' else '\000')
   | Int _ | Int64 _ | Float _ ->
     let f = num_as_float v in
     if Float.is_nan f then add_be64 b 0L (* below every real number *) else add_be64 b (sortable_float f)
   | String s | Symbol s -> add_escaped b s
   | Object_id s -> Buffer.add_string b s
   | Date d -> add_be64 b (Int64.logxor d Int64.min_int) (* signed -> sortable unsigned *)
   | (Document _ | Array _ | Binary _ | Timestamp _ | Regex _ | Code _ | Code_with_scope _ | Decimal128 _) as x ->
     raise (Unsupported (Bson.to_string x)));
  Buffer.contents b

(* Compound-index key prefix: concatenate per-field encodings (each is prefix-free, so lexicographic
   byte order == field-major value order, all ascending). Descending fields invert bytes — layered in
   the Index module, not here. *)
let encode_fields (vs : Bson.t list) : string = String.concat "" (List.map encode vs)

(* ---- inline tests ---------------------------------------------------------------------------
   The codec's law: byte order of encoded keys = [Bson.compare]'s value order, exactly — evidenced
   over 200k random scalar pairs (fixed seeds; counterexamples printed). *)

let%test "memcomparable key codec is order-preserving vs Bson.compare (200k random scalar pairs)" =
  let module B = Bson in
  let sign n = if n < 0 then -1 else if n > 0 then 1 else 0 in
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
  in
  let rand_int64 () =
    match Random.int 5 with
    | 0 -> Int64.of_int (Random.int 21 - 10)
    | 1 -> Int64.max_int
    | 2 -> Int64.min_int
    | 3 -> 0L
    | _ -> Int64.of_int (Random.int 4000001 - 2000000)
  in
  let rand_string () =
    if Random.int 5 = 0 then String.init (Random.int 6) (fun _ -> Char.chr (Random.int 256))
    else String.init (Random.int 5) (fun _ -> "abc".[Random.int 3])
  in
  let rand_oid () = String.init 24 (fun _ -> "0123456789abcdef".[Random.int 16]) in
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
  in
  Random.init 0x5eed;
  let ok = ref true and shown = ref 0 in
  for _ = 1 to 200_000 do
    let a = rand_scalar () and b = rand_scalar () in
    let kc = sign (String.compare (encode a) (encode b)) in
    let bc = sign (B.compare a b) in
    if kc <> bc then begin
      ok := false;
      if !shown < 12 then begin
        incr shown;
        Printf.eprintf "MISMATCH  a=%-20s b=%-20s  key=%2d  bson=%2d\n%!" (B.to_string a) (B.to_string b) kc bc
      end
    end
  done;
  (* with prefix-free strings, concatenated index entries (field ++ _id) must order field-major *)
  Random.init 0xC0DE;
  let pair () = (rand_scalar (), rand_scalar ()) in
  let cmp2 (a1, a2) (b1, b2) = match sign (B.compare a1 b1) with 0 -> sign (B.compare a2 b2) | s -> s in
  for _ = 1 to 200_000 do
    let x = pair () and y = pair () in
    match (encode_fields [ fst x; snd x ], encode_fields [ fst y; snd y ]) with
    | kx, ky -> if sign (String.compare kx ky) <> cmp2 x y then ok := false
    | exception Unsupported _ -> ()
  done;
  !ok

let%test "cross-type number equality: Int 5 = Int64 5 = Float 5.0 produce the same key" =
  encode (Bson.Int 5) = encode (Bson.Int64 5L) && encode (Bson.Int64 5L) = encode (Bson.Float 5.0)

let%test "type bracketing: a Number key < a String key < an ObjectId key" =
  encode (Bson.Float 1e300) < encode (Bson.String "")
  && encode (Bson.String "zzz") < encode (Bson.Object_id (String.make 24 '0'))
