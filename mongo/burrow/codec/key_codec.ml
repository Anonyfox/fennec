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
