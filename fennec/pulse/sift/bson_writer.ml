(* bson_writer.ml — the zero-copy ENCODE path (SIFT-K3), mirror of bson_reader.

   Today's encode is [write shape v : Bson.t] (builds the WHOLE tree) then [Bson_wire.encode : string]
   (serialises it). The tree is the overhead. Here we emit the BSON wire bytes STRAIGHT into a buffer
   in a single pass, no tree: {!Engine.size} computes the exact total once, we allocate one buffer, and
   write into it — each document's int32 length is written as a placeholder then BACKPATCHED once its
   end is known (Bigstringaf is mutable), so nesting needs no per-doc size pass.

   CORRECTNESS BAR: [encode_bytes shape v] is byte-identical to [Bson_wire.encode (Engine.write shape v)]
   — tag choices (int32 vs int64 by range, oid vs string for an id) and the optional/opt_list omission
   mirror {!Engine.write}/{!Shape.bound_field}'s [omit] exactly. Differential-tested. *)

open Shape

(* a write head over a buffer sized EXACTLY by {!Engine.size}; the setters bounds-check, so a size/write
   divergence raises rather than corrupting (and {!encode_bytes} asserts the end position equals size) *)
type w = { buf : Bigstringaf.t; mutable pos : int }

let u8 w b = Bigstringaf.set w.buf w.pos (Char.unsafe_chr (b land 0xff)); w.pos <- w.pos + 1
let i32 w n = Bigstringaf.set_int32_le w.buf w.pos (Int32.of_int n); w.pos <- w.pos + 4
let i64 w n = Bigstringaf.set_int64_le w.buf w.pos n; w.pos <- w.pos + 8
let str w s = let n = String.length s in Bigstringaf.blit_from_string s ~src_off:0 w.buf ~dst_off:w.pos ~len:n; w.pos <- w.pos + n
let cstring w s = str w s; u8 w 0
let bson_string w s = i32 w (String.length s + 1); str w s; u8 w 0 (* BSON string: int32 (len+1) + bytes + NUL *)
let backpatch w slot = Bigstringaf.set_int32_le w.buf slot (Int32.of_int (w.pos - slot)) (* the doc length, now known *)

(* a 24-char hex ObjectId -> 12 bytes (mirrors Bson_wire.hex_to_bytes) *)
let hexval c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
  else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
  else 0

let write_oid w (h : string) = for i = 0 to 11 do u8 w ((hexval h.[i * 2] lsl 4) lor hexval h.[(i * 2) + 1]) done
let subtype_byte st = if String.length st >= 2 then (hexval st.[0] lsl 4) lor hexval st.[1] else 0

(* ---- the [TBson] escape hatch: write an arbitrary Bson.t (mirrors Bson_wire.emit_value / tag_of) ---- *)

let bson_tag_of : Bson.t -> int = function
  | Bson.Float _ -> 0x01
  | Bson.String _ -> 0x02
  | Bson.Document _ -> 0x03
  | Bson.Array _ -> 0x04
  | Bson.Binary _ -> 0x05
  | Bson.Object_id _ -> 0x07
  | Bson.Bool _ -> 0x08
  | Bson.Date _ -> 0x09
  | Bson.Null -> 0x0A
  | Bson.Regex _ -> 0x0B
  | Bson.Code _ -> 0x0D
  | Bson.Symbol _ -> 0x0E
  | Bson.Code_with_scope _ -> 0x0F
  | Bson.Int n -> if Engine.in_i32 n then 0x10 else 0x12
  | Bson.Timestamp _ -> 0x11
  | Bson.Int64 _ -> 0x12
  | Bson.Decimal128 _ -> 0x13
  | Bson.Min_key -> 0xFF
  | Bson.Max_key -> 0x7F

let rec write_bson w (b : Bson.t) : unit =
  match b with
  | Bson.Float f -> i64 w (Int64.bits_of_float f)
  | Bson.String s | Bson.Code s | Bson.Symbol s -> bson_string w s
  | Bson.Document fields -> write_bson_doc w fields
  | Bson.Array xs -> write_bson_doc w (List.mapi (fun i x -> (string_of_int i, x)) xs)
  | Bson.Binary { subtype; base64 } ->
      let raw = (try Base64.decode_exn base64 with _ -> "") in
      i32 w (String.length raw);
      u8 w (subtype_byte subtype);
      str w raw
  | Bson.Object_id h -> write_oid w h
  | Bson.Bool x -> u8 w (if x then 1 else 0)
  | Bson.Date ms -> i64 w ms
  | Bson.Null | Bson.Min_key | Bson.Max_key -> ()
  | Bson.Regex { pattern; options } -> cstring w pattern; cstring w options
  | Bson.Code_with_scope (code, scope) ->
      let slot = w.pos in
      i32 w 0;
      bson_string w code;
      write_bson_doc w scope;
      backpatch w slot
  | Bson.Int n -> if Engine.in_i32 n then i32 w n else i64 w (Int64.of_int n)
  | Bson.Int64 n -> i64 w n
  | Bson.Timestamp { t; i } -> i32 w i; i32 w t (* two u32s; Int32.of_int truncates to the low 32 bits *)
  | Bson.Decimal128 _ -> invalid_arg "Sift.encode_bytes: Decimal128 not supported"

and write_bson_doc w fields =
  let slot = w.pos in
  i32 w 0;
  List.iter (fun (k, v) -> u8 w (bson_tag_of v); cstring w k; write_bson w v) fields;
  u8 w 0;
  backpatch w slot

(* ---- the schema-directed write: the wire tag for a value, and its bytes ----------------------- *)

let rec enc_tag : type a. a shape -> a -> int =
 fun shape v ->
  match shape with
  | TString -> 0x02
  | TInt -> if Engine.in_i32 v then 0x10 else 0x12
  | TFloat _ -> 0x01
  | TBool -> 0x08
  | TDate -> 0x09
  | TId -> if Engine.looks_like_oid v then 0x07 else 0x02
  | TDyn -> bson_tag_of (Value_bson.to_bson v)
  | TUnit -> 0x0A
  | TList _ -> 0x04
  | TOption el -> ( match v with Some x -> enc_tag el x | None -> 0x0A)
  | TMap _ -> 0x03
  | TCheck (_, _, _, inner) -> enc_tag inner v
  | TNorm (f, inner) -> enc_tag inner (f v)
  | TConv (inj, _, inner) -> enc_tag inner (inj v)
  | TCoerce inner -> enc_tag inner v
  | TLazy l -> enc_tag (Lazy.force l) v
  | TObj _ -> 0x03
  | TVariant _ -> 0x03

let rec write_value : type a. a shape -> a -> w -> unit =
 fun shape v w ->
  match shape with
  | TString -> bson_string w v
  | TInt -> if Engine.in_i32 v then i32 w v else i64 w (Int64.of_int v)
  | TFloat _ -> i64 w (Int64.bits_of_float v)
  | TBool -> u8 w (if v then 1 else 0)
  | TDate -> i64 w v
  | TId -> if Engine.looks_like_oid v then write_oid w v else bson_string w v
  | TDyn -> write_bson w (Value_bson.to_bson v)
  | TUnit -> () (* Null: no value bytes *)
  | TList el -> write_array el v w
  | TOption el -> ( match v with Some x -> write_value el x w | None -> ())
  | TMap el -> write_map el v w
  | TCheck (_, _, _, inner) -> write_value inner v w
  | TNorm (f, inner) -> write_value inner (f v) w
  | TConv (inj, _, inner) -> write_value inner (inj v) w
  | TCoerce inner -> write_value inner v w
  | TLazy l -> write_value (Lazy.force l) v w
  | TObj o -> write_members o.members v w
  | TVariant { tag; cases } -> write_variant tag cases v w

and write_members : type r. r bound_field list -> r -> w -> unit =
 fun members r w ->
  let slot = w.pos in
  i32 w 0;
  List.iter (fun (Bound_field f) -> let v = f.get r in if not (f.omit v) then (u8 w (enc_tag f.shape v); cstring w f.name; write_value f.shape v w)) members;
  u8 w 0;
  backpatch w slot

and write_array : type a. a shape -> a list -> w -> unit =
 fun el xs w ->
  let slot = w.pos in
  i32 w 0;
  List.iteri (fun i x -> u8 w (enc_tag el x); cstring w (string_of_int i); write_value el x w) xs;
  u8 w 0;
  backpatch w slot

and write_map : type a. a shape -> (string * a) list -> w -> unit =
 fun el kvs w ->
  let slot = w.pos in
  i32 w 0;
  List.iter (fun (k, x) -> u8 w (enc_tag el x); cstring w k; write_value el x w) kvs;
  u8 w 0;
  backpatch w slot

and write_variant : type r. string -> r case list -> r -> w -> unit =
 fun tag cases v w ->
  let rec go = function
    | [] -> invalid_arg "Sift: variant value matches no declared case"
    | Case c :: rest -> (
        match c.project v with
        | Some a ->
            let slot = w.pos in
            i32 w 0;
            u8 w 0x02; (* the discriminator: a string field [tag] = case name *)
            cstring w tag;
            bson_string w c.name;
            List.iter (fun (Bound_field f) -> let fv = f.get a in if not (f.omit fv) then (u8 w (enc_tag f.shape fv); cstring w f.name; write_value f.shape fv w)) c.body.members;
            u8 w 0;
            backpatch w slot
        | None -> go rest)
  in
  go cases

(* ---- entry: encode a value straight into a freshly-sized buffer -------------------------------- *)

let encode_value_bytes (shape : 'a shape) (v : 'a) : Bigstringaf.t =
  let n = Engine.size shape v in
  let buf = Bigstringaf.create n in
  let w = { buf; pos = 0 } in
  write_value shape v w;
  if w.pos <> n then invalid_arg "Sift.encode_bytes: size/write mismatch";
  buf
