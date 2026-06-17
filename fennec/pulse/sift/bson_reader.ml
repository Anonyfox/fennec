(* bson_reader.ml — the zero-copy, schema-directed BSON decode path (SIFT-K2).

   Today's decode is [Bson_wire.decode buf |> Engine.read shape]: parse the WHOLE buffer into a
   {!Bson.t} tree (every field, every nested doc, the kvs lists), then walk that tree matching it to
   the shape. The tree is pure overhead — Sift already KNOWS the shape.

   This module decodes STRAIGHT from the byte buffer, shape-directed: a single linear pass over each
   document level scans its fields into a flat span-tape (one [int array], stride 4 — no per-field
   boxing, no value materialization), then the shape pulls exactly the fields it wants and reads each
   value's bytes directly into the OCaml value it asks for. Unwanted fields (and their nested
   structure) are skipped by their length prefix, never materialized. Keys are matched by comparing
   buffer bytes to the field name in place — never copied. Strings are copied only when an owned OCaml
   string is the demanded result (T2 owned materialization).

   CORRECTNESS BAR: every value and every collected error matches [Bson_wire.decode |> Engine.read]
   byte-for-byte — {!read_buf} mirrors {!Engine.read}'s coercions exactly (e.g. an [int] accepts a
   wire int32 / integral double but NOT an int64; a [date] accepts a wire datetime / int32 / integral
   double), and the refinement phase is the SAME {!Engine.run_checks} the tree path runs. The shared
   record constructor is threaded through {!Shape.field_reader} (see {!Combinators}); this module just
   supplies a buffer-backed reader in place of the kvs-index one.

   Additive: the working [Sift.*] facade still routes through the tree path; {!decode_bytes} is the new
   path, exposed alongside and benchmarked, until it is proven enough to become the default (K7). *)

open Shape

(* a malformed buffer (bad tag, inconsistent length, unsupported type) aborts the pass and is turned
   into a structured [Error] at the entry — never an escaping exception, never an out-of-bounds read *)
exception Malformed of string

(* ---- BSON wire type tags (mirrors mongo/wire/bson_wire.ml — the canonical on-the-wire codec) ---- *)

let tag_double = 0x01
let tag_string = 0x02
let tag_document = 0x03
let tag_array = 0x04
let tag_binary = 0x05
let tag_oid = 0x07
let tag_bool = 0x08
let tag_date = 0x09
let tag_null = 0x0A
let tag_regex = 0x0B
let tag_code = 0x0D
let tag_symbol = 0x0E
let tag_code_w_scope = 0x0F
let tag_int32 = 0x10
let tag_timestamp = 0x11
let tag_int64 = 0x12
let tag_decimal128 = 0x13
let tag_min_key = 0xFF
let tag_max_key = 0x7F

(* the name a tag decodes to, EXACTLY as {!Engine.type_name} names the corresponding {!Bson.t} — so a
   type-mismatch error reports the same "got X" as the tree path (anything Engine doesn't name → "value":
   that is how a stored int64 reports "got value" when an [int] field rejects it, matching Engine) *)
let type_name_of_tag tag =
  if tag = tag_string then "string"
  else if tag = tag_int32 then "int"
  else if tag = tag_double then "float"
  else if tag = tag_bool then "bool"
  else if tag = tag_document then "document"
  else if tag = tag_array then "array"
  else if tag = tag_null then "null"
  else if tag = tag_date then "date"
  else if tag = tag_oid then "objectid"
  else "value"

let expected_tag what tag : ('a, error list) result =
  fail ~code:"type" ~params:[ ("expected", what); ("got", type_name_of_tag tag) ]
    (Printf.sprintf "expected %s, got %s" what (type_name_of_tag tag))

(* ---- low-level value reads over a Cursor (each positioned at a value, leaving it just past) ------ *)

let hexchars = "0123456789abcdef"

(* a spec BSON string at the cursor: int32 (byte-length incl. NUL) + bytes + NUL. Owned copy. *)
let read_bson_string (c : Cursor.t) : string =
  let len = Cursor.len32 c in
  if len < 1 then raise (Malformed "BSON string: bad length");
  let off = Cursor.take c ~len:(len - 1) in
  let s = Cursor.substring c ~off ~len:(len - 1) in
  let _nul = Cursor.uint8 c in
  s

(* a 12-byte ObjectId at the cursor -> its 24-char lowercase hex (matches Bson_wire.bytes_to_hex) *)
let read_oid_hex (c : Cursor.t) : string =
  let off = Cursor.take c ~len:12 in
  String.init 24 (fun i ->
      let byte = Char.code (Cursor.byte_at c (off + (i / 2))) in
      if i land 1 = 0 then hexchars.[byte lsr 4] else hexchars.[byte land 0xf])

let byte_to_subtype b = Printf.sprintf "%02x" (b land 0xff)

(* an owned NUL-terminated string at the cursor (regex pattern/options) *)
let read_cstr_owned (c : Cursor.t) : string =
  let off = Cursor.pos c in
  let len = Cursor.key_len c in
  Cursor.substring c ~off ~len

(* the number of bytes a value of [tag] occupies, NOT counting the type byte or key — peeked WITHOUT
   advancing (the scanner uses it to skip a field's value). Mirrors Bson_wire.value_size. *)
let regex_size (c : Cursor.t) (off : int) : int =
  (* two NUL-terminated cstrings (pattern, options) — the rare path; a small inner scan is fine *)
  let lim = Cursor.limit c in
  let rec nul i = if i >= lim then raise (Malformed "BSON regex: unterminated") else if Cursor.byte_at c i = '\000' then i else nul (i + 1) in
  let e1 = nul off in
  let e2 = nul (e1 + 1) in
  e2 + 1 - off

let[@inline] value_size (c : Cursor.t) (tag : int) : int =
  (* a [match] (jump table), NOT a sequential if-chain — the commonest tag (int32) must not cost a dozen
     comparisons per skipped field. Literal tags (= the tag_* constants) so the compiler builds the table. *)
  match tag with
  | 0x10 -> 4 (* int32 *)
  | 0x02 | 0x0d | 0x0e -> 4 + Cursor.peek_int32 c (Cursor.pos c) (* string / code / symbol *)
  | 0x03 | 0x04 | 0x0f -> Cursor.peek_int32 c (Cursor.pos c) (* document / array / code-with-scope *)
  | 0x01 | 0x09 | 0x11 | 0x12 -> 8 (* double / datetime / timestamp / int64 *)
  | 0x08 -> 1 (* bool *)
  | 0x07 -> 12 (* objectid *)
  | 0x0a | 0xff | 0x7f -> 0 (* null / min_key / max_key *)
  | 0x05 -> 5 + Cursor.peek_int32 c (Cursor.pos c) (* binary: int32 len + 1 subtype byte *)
  | 0x13 -> 16 (* decimal128 *)
  | 0x0b -> regex_size c (Cursor.pos c)
  | _ -> raise (Malformed (Printf.sprintf "BSON: unknown type tag 0x%02x" tag))

(* materialize ANY value as a {!Bson.t} — the [TBson] escape hatch (round-trips every BSON type). This
   one path DOES allocate a tree (the caller asked for the raw value); mirrors Bson_wire.read_value. *)
let rec materialize (c : Cursor.t) (tag : int) : Bson.t =
  if tag = tag_double then Bson.Float (Cursor.double c)
  else if tag = tag_string then Bson.String (read_bson_string c)
  else if tag = tag_document then Bson.Document (materialize_doc c)
  else if tag = tag_array then Bson.Array (List.map snd (materialize_doc c))
  else if tag = tag_binary then (
    let len = Cursor.int32 c in
    let sub = Cursor.uint8 c in
    let off = Cursor.take c ~len in
    Bson.Binary { subtype = byte_to_subtype sub; base64 = Base64.encode_string (Cursor.substring c ~off ~len) })
  else if tag = tag_oid then Bson.Object_id (read_oid_hex c)
  else if tag = tag_bool then Bson.Bool (Cursor.uint8 c <> 0)
  else if tag = tag_date then Bson.Date (Cursor.int64 c)
  else if tag = tag_null then Bson.Null
  else if tag = tag_regex then (
    let pattern = read_cstr_owned c in
    let options = read_cstr_owned c in
    Bson.Regex { pattern; options })
  else if tag = tag_code then Bson.Code (read_bson_string c)
  else if tag = tag_symbol then Bson.Symbol (read_bson_string c)
  else if tag = tag_code_w_scope then (
    let _len = Cursor.int32 c in
    let code = read_bson_string c in
    Bson.Code_with_scope (code, materialize_doc c))
  else if tag = tag_int32 then Bson.Int (Cursor.int32 c)
  else if tag = tag_timestamp then (
    let i = Cursor.uint32 c in
    let t = Cursor.uint32 c in
    Bson.Timestamp { t; i })
  else if tag = tag_int64 then Bson.Int64 (Cursor.int64 c)
  else if tag = tag_decimal128 then raise (Malformed "Decimal128 not supported")
  else if tag = tag_min_key then Bson.Min_key
  else if tag = tag_max_key then Bson.Max_key
  else raise (Malformed (Printf.sprintf "BSON: unknown type tag 0x%02x" tag))

(* a document's ordered (key, value) list, advancing the cursor past the whole document *)
and materialize_doc (c : Cursor.t) : (string * Bson.t) list =
  let start = Cursor.pos c in
  let len = Cursor.int32 c in
  if len < 5 then raise (Malformed "BSON document: bad length");
  let stop = start + len in
  let rec loop acc =
    let tag = Cursor.uint8 c in
    if tag = 0 then List.rev acc
    else
      let koff = Cursor.pos c in
      let klen = Cursor.key_len c in
      let key = Cursor.substring c ~off:koff ~len:klen in
      let v = materialize c tag in
      loop ((key, v) :: acc)
  in
  let kvs = loop [] in
  if Cursor.pos c <> stop then raise (Malformed "BSON document: length mismatch");
  kvs

(* ---- document bounds + schema-directed field scan (ONE shared cursor, no tape, no per-field alloc) *)

(* read a document's int32 length (cursor at it); return [doc_end] and leave the cursor just after the
   length, i.e. AT the first element ([Cursor.pos c] is the document start). The terminator 0x00 sits at
   [doc_end - 1]. Returns a bare int (not a tuple) — the caller grabs the start from [Cursor.pos] when it
   needs it, so no allocation. *)
let doc_bounds (c : Cursor.t) : int =
  let start = Cursor.pos c in
  let len = Cursor.len32 c in
  if len < 5 then raise (Malformed "BSON document: bad length");
  let doc_end = start + len in
  if doc_end > Cursor.limit c then raise (Malformed "BSON document: length exceeds buffer");
  doc_end

(* scan the document for [key] from the cursor's CURRENT position (the caller rewinds to the document
   start first). On a match, leave the cursor AT the value and return its wire tag (1..255); on miss
   (the 0x00 terminator), return 0 — a sentinel, never a real tag, so no [option] box in this hot loop.
   Keys are compared to buffer bytes in place — never copied; unwanted values are stepped over by their
   length prefix. *)
(* top-level recursion (NOT a [let rec loop] nested inside — that would allocate a closure capturing
   [c]/[key] on every call, i.e. once per record decode) *)
let rec scan_find (c : Cursor.t) (key : string) : int =
  let tag = Cursor.uint8 c in
  if tag = 0 then 0
  else
    let koff = Cursor.pos c in
    let klen = Cursor.key_len c in
    if Cursor.span_eq_string c ~off:koff ~len:klen key then tag else (Cursor.skip c (value_size c tag); scan_find c key)

(* ---- the schema-directed decode: read a value of [shape] given its wire [tag], from [c] at it ----

   ONE cursor is threaded through the whole decode — no tape, no fresh cursor per field. A record
   rewinds the cursor to its start to find each wanted field (BSON fields are few; in-place byte
   compares cost nothing and allocate nothing); a list/map walks its elements forward. The only
   allocations are the result value itself, owned strings when demanded, and one small reader closure
   per nested document level — strictly less than building the Bson.t tree. *)

let rec read_buf : type a. a shape -> tag:int -> Cursor.t -> (a, error list) result =
 fun shape ~tag c ->
  match shape with
  | TString -> if tag = tag_string then Ok (read_bson_string c) else expected_tag "string" tag
  | TInt ->
      if tag = tag_int32 then Ok (Cursor.int32 c)
      else if tag = tag_double then (let f = Cursor.double c in if Float.is_integer f then Ok (int_of_float f) else expected_tag "int" tag)
      else expected_tag "int" tag
  | TFloat { allow_nonfinite } ->
      if tag = tag_double then (let f = Cursor.double c in if (not allow_nonfinite) && not (Float.is_finite f) then fail "non-finite float" else Ok f)
      else if tag = tag_int32 then Ok (float_of_int (Cursor.int32 c))
      else expected_tag "float" tag
  | TBool -> if tag = tag_bool then Ok (Cursor.uint8 c <> 0) else expected_tag "bool" tag
  | TDate ->
      if tag = tag_date then Ok (Cursor.int64 c)
      else if tag = tag_int32 then Ok (Int64.of_int (Cursor.int32 c))
      else if tag = tag_double then (let f = Cursor.double c in if Float.is_integer f then Ok (Int64.of_float f) else expected_tag "date" tag)
      else expected_tag "date" tag
  | TId ->
      if tag = tag_string then Ok (read_bson_string c)
      else if tag = tag_oid then Ok (read_oid_hex c)
      else expected_tag "id (string or objectid)" tag
  | TBson -> Ok (materialize c tag)
  | TUnit -> if tag = tag_null then Ok () else expected_tag "null" tag
  | TList el ->
      if tag <> tag_array then expected_tag "array" tag
      else (
        let de = doc_bounds c in
        (* array elements are positional ("0","1",…) and in order — walk forward, skipping each key.
           Advance to each value's end EXPLICITLY (a type-mismatch read returns without consuming the
           value, which would desync the shared cursor): compute the end from its length prefix. *)
        let rec collect i oks errs =
          let etag = Cursor.uint8 c in
          if etag = 0 then (Cursor.seek c de; if errs = [] then Ok (List.rev oks) else Error (List.rev errs))
          else (
            let _klen = Cursor.key_len c in
            let vend = Cursor.pos c + value_size c etag in
            let r = read_buf el ~tag:etag c in
            Cursor.seek c vend;
            match r with
            | Ok v -> collect (i + 1) (v :: oks) errs
            | Error es -> collect (i + 1) oks (List.rev_append (List.map (at (string_of_int i)) es) errs))
        in
        collect 0 [] [])
  | TOption el -> if tag = tag_null then Ok None else ( match read_buf el ~tag c with Ok x -> Ok (Some x) | Error e -> Error e)
  | TMap el ->
      if tag <> tag_document then expected_tag "document" tag
      else (
        let de = doc_bounds c in
        let rec collect oks errs =
          let etag = Cursor.uint8 c in
          if etag = 0 then (Cursor.seek c de; if errs = [] then Ok (List.rev oks) else Error (List.rev errs))
          else (
            let koff = Cursor.pos c in
            let klen = Cursor.key_len c in
            let k = Cursor.substring c ~off:koff ~len:klen in
            let vend = Cursor.pos c + value_size c etag in
            let r = read_buf el ~tag:etag c in
            Cursor.seek c vend;
            match r with
            | Ok v -> collect ((k, v) :: oks) errs
            | Error es -> collect oks (List.rev_append (List.map (at k) es) errs))
        in
        collect [] [])
  | TCheck (_, _, _, inner) -> read_buf inner ~tag c (* RAW phase: shape only; refinements run in run_checks *)
  | TNorm (f, inner) -> ( match read_buf inner ~tag c with Ok v -> Ok (f v) | Error e -> Error e)
  | TConv (_inj, proj, inner) -> (
      match read_buf inner ~tag c with Ok v -> ( match proj v with Ok x -> Ok x | Error m -> fail m) | Error e -> Error e)
  | TObj o ->
      if tag <> tag_document then expected_tag "document" tag
      else (
        let de = doc_bounds c in
        let ds = Cursor.pos c in
        let r = o.decode_src (field_reader c ds) in
        Cursor.seek c de;
        r)
  | TVariant { tag = tagf; cases } ->
      if tag <> tag_document then expected_tag "document" tag
      else (
        let de = doc_bounds c in
        let ds = Cursor.pos c in
        let k = match scan_find c tagf with vt when vt = tag_string -> Some (read_bson_string c) | _ -> None in
        let r =
          match k with
          | None -> fail (Printf.sprintf "missing tag field %s" tagf)
          | Some k -> (
              match List.find_opt (fun (Case cs) -> cs.name = k) cases with
              | None -> fail (Printf.sprintf "unknown %s %S" tagf k)
              | Some (Case cs) -> ( match cs.body.decode_src (field_reader c ds) with Ok a -> Ok (cs.inject a) | Error e -> Error (List.map (at k) e)))
        in
        Cursor.seek c de;
        r)
  | TLazy l -> read_buf (Lazy.force l) ~tag c
  | TCoerce inner ->
      if tag = tag_string then (let s = read_bson_string c in Engine.read inner (Engine.coerce_string inner s))
      else read_buf inner ~tag c

(* the {!Shape.field_reader} for the document spanning [ds, …]: rewind the shared cursor to [ds] and
   scan for the field's key, then read its value in place — exactly the tree path's [decode_field], but
   over the raw bytes instead of a kvs index. One closure per document level; no per-field allocation. *)
and field_reader (c : Cursor.t) (ds : int) : field_reader =
  {
    read_field =
      (fun (type a) (f : a field) : (a, error list) result ->
        Cursor.seek c ds;
        let vtag = scan_find c f.key in
        if vtag = 0 then ( match f.fallback with Some d -> Ok d | None -> Error [ mkerr ~code:"required" ~path:[ f.key ] "is required" ])
        else match read_buf f.item ~tag:vtag c with Ok _ as ok -> ok | Error es -> Error (List.map (at f.key) es));
  }

(* ---- entry: decode a BSON document buffer into the shape's value ------------------------------- *)

(* the buffer IS a top-level BSON document, so we enter {!read_buf} as if reading a [tag_document]
   value at position 0. RAW decode then the SHARED {!Engine.run_checks} refinement phase — identical
   to {!Codec.decode_value}, so [decode_bytes] collects exactly the same errors as the tree path. A
   malformed buffer becomes a structured error, never an escaping exception. *)
let decode_value_bytes (shape : 'a shape) (buf : Cursor.buffer) : ('a, error list) result =
  match (try read_buf shape ~tag:tag_document (Cursor.make buf) with Cursor.Truncated _ -> Error [ mkerr ~code:"malformed" "malformed BSON: truncated or out of bounds" ] | Malformed m -> Error [ mkerr ~code:"malformed" m ] | Invalid_argument _ -> Error [ mkerr ~code:"malformed" "malformed BSON" ]) with
  | Error es -> Error es
  | Ok v -> if not (Engine.needs_checks shape) then Ok v else ( match Engine.run_checks shape v with [] -> Ok v | es -> Error es)

(* ---- projection: decode ONE top-level field, reading only its bytes ---------------------------- *)

(* scan the buffer's top-level document for [key] and decode JUST that field's value with [shape] (its
   checks run too) — the rest of the document is stepped over by length prefix, never materialized.
   [None] when the field is absent; [Some (Ok v)] / [Some (Error _)] when present. The buffer analog of
   the tree-side [Combinators.field_get] — for routing a raw document by a field (_id, a discriminant)
   or pulling one value without decoding the whole record. Errors are path-tagged with [key], matching
   [field_get]. A malformed buffer yields [Some (Error _)], never an escaping exception. *)
let peek_field (shape : 'a shape) (key : string) (buf : Cursor.buffer) : ('a, error list) result option =
  try
    let c = Cursor.make buf in
    let _de = doc_bounds c in
    let vtag = scan_find c key in
    if vtag = 0 then None
    else
      let r =
        match read_buf shape ~tag:vtag c with
        | Error es -> Error (List.map (at key) es)
        | Ok v -> if not (Engine.needs_checks shape) then Ok v else ( match Engine.run_checks shape v with [] -> Ok v | es -> Error (List.map (at key) es))
      in
      Some r
  with Cursor.Truncated _ | Malformed _ | Invalid_argument _ -> Some (Error [ mkerr ~code:"malformed" ~path:[ key ] "malformed BSON" ])

(* ---- T1: alloc-free validation (the borrowed tier — verdict without materializing the value) ----

   This is where zero-copy is unambiguous: validate a buffer against a shape WITHOUT building any OCaml
   value, so it runs at scan speed (which beats libbson, per the bench) at ~0 allocation. {!check_type}
   mirrors {!read_buf}'s tag acceptance EXACTLY (same coercions: int accepts int32/integral-double, date
   accepts datetime/int32/integral-double, …) and the record/required logic, but returns errors instead
   of values and walks {!Shape.bound_field} members (no constructor needed). Numbers materialize only the
   unavoidable boxed float for the integral/finite check (OCaml's float tax); everything else is alloc-free
   on the valid path. *)

let terr what tag = [ mkerr ~code:"type" ~params:[ ("expected", what); ("got", type_name_of_tag tag) ] (Printf.sprintf "expected %s, got %s" what (type_name_of_tag tag)) ]

let rec check_type : type a. a shape -> tag:int -> Cursor.t -> error list =
 fun shape ~tag c ->
  match shape with
  | TString -> if tag = tag_string then [] else terr "string" tag
  | TInt ->
      if tag = tag_int32 then []
      else if tag = tag_double then (if Float.is_integer (Cursor.double c) then [] else terr "int" tag)
      else terr "int" tag
  | TFloat { allow_nonfinite } ->
      if tag = tag_double then (if (not allow_nonfinite) && not (Float.is_finite (Cursor.double c)) then [ mkerr "non-finite float" ] else [])
      else if tag = tag_int32 then []
      else terr "float" tag
  | TBool -> if tag = tag_bool then [] else terr "bool" tag
  | TDate ->
      if tag = tag_date || tag = tag_int32 then []
      else if tag = tag_double then (if Float.is_integer (Cursor.double c) then [] else terr "date" tag)
      else terr "date" tag
  | TId -> if tag = tag_string || tag = tag_oid then [] else terr "id (string or objectid)" tag
  | TBson -> []
  | TUnit -> if tag = tag_null then [] else terr "null" tag
  | TList el -> if tag <> tag_array then terr "array" tag else check_seq el c
  | TOption el -> if tag = tag_null then [] else check_type el ~tag c
  | TMap el -> if tag <> tag_document then terr "document" tag else check_kvs el c
  | TCheck (_, _, _, inner) -> check_type inner ~tag c (* refinements handled by check_refine, gated by simply_checkable *)
  | TNorm (_, inner) -> check_type inner ~tag c
  | TConv (_, _, inner) -> check_type inner ~tag c
  | TCoerce inner -> if tag = tag_string then [] else check_type inner ~tag c
  | TLazy l -> check_type (Lazy.force l) ~tag c
  | TObj o -> if tag <> tag_document then terr "document" tag else (let de = doc_bounds c in let ds = Cursor.pos c in check_members o.members c ds de)
  | TVariant { tag = tagf; cases } -> if tag <> tag_document then terr "document" tag else check_variant tagf cases c

and check_seq : type a. a shape -> Cursor.t -> error list =
 fun el c ->
  let de = doc_bounds c in
  let r = check_seq_loop el c 0 [] in
  Cursor.seek c de;
  List.rev r

and check_seq_loop : type a. a shape -> Cursor.t -> int -> error list -> error list =
 fun el c i acc ->
  let etag = Cursor.uint8 c in
  if etag = 0 then acc
  else (
    let _klen = Cursor.key_len c in
    let vend = Cursor.pos c + value_size c etag in
    let e = check_type el ~tag:etag c in
    Cursor.seek c vend;
    check_seq_loop el c (i + 1) (match e with [] -> acc | _ -> List.rev_append (List.map (at (string_of_int i)) e) acc))

and check_kvs : type a. a shape -> Cursor.t -> error list =
 fun el c ->
  let de = doc_bounds c in
  let r = check_kvs_loop el c [] in
  Cursor.seek c de;
  List.rev r

and check_kvs_loop : type a. a shape -> Cursor.t -> error list -> error list =
 fun el c acc ->
  let etag = Cursor.uint8 c in
  if etag = 0 then acc
  else (
    let koff = Cursor.pos c in
    let klen = Cursor.key_len c in
    let vend = Cursor.pos c + value_size c etag in
    let e = check_type el ~tag:etag c in
    Cursor.seek c vend;
    check_kvs_loop el c (match e with [] -> acc | _ -> let k = Cursor.substring c ~off:koff ~len:klen in List.rev_append (List.map (at k) e) acc))

and check_members : type r. r bound_field list -> Cursor.t -> int -> int -> error list =
 fun members c ds de ->
  let errs = List.rev (check_members_loop members c ds []) in
  Cursor.seek c de;
  errs

and check_members_loop : type r. r bound_field list -> Cursor.t -> int -> error list -> error list =
 fun members c ds acc ->
  match members with
  | [] -> acc
  | Bound_field f :: tl ->
      Cursor.seek c ds;
      let vt = scan_find c f.name in
      let e =
        if vt = 0 then (if f.required then [ mkerr ~code:"required" ~path:[ f.name ] "is required" ] else [])
        else List.map (at f.name) (check_type f.shape ~tag:vt c)
      in
      check_members_loop tl c ds (match e with [] -> acc | _ -> List.rev_append e acc)

and check_variant : type r. string -> r case list -> Cursor.t -> error list =
 fun tagf cases c ->
  let de = doc_bounds c in
  let ds = Cursor.pos c in
  let k = match scan_find c tagf with vt when vt = tag_string -> Some (read_bson_string c) | _ -> None in
  let errs =
    match k with
    | None -> [ mkerr (Printf.sprintf "missing tag field %s" tagf) ]
    | Some k -> (
        match List.find_opt (fun (Case cs) -> cs.name = k) cases with
        | None -> [ mkerr (Printf.sprintf "unknown %s %S" tagf k) ]
        | Some (Case cs) -> List.map (at k) (check_members cs.body.members c ds de))
  in
  Cursor.seek c de;
  errs

(* ---- structural BSON validity: shape-agnostic, single pass, zero allocation ------------------- *)

(* recursively verify a document: consistent lengths, known type tags, terminated keys, nested docs
   sound, no out-of-bounds. Raises on the first malformation. *)
let rec structural_walk (c : Cursor.t) : unit =
  let de = doc_bounds c in
  structural_loop c;
  if Cursor.pos c <> de then raise (Malformed "BSON document: length mismatch")

and structural_loop (c : Cursor.t) : unit =
  let tag = Cursor.uint8 c in
  if tag = 0 then ()
  else (
    let _ = Cursor.key_len c in
    if tag = tag_document || tag = tag_array then structural_walk c else Cursor.skip c (value_size c tag);
    structural_loop c)

let scan_valid (buf : Cursor.buffer) : bool =
  try
    let c = Cursor.make buf in
    structural_walk c;
    Cursor.pos c = Bigstringaf.length buf
  with Cursor.Truncated _ | Malformed _ | Invalid_argument _ -> false

(* ---- entry: validate a buffer against a shape, alloc-free where the checks allow -------------- *)

(* A COMPLETE validator — its Ok/Error verdict always equals {!decode_value_bytes}'s. Alloc-free when the
   shape's checks are all structural (no refinements): [check_type] alone is then the full validation. When
   the shape carries refinements / cross-field rules ([needs_checks]), those need the materialized value, so
   it falls back to the full decode (and discards the value). A malformed buffer is a structured error. *)
let valid_value_bytes (shape : 'a shape) (buf : Cursor.buffer) : (unit, error list) result =
  if Engine.needs_checks shape then match decode_value_bytes shape buf with Ok _ -> Ok () | Error es -> Error es
  else
    match (try check_type shape ~tag:tag_document (Cursor.make buf) with Cursor.Truncated _ -> [ mkerr ~code:"malformed" "malformed BSON: truncated or out of bounds" ] | Malformed m -> [ mkerr ~code:"malformed" m ] | Invalid_argument _ -> [ mkerr ~code:"malformed" "malformed BSON" ]) with
    | [] -> Ok ()
    | es -> Error es
