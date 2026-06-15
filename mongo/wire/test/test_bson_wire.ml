(* The wire BSON codec must be byte-exact spec BSON (a driver decodes it with libbson), and bounds-safe
   against hostile frames. We check: exact byte vectors for the canonical small docs; a full round-trip
   over every supported type; that [Binary] goes out as RAW bytes + subtype (not base64 text — proven by
   the exact encoded length); a large payload; that [Decimal128] raises rather than corrupts; and that a
   malformed length raises rather than over-reads. End-to-end spec conformance is later re-proven by the
   real libmongoc driver talking to our server. *)
module W = Mongo_wire.Bson_wire
module B = Bson

(* does [hay] contain [needle] verbatim? (Binary payload must survive byte-for-byte) *)
let contains hay needle =
  let nl = String.length needle and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0

let () =
  (* exact spec vectors: {} and {"a": <int32 1>} *)
  assert (W.encode (B.Document []) = "\x05\x00\x00\x00\x00");
  assert (W.encode (B.doc [ ("a", B.int 1) ]) = "\x0c\x00\x00\x00\x10\x61\x00\x01\x00\x00\x00\x00");

  (* round-trip every supported type (Decimal128 deferred — checked separately below) *)
  let sample =
    B.doc
      [ ("f", B.Float 3.14159);
        ("s", B.str "h\xc3\xa9llo w\xc3\xb6rld");
        ("i", B.int 42);
        ("i64", B.Int64 (-1L));
        ("b", B.bool true);
        ("n", B.Null);
        ("date", B.Date 1_700_000_000_000L);
        ("oid", B.oid "0123456789abcdef01234567");
        ("ts", B.Timestamp { t = 1700; i = 7 });
        ("re", B.Regex { pattern = "^a.*z$"; options = "i" });
        ("code", B.Code "function(){return 1}");
        ("sym", B.Symbol "sym");
        ("cws", B.Code_with_scope ("return x", [ ("x", B.int 9) ]));
        ("min", B.Min_key);
        ("max", B.Max_key);
        ("arr", B.array [ B.int 1; B.str "two"; B.bool false ]);
        ("nested", B.doc [ ("deep", B.doc [ ("x", B.int 1) ]) ]);
        ("bin", B.Binary { subtype = "00"; base64 = Base64.encode_string "\x00\x01\x02\xfftail" });
        ("bin80", B.Binary { subtype = "80"; base64 = Base64.encode_string "userdef" })
      ]
  in
  assert (W.decode (W.encode sample) = sample);

  (* standard BSON widening: an [Int] beyond int32 is stored as int64 and decodes to [Int64] (BSON has
     no "64-bit int" tag distinct from int64) — a value-preserving, type-widening round-trip *)
  assert (W.decode (W.encode (B.doc [ ("big", B.int 5_000_000_000) ])) = B.doc [ ("big", B.Int64 5_000_000_000L) ]);

  (* Binary is the spec layout: RAW bytes + subtype byte, not base64 text. For raw len 8 the encoded
     doc is exactly 21 bytes (4 doc-len + 3 element-header + 4 int32-len + 1 subtype + 8 raw + 1 nul);
     base64 text of those 8 bytes would be 12 chars => a different length. And the raw bytes appear
     verbatim (incl the embedded NUL and 0xff). *)
  let raw = "\x00\x01\x02\xfftail" in
  let enc = W.encode (B.doc [ ("x", B.Binary { subtype = "00"; base64 = Base64.encode_string raw }) ]) in
  assert (String.length enc = 21);
  assert (contains enc raw);

  (* a large binary payload round-trips (well past anything inline) *)
  let big = Base64.encode_string (String.init 100_000 (fun i -> Char.chr (i land 0xff))) in
  let d = B.doc [ ("blob", B.Binary { subtype = "00"; base64 = big }) ] in
  assert (W.decode (W.encode d) = d);

  (* Decimal128 is the one documented wire gap: raises (server -> error reply), never corrupts *)
  (match W.encode (B.doc [ ("d", B.Decimal128 "1.5") ]) with
   | (_ : string) -> assert false
   | exception W.Unsupported _ -> ());

  (* hostile/malformed length raises (bounds-safe), never over-reads or over-allocates *)
  (match W.decode "\xff\xff\xff\x7f" with (_ : B.t) -> assert false | exception _ -> ());

  print_string "bson_wire (spec BSON codec): OK\n"
