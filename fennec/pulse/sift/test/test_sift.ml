(* The shape language: every catalog entry proven — refinements (stacking, collection, hints),
   normalizers, options/absence, maps, nested records with error paths, variants, cross-field checks,
   encode-side validation, pp, view reflection, the staged objN decoder, plain JSON I/O, and the
   shape-derived operations (equal/compare/hash/default/arbitrary/diff/patch). All over the Bson.t tree
   codec — the one path fennec actually runs. *)

module B = Bson

let ok = function Ok _ -> true | Error _ -> false
let err = function Error _ -> true | Ok _ -> false

(* ── refinements: stack, collect, message ── *)
let%test "refinements stack and COLLECT all violations with messages" =
  let c = Sift.(min_len 3 (max_len 5 (slug string))) in
  (match Sift.decode c (B.str "ab") with
  | Error [ e ] -> e.Sift.msg = "must be at least 3 characters"
  | _ -> false)
  && ok (Sift.decode c (B.str "abc-1"))
  && (match Sift.decode c (B.str "A!") with
     | Error es -> List.length es = 2 (* too short AND not a slug — both collected *)
     | Ok _ -> false)

let%test "normalizers run before checks, both directions" =
  let c = Sift.(non_empty (trim string)) in
  (match Sift.decode c (B.str "  hi  ") with Ok "hi" -> true | _ -> false)
  && err (Sift.decode c (B.str "   "))
  && (match Sift.validate c "   " with
     | Error [ e ] -> e.Sift.path = [] && e.Sift.msg = "must not be empty" && e.Sift.code = "min_len"
     | _ -> false)
  && (match Sift.to_bson (Sift.lowercase Sift.string) "AbC" with B.String "abc" -> true | _ -> false)

let%test "numeric refinements + float sanity (nan rejected by default, opt-out works)" =
  err (Sift.decode Sift.float (B.Float Float.nan))
  && ok (Sift.decode Sift.float_nonfinite (B.Float Float.infinity))
  && err (Sift.decode Sift.(min_i 1 int) (B.int 0))
  && ok (Sift.decode Sift.(multiple_of 0.5 float) (B.Float 2.5))
  && err (Sift.decode Sift.(positive float) (B.Float 0.0))
  && err (Sift.validate Sift.float Float.nan)

let%test "list refinements + element errors carry the index" =
  let c = Sift.(max_items 2 (unique_items (list (non_empty string)))) in
  err (Sift.decode c (B.array [ B.str "a"; B.str "a" ]))
  && err (Sift.decode c (B.array [ B.str "a"; B.str "b"; B.str "c" ]))
  && (match Sift.decode (Sift.list (Sift.non_empty Sift.string)) (B.array [ B.str "x"; B.str "" ]) with
     | Error [ e ] -> e.Sift.path = [ "1" ]
     | _ -> false)

let%test "pattern: the portable subset (anchors, classes, quantifiers); email/slug presets" =
  let zip = Sift.(pattern "^[0-9][0-9][0-9][0-9][0-9]$" string) in
  ok (Sift.decode zip (B.str "12345"))
  && err (Sift.decode zip (B.str "1234x"))
  && ok (Sift.decode (Sift.email Sift.string) (B.str "a.b+c@ex-ample.org"))
  && err (Sift.decode (Sift.email Sift.string) (B.str "not-an-email"))
  && ok (Sift.decode (Sift.slug Sift.string) (B.str "my-page-2"))
  && err (Sift.decode (Sift.slug Sift.string) (B.str "My Page"))

(* ── options / absence / maps / id ── *)
let%test "option: absent and null decode None; None omits the key; checks apply to Some" =
  let c = Sift.(seal (record (fun a -> a) |> field (opt "note" (min_len 2 string)) (fun x -> x))) in
  (match Sift.decode c (B.doc []) with Ok None -> true | _ -> false)
  && (match Sift.decode c (B.doc [ ("note", B.Null) ]) with Ok None -> true | _ -> false)
  && err (Sift.decode c (B.doc [ ("note", B.str "x") ]))
  && (match Sift.to_bson c None with B.Document [] -> true | _ -> false)

let%test "opt_list tolerates absence; dft fills; str_map checks each value with its key in the path" =
  let c =
    Sift.(
      seal
        (record (fun tags n i18n -> (tags, n, i18n))
        |> field (opt_list "tags" string) (fun (t, _, _) -> t)
        |> field (dft "n" int 7) (fun (_, n, _) -> n)
        |> field (req "i18n" (str_map (non_empty string))) (fun (_, _, m) -> m)))
  in
  (match Sift.decode c (B.doc [ ("i18n", B.doc [ ("de", B.str "Hallo") ]) ]) with
  | Ok ([], 7, [ ("de", "Hallo") ]) -> true
  | _ -> false)
  && (match Sift.decode c (B.doc [ ("i18n", B.doc [ ("fr", B.str "") ]) ]) with
     | Error [ e ] -> e.Sift.path = [ "i18n"; "fr" ]
     | _ -> false)

let%test "id: accepts String and ObjectId; re-encodes oid-looking strings as ObjectId" =
  let oid = String.make 24 'a' in
  (match Sift.decode Sift.id (B.Object_id oid) with Ok s -> s = oid | _ -> false)
  && (match Sift.to_bson Sift.id oid with B.Object_id _ -> true | _ -> false)
  && (match Sift.to_bson Sift.id "plain" with B.String "plain" -> true | _ -> false)

(* ── nested records, cross-field checks, error paths ── *)
type addr = { zip : string }
type person = { name : string; addr : addr; from_ : int64; till : int64 }

let addr_c = Sift.(seal (record (fun zip -> { zip }) |> field (req "zip" (pattern "^[0-9][0-9][0-9]$" string)) (fun a -> a.zip)))

let person_c =
  Sift.(
    seal
      (record (fun name addr from_ till -> { name; addr; from_; till })
      |> field (req "name" (min_len 2 string)) (fun p -> p.name)
      |> field (req "addr" addr_c) (fun p -> p.addr)
      |> field (req "from" date) (fun p -> p.from_)
      |> field (req "till" date) (fun p -> p.till)
      |> checking (fun p -> p.from_ < p.till) "from must precede till"))

let%test "nested record errors carry the full path; cross-field checks run on decode AND validate" =
  let bad = B.doc [ ("name", B.str "Jo"); ("addr", B.doc [ ("zip", B.str "12x") ]); ("from", B.Date 9L); ("till", B.Date 3L) ] in
  (match Sift.decode person_c bad with
  | Error es ->
      List.exists (fun e -> e.Sift.path = [ "addr"; "zip" ]) es
      && List.exists (fun e -> e.Sift.msg = "from must precede till") es
  | Ok _ -> false)
  && err (Sift.validate person_c { name = "Jo"; addr = { zip = "123" }; from_ = 9L; till = 3L })
  && ok (Sift.validate person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L })

let%test "encode_checked is the one-call write boundary" =
  (match Sift.encode_checked person_c { name = "J"; addr = { zip = "123" }; from_ = 1L; till = 9L } with
  | Error [ e ] -> e.Sift.path = [ "name" ]
  | _ -> false)
  &&
  match Sift.encode_checked person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L } with
  | Ok (B.Document kvs) -> List.mem_assoc "addr" kvs
  | _ -> false

(* ── variants ── *)
type price = Fixed of float | Auction of float * float

let price_c =
  Sift.(
    variant ~tag:"kind"
      [
        case "fixed"
          (record (fun a -> Fixed a) |> field (req "amount" (positive float)) (function Fixed a -> a | _ -> 0.))
          ~inj:Fun.id ~proj:(function Fixed _ as v -> Some v | _ -> None);
        case "auction"
          (record (fun f s -> Auction (f, s))
          |> field (req "floor" (positive float)) (function Auction (f, _) -> f | _ -> 0.)
          |> field (req "step" (min_f 0.5 float)) (function Auction (_, s) -> s | _ -> 0.))
          ~inj:Fun.id ~proj:(function Auction _ as v -> Some v | _ -> None);
      ])

let%test "variants: tag dispatch round-trips; unknown tags and bad payloads error; checks apply per case" =
  (match Sift.decode price_c (Sift.to_bson price_c (Auction (5.0, 1.0))) with Ok (Auction (5.0, 1.0)) -> true | _ -> false)
  && (match Sift.to_bson price_c (Fixed 2.0) with
     | B.Document (("kind", B.String "fixed") :: _) -> true
     | _ -> false)
  && err (Sift.decode price_c (B.doc [ ("kind", B.str "rental") ]))
  && err (Sift.decode price_c (B.doc [ ("kind", B.str "fixed"); ("amount", B.Float 0.0) ]))
  && err (Sift.validate price_c (Fixed (-1.0)))

(* ── pp + view ── *)
let%test "pp renders nested values readably (records, lists, options, variants)" =
  let s = Sift.show person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L } in
  let has sub =
    let sl = String.length sub and l = String.length s in
    let rec go i = i + sl <= l && (String.sub s i sl = sub || go (i + 1)) in
    go 0
  in
  has "name = \"Jo\"" && has "zip = \"123\"" && has "date(9)"
  && (let v = Sift.show price_c (Fixed 2.5) in
      let hl sub x = let sl = String.length sub and l = String.length x in
        let rec go i = i + sl <= l && (String.sub x i sl = sub || go (i + 1)) in go 0 in
      hl "fixed" v && hl "2.5" v)

let%test "view: reflection exposes fields, requiredness, and hints for renderers" =
  match Sift.view person_c with
  | Sift.V_obj fields ->
      List.length fields = 4
      && (match List.find_opt (fun (n, _, _) -> n = "name") fields with
         | Some (_, true, Sift.V_check (Sift.H_min_len 2, Sift.V_string)) -> true
         | _ -> false)
      && (match List.find_opt (fun (n, _, _) -> n = "addr") fields with
         | Some (_, true, Sift.V_obj [ ("zip", true, Sift.V_check (Sift.H_pattern _, Sift.V_string)) ]) -> true
         | _ -> false)
  | _ -> false

let%test "view: variants reflect tag and per-case fields; opt fields reflect as not-required" =
  (match Sift.view price_c with
  | Sift.V_variant ("kind", [ ("fixed", [ ("amount", true, _) ]); ("auction", _) ]) -> true
  | _ -> false)
  &&
  let c = Sift.(seal (record (fun a -> a) |> field (opt "x" string) (fun v -> v))) in
  match Sift.view c with Sift.V_obj [ ("x", false, Sift.V_option Sift.V_string) ] -> true | _ -> false

(* ── structured error codes + params + i18n translate ── *)
let%test "errors carry machine codes + params (for i18n) and translate remaps the message" =
  let c = Sift.(req "title" (max_len 5 string)) in
  match Sift.field_get c (B.doc [ ("title", B.str "way too long") ]) with
  | Error [ e ] ->
      e.Sift.code = "max_len" && List.assoc "n" e.Sift.params = "5"
      && (* a translator keyed on code+params *)
      (Sift.translate (fun e -> Printf.sprintf "trop long (max %s)" (List.assoc "n" e.Sift.params)) e).Sift.msg
         = "trop long (max 5)"
  | _ -> false

let%test "a missing required field has code \"required\"; a type mismatch has code \"type\"" =
  let c = Sift.(seal (record (fun a -> a) |> field (req "n" int) (fun v -> v))) in
  (match Sift.decode c (B.doc []) with Error [ e ] -> e.Sift.code = "required" | _ -> false)
  && match Sift.decode c (B.doc [ ("n", B.str "x") ]) with Error [ e ] -> e.Sift.code = "type" | _ -> false

(* ── fix: self-referential codecs (trees, threads) ── *)
type tree = { v : string; kids : tree list }

let tree_codec =
  Sift.(fix (fun tree -> seal (record (fun v kids -> { v; kids }) |> field (req "v" string) (fun t -> t.v) |> field (opt_list "kids" tree) (fun t -> t.kids))))

let%test "fix: a recursive tree round-trips at depth" =
  let t = { v = "root"; kids = [ { v = "a"; kids = [] }; { v = "b"; kids = [ { v = "b1"; kids = [] } ] } ] } in
  match Sift.decode tree_codec (Sift.to_bson tree_codec t) with Ok t' -> t' = t | Error _ -> false
let%test "fix: a nested decode error is still path-tagged" =
  ( match Sift.decode tree_codec (B.doc [ ("v", B.str "r"); ("kids", B.array [ B.doc [ ("v", B.int 5) ] ]) ]) with Error (e :: _) -> e.Sift.path <> [] | _ -> false)

(* ── from_string: accept-and-parse stringly input ── *)
let%test "from_string: native OR stringly number, then the inner checks run" =
  let c = Sift.(from_string (min_i 1 int)) in
  (match Sift.decode c (B.int 5) with Ok 5 -> true | _ -> false)
  && (match Sift.decode c (B.str "5") with Ok 5 -> true | _ -> false)
  && err (Sift.decode c (B.str "0")) (* parsed to 0, then min_i 1 rejects *)
  && err (Sift.decode c (B.str "abc")) (* unparseable → type error *)
let%test "from_string: bool/float coercions" =
  (match Sift.decode Sift.(from_string bool) (B.str "true") with Ok true -> true | _ -> false) && (match Sift.decode Sift.(from_string float) (B.str "2.5") with Ok 2.5 -> true | _ -> false)
let%test "from_string: encode stays native, reflects as the inner type" =
  (match Sift.to_bson (Sift.from_string Sift.int) 7 with B.Int 7 -> true | _ -> false) && Sift.view Sift.(from_string int) = Sift.V_int

(* ── map: total two-way transform ── *)
let%test "map: round-trips through a transform both ways" =
  let c = Sift.(map (fun s -> `Tag s) (function `Tag s -> s) string) in
  (match Sift.decode c (B.str "x") with Ok (`Tag "x") -> true | _ -> false) && (match Sift.to_bson c (`Tag "y") with B.String "y" -> true | _ -> false)

(* ── extended-JSON I/O ($oid/$date via the bson_json bridge): one shape drives BOTH BSON and JSON ── *)
let json_c =
  Sift.(seal (record (fun id n tags -> (id, n, tags)) |> field (req "id" string) (fun (i, _, _) -> i) |> field (req "n" (min_i 1 int)) (fun (_, n, _) -> n) |> field (opt_list "tags" string) (fun (_, _, t) -> t)))

let%test "json: value → JSON string → value round-trips" =
  let v = ("x", 3, [ "a"; "b" ]) in
  match Sift.of_json_string json_c (Sift.to_json_string json_c v) with Ok v' -> v' = v | Error _ -> false
let%test "json decode validates with the same path-collected errors as BSON decode" =
  err (Sift.of_json_string json_c {|{"id":"x","n":0}|}) (* n=0 fails min_i 1 *)
  && (match Sift.of_json_string json_c "not json at all" with Error [ e ] -> e.Sift.code = "json" | _ -> false)

(* ── fixtures exercised by the encode-size, staged-decoder, derived-ops, diff/patch and plain-JSON
   tests below — a scalar bag covering every leaf, options, lists, maps, nested, variants, the dynamic
   [bson] escape, a from_string field, and a cross-field invariant ── *)
module W = Mongo_wire.Bson_wire

let bag_c =
  Sift.(
    seal
      (record (fun s n f b d i -> (s, n, f, b, d, i))
      |> field (req "s" string) (fun (s, _, _, _, _, _) -> s)
      |> field (req "n" int) (fun (_, n, _, _, _, _) -> n)
      |> field (req "f" float) (fun (_, _, f, _, _, _) -> f)
      |> field (req "b" bool) (fun (_, _, _, b, _, _) -> b)
      |> field (req "d" date) (fun (_, _, _, _, d, _) -> d)
      |> field (req "i" id) (fun (_, _, _, _, _, i) -> i)))

let opt_c = Sift.(seal (record (fun a b -> (a, b)) |> field (opt "a" string) fst |> field (opt_list "b" int) snd))
let list_c = Sift.(seal (record (fun xs -> xs) |> field (req "xs" (list (non_empty string))) (fun xs -> xs)))
let map_c = Sift.(seal (record (fun m -> m) |> field (req "m" (str_map int)) (fun m -> m)))

let nested_c =
  let author =
    Sift.(seal (record (fun n e -> (n, e)) |> field (req "name" (non_empty string)) fst |> field (req "email" (email string)) snd))
  in
  Sift.(seal (record (fun t a -> (t, a)) |> field (req "title" (max_len 5 string)) fst |> field (req "author" author) snd))

type figure = Circle of float | Rect of (float * float)

let figure_c =
  Sift.(
    variant ~tag:"kind"
      [ case "circle" (record (fun r -> r) |> field (req "r" float) (fun r -> r)) ~inj:(fun r -> Circle r) ~proj:(function Circle r -> Some r | _ -> None);
        case "rect" (record (fun w h -> (w, h)) |> field (req "w" float) fst |> field (req "h" float) snd) ~inj:(fun (w, h) -> Rect (w, h)) ~proj:(function Rect (w, h) -> Some (w, h) | _ -> None) ])

let coerce_c = Sift.(seal (record (fun n -> n) |> field (req "n" (from_string (min_i 1 int))) (fun n -> n)))
let bson_c = Sift.(seal (record (fun v -> v) |> field (req "v" bson) (fun v -> v)))
let range_c = Sift.(seal (record (fun lo hi -> (lo, hi)) |> field (req "lo" int) fst |> field (req "hi" int) snd |> checking (fun (lo, hi) -> lo <= hi) "lo must be <= hi"))

(* ── encode mirror: [size] equals the EXACT byte length the codec's BSON encodes to (optional/opt_list
   omission, int32-vs-int64 / oid-vs-string choices) ── *)
let size_matches (c : 'a Sift.t) (v : 'a) : bool = Sift.size c v = String.length (W.encode (Sift.to_bson c v))

let%test "size == encoded length — scalars, ids, large ints" =
  size_matches bag_c ("hi", 42, 3.5, true, 1700000000000L, "507f1f77bcf86cd799439011")
  && size_matches bag_c ("", 0, 0.0, false, 0L, "plain")
  && size_matches bag_c ("unicode → ✓", 5_000_000_000, -2.5, true, -1L, "507f1f77bcf86cd799439011")

let%test "size == encoded length — options (present + omitted), lists, maps" =
  size_matches opt_c (Some "x", [ 1; 2; 3 ])
  && size_matches opt_c (None, [])
  && size_matches opt_c (Some "", [])
  && size_matches list_c [ "a"; "bb"; "ccc"; "" ]
  && size_matches list_c []
  && size_matches map_c [ ("x", 1); ("y", 22); ("z", 333) ]

let%test "size == encoded length — nested records and variants" =
  size_matches nested_c ("ok", ("Ada Lovelace", "ada@x.io"))
  && size_matches figure_c (Circle 2.5)
  && size_matches figure_c (Rect (3.0, 4.0))
  && size_matches range_c (1, 9)
  && size_matches coerce_c 5

(* ── the staged objN decoder is byte-identical to the applicative builder ── *)
let viaobj = Sift.(obj3 (req "s" string) (req "n" (min_i 1 int)) (opt "note" string) ~make:(fun s n note -> (s, n, note)) ~split:(fun (s, n, note) -> (s, n, note)))
let viabuilder = Sift.(seal (record (fun s n note -> (s, n, note)) |> field (req "s" string) (fun (s, _, _) -> s) |> field (req "n" (min_i 1 int)) (fun (_, n, _) -> n) |> field (opt "note" string) (fun (_, _, x) -> x)))

let%test "objN decoder == applicative builder (decode verdict+errors, encode, size)" =
  let same_decode b = Sift.decode viaobj b = Sift.decode viabuilder b in
  let same_enc v = Sift.to_bson viaobj v = Sift.to_bson viabuilder v && Sift.size viaobj v = Sift.size viabuilder v in
  same_decode (B.doc [ ("s", B.str "x"); ("n", B.int 5); ("note", B.str "hi") ]) (* ok *)
  && same_decode (B.doc [ ("s", B.str "x"); ("n", B.int 5) ]) (* note absent → None *)
  && same_decode (B.doc [ ("s", B.int 1); ("n", B.int 0) ]) (* TWO errors: s type + n<min — must collect in same order *)
  && same_decode (B.doc []) (* required s, n missing *)
  && same_enc ("a", 7, Some "z")
  && same_enc ("a", 7, None) (* None omits the key, both ways *)
  && (match Sift.decode viaobj (B.doc [ ("s", B.str "x"); ("n", B.int 5); ("note", B.str "hi") ]) with Ok ("x", 5, Some "hi") -> true | _ -> false)

(* ── derived ops: equal / compare / hash / default, shape-derived ── *)
let%test "equal — reflexive, distinguishes fields, variant cases" =
  let v = ("hi", 42, 3.5, true, 1L, "id") in
  Sift.equal bag_c v v
  && (not (Sift.equal bag_c v ("hi", 43, 3.5, true, 1L, "id")))
  && Sift.equal opt_c (None, []) (None, [])
  && (not (Sift.equal opt_c (Some "x", []) (None, [])))
  && Sift.equal figure_c (Circle 2.0) (Circle 2.0)
  && (not (Sift.equal figure_c (Circle 2.0) (Rect (2.0, 3.0))))
  && Sift.equal list_c [ "a"; "b" ] [ "a"; "b" ]
  && (not (Sift.equal list_c [ "a"; "b" ] [ "a"; "c" ]))
  && not (Sift.equal list_c [ "a" ] [ "a"; "b" ])

let%test "compare — total order, consistent with equal, sorts correctly" =
  Sift.compare bag_c ("a", 1, 0., true, 0L, "x") ("a", 1, 0., true, 0L, "x") = 0
  && Sift.compare bag_c ("a", 1, 0., true, 0L, "x") ("a", 2, 0., true, 0L, "x") < 0
  && Sift.compare figure_c (Circle 1.0) (Rect (1., 1.)) < 0 (* Circle declared before Rect *)
  && (let sorted = List.sort (Sift.compare range_c) [ (3, 9); (1, 2); (2, 5); (1, 1) ] in sorted = [ (1, 1); (1, 2); (2, 5); (3, 9) ])

let%test "hash — equal values hash equal" =
  Sift.hash bag_c ("hi", 42, 3.5, true, 1L, "id") = Sift.hash bag_c ("hi", 42, 3.5, true, 1L, "id")
  && Sift.hash figure_c (Circle 2.0) = Sift.hash figure_c (Circle 2.0)
  && Sift.hash opt_c (None, []) = Sift.hash opt_c (None, [])

let%test "default — sensible zeros; round-trips through encode/decode" =
  Sift.default bag_c = ("", 0, 0.0, false, 0L, "")
  && Sift.default opt_c = (None, [])
  && Sift.default list_c = []
  && (match Sift.default figure_c with Circle 0.0 -> true | _ -> false)
  && Sift.default nested_c = ("", ("", "")) (* nested record built from leaf defaults *)
  && Sift.default coerce_c = 0 (* structural zero — note 0 would fail this shape's [min_i 1] on validation *)
  && (match Sift.decode bag_c (Sift.to_bson bag_c (Sift.default bag_c)) with Ok ("", 0, 0.0, false, 0L, "") -> true | _ -> false)

let%test "diff — no change, field change, optional set/unset" =
  let v = ("a", 1, 0.0, true, 0L, "x") in
  Sift.diff bag_c v v = { Sift.set = []; unset = [] }
  && (match Sift.diff bag_c v ("a", 2, 0.0, true, 0L, "x") with { Sift.set = [ ("n", B.Int 2) ]; unset = [] } -> true | _ -> false)
  && (match Sift.diff opt_c (Some "x", [ 1 ]) (None, [ 1 ]) with { Sift.set = []; unset = [ "a" ] } -> true | _ -> false)
  && (match Sift.diff opt_c (None, []) (Some "y", []) with { Sift.set = [ ("a", B.String "y") ]; unset = [] } -> true | _ -> false)
  && (match Sift.diff opt_c (Some "x", [ 1; 2 ]) (Some "x", []) with { Sift.set = []; unset = [ "b" ] } -> true | _ -> false)

let bare_map_c = Sift.(str_map int) (* a top-level dynamic-key map (not a record wrapping one) *)

let%test "diff — maps (changed/added/removed keys), variants (body + case change)" =
  (match Sift.diff bare_map_c [ ("x", 1); ("y", 2) ] [ ("x", 1); ("y", 9); ("z", 3) ] with { Sift.set = [ ("y", B.Int 9); ("z", B.Int 3) ]; unset = [] } -> true | _ -> false)
  && (match Sift.diff bare_map_c [ ("x", 1); ("y", 2) ] [ ("x", 1) ] with { Sift.set = []; unset = [ "y" ] } -> true | _ -> false)
  && (match Sift.diff figure_c (Rect (1., 2.)) (Rect (1., 9.)) with { Sift.set = [ ("h", B.Float 9.) ]; unset = [] } -> true | _ -> false)
  && (let d = Sift.diff figure_c (Circle 1.) (Rect (3., 4.)) in List.mem "kind" (List.map fst d.Sift.set) && List.mem "w" (List.map fst d.Sift.set) && d.Sift.unset = [ "r" ])

let%test "arbitrary — random values round-trip through encode/decode (and equal themselves)" =
  let st = Random.State.make [| 1; 2; 3 |] in
  let survives : 'a. 'a Sift.t -> bool =
   fun c ->
    let rec loop i = i = 0 || (let v = Sift.arbitrary c st in (match Sift.decode c (Sift.to_bson c v) with Ok v' -> Sift.equal c v v' | Error _ -> false) && loop (i - 1)) in
    loop 200
  in
  (* shapes whose refinements rejection CAN meet (length/non-empty); a [pattern] like email or a
     cross-field invariant is the documented best-effort limit, so not exercised here *)
  survives bag_c && survives opt_c && survives list_c && survives figure_c

let%test "patch — the inverse of diff (patch old (diff old new) = new)" =
  let rt : 'a. 'a Sift.t -> 'a -> 'a -> bool =
   fun c old new_ -> match Sift.patch c old (Sift.diff c old new_) with Ok v -> Sift.equal c v new_ | Error _ -> false
  in
  rt bag_c ("a", 1, 0., true, 0L, "x") ("b", 2, 3., false, 5L, "y")
  && rt opt_c (Some "x", [ 1; 2 ]) (None, [ 3 ])
  && rt opt_c (None, []) (Some "z", [ 1 ])
  && rt opt_c (Some "x", []) (Some "x", []) (* no change → identity *)
  && rt figure_c (Circle 1.) (Rect (3., 4.)) (* variant case change *)
  && rt figure_c (Rect (1., 2.)) (Rect (1., 9.)) (* same-case body change *)
  && rt bare_map_c [ ("x", 1); ("y", 2) ] [ ("x", 1); ("y", 9); ("z", 3) ]

(* ── plain/relaxed JSON (encode_json / decode_json) — the HTTP wire form, no $oid/$date wrapping ── *)

let%test "encode_json — relaxed (plain) JSON, round-trips, escapes, no extended wrapping" =
  let rt : 'a. 'a Sift.t -> 'a -> bool =
   fun c v -> match Sift.decode_json c (Sift.encode_json c v) with Ok v' -> Sift.equal c v v' | Error _ -> false
  in
  let contains hay sub =
    let hl = String.length hay and sl = String.length sub in
    let rec go i = i + sl <= hl && (String.sub hay i sl = sub || go (i + 1)) in
    go 0
  in
  rt bag_c ("hi\"x\\y", 42, 3.5, true, 1700000000000L, "507f1f77bcf86cd799439011")
  && rt opt_c (Some "x", [ 1; 2; 3 ])
  && rt opt_c (None, [])
  && rt list_c [ "a"; "b\nc"; "d" ] (* non-empty elements: list_c's element has non_empty *)
  && rt figure_c (Rect (3.0, 4.0))
  && rt nested_c ("ok", ("Ada", "ada@x.io"))
  && (let j = Sift.encode_json bag_c ("x", 42, 1.0, true, 0L, "abc") in contains j "42" && contains j "\"abc\"" && not (contains j "$number") && not (contains j "$oid"))

let%test "decode_json parses relaxed JSON (numbers/escapes/unicode/nesting), validates, rejects junk" =
  (match Sift.decode_json bag_c {|{"s":"a\nb","n":42,"f":3.5,"b":true,"d":1700000000000,"i":"507f1f77bcf86cd799439011"}|} with
   | Ok ("a\nb", 42, 3.5, true, 1700000000000L, "507f1f77bcf86cd799439011") -> true
   | _ -> false)
  && (match Sift.decode_json Sift.string {|"café"|} with Ok "café" -> true | _ -> false) (* \u + UTF-8 *)
  && (match Sift.decode_json (Sift.list Sift.int) "[1, 2, 3]" with Ok [ 1; 2; 3 ] -> true | _ -> false)
  && (match Sift.decode_json viaobj {|{"s":"x","n":0}|} with Error _ -> true | _ -> false) (* n < min_i 1 *)
  && (match Sift.decode_json bag_c "not json" with Error [ e ] -> e.Sift.code = "json" | _ -> false)
  && (match Sift.decode_json Sift.int "42 oops" with Error [ e ] -> e.Sift.code = "json" | _ -> false) (* trailing data *)
  && (match Sift.decode_json (Sift.list Sift.int) "[1, 2" with Error _ -> true | _ -> false) (* unterminated *)

(* ── the [bson] dynamic escape: an arbitrary raw Bson.t value round-trips (any shape, untyped) ── *)
let%test "bson: the dynamic escape round-trips an arbitrary rich Bson.t value" =
  let v = B.doc [ ("a", B.array [ B.int 1; B.Float 2.5; B.bool true ]); ("o", B.oid "507f1f77bcf86cd799439011"); ("d", B.date 99L); ("big", B.Int64 9000000000L) ] in
  (match Sift.decode bson_c (Sift.to_bson bson_c v) with Ok v' -> Sift.equal bson_c v v' | _ -> false)
  && (match Sift.decode bson_c (Sift.to_bson bson_c B.Null) with Ok B.Null -> true | _ -> false)

let () = exit (Fennec_hunt_unit.run ())
