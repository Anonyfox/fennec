(* The shape language: every catalog entry proven — refinements (stacking, collection, hints),
   normalizers, options/absence, maps, nested records with error paths, variants, cross-field
   checks, encode-side validation, pp, and the view reflection. *)

module B = Bson

let ok = function Ok _ -> true | Error _ -> false
let err = function Error _ -> true | Ok _ -> false

(* ── refinements: stack, collect, message ── *)
let%test "refinements stack and COLLECT all violations with messages" =
  let c = Codec.(min_len 3 (max_len 5 (slug string))) in
  (match Codec.decode c (B.str "ab") with
  | Error [ e ] -> e.Codec.msg = "must be at least 3 characters"
  | _ -> false)
  && ok (Codec.decode c (B.str "abc-1"))
  && (match Codec.decode c (B.str "A!") with
     | Error es -> List.length es = 2 (* too short AND not a slug — both collected *)
     | Ok _ -> false)

let%test "normalizers run before checks, both directions" =
  let c = Codec.(non_empty (trim string)) in
  (match Codec.decode c (B.str "  hi  ") with Ok "hi" -> true | _ -> false)
  && err (Codec.decode c (B.str "   "))
  && Codec.validate c "   " = Error [ { Codec.path = []; msg = "must not be empty" } ]
  && (match (Codec.lowercase Codec.string).Codec.enc "AbC" with B.String "abc" -> true | _ -> false)

let%test "numeric refinements + float sanity (nan rejected by default, opt-out works)" =
  err (Codec.decode Codec.float (B.Float Float.nan))
  && ok (Codec.decode Codec.float_nonfinite (B.Float Float.infinity))
  && err (Codec.decode Codec.(min_i 1 int) (B.int 0))
  && ok (Codec.decode Codec.(multiple_of 0.5 float) (B.Float 2.5))
  && err (Codec.decode Codec.(positive float) (B.Float 0.0))
  && err (Codec.validate Codec.float Float.nan)

let%test "list refinements + element errors carry the index" =
  let c = Codec.(max_items 2 (unique_items (list (non_empty string)))) in
  err (Codec.decode c (B.array [ B.str "a"; B.str "a" ]))
  && err (Codec.decode c (B.array [ B.str "a"; B.str "b"; B.str "c" ]))
  && (match Codec.decode (Codec.list (Codec.non_empty Codec.string)) (B.array [ B.str "x"; B.str "" ]) with
     | Error [ e ] -> e.Codec.path = [ "1" ]
     | _ -> false)

let%test "pattern: the portable subset (anchors, classes, quantifiers); email/slug presets" =
  let zip = Codec.(pattern "^[0-9][0-9][0-9][0-9][0-9]$" string) in
  ok (Codec.decode zip (B.str "12345"))
  && err (Codec.decode zip (B.str "1234x"))
  && ok (Codec.decode (Codec.email Codec.string) (B.str "a.b+c@ex-ample.org"))
  && err (Codec.decode (Codec.email Codec.string) (B.str "not-an-email"))
  && ok (Codec.decode (Codec.slug Codec.string) (B.str "my-page-2"))
  && err (Codec.decode (Codec.slug Codec.string) (B.str "My Page"))

(* ── options / absence / maps / id ── *)
let%test "option: absent and null decode None; None omits the key; checks apply to Some" =
  let c = Codec.(seal (record (fun a -> a) |> field (opt "note" (min_len 2 string)) (fun x -> x))) in
  (match Codec.decode c (B.doc []) with Ok None -> true | _ -> false)
  && (match Codec.decode c (B.doc [ ("note", B.Null) ]) with Ok None -> true | _ -> false)
  && err (Codec.decode c (B.doc [ ("note", B.str "x") ]))
  && (match c.Codec.enc None with B.Document [] -> true | _ -> false)

let%test "opt_list tolerates absence; dft fills; str_map checks each value with its key in the path" =
  let c =
    Codec.(
      seal
        (record (fun tags n i18n -> (tags, n, i18n))
        |> field (opt_list "tags" string) (fun (t, _, _) -> t)
        |> field (dft "n" int 7) (fun (_, n, _) -> n)
        |> field (req "i18n" (str_map (non_empty string))) (fun (_, _, m) -> m)))
  in
  (match Codec.decode c (B.doc [ ("i18n", B.doc [ ("de", B.str "Hallo") ]) ]) with
  | Ok ([], 7, [ ("de", "Hallo") ]) -> true
  | _ -> false)
  && (match Codec.decode c (B.doc [ ("i18n", B.doc [ ("fr", B.str "") ]) ]) with
     | Error [ e ] -> e.Codec.path = [ "i18n"; "fr" ]
     | _ -> false)

let%test "id: accepts String and ObjectId; re-encodes oid-looking strings as ObjectId" =
  let oid = String.make 24 'a' in
  (match Codec.decode Codec.id (B.Object_id oid) with Ok s -> s = oid | _ -> false)
  && (match Codec.id.Codec.enc oid with B.Object_id _ -> true | _ -> false)
  && (match Codec.id.Codec.enc "plain" with B.String "plain" -> true | _ -> false)

(* ── nested records, cross-field checks, error paths ── *)
type addr = { zip : string }
type person = { name : string; addr : addr; from_ : int64; till : int64 }

let addr_c = Codec.(seal (record (fun zip -> { zip }) |> field (req "zip" (pattern "^[0-9][0-9][0-9]$" string)) (fun a -> a.zip)))

let person_c =
  Codec.(
    seal
      (record (fun name addr from_ till -> { name; addr; from_; till })
      |> field (req "name" (min_len 2 string)) (fun p -> p.name)
      |> field (req "addr" addr_c) (fun p -> p.addr)
      |> field (req "from" date) (fun p -> p.from_)
      |> field (req "till" date) (fun p -> p.till)
      |> checking (fun p -> p.from_ < p.till) "from must precede till"))

let%test "nested record errors carry the full path; cross-field checks run on decode AND validate" =
  let bad = B.doc [ ("name", B.str "Jo"); ("addr", B.doc [ ("zip", B.str "12x") ]); ("from", B.Date 9L); ("till", B.Date 3L) ] in
  (match Codec.decode person_c bad with
  | Error es ->
      List.exists (fun e -> e.Codec.path = [ "addr"; "zip" ]) es
      && List.exists (fun e -> e.Codec.msg = "from must precede till") es
  | Ok _ -> false)
  && err (Codec.validate person_c { name = "Jo"; addr = { zip = "123" }; from_ = 9L; till = 3L })
  && ok (Codec.validate person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L })

let%test "encode_checked is the one-call write boundary" =
  (match Codec.encode_checked person_c { name = "J"; addr = { zip = "123" }; from_ = 1L; till = 9L } with
  | Error [ e ] -> e.Codec.path = [ "name" ]
  | _ -> false)
  &&
  match Codec.encode_checked person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L } with
  | Ok (B.Document kvs) -> List.mem_assoc "addr" kvs
  | _ -> false

(* ── variants ── *)
type price = Fixed of float | Auction of float * float

let price_c =
  Codec.(
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
  (match Codec.decode price_c (price_c.Codec.enc (Auction (5.0, 1.0))) with Ok (Auction (5.0, 1.0)) -> true | _ -> false)
  && (match price_c.Codec.enc (Fixed 2.0) with
     | B.Document (("kind", B.String "fixed") :: _) -> true
     | _ -> false)
  && err (Codec.decode price_c (B.doc [ ("kind", B.str "rental") ]))
  && err (Codec.decode price_c (B.doc [ ("kind", B.str "fixed"); ("amount", B.Float 0.0) ]))
  && err (Codec.validate price_c (Fixed (-1.0)))

(* ── pp + view ── *)
let%test "pp renders nested values readably (records, lists, options, variants)" =
  let s = Codec.show person_c { name = "Jo"; addr = { zip = "123" }; from_ = 1L; till = 9L } in
  let has sub =
    let sl = String.length sub and l = String.length s in
    let rec go i = i + sl <= l && (String.sub s i sl = sub || go (i + 1)) in
    go 0
  in
  has "name = \"Jo\"" && has "zip = \"123\"" && has "date(9)"
  && (let v = Codec.show price_c (Fixed 2.5) in
      let hl sub x = let sl = String.length sub and l = String.length x in
        let rec go i = i + sl <= l && (String.sub x i sl = sub || go (i + 1)) in go 0 in
      hl "fixed" v && hl "2.5" v)

let%test "view: reflection exposes fields, requiredness, and hints for renderers" =
  match Codec.view person_c with
  | Codec.V_obj fields ->
      List.length fields = 4
      && (match List.find_opt (fun (n, _, _) -> n = "name") fields with
         | Some (_, true, Codec.V_check (Codec.H_min_len 2, Codec.V_string)) -> true
         | _ -> false)
      && (match List.find_opt (fun (n, _, _) -> n = "addr") fields with
         | Some (_, true, Codec.V_obj [ ("zip", true, Codec.V_check (Codec.H_pattern _, Codec.V_string)) ]) -> true
         | _ -> false)
  | _ -> false

let%test "view: variants reflect tag and per-case fields; opt fields reflect as not-required" =
  (match Codec.view price_c with
  | Codec.V_variant ("kind", [ ("fixed", [ ("amount", true, _) ]); ("auction", _) ]) -> true
  | _ -> false)
  &&
  let c = Codec.(seal (record (fun a -> a) |> field (opt "x" string) (fun v -> v))) in
  match Codec.view c with Codec.V_obj [ ("x", false, Codec.V_option Codec.V_string) ] -> true | _ -> false

let () = exit (Fennec_hunt_unit.run ())
