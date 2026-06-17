(* The shape language: every catalog entry proven — refinements (stacking, collection, hints),
   normalizers, options/absence, maps, nested records with error paths, variants, cross-field
   checks, encode-side validation, pp, and the view reflection. *)

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
  && (match (Sift.lowercase Sift.string).Sift.enc "AbC" with B.String "abc" -> true | _ -> false)

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
  && (match c.Sift.enc None with B.Document [] -> true | _ -> false)

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
  && (match Sift.id.Sift.enc oid with B.Object_id _ -> true | _ -> false)
  && (match Sift.id.Sift.enc "plain" with B.String "plain" -> true | _ -> false)

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
  (match Sift.decode price_c (price_c.Sift.enc (Auction (5.0, 1.0))) with Ok (Auction (5.0, 1.0)) -> true | _ -> false)
  && (match price_c.Sift.enc (Fixed 2.0) with
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

let () = exit (Fennec_hunt_unit.run ())
