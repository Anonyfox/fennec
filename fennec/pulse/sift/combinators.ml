(* The combinator surface used to BUILD shapes: primitives, refinements + normalizers, the record
   builder, variants. New features (validators, shapes) grow here. *)

open Shape
open Engine
open Bson_engine
open Codec

let string = of_shape TString
let int = of_shape TInt
let float = of_shape (TFloat { allow_nonfinite = false })
let float_nonfinite = of_shape (TFloat { allow_nonfinite = true })
let bool = of_shape TBool
let date = of_shape TDate
let id = of_shape TId
let dyn = of_shape TDyn
let unit = of_shape TUnit
let list c = of_shape (TList c.shape)
let option c = of_shape (TOption c.shape)
let str_map c = of_shape (TMap c.shape)
let conv proj inj c = of_shape (TConv (inj, proj, c.shape))

(* the dynamic escape, typed as a raw {!Bson.t} — rebuilt LOSSLESSLY as a conv over the neutral {!dyn}
   (the rich Value mirrors Bson 1:1). Keeps the [Sift.bson] surface + the deriver's [Sift.bson] emit
   working; when Sift goes dependency-free this binding moves to the [sift.bson] plugin. *)
let bson = conv (fun v -> Ok (Value_bson.to_bson v)) Value_bson.of_bson dyn
let make ~enc ~dec = conv dec enc bson

(* a total, two-way transform — [conv] without the fallible decode side (the common case) *)
let map (to_ : 'a -> 'b) (of_ : 'b -> 'a) (c : 'a t) : 'b t = conv (fun a -> Ok (to_ a)) of_ c

(* decode ALSO accepts a [String] and parses it to the leaf (numbers/bools/dates that arrive stringly —
   forms, query strings, lenient JSON); a native value still decodes directly. Encode is unchanged. *)
let from_string c = of_shape (TCoerce c.shape)

(* a self-referential codec: [fix (fun self -> … self …)] — trees, comment threads, nested menus. The
   [self] passed in refers back to the whole codec; it's forced lazily, so finite values terminate. *)
let fix (f : 'a t -> 'a t) : 'a t =
  let rec lazy_ty = lazy ((f (of_shape (TLazy lazy_ty))).shape) in
  of_shape (Lazy.force lazy_ty)

(* ---- refinements + normalizers ----------------------------------------------------- *)

let check ?(msg = "invalid value") p c = of_shape (TCheck (p, msg, H_none, c.shape))
let refine p msg hint c = of_shape (TCheck (p, msg, hint, c.shape))

let min_len n c = refine (fun s -> String.length s >= n) (Printf.sprintf "must be at least %d characters" n) (H_min_len n) c
let max_len n c = refine (fun s -> String.length s <= n) (Printf.sprintf "must be at most %d characters" n) (H_max_len n) c
let non_empty c = refine (fun s -> String.length s > 0) "must not be empty" (H_min_len 1) c
let one_of vs c = refine (fun s -> List.mem s vs) ("must be one of: " ^ String.concat ", " vs) (H_enum vs) c

(* a small, DELIBERATELY portable matcher: ^ $ anchors, classes [a-z0-9-], +, *, ?, '.', literals —
   the common subset that means the same here, in the browser, and in mongod's $jsonSchema *)
let pattern_matches re s =
  let rl = String.length re and sl = String.length s in
  (* parse one atom at [ri]; returns (matcher, next ri) *)
  let parse_class ri =
    let close = String.index_from re ri ']' in
    let body = String.sub re (ri + 1) (close - ri - 1) in
    let neg = String.length body > 0 && body.[0] = '^' in
    let body = if neg then String.sub body 1 (String.length body - 1) else body in
    let inside c =
      let bl = String.length body in
      let rec go i =
        if i >= bl then false
        else if i + 2 < bl && body.[i + 1] = '-' then if c >= body.[i] && c <= body.[i + 2] then true else go (i + 3)
        else if body.[i] = c then true
        else go (i + 1)
      in
      go 0
    in
    ((fun c -> if neg then not (inside c) else inside c), close + 1)
  in
  let rec mtch ri si =
    if ri >= rl then si = sl || re.[rl - 1] <> '$'
    else if re.[ri] = '$' && ri = rl - 1 then si = sl
    else
      let atom, ri' =
        match re.[ri] with
        | '[' -> parse_class ri
        | '.' -> ((fun _ -> true), ri + 1)
        | '\\' when ri + 1 < rl -> (( = ) re.[ri + 1], ri + 2)
        | c -> (( = ) c, ri + 1)
      in
      let quant = if ri' < rl then Some re.[ri'] else None in
      match quant with
      | Some '+' -> if si < sl && atom s.[si] then star atom (ri' + 1) (si + 1) else false
      | Some '*' -> star atom (ri' + 1) si
      | Some '?' ->
          (si < sl && atom s.[si] && mtch (ri' + 1) (si + 1)) || mtch (ri' + 1) si
      | _ -> si < sl && atom s.[si] && mtch ri' (si + 1)
  and star atom ri si = mtch ri si || (si < sl && atom s.[si] && star atom ri (si + 1)) in
  if rl > 0 && re.[0] = '^' then mtch 1 0
  else
    (* unanchored: try every start *)
    let rec any si = mtch 0 si || (si < sl && any (si + 1)) in
    any 0

let pattern re c = refine (fun s -> pattern_matches re s) (Printf.sprintf "must match %s" re) (H_pattern re) c
let email c = pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z][a-zA-Z]+$" c
let url c = pattern "^https?://[^ ]+$" c
let slug c = pattern "^[a-z0-9-]+$" c
let trim c = of_shape (TNorm (String.trim, c.shape))
let lowercase c = of_shape (TNorm (String.lowercase_ascii, c.shape))

let min_i n c = refine (fun v -> v >= n) (Printf.sprintf "must be ≥ %d" n) (H_min (float_of_int n)) c
let max_i n c = refine (fun v -> v <= n) (Printf.sprintf "must be ≤ %d" n) (H_max (float_of_int n)) c
let min_f x c = refine (fun v -> v >= x) (Printf.sprintf "must be ≥ %g" x) (H_min x) c
let max_f x c = refine (fun v -> v <= x) (Printf.sprintf "must be ≤ %g" x) (H_max x) c
let positive c = refine (fun v -> v > 0.0) "must be positive" (H_min epsilon_float) c
let positive_i c = refine (fun v -> v > 0) "must be positive" (H_min 1.0) c
let non_negative c = refine (fun v -> v >= 0.0) "must not be negative" (H_min 0.0) c
let multiple_of n c =
  refine (fun v -> Float.is_integer (v /. n)) (Printf.sprintf "must be a multiple of %g" n) (H_multiple_of n) c

let min_items n c = refine (fun l -> List.length l >= n) (Printf.sprintf "must have at least %d items" n) (H_min_items n) c
let max_items n c = refine (fun l -> List.length l <= n) (Printf.sprintf "must have at most %d items" n) (H_max_items n) c
let unique_items c =
  refine (fun l -> List.length (List.sort_uniq compare l) = List.length l) "duplicate items" H_unique_items c

(* ---- records: the builder (the deriver's target) ------------------------------------ *)

(* [type 'a field] now lives in {!Shape} (so {!Shape.record_shape} can carry a format-agnostic
   {!Shape.field_reader} that pulls fields of every type). The constructors stay here. *)

let req name c = { key = name; item = c.shape; needed = true; fallback = None }
let opt name c = { key = name; item = (option c).shape; needed = false; fallback = Some None }
let opt_list name c = { key = name; item = (list c).shape; needed = false; fallback = Some [] }
let dft name c v = { key = name; item = c.shape; needed = false; fallback = Some v }

(* the wire-omission rule, Bson-FREE: reproduce {!Engine.write}'s Null / empty-array production without
   building any Bson — a non-required field is dropped when it would encode to Null (an absent option /
   unit / null dyn, through any wrapper) or to the matching empty array (an opt_list default). *)
let rec writes_null : type a. a shape -> a -> bool =
 fun shape v ->
  match shape with
  | TUnit -> true
  | TDyn -> ( match v with Value.Null -> true | _ -> false)
  | TOption el -> ( match v with None -> true | Some x -> writes_null el x)
  | TNorm (f, inner) -> writes_null inner (f v)
  | TConv (inj, _, inner) -> writes_null inner (inj v)
  | TCheck (_, _, _, inner) -> writes_null inner v
  | TCoerce inner -> writes_null inner v
  | TLazy l -> writes_null (Lazy.force l) v
  | _ -> false

let rec writes_empty_array : type a. a shape -> a -> bool =
 fun shape v ->
  match shape with
  | TList _ -> ( match v with [] -> true | _ -> false)
  | TOption el -> ( match v with None -> false | Some x -> writes_empty_array el x)
  | TNorm (f, inner) -> writes_empty_array inner (f v)
  | TConv (inj, _, inner) -> writes_empty_array inner (inj v)
  | TCheck (_, _, _, inner) -> writes_empty_array inner v
  | TCoerce inner -> writes_empty_array inner v
  | TLazy l -> writes_empty_array (Lazy.force l) v
  | _ -> false

let omit_of (f : 'a field) : 'a -> bool =
 fun v -> (not f.needed) && (writes_null f.item v || (writes_empty_array f.item v && f.fallback = Some v))

let decode_named (f : 'a field) kvs : ('a, error list) result =
  match List.assoc_opt f.key kvs with
  | Some v -> ( match read f.item v with Ok x -> Ok x | Error es -> Error (List.map (at f.key) es))
  | None -> (
      match f.fallback with
      | Some d -> Ok d
      (* path-tagged by field name (like the type-mismatch case above), so a missing required field
         is attributable to it — what per-field consumers (form feedback) need *)
      | None -> Error [ mkerr ~code:"required" ~path:[ f.key ] "is required" ])


type ('r, 'k) builder = {
  (* decode is driven by a {!Shape.field_reader} — the SAME builder feeds every format (the engine's
     kvs index, the zero-copy buffer spans, …), the constructor threaded once. The source lives in the
     reader's closure, not in this type. Encode is reconstructed per-format from {!acc_members} (name +
     shape + omit), so no format-typed accumulator lives here — the builder is format-agnostic. *)
  acc_decode : field_reader -> ('k, error list) result;
  acc_members : 'r bound_field list; (* reverse order *)
  acc_invariants : (('r -> bool) * string) list;
}

let record (make : 'k) : ('r, 'k) builder = { acc_decode = (fun _ -> Ok make); acc_members = []; acc_invariants = [] }

let field (f : 'a field) (get : 'r -> 'a) (b : ('r, 'a -> 'k) builder) : ('r, 'k) builder =
  {
    acc_decode =
      (fun fr ->
        match (b.acc_decode fr, fr.read_field f) with
        | Ok k, Ok a -> Ok (k a)
        | Error e1, Error e2 -> Error (e1 @ e2)
        | Error e, Ok _ | Ok _, Error e -> Error e);
    acc_members =
      Bound_field
        { name = f.key;
          shape = f.item;
          get;
          required = f.needed;
          omit = omit_of f;
        }
      :: b.acc_members;
    acc_invariants = b.acc_invariants;
  }

let checking p msg b = { b with acc_invariants = (p, msg) :: b.acc_invariants }

let record_of_builder (b : ('r, 'r) builder) : 'r record_shape =
  { decode_src = b.acc_decode; members = List.rev b.acc_members; invariants = List.rev b.acc_invariants }

let seal (b : ('r, 'r) builder) : 'r t = of_shape (TObj (record_of_builder b))
let doc_id = req "_id" id

(* navigate into an embedded record for a SELECTOR/MODIFIER path: the resulting field has the dotted
   wire name ("author.name") and the LEAF's shape/checks — so [Filter.eq (dot Fields.author
   Author.Fields.name) v] checks both names AND the value type, value-level (no ppx). The outer
   field's own codec is irrelevant here (we only borrow its wire name); the leaf decides the type.
   Required at the leaf (a dotted equality on an absent path simply matches nothing). *)
let dot (outer : _ field) (inner : 'a field) : 'a field =
  { key = outer.key ^ "." ^ inner.key;
    item = inner.item;
    needed = inner.needed;
    fallback = inner.fallback }

(* accessors for the collection vocabulary (Filter/M/Index build on field handles) *)
let field_name f = f.key
let field_enc f v = write f.item v

(* encode ONE element of a list field — total through any check/norm/conv wrapping: encode the
   singleton list and unwrap (the list encoder is structural) *)
let field_elem_enc (f : 'a list field) (v : 'a) : Bson.t =
  match write f.item [ v ] with Bson.Array [ x ] -> x | _ -> invalid_arg "Sift.field_elem_enc"

let field_validate f v = match run_checks f.item v with [] -> Ok () | es -> Error (List.map (at f.key) es)

(* decode ONE field out of a document (raw decode + the field's checks) — the projection primitive:
   reading a projected slice without ever constructing the full record *)
let field_get (f : 'a field) (doc : Bson.t) : ('a, error list) result =
  match doc with
  | Bson.Document kvs -> (
      match decode_named f kvs with
      | Error es -> Error es
      | Ok v -> ( match run_checks f.item v with [] -> Ok v | es -> Error (List.map (at f.key) es)))
  | _ -> Error [ mkerr ~code:"type" ~path:[ f.key ] "expected document" ]

(* ---- variants (tagged unions over a discriminator field) ----------------------------- *)

type 'r vcase = 'r case

let case name (b : ('a, 'a) builder) ~(inj : 'a -> 'r) ~(proj : 'r -> 'a option) : 'r vcase =
  Case { name = name; body = record_of_builder b; inject = inj; project = proj }

let variant ~tag cases : 'r t = of_shape (TVariant { tag; cases })

(* ---- objN: DIRECT record construction (the staged decoder the deriver targets) ------------------

   The applicative builder ([record … |> field … |> seal]) applies an N-ary constructor ONE field at a
   time, which compiles to O(N) growing caml_curry closures — measured at ~82% of decode allocation.
   These objN read all N fields through the reader, then apply [make] to all of them at ONCE (a single
   caml_applyN, no intermediate closures). Errors COLLECT in field order, byte-identical to the
   applicative builder. They build the [record_shape] directly (their own [decode_src]); decode reads
   fields via the SAME field_reader, so both the tree path AND decode_bytes get the win. [@@deriving
   collection/model] emits these for arity ≤ 8; a wider record falls back to the applicative builder
   (still correct, just not staged). *)

let errs : type a. (a, error list) result -> error list = function Ok _ -> [] | Error e -> e

(* a member with the standard wire-omission predicate (mirrors {!field}) *)
let bound (f : 'a field) (get : 'r -> 'a) : 'r bound_field =
  Bound_field { name = f.key; shape = f.item; get; required = f.needed; omit = omit_of f }

let direct (decode_src : field_reader -> ('r, error list) result) (members : 'r bound_field list) : 'r t =
  of_shape (TObj { decode_src; members; invariants = [] })

let obj1 fa ~make ~split =
  direct (fun fr -> match fr.read_field fa with Ok a -> Ok (make a) | r -> Error (errs r)) [ bound fa split ]

let obj2 fa fb ~make ~split =
  direct
    (fun fr -> match (fr.read_field fa, fr.read_field fb) with Ok a, Ok b -> Ok (make a b) | r1, r2 -> Error (errs r1 @ errs r2))
    [ bound fa (fun r -> fst (split r)); bound fb (fun r -> snd (split r)) ]

let obj3 fa fb fc ~make ~split =
  direct
    (fun fr -> match (fr.read_field fa, fr.read_field fb, fr.read_field fc) with Ok a, Ok b, Ok c -> Ok (make a b c) | r1, r2, r3 -> Error (errs r1 @ errs r2 @ errs r3))
    [ bound fa (fun r -> let a, _, _ = split r in a); bound fb (fun r -> let _, b, _ = split r in b); bound fc (fun r -> let _, _, c = split r in c) ]

let obj4 fa fb fc fd ~make ~split =
  direct
    (fun fr ->
      match (fr.read_field fa, fr.read_field fb, fr.read_field fc, fr.read_field fd) with
      | Ok a, Ok b, Ok c, Ok d -> Ok (make a b c d)
      | r1, r2, r3, r4 -> Error (errs r1 @ errs r2 @ errs r3 @ errs r4))
    [ bound fa (fun r -> let a, _, _, _ = split r in a); bound fb (fun r -> let _, b, _, _ = split r in b); bound fc (fun r -> let _, _, c, _ = split r in c); bound fd (fun r -> let _, _, _, d = split r in d) ]

let obj5 fa fb fc fd fe ~make ~split =
  direct
    (fun fr ->
      match (fr.read_field fa, fr.read_field fb, fr.read_field fc, fr.read_field fd, fr.read_field fe) with
      | Ok a, Ok b, Ok c, Ok d, Ok e -> Ok (make a b c d e)
      | r1, r2, r3, r4, r5 -> Error (errs r1 @ errs r2 @ errs r3 @ errs r4 @ errs r5))
    [ bound fa (fun r -> let a, _, _, _, _ = split r in a); bound fb (fun r -> let _, b, _, _, _ = split r in b); bound fc (fun r -> let _, _, c, _, _ = split r in c); bound fd (fun r -> let _, _, _, d, _ = split r in d); bound fe (fun r -> let _, _, _, _, e = split r in e) ]

let obj6 fa fb fc fd fe ff ~make ~split =
  direct
    (fun fr ->
      match (fr.read_field fa, fr.read_field fb, fr.read_field fc, fr.read_field fd, fr.read_field fe, fr.read_field ff) with
      | Ok a, Ok b, Ok c, Ok d, Ok e, Ok f -> Ok (make a b c d e f)
      | r1, r2, r3, r4, r5, r6 -> Error (errs r1 @ errs r2 @ errs r3 @ errs r4 @ errs r5 @ errs r6))
    [ bound fa (fun r -> let a, _, _, _, _, _ = split r in a); bound fb (fun r -> let _, b, _, _, _, _ = split r in b); bound fc (fun r -> let _, _, c, _, _, _ = split r in c); bound fd (fun r -> let _, _, _, d, _, _ = split r in d); bound fe (fun r -> let _, _, _, _, e, _ = split r in e); bound ff (fun r -> let _, _, _, _, _, f = split r in f) ]

let obj7 fa fb fc fd fe ff fg ~make ~split =
  direct
    (fun fr ->
      match (fr.read_field fa, fr.read_field fb, fr.read_field fc, fr.read_field fd, fr.read_field fe, fr.read_field ff, fr.read_field fg) with
      | Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g -> Ok (make a b c d e f g)
      | r1, r2, r3, r4, r5, r6, r7 -> Error (errs r1 @ errs r2 @ errs r3 @ errs r4 @ errs r5 @ errs r6 @ errs r7))
    [ bound fa (fun r -> let a, _, _, _, _, _, _ = split r in a); bound fb (fun r -> let _, b, _, _, _, _, _ = split r in b); bound fc (fun r -> let _, _, c, _, _, _, _ = split r in c); bound fd (fun r -> let _, _, _, d, _, _, _ = split r in d); bound fe (fun r -> let _, _, _, _, e, _, _ = split r in e); bound ff (fun r -> let _, _, _, _, _, f, _ = split r in f); bound fg (fun r -> let _, _, _, _, _, _, g = split r in g) ]

let obj8 fa fb fc fd fe ff fg fh ~make ~split =
  direct
    (fun fr ->
      match (fr.read_field fa, fr.read_field fb, fr.read_field fc, fr.read_field fd, fr.read_field fe, fr.read_field ff, fr.read_field fg, fr.read_field fh) with
      | Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g, Ok h -> Ok (make a b c d e f g h)
      | r1, r2, r3, r4, r5, r6, r7, r8 -> Error (errs r1 @ errs r2 @ errs r3 @ errs r4 @ errs r5 @ errs r6 @ errs r7 @ errs r8))
    [ bound fa (fun r -> let a, _, _, _, _, _, _, _ = split r in a); bound fb (fun r -> let _, b, _, _, _, _, _, _ = split r in b); bound fc (fun r -> let _, _, c, _, _, _, _, _ = split r in c); bound fd (fun r -> let _, _, _, d, _, _, _, _ = split r in d); bound fe (fun r -> let _, _, _, _, e, _, _, _ = split r in e); bound ff (fun r -> let _, _, _, _, _, f, _, _ = split r in f); bound fg (fun r -> let _, _, _, _, _, _, g, _ = split r in g); bound fh (fun r -> let _, _, _, _, _, _, _, h = split r in h) ]

(* ---- introspection: the neutral reflection renderers consume --------------------------- *)

