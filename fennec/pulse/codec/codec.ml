(* The shape language. ONE GADT type representation ([ty]) inside; plain combinators outside.
   Everything else derives from [ty]: the codec (decode collects path-tagged errors; encode is
   total), encode-side validation ([validate] / [encode_checked] — an invalid value cannot pass a
   write boundary), normalizers (run BEFORE checks, on both directions), derived pretty-printing
   ([pp]/[show], nested), and the neutral [view] reflection downstream renderers consume
   ($jsonSchema, OpenAPI, admin) without this module knowing them.

   Refinements carry a [hint] — the machine-readable half a renderer can translate (min_len →
   minLength…); an arbitrary [check] carries [H_none] and is honestly app-side-only. Floats reject
   nan/inf by default (a Bson Float can carry them; silently storing them is how data rots).
   Options: absent OR null decode to [None]; [None] encodes by OMITTING the key (Mongo-idiomatic).

   Back-compat: the original combinator surface (string/int/…/req/opt/obj1-4/a0-a3, and the public
   [enc]/[dec] record fields) is preserved verbatim — every existing call site keeps compiling. *)

(* ---- errors ------------------------------------------------------------------ *)

type error = { path : string list; msg : string }

let error_to_string e =
  match e.path with [] -> e.msg | p -> String.concat "." p ^ ": " ^ e.msg

let errors_to_string errs = String.concat "; " (List.map error_to_string errs)
let at name e = { e with path = name :: e.path }
let fail msg = Error [ { path = []; msg } ]

(* ---- the type representation -------------------------------------------------- *)

(* the renderable half of a refinement — what $jsonSchema/OpenAPI can translate *)
type hint =
  | H_none
  | H_min_len of int
  | H_max_len of int
  | H_pattern of string
  | H_enum of string list
  | H_min of float
  | H_max of float
  | H_multiple_of of float
  | H_min_items of int
  | H_max_items of int
  | H_unique_items

type _ ty =
  | TString : string ty
  | TInt : int ty
  | TFloat : { allow_nonfinite : bool } -> float ty
  | TBool : bool ty
  | TDate : int64 ty (* Bson.Date, ms since epoch *)
  | TId : string ty (* "_id" values: String or ObjectId, surfaced as string *)
  | TBson : Bson.t ty (* the dynamic escape hatch *)
  | TUnit : unit ty
  | TList : 'a ty -> 'a list ty
  | TOption : 'a ty -> 'a option ty
  | TMap : 'a ty -> (string * 'a) list ty (* dynamic-key subdocuments *)
  | TCheck : ('a -> bool) * string * hint * 'a ty -> 'a ty
  | TNorm : ('a -> 'a) * 'a ty -> 'a ty
  | TConv : ('b -> 'a) * ('a -> ('b, string) result) * 'a ty -> 'b ty
  | TObj : 'r obj -> 'r ty
  | TVariant : { tag : string; cases : 'r case list } -> 'r ty

and 'r obj = {
  o_dec : (string * Bson.t) list -> ('r, error list) result;
  o_enc : 'r -> (string * Bson.t) list;
  o_fields : 'r packed_field list;
  o_checks : (('r -> bool) * string) list; (* record-level (cross-field) checks *)
}

and 'r packed_field =
  | PF : { f_name : string; f_ty : 'a ty; f_get : 'r -> 'a; f_required : bool } -> 'r packed_field

and 'r case =
  | Case : { c_name : string; c_obj : 'a obj; c_inj : 'a -> 'r; c_proj : 'r -> 'a option } -> 'r case

(* ---- decode / encode / checks, derived from ty -------------------------------- *)

let looks_like_oid s =
  String.length s = 24
  && (let ok = ref true in
      String.iter (fun c -> match c with '0' .. '9' | 'a' .. 'f' -> () | _ -> ok := false) s;
      !ok)

let type_name (b : Bson.t) =
  match b with
  | Bson.String _ -> "string"
  | Bson.Int _ -> "int"
  | Bson.Float _ -> "float"
  | Bson.Bool _ -> "bool"
  | Bson.Document _ -> "document"
  | Bson.Array _ -> "array"
  | Bson.Null -> "null"
  | Bson.Date _ -> "date"
  | Bson.Object_id _ -> "objectid"
  | _ -> "value"

let expected what got = fail (Printf.sprintf "expected %s, got %s" what (type_name got))

let rec dec_ty : type a. a ty -> Bson.t -> (a, error list) result =
 fun ty b ->
  match ty with
  | TString -> ( match b with Bson.String s -> Ok s | v -> expected "string" v)
  | TInt -> (
      match b with
      | Bson.Int i -> Ok i
      | Bson.Float f when Float.is_integer f -> Ok (int_of_float f)
      | v -> expected "int" v)
  | TFloat { allow_nonfinite } -> (
      match b with
      | Bson.Float f ->
          if (not allow_nonfinite) && not (Float.is_finite f) then fail "non-finite float" else Ok f
      | Bson.Int i -> Ok (float_of_int i)
      | v -> expected "float" v)
  | TBool -> ( match b with Bson.Bool x -> Ok x | v -> expected "bool" v)
  | TDate -> (
      match b with
      | Bson.Date ms -> Ok ms
      | Bson.Int i -> Ok (Int64.of_int i)
      | Bson.Float f when Float.is_integer f -> Ok (Int64.of_float f)
      | v -> expected "date" v)
  | TId -> (
      match b with
      | Bson.String s -> Ok s
      | Bson.Object_id s -> Ok s
      | v -> expected "id (string or objectid)" v)
  | TBson -> Ok b
  | TUnit -> ( match b with Bson.Null -> Ok () | v -> expected "null" v)
  | TList el -> (
      match b with
      | Bson.Array xs ->
          let oks, errs =
            List.fold_left
              (fun (oks, errs) (i, x) ->
                match dec_ty el x with
                | Ok v -> (v :: oks, errs)
                | Error es -> (oks, List.rev_append (List.map (at (string_of_int i)) es) errs))
              ([], [])
              (List.mapi (fun i x -> (i, x)) xs)
          in
          if errs = [] then Ok (List.rev oks) else Error (List.rev errs)
      | v -> expected "array" v)
  | TOption el -> (
      match b with Bson.Null -> Ok None | v -> ( match dec_ty el v with Ok x -> Ok (Some x) | Error e -> Error e))
  | TMap el -> (
      match b with
      | Bson.Document kvs ->
          let oks, errs =
            List.fold_left
              (fun (oks, errs) (k, v) ->
                match dec_ty el v with
                | Ok x -> ((k, x) :: oks, errs)
                | Error es -> (oks, List.rev_append (List.map (at k) es) errs))
              ([], []) kvs
          in
          if errs = [] then Ok (List.rev oks) else Error (List.rev errs)
      | v -> expected "document" v)
  | TCheck (_, _, _, inner) -> dec_ty inner b (* RAW phase: shape only; refinements run in check_ty *)
  | TNorm (f, inner) -> ( match dec_ty inner b with Ok v -> Ok (f v) | Error e -> Error e)
  | TConv (_inj, proj, inner) -> (
      match dec_ty inner b with
      | Ok v -> ( match proj v with Ok x -> Ok x | Error m -> fail m)
      | Error e -> Error e)
  | TObj o -> (
      (* RAW phase: build the record (missing/type errors); record-level checks run in check_ty,
         so a field's refinement failure can't mask a cross-field violation — all collect *)
      match b with Bson.Document kvs -> o.o_dec kvs | v -> expected "document" v)
  | TVariant { tag; cases } -> (
      match b with
      | Bson.Document kvs -> (
          match List.assoc_opt tag kvs with
          | Some (Bson.String k) -> (
              match List.find_opt (fun (Case c) -> c.c_name = k) cases with
              | Some (Case c) -> (
                  match c.c_obj.o_dec kvs with Ok a -> Ok (c.c_inj a) | Error e -> Error (List.map (at k) e))
              | None -> fail (Printf.sprintf "unknown %s %S" tag k))
          | _ -> fail (Printf.sprintf "missing tag field %s" tag))
      | v -> expected "document" v)

(* encode is TOTAL (a typed value always serializes); refinement checks belong to [check_ty] *)
let rec enc_ty : type a. a ty -> a -> Bson.t =
 fun ty v ->
  match ty with
  | TString -> Bson.String v
  | TInt -> Bson.Int v
  | TFloat _ -> Bson.Float v
  | TBool -> Bson.Bool v
  | TDate -> Bson.Date v
  | TId -> if looks_like_oid v then Bson.Object_id v else Bson.String v
  | TBson -> v
  | TUnit -> Bson.Null
  | TList el -> Bson.Array (List.map (enc_ty el) v)
  | TOption el -> ( match v with Some x -> enc_ty el x | None -> Bson.Null)
  | TMap el -> Bson.Document (List.map (fun (k, x) -> (k, enc_ty el x)) v)
  | TCheck (_, _, _, inner) -> enc_ty inner v
  | TNorm (f, inner) -> enc_ty inner (f v)
  | TConv (inj, _, inner) -> enc_ty inner (inj v)
  | TObj o -> Bson.Document (o.o_enc v)
  | TVariant { tag; cases } -> (
      let rec go = function
        | [] -> invalid_arg "Codec: variant value matches no declared case"
        | Case c :: rest -> (
            match c.c_proj v with
            | Some a -> Bson.Document ((tag, Bson.String c.c_name) :: c.c_obj.o_enc a)
            | None -> go rest)
      in
      go cases)

(* the inner normalizer chain applied to a value (normalizers must be idempotent) *)
let rec norm_ty : type a. a ty -> a -> a =
 fun ty v ->
  match ty with
  | TNorm (f, inner) -> f (norm_ty inner v)
  | TCheck (_, _, _, inner) -> norm_ty inner v
  | _ -> v

(* run every check against an in-memory value (the encode-side gate: writes validate). ONE engine
   for both directions: [decode] = raw shape decode, then this. *)
let rec check_ty : type a. a ty -> a -> error list =
 fun ty v ->
  match ty with
  | TString | TInt | TBool | TDate | TId | TBson | TUnit -> []
  | TFloat { allow_nonfinite } ->
      if (not allow_nonfinite) && not (Float.is_finite v) then [ { path = []; msg = "non-finite float" } ] else []
  | TList el -> List.concat (List.mapi (fun i x -> List.map (at (string_of_int i)) (check_ty el x)) v)
  | TOption el -> ( match v with Some x -> check_ty el x | None -> [])
  | TMap el -> List.concat (List.map (fun (k, x) -> List.map (at k) (check_ty el x)) v)
  | TCheck (p, msg, _, inner) ->
      let inner_errs = check_ty inner v in
      (* the predicate sees the NORMALIZED value — decode/validate parity *)
      if p (norm_ty inner v) then inner_errs else { path = []; msg } :: inner_errs
  | TNorm (f, inner) -> check_ty inner (f v)
  | TConv (inj, _, inner) -> check_ty inner (inj v)
  | TObj o ->
      let field_errs =
        List.concat
          (List.map (fun (PF f) -> List.map (at f.f_name) (check_ty f.f_ty (f.f_get v))) o.o_fields)
      in
      let rec_errs = List.filter_map (fun (p, msg) -> if p v then None else Some { path = []; msg }) o.o_checks in
      field_errs @ rec_errs
  | TVariant { cases; _ } -> (
      let rec go = function
        | [] -> []
        | Case c :: rest -> (
            match c.c_proj v with
            | Some a ->
                List.concat (List.map (fun (PF f) -> List.map (at f.f_name) (check_ty f.f_ty (f.f_get a))) c.c_obj.o_fields)
            | None -> go rest)
      in
      go cases)

(* ---- derived pretty-printing --------------------------------------------------- *)

let rec pp_ty : type a. a ty -> Format.formatter -> a -> unit =
 fun ty fmt v ->
  match ty with
  | TString -> Format.fprintf fmt "%S" v
  | TInt -> Format.fprintf fmt "%d" v
  | TFloat _ -> Format.fprintf fmt "%g" v
  | TBool -> Format.fprintf fmt "%b" v
  | TDate -> Format.fprintf fmt "date(%Ld)" v
  | TId -> Format.fprintf fmt "#%s" v
  | TBson -> Format.fprintf fmt "%s" (Bson.to_string v)
  | TUnit -> Format.fprintf fmt "()"
  | TList el ->
      Format.fprintf fmt "@[<hv 1>[%a]@]"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ") (pp_ty el))
        v
  | TOption el -> ( match v with Some x -> pp_ty el fmt x | None -> Format.fprintf fmt "-")
  | TMap el ->
      Format.fprintf fmt "@[<hv 2>{%a}@]"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
           (fun f (k, x) -> Format.fprintf f "%s: %a" k (pp_ty el) x))
        v
  | TCheck (_, _, _, inner) -> pp_ty inner fmt v
  | TNorm (_, inner) -> pp_ty inner fmt v
  | TConv (inj, _, inner) -> pp_ty inner fmt (inj v)
  | TObj o ->
      Format.fprintf fmt "@[<hv 2>{ %a }@]"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
           (fun f (PF p) -> Format.fprintf f "%s = %a" p.f_name (pp_ty p.f_ty) (p.f_get v)))
        o.o_fields
  | TVariant { cases; _ } -> (
      let rec go = function
        | [] -> Format.fprintf fmt "<?>"
        | Case c :: rest -> (
            match c.c_proj v with
            | Some a ->
                Format.fprintf fmt "@[<hv 2>%s { %a }@]" c.c_name
                  (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
                     (fun f (PF p) -> Format.fprintf f "%s = %a" p.f_name (pp_ty p.f_ty) (p.f_get a)))
                  c.c_obj.o_fields
            | None -> go rest)
      in
      go cases)

(* ---- the public codec value ----------------------------------------------------- *)

(* [enc]/[dec] kept as plain record fields for back-compat ([dec]'s error is the rendered string);
   [ty] is the representation everything else derives from. *)
type 'a t = { ty : 'a ty; enc : 'a -> Bson.t; dec : Bson.t -> ('a, string) result }

(* decode = RAW shape decode, then the check phase — so refinement violations COLLECT across
   stacked checks, sibling fields, and record-level rules instead of short-circuiting *)
let dec_full ty b =
  match dec_ty ty b with
  | Error es -> Error es
  | Ok v -> ( match check_ty ty v with [] -> Ok v | es -> Error es)

let of_ty ty =
  { ty;
    enc = enc_ty ty;
    dec = (fun b -> match dec_full ty b with Ok v -> Ok v | Error es -> Error (errors_to_string es)) }

let decode c b = dec_full c.ty b
let validate c v = match check_ty c.ty v with [] -> Ok () | es -> Error es
let encode_checked c v = match check_ty c.ty v with [] -> Ok (c.enc v) | es -> Error es
let pp c fmt v = pp_ty c.ty fmt v
let show c v = Format.asprintf "%a" (pp c) v

(* ---- primitives ------------------------------------------------------------------ *)

let string = of_ty TString
let int = of_ty TInt
let float = of_ty (TFloat { allow_nonfinite = false })
let float_nonfinite = of_ty (TFloat { allow_nonfinite = true })
let bool = of_ty TBool
let date = of_ty TDate
let id = of_ty TId
let bson = of_ty TBson
let unit = of_ty TUnit
let list c = of_ty (TList c.ty)
let option c = of_ty (TOption c.ty)
let str_map c = of_ty (TMap c.ty)
let conv proj inj c = of_ty (TConv (inj, proj, c.ty))
let make ~enc ~dec = conv dec enc bson

(* ---- refinements + normalizers ----------------------------------------------------- *)

let check ?(msg = "invalid value") p c = of_ty (TCheck (p, msg, H_none, c.ty))
let refine p msg hint c = of_ty (TCheck (p, msg, hint, c.ty))

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
let trim c = of_ty (TNorm (String.trim, c.ty))
let lowercase c = of_ty (TNorm (String.lowercase_ascii, c.ty))

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

type 'a field = { fld_name : string; fld_ty : 'a ty; fld_required : bool; fld_default : 'a option }

let req name c = { fld_name = name; fld_ty = c.ty; fld_required = true; fld_default = None }
let opt name c = { fld_name = name; fld_ty = (option c).ty; fld_required = false; fld_default = Some None }
let opt_list name c = { fld_name = name; fld_ty = (list c).ty; fld_required = false; fld_default = Some [] }
let dft name c v = { fld_name = name; fld_ty = c.ty; fld_required = false; fld_default = Some v }

let dec_field (f : 'a field) kvs : ('a, error list) result =
  match List.assoc_opt f.fld_name kvs with
  | Some v -> ( match dec_ty f.fld_ty v with Ok x -> Ok x | Error es -> Error (List.map (at f.fld_name) es))
  | None -> (
      match f.fld_default with
      | Some d -> Ok d
      | None -> Error [ { path = []; msg = "missing field " ^ f.fld_name } ])

(* [None]/default-empty encodes by omitting the key — Mongo-idiomatic absence *)
let enc_field (f : 'a field) (v : 'a) : (string * Bson.t) option =
  match enc_ty f.fld_ty v with
  | Bson.Null when not f.fld_required -> None
  | Bson.Array [] when f.fld_default = Some v -> None
  | b -> Some (f.fld_name, b)

type ('r, 'k) builder = {
  b_dec : (string * Bson.t) list -> ('k, error list) result;
  b_enc : 'r -> (string * Bson.t) list;
  b_fields : 'r packed_field list; (* reverse order *)
  b_checks : (('r -> bool) * string) list;
}

let record (make : 'k) : ('r, 'k) builder =
  { b_dec = (fun _ -> Ok make); b_enc = (fun _ -> []); b_fields = []; b_checks = [] }

let field (f : 'a field) (get : 'r -> 'a) (b : ('r, 'a -> 'k) builder) : ('r, 'k) builder =
  {
    b_dec =
      (fun kvs ->
        match (b.b_dec kvs, dec_field f kvs) with
        | Ok k, Ok a -> Ok (k a)
        | Error e1, Error e2 -> Error (e1 @ e2)
        | Error e, Ok _ | Ok _, Error e -> Error e);
    b_enc = (fun r -> b.b_enc r @ (match enc_field f (get r) with Some kv -> [ kv ] | None -> []));
    b_fields = PF { f_name = f.fld_name; f_ty = f.fld_ty; f_get = get; f_required = f.fld_required } :: b.b_fields;
    b_checks = b.b_checks;
  }

let checking p msg b = { b with b_checks = (p, msg) :: b.b_checks }

let obj_of_builder (b : ('r, 'r) builder) : 'r obj =
  { o_dec = b.b_dec; o_enc = b.b_enc; o_fields = List.rev b.b_fields; o_checks = List.rev b.b_checks }

let seal (b : ('r, 'r) builder) : 'r t = of_ty (TObj (obj_of_builder b))
let doc_id = req "_id" id

(* navigate into an embedded record for a SELECTOR/MODIFIER path: the resulting field has the dotted
   wire name ("author.name") and the LEAF's shape/checks — so [Filter.eq (dot Fields.author
   Author.Fields.name) v] checks both names AND the value type, value-level (no ppx). The outer
   field's own codec is irrelevant here (we only borrow its wire name); the leaf decides the type.
   Required at the leaf (a dotted equality on an absent path simply matches nothing). *)
let dot (outer : _ field) (inner : 'a field) : 'a field =
  { fld_name = outer.fld_name ^ "." ^ inner.fld_name;
    fld_ty = inner.fld_ty;
    fld_required = inner.fld_required;
    fld_default = inner.fld_default }

(* accessors for the collection vocabulary (Filter/M/Index build on field handles) *)
let field_name f = f.fld_name
let field_enc f v = enc_ty f.fld_ty v

(* encode ONE element of a list field — total through any check/norm/conv wrapping: encode the
   singleton list and unwrap (the list encoder is structural) *)
let field_elem_enc (f : 'a list field) (v : 'a) : Bson.t =
  match enc_ty f.fld_ty [ v ] with Bson.Array [ x ] -> x | _ -> invalid_arg "Codec.field_elem_enc"

let field_validate f v = match check_ty f.fld_ty v with [] -> Ok () | es -> Error (List.map (at f.fld_name) es)

(* decode ONE field out of a document (raw decode + the field's checks) — the projection primitive:
   reading a projected slice without ever constructing the full record *)
let field_get (f : 'a field) (doc : Bson.t) : ('a, error list) result =
  match doc with
  | Bson.Document kvs -> (
      match dec_field f kvs with
      | Error es -> Error es
      | Ok v -> ( match check_ty f.fld_ty v with [] -> Ok v | es -> Error (List.map (at f.fld_name) es)))
  | _ -> Error [ { path = [ f.fld_name ]; msg = "expected document" } ]

(* ---- variants (tagged unions over a discriminator field) ----------------------------- *)

type 'r vcase = 'r case

let case name (b : ('a, 'a) builder) ~(inj : 'a -> 'r) ~(proj : 'r -> 'a option) : 'r vcase =
  Case { c_name = name; c_obj = obj_of_builder b; c_inj = inj; c_proj = proj }

let variant ~tag cases : 'r t = of_ty (TVariant { tag; cases })

(* ---- back-compat: the tuple-style objN over the builder ------------------------------- *)

let obj1 fa ~make ~split = seal (record make |> field fa split)

let obj2 fa fb ~make ~split =
  seal (record make |> field fa (fun r -> fst (split r)) |> field fb (fun r -> snd (split r)))

let obj3 fa fb fc ~make ~split =
  seal
    (record make
    |> field fa (fun r -> let a, _, _ = split r in a)
    |> field fb (fun r -> let _, b, _ = split r in b)
    |> field fc (fun r -> let _, _, c = split r in c))

let obj4 fa fb fc fd ~make ~split =
  seal
    (record make
    |> field fa (fun r -> let a, _, _, _ = split r in a)
    |> field fb (fun r -> let _, b, _, _ = split r in b)
    |> field fc (fun r -> let _, _, c, _ = split r in c)
    |> field fd (fun r -> let _, _, _, d = split r in d))

(* ---- introspection: the neutral reflection renderers consume --------------------------- *)

type view =
  | V_string
  | V_int
  | V_float
  | V_bool
  | V_date
  | V_id
  | V_bson
  | V_unit
  | V_list of view
  | V_option of view
  | V_map of view
  | V_check of hint * view
  | V_obj of (string * bool (* required *) * view) list
  | V_variant of string * (string * (string * bool * view) list) list

let rec view_of_ty : type a. a ty -> view = function
  | TString -> V_string
  | TInt -> V_int
  | TFloat _ -> V_float
  | TBool -> V_bool
  | TDate -> V_date
  | TId -> V_id
  | TBson -> V_bson
  | TUnit -> V_unit
  | TList el -> V_list (view_of_ty el)
  | TOption el -> V_option (view_of_ty el)
  | TMap el -> V_map (view_of_ty el)
  | TCheck (_, _, h, inner) -> V_check (h, view_of_ty inner)
  | TNorm (_, inner) -> view_of_ty inner
  | TConv (_, _, inner) -> view_of_ty inner
  | TObj o -> V_obj (List.map (fun (PF p) -> (p.f_name, p.f_required, view_of_ty p.f_ty)) o.o_fields)
  | TVariant { tag; cases } ->
      V_variant
        (tag,
         List.map
           (fun (Case c) -> (c.c_name, List.map (fun (PF p) -> (p.f_name, p.f_required, view_of_ty p.f_ty)) c.c_obj.o_fields))
           cases)

let view c = view_of_ty c.ty

(* ---- positional parameter lists (DDP method params) — unchanged surface ----------------- *)

type 'a args = { enc_args : 'a -> Bson.t list; dec_args : Bson.t list -> ('a, string) result }

let a0 = { enc_args = (fun () -> []); dec_args = (function [] -> Ok () | _ -> Error "expected no arguments") }

let a1 c =
  { enc_args = (fun a -> [ c.enc a ]);
    dec_args = (function [ x ] -> c.dec x | l -> Error (Printf.sprintf "expected 1 argument, got %d" (List.length l))) }

let a2 c1 c2 =
  { enc_args = (fun (a, b) -> [ c1.enc a; c2.enc b ]);
    dec_args =
      (function
      | [ x; y ] -> ( match (c1.dec x, c2.dec y) with Ok a, Ok b -> Ok (a, b) | Error e, _ | _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 2 arguments, got %d" (List.length l))) }

let a3 c1 c2 c3 =
  { enc_args = (fun (a, b, c) -> [ c1.enc a; c2.enc b; c3.enc c ]);
    dec_args =
      (function
      | [ x; y; z ] -> (
          match (c1.dec x, c2.dec y, c3.dec z) with
          | Ok a, Ok b, Ok c -> Ok (a, b, c)
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
      | l -> Error (Printf.sprintf "expected 3 arguments, got %d" (List.length l))) }
