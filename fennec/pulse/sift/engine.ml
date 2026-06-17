(* The engine: the interpreters that walk a {!Shape.t}. Low-level / perf-sensitive — this is where
   staging + lowlevel hacks land later. *)

open Shape

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

let expected what got =
  fail ~code:"type"
    ~params:[ ("expected", what); ("got", type_name got) ]
    (Printf.sprintf "expected %s, got %s" what (type_name got))

(* coerce a submitted String to the inner leaf's natural Bson, so {!from_string} can re-decode it —
   numbers / bools / dates that arrived stringly (forms, query strings, lenient JSON). Non-leaf or
   unparseable values pass the String through, where the inner decoder rejects it with a typed error. *)
let rec coerce_string : type a. a shape -> string -> Bson.t =
 fun shape s ->
  match shape with
  | TInt -> ( match int_of_string_opt (String.trim s) with Some i -> Bson.Int i | None -> Bson.String s)
  | TFloat _ -> ( match float_of_string_opt (String.trim s) with Some f -> Bson.Float f | None -> Bson.String s)
  | TBool -> Bson.Bool (match String.lowercase_ascii (String.trim s) with "true" | "1" | "on" | "yes" -> true | _ -> false)
  | TDate -> ( match Int64.of_string_opt (String.trim s) with Some ms -> Bson.Date ms | None -> Bson.String s)
  | TCheck (_, _, _, inner) | TNorm (_, inner) | TCoerce inner -> coerce_string inner s
  | _ -> Bson.String s

let rec read : type a. a shape -> Bson.t -> (a, error list) result =
 fun shape b ->
  match shape with
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
                match read el x with
                | Ok v -> (v :: oks, errs)
                | Error es -> (oks, List.rev_append (List.map (at (string_of_int i)) es) errs))
              ([], [])
              (List.mapi (fun i x -> (i, x)) xs)
          in
          if errs = [] then Ok (List.rev oks) else Error (List.rev errs)
      | v -> expected "array" v)
  | TOption el -> (
      match b with Bson.Null -> Ok None | v -> ( match read el v with Ok x -> Ok (Some x) | Error e -> Error e))
  | TMap el -> (
      match b with
      | Bson.Document kvs ->
          let oks, errs =
            List.fold_left
              (fun (oks, errs) (k, v) ->
                match read el v with
                | Ok x -> ((k, x) :: oks, errs)
                | Error es -> (oks, List.rev_append (List.map (at k) es) errs))
              ([], []) kvs
          in
          if errs = [] then Ok (List.rev oks) else Error (List.rev errs)
      | v -> expected "document" v)
  | TCheck (_, _, _, inner) -> read inner b (* RAW phase: shape only; refinements run in run_checks *)
  | TNorm (f, inner) -> ( match read inner b with Ok v -> Ok (f v) | Error e -> Error e)
  | TConv (_inj, proj, inner) -> (
      match read inner b with
      | Ok v -> ( match proj v with Ok x -> Ok x | Error m -> fail m)
      | Error e -> Error e)
  | TObj o -> (
      (* RAW phase: build the record (missing/type errors); record-level checks run in run_checks,
         so a field's refinement failure can't mask a cross-field violation — all collect *)
      match b with Bson.Document kvs -> o.decode_doc kvs | v -> expected "document" v)
  | TVariant { tag; cases } -> (
      match b with
      | Bson.Document kvs -> (
          match List.assoc_opt tag kvs with
          | Some (Bson.String k) -> (
              match List.find_opt (fun (Case c) -> c.name = k) cases with
              | Some (Case c) -> (
                  match c.body.decode_doc kvs with Ok a -> Ok (c.inject a) | Error e -> Error (List.map (at k) e))
              | None -> fail (Printf.sprintf "unknown %s %S" tag k))
          | _ -> fail (Printf.sprintf "missing tag field %s" tag))
      | v -> expected "document" v)
  | TLazy l -> read (Lazy.force l) b
  | TCoerce inner -> ( match b with Bson.String s -> read inner (coerce_string inner s) | other -> read inner other)

(* encode is TOTAL (a typed value always serializes); refinement checks belong to [run_checks] *)
let rec write : type a. a shape -> a -> Bson.t =
 fun shape v ->
  match shape with
  | TString -> Bson.String v
  | TInt -> Bson.Int v
  | TFloat _ -> Bson.Float v
  | TBool -> Bson.Bool v
  | TDate -> Bson.Date v
  | TId -> if looks_like_oid v then Bson.Object_id v else Bson.String v
  | TBson -> v
  | TUnit -> Bson.Null
  | TList el -> Bson.Array (List.map (write el) v)
  | TOption el -> ( match v with Some x -> write el x | None -> Bson.Null)
  | TMap el -> Bson.Document (List.map (fun (k, x) -> (k, write el x)) v)
  | TCheck (_, _, _, inner) -> write inner v
  | TNorm (f, inner) -> write inner (f v)
  | TConv (inj, _, inner) -> write inner (inj v)
  | TObj o -> Bson.Document (o.encode_doc v)
  | TVariant { tag; cases } -> (
      let rec go = function
        | [] -> invalid_arg "Sift: variant value matches no declared case"
        | Case c :: rest -> (
            match c.project v with
            | Some a -> Bson.Document ((tag, Bson.String c.name) :: c.body.encode_doc a)
            | None -> go rest)
      in
      go cases)
  | TLazy l -> write (Lazy.force l) v
  | TCoerce inner -> write inner v

(* the inner normalizer chain applied to a value (normalizers must be idempotent) *)
let rec normalize : type a. a shape -> a -> a =
 fun shape v ->
  match shape with
  | TNorm (f, inner) -> f (normalize inner v)
  | TCheck (_, _, _, inner) -> normalize inner v
  | TCoerce inner -> normalize inner v
  | TLazy l -> normalize (Lazy.force l) v
  | _ -> v

(* run every check against an in-memory value (the encode-side gate: writes validate). ONE engine
   for both directions: [decode] = raw shape decode, then this. *)
let rec run_checks : type a. a shape -> a -> error list =
 fun shape v ->
  match shape with
  | TString | TInt | TBool | TDate | TId | TBson | TUnit -> []
  | TFloat { allow_nonfinite } ->
      if (not allow_nonfinite) && not (Float.is_finite v) then [ mkerr ~code:"finite" "non-finite float" ] else []
  | TList el -> List.concat (List.mapi (fun i x -> List.map (at (string_of_int i)) (run_checks el x)) v)
  | TOption el -> ( match v with Some x -> run_checks el x | None -> [])
  | TMap el -> List.concat (List.map (fun (k, x) -> List.map (at k) (run_checks el x)) v)
  | TCheck (p, msg, hint, inner) ->
      let inner_errs = run_checks inner v in
      (* the predicate sees the NORMALIZED value — decode/validate parity *)
      if p (normalize inner v) then inner_errs
      else mkerr ~code:(code_of_hint hint) ~params:(params_of_hint hint) msg :: inner_errs
  | TNorm (f, inner) -> run_checks inner (f v)
  | TConv (inj, _, inner) -> run_checks inner (inj v)
  | TObj o ->
      let field_errs =
        List.concat
          (List.map (fun (Bound_field f) -> List.map (at f.name) (run_checks f.shape (f.get v))) o.members)
      in
      let rec_errs = List.filter_map (fun (p, msg) -> if p v then None else Some (mkerr ~code:"check" msg)) o.invariants in
      field_errs @ rec_errs
  | TVariant { cases; _ } -> (
      let rec go = function
        | [] -> []
        | Case c :: rest -> (
            match c.project v with
            | Some a ->
                List.concat (List.map (fun (Bound_field f) -> List.map (at f.name) (run_checks f.shape (f.get a))) c.body.members)
            | None -> go rest)
      in
      go cases)
  | TLazy l -> run_checks (Lazy.force l) v
  | TCoerce inner -> run_checks inner v

(* does this SHAPE carry any check that {!run_checks} could fire — a refinement ([TCheck]), a
   finite-float guard, or a record/variant invariant — anywhere in its tree? A property of the shape
   (O(shape), NOT O(value)) and allocation-free, so decode can SKIP the whole [run_checks] value-walk
   when there is nothing to check (the common read path: data validated on write, trusted on read). A
   recursive shape ([TLazy], from [fix]) can't be traversed finitely → answer conservatively [true]
   (validate it), never a false negative that would silently skip a real check. *)
let rec needs_checks : type a. a shape -> bool =
 fun shape ->
  match shape with
  | TString | TInt | TBool | TDate | TId | TBson | TUnit -> false
  | TFloat { allow_nonfinite } -> not allow_nonfinite
  | TList el -> needs_checks el
  | TOption el -> needs_checks el
  | TMap el -> needs_checks el
  | TCheck _ -> true
  | TNorm (_, inner) -> needs_checks inner
  | TConv (_, _, inner) -> needs_checks inner
  | TCoerce inner -> needs_checks inner
  | TObj o -> obj_needs_checks o
  | TVariant { cases; _ } -> cases_need_checks cases
  | TLazy _ -> true
and obj_needs_checks : type a. a record_shape -> bool =
 fun o -> (match o.invariants with [] -> false | _ :: _ -> true) || members_need_checks o.members
and members_need_checks : type a. a bound_field list -> bool = function
  | [] -> false
  | Bound_field f :: tl -> needs_checks f.shape || members_need_checks tl
and cases_need_checks : type a. a case list -> bool = function
  | [] -> false
  | Case c :: tl -> (match c.body.invariants with [] -> false | _ :: _ -> true) || members_need_checks c.body.members || cases_need_checks tl

(* ---- derived pretty-printing --------------------------------------------------- *)

let rec pretty : type a. a shape -> Format.formatter -> a -> unit =
 fun shape fmt v ->
  match shape with
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
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ") (pretty el))
        v
  | TOption el -> ( match v with Some x -> pretty el fmt x | None -> Format.fprintf fmt "-")
  | TMap el ->
      Format.fprintf fmt "@[<hv 2>{%a}@]"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
           (fun f (k, x) -> Format.fprintf f "%s: %a" k (pretty el) x))
        v
  | TCheck (_, _, _, inner) -> pretty inner fmt v
  | TNorm (_, inner) -> pretty inner fmt v
  | TConv (inj, _, inner) -> pretty inner fmt (inj v)
  | TObj o ->
      Format.fprintf fmt "@[<hv 2>{ %a }@]"
        (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
           (fun f (Bound_field p) -> Format.fprintf f "%s = %a" p.name (pretty p.shape) (p.get v)))
        o.members
  | TVariant { cases; _ } -> (
      let rec go = function
        | [] -> Format.fprintf fmt "<?>"
        | Case c :: rest -> (
            match c.project v with
            | Some a ->
                Format.fprintf fmt "@[<hv 2>%s { %a }@]" c.name
                  (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ";@ ")
                     (fun f (Bound_field p) -> Format.fprintf f "%s = %a" p.name (pretty p.shape) (p.get a)))
                  c.body.members
            | None -> go rest)
      in
      go cases)
  | TLazy l -> pretty (Lazy.force l) fmt v
  | TCoerce inner -> pretty inner fmt v
