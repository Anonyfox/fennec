(* engine.ml — the FORMAT-AGNOSTIC interpreters over a {!Shape.t}: normalization, the refinement-check
   phase (the encode/decode validation gate), the [needs_checks] gate that lets a clean read skip it,
   and derived pretty-printing. NO Bson — these walk values + shapes, never a wire format. The
   BSON-tree read/write/size live in {!Bson_engine} (the [sift.bson] plugin). *)

open Shape

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

(* ---- derived pretty-printing — nested values, readable; the [TDyn] escape via {!Value} --------- *)

let rec pretty : type a. a shape -> Format.formatter -> a -> unit =
 fun shape fmt v ->
  match shape with
  | TString -> Format.fprintf fmt "%S" v
  | TInt -> Format.fprintf fmt "%d" v
  | TFloat _ -> Format.fprintf fmt "%g" v
  | TBool -> Format.fprintf fmt "%b" v
  | TDate -> Format.fprintf fmt "date(%Ld)" v
  | TId -> Format.fprintf fmt "#%s" v
  | TBson -> Bson.pp fmt v
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
