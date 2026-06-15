module B = Bson

let is_scalar = function B.Document _ | B.Array _ -> false | _ -> true
let is_pointable = is_scalar
let has_ops kvs = List.exists (fun (k, _) -> String.length k > 0 && k.[0] = '$') kvs

(* what the selector says about one field *)
type fc =
  | Eq of B.t
  | Range of { lo : B.t option; excl_lo : bool; hi : B.t option; excl_hi : bool }
  | In of B.t list
  | Unconstrained

let constraint_of selector field : fc =
  match B.get selector field with
  | None -> Unconstrained
  | Some (B.Document kvs) when has_ops kvs -> (
    match List.assoc_opt "$in" kvs with
    | Some (B.Array vs) when vs <> [] && List.for_all is_scalar vs -> In vs
    | Some _ -> Unconstrained (* $in with composite values / non-array: not an index point *)
    | None ->
      let lo = ref None and excl_lo = ref false and hi = ref None and excl_hi = ref false and any = ref false in
      List.iter
        (fun (op, x) ->
          if is_scalar x then
            match op with
            | "$eq" -> lo := Some x; hi := Some x; any := true
            | "$gte" -> lo := Some x; excl_lo := false; any := true
            | "$gt" -> lo := Some x; excl_lo := true; any := true
            | "$lte" -> hi := Some x; excl_hi := false; any := true
            | "$lt" -> hi := Some x; excl_hi := true; any := true
            | _ -> ())
        kvs;
      if !any then Range { lo = !lo; excl_lo = !excl_lo; hi = !hi; excl_hi = !excl_hi } else Unconstrained)
  | Some v when is_scalar v -> Eq v
  | Some _ -> Unconstrained (* a subdocument / array literal is not a scalar index point *)

(* Build the index ranges for a selector: consume leading EQUALITY fields into a shared prefix, then a
   single trailing range (ascending field only), or a [$in] fan-out on the leading field, or a prefix
   scan when the next field is unconstrained. [None] when the index can't be used at all. *)
let build_index (idx : Catalog.index) selector : Plan.range list option =
  let prefix_range prefix = { Plan.lo = Some prefix; excl_lo = false; hi = Some prefix; excl_hi = false } in
  let rec go fields prefix =
    match fields with
    | [] -> if prefix = "" then None else Some [ prefix_range prefix ]
    | (field, dir) :: rest -> (
      match constraint_of selector field with
      | Eq v -> go rest (prefix ^ Index.encode_value v dir)
      | In vs when prefix = "" ->
        Some (List.map (fun v -> let e = Index.encode_value v dir in { Plan.lo = Some e; excl_lo = false; hi = Some e; excl_hi = false }) vs)
      | Range { lo; excl_lo; hi; excl_hi } ->
        let enc x = Index.encode_value x dir in
        let openb = if prefix = "" then None else Some prefix in
        let mk = function Some x -> Some (prefix ^ enc x) | None -> openb in
        let r =
          if dir = 1 then
            (* ascending: value-low -> byte-low, value-high -> byte-high *)
            { Plan.lo = mk lo; excl_lo = (match lo with Some _ -> excl_lo | None -> false);
              hi = mk hi; excl_hi = (match hi with Some _ -> excl_hi | None -> false) }
          else
            (* descending (complemented bytes reverse the order): value-high -> byte-low, value-low -> byte-high *)
            { Plan.lo = mk hi; excl_lo = (match hi with Some _ -> excl_hi | None -> false);
              hi = mk lo; excl_hi = (match lo with Some _ -> excl_lo | None -> false) }
        in
        Some [ r ]
      | _ -> if prefix = "" then None else Some [ prefix_range prefix ])
  in
  go idx.Catalog.keys ""

let sort_spec = function
  | B.Document kvs -> List.map (fun (f, v) -> (f, match v with B.Int n -> n | B.Float x -> int_of_float x | _ -> 1)) kvs
  | _ -> []

(* The index's forward-scan order — its key fields in their own directions, then the [_id] record-key
   suffix ascending — satisfies the requested sort iff the sort matches a prefix of the (non-multikey)
   index keys (direction for direction), with a trailing [("_id", 1)] permitted only AFTER every key
   field is consumed (that's where the suffix sits). So an ascending index serves an ascending sort and
   a descending index serves a descending sort, both by a forward scan. Lets the executor skip the sort. *)
let serves_sort (idx : Catalog.index) sort =
  (not idx.Catalog.multikey)
  &&
  let rec go ss keys =
    match (ss, keys) with
    | [], _ -> true
    | [ ("_id", 1) ], [] -> true
    | (sf, sd) :: st, (kf, kd) :: kt -> sf = kf && sd = kd && go st kt
    | _ -> false
  in
  let ss = sort_spec sort in
  ss <> [] && go ss idx.Catalog.keys

(* an estimate of how tight a scan is — higher = fewer candidates. Purely a cost heuristic; any usable
   index is correct (the residual matcher filters), so this only affects speed. *)
let is_eq_point = function
  | [ ({ Plan.lo = Some a; hi = Some b; _ } : Plan.range) ] -> a = b
  | _ -> false

let range_score (r : Plan.range) =
  let plen = match (r.Plan.lo, r.Plan.hi) with Some s, _ | _, Some s -> String.length s | None, None -> 0 in
  match (r.Plan.lo, r.Plan.hi) with Some a, Some b when a = b -> 1000 + plen (* equality / prefix *) | _ -> 100 + plen

let score (idx : Catalog.index) ranges =
  match ranges with
  | [ r ] -> if idx.Catalog.unique && is_eq_point ranges then range_score r + 100_000 else range_score r
  | r :: _ -> range_score r / (1 + List.length ranges) (* $in: less selective the more values *)
  | [] -> 0

(* the most selective single index usable for a (sub-)selector *)
let best_usable (indexes : Catalog.index list) selector =
  match List.filter_map (fun idx -> match build_index idx selector with Some r -> Some (idx, r) | None -> None) indexes with
  | [] -> None
  | usable ->
    let scored = List.map (fun (idx, r) -> (idx, r, score idx r)) usable in
    let idx, r, _ =
      List.fold_left (fun (bi, br, bs) (i, rr, s) -> if s > bs then (i, rr, s) else (bi, br, bs)) (List.hd scored) (List.tl scored)
    in
    Some (idx, r)

(* a top-level [$or] where EVERY clause is index-served becomes a union of per-clause index scans
   (deduped). If any clause lacks an index the union would miss matches, so we bail (collection scan). *)
let try_or_union (indexes : Catalog.index list) selector : Plan.t option =
  match B.get selector "$or" with
  | Some (B.Array (_ :: _ as clauses)) ->
    let per = List.map (fun cl -> Option.map (fun (idx, r) -> (idx.Catalog.iname, r)) (best_usable indexes cl)) clauses in
    if List.for_all Option.is_some per then Some (Plan.Index_union (List.filter_map Fun.id per)) else None
  | _ -> None

let plan (indexes : Catalog.index list) ~selector ~sort : Plan.t =
  match B.get selector "_id" with
  | Some v when is_pointable v -> Plan.Id_point v
  | _ ->
    let usable = List.filter_map (fun idx -> match build_index idx selector with Some r -> Some (idx, r) | None -> None) indexes in
    let single = function [ _ ] -> true | _ -> false in
    let scan (idx : Catalog.index) ranges sorted = Plan.Index_scan { index = idx.Catalog.iname; ranges; sorted } in
    let want_sort = sort_spec sort <> [] in
    match usable with
    | [] -> (
      (* no single filter index: an all-indexed top-level $or becomes a union; else use an index purely
         for the sort order if one fits; else scan *)
      match try_or_union indexes selector with
      | Some u -> u
      | None ->
        if want_sort then (
          match List.find_opt (fun idx -> serves_sort idx sort) indexes with
          | Some idx -> scan idx [ { Plan.lo = None; excl_lo = false; hi = None; excl_hi = false } ] true
          | None -> Plan.Collection_scan)
        else Plan.Collection_scan)
    | _ ->
      (* the most selective usable index *)
      let scored = List.map (fun (idx, r) -> (idx, r, score idx r)) usable in
      let bidx, branges, _ =
        List.fold_left (fun (bi, br, bs) (idx, r, sc) -> if sc > bs then (idx, r, sc) else (bi, br, bs)) (List.hd scored) (List.tl scored)
      in
      if single branges && serves_sort bidx sort then scan bidx branges true (* tight filter AND ordered *)
      else if want_sort && not (is_eq_point branges) then
        (* tightest filter is an open range and we must sort: prefer an index that yields the order, to
           skip sorting a potentially large result *)
        match List.find_opt (fun (idx, r) -> single r && serves_sort idx sort) usable with
        | Some (idx, r) -> scan idx r true
        | None -> scan bidx branges false
      else scan bidx branges false
