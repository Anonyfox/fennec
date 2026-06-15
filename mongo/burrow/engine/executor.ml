module Store = Burrow_store.Store
module B = Bson
module M = Query.Matcher

let empty = B.Document []

let full_scan txn (c : Catalog.collection) : B.t list =
  let acc = ref [] in
  Record.iter txn c.records (fun ~id_key:_ ~doc -> acc := doc :: !acc; true);
  List.rev !acc

(* stop once the entry's value passes the range's upper bound; emit unless below an exclusive lower *)
let range_stop (r : Plan.range) entry =
  match r.Plan.hi with
  | None -> false
  | Some e -> if r.Plan.excl_hi then not (entry < e) else not (entry < e || String.starts_with ~prefix:e entry)

let range_emit (r : Plan.range) entry =
  match r.Plan.lo with Some e when r.Plan.excl_lo -> not (String.starts_with ~prefix:e entry) | _ -> true

(* scan one index range, appending fresh record keys (deduped via [seen]) to [acc] in scan order *)
let scan_range txn (idx : Catalog.index) seen acc (r : Plan.range) =
  Store.iter txn idx.Catalog.db ?from:r.Plan.lo (fun ~key ~data ->
      if range_stop r key then false
      else begin
        if range_emit r key && not (Hashtbl.mem seen data) then begin
          Hashtbl.add seen data ();
          acc := data :: !acc
        end;
        true
      end)

let find_index (c : Catalog.collection) index = List.find_opt (fun i -> i.Catalog.iname = index) c.indexes

(* candidate documents from the access path (a superset of the matches; the residual matcher filters).
   For an index scan the order is index order (so a [sorted] plan needs no re-sort). *)
let candidates txn (c : Catalog.collection) (plan : Plan.t) : B.t list =
  match plan with
  | Plan.Id_point id -> ( match Record.get txn c.records ~id with Some d -> [ d ] | None -> [])
  | Plan.Collection_scan -> full_scan txn c
  | Plan.Index_scan { index; ranges; _ } -> (
    match find_index c index with
    | None -> full_scan txn c (* index vanished concurrently: correctness over speed *)
    | Some idx ->
      let seen = Hashtbl.create 64 and acc = ref [] in
      List.iter (scan_range txn idx seen acc) ranges;
      List.filter_map (fun rk -> Record.get_by_key txn c.records rk) (List.rev !acc))
  | Plan.Index_union pairs ->
    (* every named index must exist, else the union would miss a clause's matches — fall back to scan *)
    if List.for_all (fun (index, _) -> find_index c index <> None) pairs then begin
      let seen = Hashtbl.create 64 and acc = ref [] in
      List.iter
        (fun (index, ranges) -> match find_index c index with Some idx -> List.iter (scan_range txn idx seen acc) ranges | None -> ())
        pairs;
      List.filter_map (fun rk -> Record.get_by_key txn c.records rk) (List.rev !acc)
    end
    else full_scan txn c

let matched txn c plan ~selector =
  let cs = candidates txn c plan in
  if selector = empty then cs else List.filter (M.doc_matches selector) cs

(* the plan already delivers candidates in the requested sort order *)
let plan_sorted = function Plan.Index_scan { sorted; _ } -> sorted | _ -> false

let rec drop n l = if n <= 0 then l else match l with [] -> [] | _ :: t -> drop (n - 1) t
let rec take n l = if n <= 0 then [] else match l with [] -> [] | h :: t -> h :: take (n - 1) t
let window ~skip ~limit xs =
  let xs = if skip > 0 then drop skip xs else xs in
  if limit > 0 then take limit xs else xs
let project fields = if fields = empty then Fun.id else Query.Projection.apply (Query.Projection.of_fields fields)

(* Streaming fast path for a sorted single-range index scan with a limit: walk the index in order,
   match as we go, and STOP once we have [skip + limit] matches — so a paginated sorted query never
   scans, fetches, or sorts the whole result set. The index is non-multikey (a sorted plan requires
   it), so each record appears once; no dedup needed. *)
let find_sorted_limited txn (c : Catalog.collection) idx (r : Plan.range) ~selector ~skip ~limit ~fields =
  let need = skip + limit in
  let proj = project fields in
  (* with no residual selector every entry matches, so the skip region is a pure index walk — fetch
     (decode) only the page. With a selector we must fetch each to test it. *)
  let no_filter = selector = empty in
  let seen = ref 0 and kept = ref [] in
  let keep_page data = if !seen > skip then match Record.get_by_key txn c.records data with Some d -> kept := proj d :: !kept | None -> () in
  Store.iter txn idx.Catalog.db ?from:r.Plan.lo (fun ~key ~data ->
      if range_stop r key then false
      else begin
        (if range_emit r key then
           if no_filter then (incr seen; keep_page data)
           else
             match Record.get_by_key txn c.records data with
             | Some d when M.doc_matches selector d ->
               incr seen;
               if !seen > skip then kept := proj d :: !kept
             | _ -> ());
        !seen < need
      end);
  List.rev !kept

let rec find txn (c : Catalog.collection) plan ~selector ~sort ~skip ~limit ~fields =
  match plan with
  | Plan.Index_scan { index; ranges = [ r ]; sorted = true } when limit > 0 -> (
    match find_index c index with
    | Some idx -> find_sorted_limited txn c idx r ~selector ~skip ~limit ~fields
    | None -> (* index vanished: fall back to the general path *) find_general txn c plan ~selector ~sort ~skip ~limit ~fields)
  | _ -> find_general txn c plan ~selector ~sort ~skip ~limit ~fields

and find_general txn c plan ~selector ~sort ~skip ~limit ~fields =
  let ms = matched txn c plan ~selector in
  let ordered = if sort = empty || plan_sorted plan then ms else Query.Sorter.sort sort ms in
  List.map (project fields) (window ~skip ~limit ordered)

let find_one txn c plan ~selector ~sort ~skip ~fields =
  match find txn c plan ~selector ~sort ~skip ~limit:1 ~fields with [] -> None | d :: _ -> Some d

(* a single-field constraint that an index bound captures EXACTLY (no residual): a scalar equality, or
   an operator doc using only the operators build_index encodes — so counting index entries equals
   counting matching documents *)
let captured_op = function "$eq" | "$gt" | "$gte" | "$lt" | "$lte" | "$in" -> true | _ -> false

let fully_captured = function
  | B.Document kvs when List.exists (fun (k, _) -> String.length k > 0 && k.[0] = '$') kvs -> List.for_all (fun (k, _) -> captured_op k) kvs
  | B.Document _ | B.Array _ -> false (* subdocument / array literal: not an index point *)
  | _ -> true (* scalar equality *)

(* count index entries in the ranges WITHOUT fetching documents. Sound only on a non-multikey index
   (one entry per document) whose bound captures the whole selector; entries across $in ranges are
   disjoint (a field has one value), so no dedup is needed. *)
let count_entries txn (idx : Catalog.index) ranges =
  let n = ref 0 in
  List.iter
    (fun r -> Store.iter txn idx.Catalog.db ?from:r.Plan.lo (fun ~key ~data:_ -> if range_stop r key then false else (if range_emit r key then incr n; true)))
    ranges;
  !n

let count txn (c : Catalog.collection) plan ~selector =
  match (selector, plan) with
  | B.Document [], _ -> Record.count txn c.records (* unfiltered: storage-layer count, no decode *)
  | B.Document [ (f, v) ], Plan.Index_scan { index; ranges; _ } when (String.length f = 0 || f.[0] <> '$') && fully_captured v -> (
    match find_index c index with
    | Some idx when (not idx.Catalog.multikey) && (match idx.Catalog.keys with (k, _) :: _ -> k = f | [] -> false) ->
      count_entries txn idx ranges (* index-only count: no document fetch/decode *)
    | _ -> List.length (matched txn c plan ~selector))
  | _ -> List.length (matched txn c plan ~selector)
