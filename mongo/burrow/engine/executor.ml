module Store = Burrow_store.Store
module B = Bson
module M = Query.Matcher

let empty = B.Document []

let full_scan txn (c : Catalog.collection) : B.t list =
  let acc = ref [] in
  Record.iter txn c.records (fun ~id_key:_ ~doc -> acc := doc :: !acc; true);
  List.rev !acc

(* scan one index range, appending fresh record keys (deduped via [seen]) to [acc] in scan order *)
let scan_range txn (idx : Catalog.index) seen acc (r : Plan.range) =
  let stop entry =
    match r.Plan.hi with
    | None -> false
    | Some e -> if r.Plan.excl_hi then not (entry < e) else not (entry < e || String.starts_with ~prefix:e entry)
  in
  let emit entry =
    match r.Plan.lo with Some e when r.Plan.excl_lo -> not (String.starts_with ~prefix:e entry) | _ -> true
  in
  Store.iter txn idx.Catalog.db ?from:r.Plan.lo (fun ~key ~data ->
      if stop key then false
      else begin
        if emit key && not (Hashtbl.mem seen data) then begin
          Hashtbl.add seen data ();
          acc := data :: !acc
        end;
        true
      end)

(* candidate documents from the access path (a superset of the matches; the residual matcher filters).
   For an index scan the order is index order (so a [sorted] plan needs no re-sort). *)
let candidates txn (c : Catalog.collection) (plan : Plan.t) : B.t list =
  match plan with
  | Plan.Id_point id -> ( match Record.get txn c.records ~id with Some d -> [ d ] | None -> [])
  | Plan.Collection_scan -> full_scan txn c
  | Plan.Index_scan { index; ranges; _ } -> (
    match List.find_opt (fun i -> i.Catalog.iname = index) c.indexes with
    | None -> full_scan txn c (* index vanished concurrently: correctness over speed *)
    | Some idx ->
      let seen = Hashtbl.create 64 and acc = ref [] in
      List.iter (scan_range txn idx seen acc) ranges;
      List.filter_map (fun rk -> Record.get_by_key txn c.records rk) (List.rev !acc))

let matched txn c plan ~selector =
  let cs = candidates txn c plan in
  if selector = empty then cs else List.filter (M.doc_matches selector) cs

(* the plan already delivers candidates in the requested sort order *)
let plan_sorted = function Plan.Index_scan { sorted; _ } -> sorted | _ -> false

let window ~skip ~limit xs =
  let dropped = if skip > 0 then List.filteri (fun i _ -> i >= skip) xs else xs in
  if limit > 0 then List.filteri (fun i _ -> i < limit) dropped else dropped

let find txn c plan ~selector ~sort ~skip ~limit ~fields =
  let ms = matched txn c plan ~selector in
  let ordered = if sort = empty || plan_sorted plan then ms else Query.Sorter.sort sort ms in
  let win = window ~skip ~limit ordered in
  if fields = empty then win
  else
    let p = Query.Projection.of_fields fields in
    List.map (Query.Projection.apply p) win

let find_one txn c plan ~selector ~sort ~skip ~fields =
  match find txn c plan ~selector ~sort ~skip ~limit:1 ~fields with [] -> None | d :: _ -> Some d

(* an unfiltered count is just the record count — no document decode, no candidate list *)
let count txn (c : Catalog.collection) plan ~selector =
  if selector = empty then Record.count txn c.records else List.length (matched txn c plan ~selector)
