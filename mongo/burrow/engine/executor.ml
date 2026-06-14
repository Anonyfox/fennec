module Store = Burrow_store.Store
module B = Bson
module M = Query.Matcher

let empty = B.Document []

let full_scan txn (c : Catalog.collection) : B.t list =
  let acc = ref [] in
  Record.iter txn c.records (fun ~id_key:_ ~doc -> acc := doc :: !acc; true);
  List.rev !acc

(* candidate documents from the access path (a superset of the matches; the residual matcher filters) *)
let candidates txn (c : Catalog.collection) (plan : Plan.t) : B.t list =
  match plan with
  | Plan.Id_point id -> ( match Record.get txn c.records ~id with Some d -> [ d ] | None -> [])
  | Plan.Collection_scan -> full_scan txn c
  | Plan.Index_scan { index; lo; excl_lo; hi; excl_hi } -> (
    match List.find_opt (fun i -> i.Catalog.iname = index) c.indexes with
    | None -> full_scan txn c (* index vanished concurrently: correctness over speed *)
    | Some idx ->
      (* stop once the entry's field value passes the upper bound; emit unless below an exclusive lower *)
      let stop entry =
        match hi with
        | None -> false
        | Some e -> if excl_hi then not (entry < e) else not (entry < e || String.starts_with ~prefix:e entry)
      in
      let emit entry =
        match lo with Some e when excl_lo -> not (String.starts_with ~prefix:e entry) | _ -> true
      in
      let seen = Hashtbl.create 64 and acc = ref [] in
      Store.iter txn idx.Catalog.db ?from:lo (fun ~key ~data ->
          if stop key then false
          else begin
            if emit key && not (Hashtbl.mem seen data) then begin
              Hashtbl.add seen data ();
              acc := data :: !acc
            end;
            true
          end);
      List.filter_map (fun rk -> Record.get_by_key txn c.records rk) (List.rev !acc))

let matched txn c plan ~selector =
  let cs = candidates txn c plan in
  if selector = empty then cs else List.filter (M.doc_matches selector) cs

let window ~skip ~limit xs =
  let dropped = if skip > 0 then List.filteri (fun i _ -> i >= skip) xs else xs in
  if limit > 0 then List.filteri (fun i _ -> i < limit) dropped else dropped

let find txn c plan ~selector ~sort ~skip ~limit ~fields =
  let ms = matched txn c plan ~selector in
  let sorted = if sort = empty then ms else Query.Sorter.sort sort ms in
  let win = window ~skip ~limit sorted in
  if fields = empty then win
  else
    let p = Query.Projection.of_fields fields in
    List.map (Query.Projection.apply p) win

let find_one txn c plan ~selector ~sort ~skip ~fields =
  match find txn c plan ~selector ~sort ~skip ~limit:1 ~fields with [] -> None | d :: _ -> Some d

let count txn c plan ~selector = List.length (matched txn c plan ~selector)
