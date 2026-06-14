module Store = Burrow_store.Store
module B = Bson
module M = Query.Matcher

let empty = B.Document []

(* candidate documents from the access path (pre-residual-match) *)
let candidates txn (c : Catalog.collection) (plan : Plan.t) : B.t list =
  match plan with
  | Plan.Id_point id -> ( match Record.get txn c.records ~id with Some d -> [ d ] | None -> [])
  | Plan.Collection_scan ->
    let acc = ref [] in
    Record.iter txn c.records (fun ~id_key:_ ~doc ->
        acc := doc :: !acc;
        true);
    List.rev !acc

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
