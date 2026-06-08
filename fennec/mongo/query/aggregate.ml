(* The aggregation pipeline over a document list — every stage is a pure transform
   [Bson.t list -> Bson.t list], so the whole engine works in memory with no storage layer (the
   "keep the Collection abstraction" model). $lookup/$unionWith resolve a foreign collection's
   documents through a caller-supplied [resolve] hook. *)

open Bson

let kvs_of = function Document kvs -> kvs | _ -> []
let strip_dollar s = if String.length s > 0 && s.[0] = '$' then String.sub s 1 (String.length s - 1) else s

(* dotted set/unset, reusing the modifier engine *)
let set_field doc path v = Modifier.apply doc (Document [ ("$set", Document [ (path, v) ]) ])
let unset_field doc path = Modifier.apply doc (Document [ ("$unset", Document [ (path, Int 1) ]) ])
let rec take n = function x :: tl when n > 0 -> x :: take (n - 1) tl | _ -> []
let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* group documents by an aggregation expression, preserving first-seen key order, and apply the
   accumulator fields *)
let group spec docs =
  let id_expr = match List.assoc_opt "_id" (kvs_of spec) with Some e -> e | None -> Null in
  let accs = List.filter (fun (k, _) -> k <> "_id") (kvs_of spec) in
  let order = ref [] (* keys in first-seen order *) in
  let tbl : (Bson.t * Bson.t list ref) list ref = ref [] in
  List.iter
    (fun d ->
      let key = Expr.eval id_expr d in
      match List.find_opt (fun (k, _) -> Bson.equal k key) !tbl with
      | Some (_, lst) -> lst := d :: !lst
      | None ->
          order := key :: !order;
          tbl := (key, ref [ d ]) :: !tbl)
    docs;
  List.rev_map
    (fun key ->
      let _, lst = List.find (fun (k, _) -> Bson.equal k key) !tbl in
      let gdocs = List.rev !lst in
      Document (("_id", key) :: List.map (fun (field, acc) -> (field, Expr.accumulate acc gdocs)) accs))
    !order

let rec run ?(resolve = fun _ -> []) (stages : Bson.t list) (docs : Bson.t list) : Bson.t list =
  List.fold_left (fun docs stage -> apply_stage resolve stage docs) docs stages

and apply_stage resolve (stage : Bson.t) (docs : Bson.t list) : Bson.t list =
  match stage with
  | Document [ (name, spec) ] -> (
      match name with
      | "$match" -> List.filter (Matcher.doc_matches spec) docs
      | "$limit" -> ( match Bson.as_float spec with Some n -> take (int_of_float n) docs | None -> docs)
      | "$skip" -> ( match Bson.as_float spec with Some n -> drop (int_of_float n) docs | None -> docs)
      | "$sort" -> Sorter.sort spec docs
      | "$count" -> ( match spec with String name -> [ Document [ (name, Int (List.length docs)) ] ] | _ -> docs)
      | "$project" -> List.map (project spec) docs
      | "$addFields" | "$set" ->
          List.map
            (fun d -> List.fold_left (fun acc (k, e) -> set_field acc k (Expr.eval e d)) d (kvs_of spec))
            docs
      | "$unset" ->
          let names =
            match spec with
            | String s -> [ s ]
            | Array xs -> List.filter_map (function String s -> Some s | _ -> None) xs
            | _ -> []
          in
          List.map (fun d -> List.fold_left unset_field d names) docs
      | "$unwind" -> unwind spec docs
      | "$group" -> group spec docs
      | "$sortByCount" ->
          let grouped = group (Document [ ("_id", spec); ("count", Document [ ("$sum", Int 1) ]) ]) docs in
          Sorter.sort (Document [ ("count", Int (-1)) ]) grouped
      | "$sample" ->
          let n = match List.assoc_opt "size" (kvs_of spec) with Some v -> ( match Bson.as_float v with Some f -> int_of_float f | None -> 0) | None -> 0 in
          take n docs (* no RNG in the pure engine: deterministic head sample *)
      | "$replaceRoot" ->
          let e = match List.assoc_opt "newRoot" (kvs_of spec) with Some e -> e | None -> Null in
          List.map (fun d -> match Expr.eval e d with Document _ as r -> r | _ -> Document []) docs
      | "$replaceWith" -> List.map (fun d -> match Expr.eval spec d with Document _ as r -> r | _ -> Document []) docs
      | "$lookup" -> lookup resolve spec docs
      | "$unionWith" ->
          let coll, pipe =
            match spec with
            | String s -> (s, [])
            | Document kvs ->
                ( (match List.assoc_opt "coll" kvs with Some (String s) -> s | _ -> ""),
                  match List.assoc_opt "pipeline" kvs with Some (Array p) -> p | _ -> [] )
            | _ -> ("", [])
          in
          docs @ run ~resolve pipe (resolve coll)
      | "$facet" ->
          [ Document (List.map (fun (k, p) -> (k, Array (run ~resolve (match p with Array xs -> xs | _ -> []) docs))) (kvs_of spec)) ]
      | "$bucket" -> bucket spec docs
      | _ -> docs (* unknown stage: pass through unchanged *))
  | _ -> docs

and project spec doc =
  let kvs = kvs_of spec in
  let included = function Int 0 | Bool false -> false | _ -> true in
  let has_inclusion = List.exists (fun (k, v) -> k <> "_id" && included v) kvs in
  if has_inclusion then begin
    let keep_id = match List.assoc_opt "_id" kvs with Some (Int 0) | Some (Bool false) -> false | _ -> true in
    let acc0 = if keep_id then ( match Bson.get doc "_id" with Some idv -> Document [ ("_id", idv) ] | None -> Document []) else Document [] in
    List.fold_left
      (fun acc (k, v) ->
        if k = "_id" then acc
        else
          match v with
          | Int 0 | Bool false -> acc
          | Int 1 | Bool true -> ( match Matcher.get_path doc k with Some fv -> set_field acc k fv | None -> acc)
          | expr -> set_field acc k (Expr.eval expr doc))
      acc0 kvs
  end
  else
    let excluded = List.filter_map (fun (k, v) -> if included v then None else Some k) kvs in
    Document (List.filter (fun (k, _) -> not (List.mem k excluded)) (kvs_of doc))

and unwind spec docs =
  let path =
    match spec with
    | String s -> strip_dollar s
    | Document kvs -> ( match List.assoc_opt "path" kvs with Some (String s) -> strip_dollar s | _ -> "")
    | _ -> ""
  in
  List.concat_map
    (fun d ->
      match Matcher.get_path d path with
      | Some (Array xs) -> List.map (fun e -> set_field d path e) xs
      | Some _ -> [ d ]
      | None -> [])
    docs

and lookup resolve spec docs =
  let kvs = kvs_of spec in
  let s k = match List.assoc_opt k kvs with Some (String v) -> v | _ -> "" in
  let from = s "from" and lf = s "localField" and ff = s "foreignField" and as_ = s "as" in
  let foreign = resolve from in
  List.map
    (fun d ->
      let lv = match Matcher.get_path d lf with Some v -> v | None -> Null in
      let matches =
        List.filter (fun fd -> match Matcher.get_path fd ff with Some fv -> Bson.equal fv lv | None -> false) foreign
      in
      set_field d as_ (Array matches))
    docs

and bucket spec docs =
  let kvs = kvs_of spec in
  let group_by = match List.assoc_opt "groupBy" kvs with Some e -> e | None -> Null in
  let boundaries = match List.assoc_opt "boundaries" kvs with Some (Array b) -> b | _ -> [] in
  let default = List.assoc_opt "default" kvs in
  let output = match List.assoc_opt "output" kvs with Some (Document o) -> o | _ -> [ ("count", Document [ ("$sum", Int 1) ]) ] in
  let barr = Array.of_list boundaries in
  let bucket_key v =
    let n = Array.length barr in
    if n = 0 || Bson.compare v barr.(0) < 0 then default
    else begin
      let rec find i =
        if i + 1 >= n then default (* at or past the last boundary: out of range *)
        else if Bson.compare v barr.(i) >= 0 && Bson.compare v barr.(i + 1) < 0 then Some barr.(i)
        else find (i + 1)
      in
      find 0
    end
  in
  (* assign each doc to a bucket, group, then sort by bucket key *)
  let assigned = List.filter_map (fun d -> match bucket_key (Expr.eval group_by d) with Some k -> Some (k, d) | None -> None) docs in
  let order = ref [] and tbl : (Bson.t * Bson.t list ref) list ref = ref [] in
  List.iter
    (fun (k, d) ->
      match List.find_opt (fun (key, _) -> Bson.equal key k) !tbl with
      | Some (_, lst) -> lst := d :: !lst
      | None -> order := k :: !order; tbl := (k, ref [ d ]) :: !tbl)
    assigned;
  let buckets =
    List.rev_map
      (fun k ->
        let _, lst = List.find (fun (key, _) -> Bson.equal key k) !tbl in
        let gdocs = List.rev !lst in
        Document (("_id", k) :: List.map (fun (field, acc) -> (field, Expr.accumulate acc gdocs)) output))
      !order
  in
  Sorter.sort (Document [ ("_id", Int 1) ]) buckets
