module B = Bson

(* a literal scalar (not an operator doc, composite, or array) *)
let is_scalar = function B.Document _ | B.Array _ -> false | _ -> true
let is_pointable = is_scalar

(* Try to turn the selector's constraint on an index's first field into scan bounds. Equality works on
   any direction; a range ($gt/$gte/$lt/$lte) only on an ascending first field for now (a descending
   field's bytes are complemented, so range bounds would have to swap + invert — deferred). *)
let index_bounds (idx : Catalog.index) selector : Plan.t option =
  match idx.Catalog.keys with
  | [] -> None
  | (field, dir) :: _ -> (
    match B.get selector field with
    | None -> None
    | Some (B.Document kvs) when List.exists (fun (k, _) -> String.length k > 0 && k.[0] = '$') kvs ->
      if dir <> 1 then None
      else begin
        let enc x = Index.encode_value x dir in
        let lo = ref None and excl_lo = ref false and hi = ref None and excl_hi = ref false in
        let usable = ref false in
        List.iter
          (fun (op, x) ->
            if is_scalar x then
              match op with
              | "$eq" -> lo := Some (enc x); hi := Some (enc x); usable := true
              | "$gte" -> lo := Some (enc x); excl_lo := false; usable := true
              | "$gt" -> lo := Some (enc x); excl_lo := true; usable := true
              | "$lte" -> hi := Some (enc x); excl_hi := false; usable := true
              | "$lt" -> hi := Some (enc x); excl_hi := true; usable := true
              | _ -> ())
          kvs;
        if !usable then
          Some (Plan.Index_scan { index = idx.Catalog.iname; lo = !lo; excl_lo = !excl_lo; hi = !hi; excl_hi = !excl_hi })
        else None
      end
    | Some v when is_scalar v ->
      let e = Index.encode_value v dir in
      Some (Plan.Index_scan { index = idx.Catalog.iname; lo = Some e; excl_lo = false; hi = Some e; excl_hi = false })
    | Some _ -> None (* subdocument / array equality: no scalar index point *))

let plan (indexes : Catalog.index list) ~selector ~sort:_ : Plan.t =
  match B.get selector "_id" with
  | Some v when is_pointable v -> Plan.Id_point v
  | _ ->
    let rec first = function
      | [] -> Plan.Collection_scan
      | idx :: rest -> ( match index_bounds idx selector with Some p -> p | None -> first rest)
    in
    first indexes
