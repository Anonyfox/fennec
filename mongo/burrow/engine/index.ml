module Store = Burrow_store.Store
module B = Bson
module Key_codec = Burrow_codec.Key_codec

(* invert bytes for a descending field: order-reversing, length-preserving, stays prefix-free *)
let complement s = String.map (fun c -> Char.chr (0xFF - Char.code c)) s

let encode_value (v : B.t) dir : string =
  let e = try Key_codec.encode v with Key_codec.Unsupported _ -> Key_codec.encode B.Null in
  if dir < 0 then complement e else e

(* the scalar value-set to index for one field of a doc — see the completeness note in the .mli *)
let field_values doc field : B.t list =
  match Query.Matcher.get_path doc field with
  | None -> [ B.Null ]
  | Some (B.Array els) ->
    let scalars = List.filter_map (function B.Document _ | B.Array _ -> None | v -> Some v) els in
    if scalars = [] then [ B.Null ] else scalars
  | Some (B.Document _) -> [ B.Null ]
  | Some v -> [ v ]

(* whether [doc] indexes an array for any of [idx]'s fields — the index must then be flagged multikey *)
let is_multikey_doc (idx : Catalog.index) (doc : B.t) =
  List.exists
    (fun (field, _) -> match Query.Matcher.get_path doc field with Some (B.Array _) -> true | _ -> false)
    idx.Catalog.keys

let keys_for_doc (idx : Catalog.index) (doc : B.t) : string list =
  let combos =
    List.fold_left
      (fun acc (field, dir) ->
        let encs = List.map (fun v -> encode_value v dir) (field_values doc field) in
        List.concat_map (fun prefix -> List.map (fun e -> prefix ^ e) encs) acc)
      [ "" ] idx.Catalog.keys
  in
  List.sort_uniq String.compare combos

let add txn (idx : Catalog.index) ~doc ~record_key =
  List.iter (fun k -> Store.put txn idx.Catalog.db (k ^ record_key) record_key) (keys_for_doc idx doc)

let remove txn (idx : Catalog.index) ~doc ~record_key =
  List.iter (fun k -> ignore (Store.del txn idx.Catalog.db (k ^ record_key))) (keys_for_doc idx doc)
