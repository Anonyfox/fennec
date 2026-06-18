(* Typed modifiers over field handles — the $-operators with the strings taken out of app code.
   Modifiers GROUP by operator when combined ([all]), exactly the document Mongo expects. *)

type t = (string * (string * Bson.t) list) list (* operator -> assignments *)

let one op kv = [ (op, [ kv ]) ]
let set f v = one "$set" (Sift.field_name f, Sift.field_enc f v)
let unset f = one "$unset" (Sift.field_name f, Bson.str "")
let inc f (n : int) = one "$inc" (Sift.field_name f, Bson.int n)
let inc_f f (x : float) = one "$inc" (Sift.field_name f, Bson.Float x)
let push f v = one "$push" (Sift.field_name f, Sift.field_elem_enc f v)
let add_to_set f v = one "$addToSet" (Sift.field_name f, Sift.field_elem_enc f v)
let pull f v = one "$pull" (Sift.field_name f, Sift.field_elem_enc f v)
let pull_all f vs = one "$pullAll" (Sift.field_name f, Bson.Array (List.map (Sift.field_elem_enc f) vs))
let pop_first f = one "$pop" (Sift.field_name f, Bson.int (-1))
let pop_last f = one "$pop" (Sift.field_name f, Bson.int 1)
let min f v = one "$min" (Sift.field_name f, Sift.field_enc f v)
let max f v = one "$max" (Sift.field_name f, Sift.field_enc f v)
let mul f (n : int) = one "$mul" (Sift.field_name f, Bson.int n)
let mul_f f (x : float) = one "$mul" (Sift.field_name f, Bson.Float x)
let set_on_insert f v = one "$setOnInsert" (Sift.field_name f, Sift.field_enc f v)
let rename f ~to_ = one "$rename" (Sift.field_name f, Bson.str (Sift.field_name to_))
let raw (b : Bson.t) : t =
  match b with
  | Bson.Document kvs ->
      List.filter_map (function op, Bson.Document fields -> Some (op, fields) | _ -> None) kvs
  | _ -> []

let all (ms : t list) : t =
  List.fold_left
    (fun acc m ->
      List.fold_left
        (fun acc (op, kvs) ->
          match List.assoc_opt op acc with
          | Some prev -> (op, prev @ kvs) :: List.remove_assoc op acc
          | None -> (op, kvs) :: acc)
        acc m)
    [] ms
  |> List.rev

let to_bson (m : t) : Bson.t = Bson.Document (List.map (fun (op, kvs) -> (op, Bson.Document kvs)) (all [ m ]))
