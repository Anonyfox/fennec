module B = Bson

(* a literal scalar [_id] (an operator doc like [{$gt:_}] or a composite Document/Array is not a
   single point — those scan) *)
let is_pointable = function B.Document _ | B.Array _ -> false | _ -> true

let plan (_indexes : Catalog.index list) ~selector ~sort:_ : Plan.t =
  match B.get selector "_id" with
  | Some v when is_pointable v -> Plan.Id_point v
  | _ -> Plan.Collection_scan
