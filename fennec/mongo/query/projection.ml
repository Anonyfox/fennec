(* Field projection — include/exclude, _id handling, first-segment dotted paths. Takes the
   projection spec as a Bson document directly (minimongo's `fields`/`projection` option). *)

open Bson

type t = No_projection | Include of string list * bool | Exclude of string list * bool

let seg k = match String.index_opt k '.' with Some i -> String.sub k 0 i | None -> k

let of_fields (spec : Bson.t) : t =
  match spec with
  | Document ((_ :: _) as kvs) ->
      let truthy = function Int 0 | Bool false -> false | _ -> true in
      let keep_id =
        match List.assoc_opt "_id" kvs with Some v -> truthy v | None -> true
      in
      let non_id = List.filter (fun (k, _) -> k <> "_id") kvs in
      let names = List.map fst non_id in
      let including = match non_id with (_, v) :: _ -> truthy v | [] -> keep_id in
      if including then Include (names, keep_id) else Exclude (names, keep_id)
  | _ -> No_projection

let apply proj (d : Bson.t) : Bson.t =
  let kvs = match d with Document kvs -> kvs | _ -> [] in
  match proj with
  | No_projection -> d
  | Include (names, keep_id) ->
      Document
        (List.filter
           (fun (k, _) -> if k = "_id" then keep_id else List.mem (seg k) names)
           kvs)
  | Exclude (names, keep_id) ->
      Document
        (List.filter
           (fun (k, _) -> if k = "_id" then keep_id else not (List.mem (seg k) names))
           kvs)

let cleared proj (names : string list) : string list =
  match proj with
  | No_projection -> names
  | Include (incl, _) -> List.filter (fun k -> List.mem (seg k) incl) names
  | Exclude (excl, _) -> List.filter (fun k -> not (List.mem (seg k) excl)) names
