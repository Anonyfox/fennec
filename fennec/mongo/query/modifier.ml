(* Update-modifier engine — applies a Mongo update document to an in-memory BSON document. Pure.

   Supported: $set, $unset, $inc, $mul, $min, $max, $rename, $setOnInsert (insert-time only), $push
   (+ $each), $addToSet (+ $each), $pull (value or sub-selector), $pullAll, $pop. Dotted paths are
   supported for $set/$unset and for the field targeted by every operator. A modifier document with
   no $-operators is treated as a full replacement (preserving _id). *)

open Bson

let is_operator_doc = function
  | Document kvs -> List.exists (fun (k, _) -> String.length k > 0 && k.[0] = '$') kvs
  | _ -> false

let split_path p = String.split_on_char '.' p
let kvs_of = function Document kvs -> kvs | _ -> []

let assoc_set kvs k v =
  if List.mem_assoc k kvs then
    List.map (fun (k2, v2) -> if k2 = k then (k2, v) else (k2, v2)) kvs
  else kvs @ [ (k, v) ]

(* set value at a (possibly nested) path, creating intermediate documents *)
let rec set_path (d : Bson.t) (path : string list) (v : Bson.t) : Bson.t =
  match path with
  | [] -> v
  | [ k ] -> Document (assoc_set (kvs_of d) k v)
  | k :: rest ->
      let kvs = kvs_of d in
      let sub = match List.assoc_opt k kvs with Some s -> s | None -> Document [] in
      Document (assoc_set kvs k (set_path sub rest v))

let rec unset_path (d : Bson.t) (path : string list) : Bson.t =
  match path with
  | [] -> d
  | [ k ] -> Document (List.filter (fun (k2, _) -> k2 <> k) (kvs_of d))
  | k :: rest -> (
      let kvs = kvs_of d in
      match List.assoc_opt k kvs with
      | Some sub -> Document (assoc_set kvs k (unset_path sub rest))
      | None -> d)

let get_path d path = Matcher.get_path d path
let set d path v = set_path d (split_path path) v
let unset d path = unset_path d (split_path path)

(* numeric helpers: keep ints when both sides are ints, else float *)
let num_binop f_int f_flt cur operand =
  match (cur, operand) with
  | Some (Int a), Int b -> Int (f_int a b)
  | Some (Int a), Float b -> Float (f_flt (float_of_int a) b)
  | Some (Float a), Int b -> Float (f_flt a (float_of_int b))
  | Some (Float a), Float b -> Float (f_flt a b)
  | None, (Int _ as b) -> b
  | None, (Float _ as b) -> b
  | _ -> ( match operand with x -> x)

let as_array = function Some (Array xs) -> xs | _ -> []

let each_values = function
  | Document kvs when List.mem_assoc "$each" kvs -> (
      match List.assoc "$each" kvs with Array xs -> xs | x -> [ x ])
  | v -> [ v ]

let apply_op (d : Bson.t) (op : string) (arg : Bson.t) : Bson.t =
  let fields = kvs_of arg in
  let over f = List.fold_left (fun acc (path, v) -> f acc path v) d fields in
  match op with
  | "$set" -> over (fun acc path v -> set acc path v)
  | "$setOnInsert" -> d (* applied only at insert time, see [apply ~insert] *)
  | "$unset" -> List.fold_left (fun acc (path, _) -> unset acc path) d fields
  | "$inc" ->
      over (fun acc path v ->
          set acc path (num_binop ( + ) ( +. ) (get_path acc path) v))
  | "$mul" ->
      over (fun acc path v ->
          set acc path (num_binop ( * ) ( *. ) (get_path acc path) v))
  | "$min" ->
      over (fun acc path v ->
          match get_path acc path with
          | Some cur ->
              if (match Matcher.bson_cmp v cur with Some c -> c < 0 | None -> false)
              then set acc path v
              else acc
          | None -> set acc path v)
  | "$max" ->
      over (fun acc path v ->
          match get_path acc path with
          | Some cur ->
              if (match Matcher.bson_cmp v cur with Some c -> c > 0 | None -> false)
              then set acc path v
              else acc
          | None -> set acc path v)
  | "$rename" ->
      List.fold_left
        (fun acc (path, dest) ->
          match (get_path acc path, dest) with
          | Some v, String to_path -> set (unset acc path) to_path v
          | _ -> acc)
        d fields
  | "$push" ->
      over (fun acc path v ->
          let cur = as_array (get_path acc path) in
          set acc path (Array (cur @ each_values v)))
  | "$addToSet" ->
      over (fun acc path v ->
          let cur = as_array (get_path acc path) in
          let adds = each_values v in
          let merged =
            List.fold_left
              (fun xs x -> if List.exists (fun e -> e = x) xs then xs else xs @ [ x ])
              cur adds
          in
          set acc path (Array merged))
  | "$pull" ->
      over (fun acc path cond ->
          let cur = as_array (get_path acc path) in
          let keep e =
            if Matcher.is_selector_like cond then not (Matcher.doc_matches cond e)
            else e <> cond
          in
          set acc path (Array (List.filter keep cur)))
  | "$pullAll" ->
      over (fun acc path v ->
          let cur = as_array (get_path acc path) in
          let bad = (match v with Array xs -> xs | _ -> []) in
          set acc path (Array (List.filter (fun e -> not (List.mem e bad)) cur)))
  | "$pop" ->
      over (fun acc path v ->
          let cur = as_array (get_path acc path) in
          let cur' =
            match (cur, v) with
            | [], _ -> []
            | _, Int n when n < 0 -> List.tl cur
            | _ -> List.rev (List.tl (List.rev cur))
          in
          set acc path (Array cur'))
  | _ -> d (* unknown operator: ignore *)

(* Apply [modifier] to [doc]. [insert] = true applies $setOnInsert too. A
   non-operator modifier replaces the document wholesale, preserving _id. *)
let apply ?(insert = false) (doc : Bson.t) (modifier : Bson.t) : Bson.t =
  if not (is_operator_doc modifier) then
    match get doc "_id" with
    | Some id -> Document (("_id", id) :: List.filter (fun (k, _) -> k <> "_id") (kvs_of modifier))
    | None -> modifier
  else
    List.fold_left
      (fun acc (op, arg) ->
        if op = "$setOnInsert" then
          if insert then apply_op acc "$set" arg else acc
        else apply_op acc op arg)
      doc (kvs_of modifier)
