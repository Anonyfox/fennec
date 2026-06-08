(* Selector matching — the pure heart of minimongo. Supports the comparison/membership/array/
   element operators a real minimongo needs ($eq, $ne, $exists, $in, $nin, $gt/$gte/$lt/$lte, $all,
   $size, $elemMatch, $not, $type, $mod) plus top-level $and/$or/$nor over dotted paths. Operators
   that would require a regex engine ($regex) or arbitrary code ($where) are intentionally omitted
   so the module stays Stdlib-only and JavaScript-safe. *)

open Bson

let bson_num = function
  | Int n -> Some (float_of_int n)
  | Int64 n -> Some (Int64.to_float n)
  | Float f -> Some f
  | Date d -> Some (Int64.to_float d)
  | _ -> None

(* a comparison usable for ranges AND sorting; [None] when incomparable *)
let bson_cmp a b =
  match (bson_num a, bson_num b) with
  | Some x, Some y -> Some (compare x y)
  | _ -> (
      match (a, b) with
      | String s, String t -> Some (String.compare s t)
      | Bool x, Bool y -> Some (compare x y)
      | _ -> None)

(* dotted-path field access: "a.b.c" walks nested documents *)
let rec get_path (d : Bson.t) (path : string) : Bson.t option =
  match String.index_opt path '.' with
  | None -> get d path
  | Some i -> (
      let head = String.sub path 0 i in
      let rest = String.sub path (i + 1) (String.length path - i - 1) in
      match get d head with Some sub -> get_path sub rest | None -> None)

let type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ | Int64 _ -> "int"
  | Float _ -> "double"
  | String _ -> "string"
  | Document _ -> "object"
  | Array _ -> "array"
  | Object_id _ -> "objectId"
  | Date _ -> "date"
  | Timestamp _ -> "timestamp"
  | Binary _ -> "binData"
  | Regex _ -> "regex"
  | Decimal128 _ -> "decimal"
  | Code _ | Code_with_scope _ -> "javascript"
  | Symbol _ -> "symbol"
  | Min_key -> "minKey"
  | Max_key -> "maxKey"

let rec matches_op field_val op v =
  match op with
  | "$eq" -> ( match field_val with Some fv -> fv = v | None -> false)
  | "$ne" -> ( match field_val with Some fv -> fv <> v | None -> true)
  | "$exists" -> (
      match v with Bool b -> (field_val <> None) = b | _ -> true)
  | "$in" -> (
      match (field_val, v) with
      | Some fv, Array xs -> List.exists (fun x -> x = fv) xs
      | _ -> false)
  | "$nin" -> (
      match (field_val, v) with
      | Some fv, Array xs -> not (List.exists (fun x -> x = fv) xs)
      | None, _ -> true
      | _ -> false)
  | "$gt" | "$gte" | "$lt" | "$lte" -> (
      match field_val with
      | Some fv -> (
          match bson_cmp fv v with
          | Some c -> (
              match op with
              | "$gt" -> c > 0
              | "$gte" -> c >= 0
              | "$lt" -> c < 0
              | _ -> c <= 0)
          | None -> false)
      | None -> false)
  | "$all" -> (
      match (field_val, v) with
      | Some (Array elems), Array wanted ->
          List.for_all (fun w -> List.exists (fun e -> e = w) elems) wanted
      | _ -> false)
  | "$size" -> (
      match (field_val, v) with
      | Some (Array elems), Int n -> List.length elems = n
      | _ -> false)
  | "$elemMatch" -> (
      match field_val with
      | Some (Array elems) -> List.exists (fun e -> doc_matches v e) elems
      | _ -> false)
  | "$not" -> not (matches_cond field_val v)
  | "$type" -> (
      match field_val with
      | Some fv -> ( match v with String t -> type_name fv = t | _ -> true)
      | None -> false)
  | "$mod" -> (
      match (field_val, v) with
      | Some fv, Array [ d; r ] -> (
          match (bson_num fv, bson_num d, bson_num r) with
          | Some x, Some dv, Some rv when dv <> 0. ->
              Float.rem (Float.of_int (int_of_float x)) dv = rv
          | _ -> false)
      | _ -> false)
  | _ -> true (* unknown operator: never wrongly hide a document *)

and matches_cond (field_val : Bson.t option) (cond : Bson.t) : bool =
  match cond with
  | Document kvs
    when List.exists (fun (k, _) -> String.length k > 0 && k.[0] = '$') kvs ->
      List.for_all (fun (op, v) -> matches_op field_val op v) kvs
  | _ -> ( match field_val with Some fv -> fv = cond | None -> false)

(* Does [d] satisfy [selector]? Top-level $and/$or/$nor, dotted paths, implicit
   AND over the remaining field conditions. *)
and doc_matches (selector : Bson.t) (d : Bson.t) : bool =
  match selector with
  | Document kvs ->
      List.for_all
        (fun (k, cond) ->
          match k with
          | "$and" -> (
              match cond with
              | Array xs -> List.for_all (fun s -> doc_matches s d) xs
              | _ -> true)
          | "$or" -> (
              match cond with
              | Array xs -> List.exists (fun s -> doc_matches s d) xs
              | _ -> true)
          | "$nor" -> (
              match cond with
              | Array xs -> not (List.exists (fun s -> doc_matches s d) xs)
              | _ -> true)
          | _ when String.length k > 0 && k.[0] = '$' -> true
          | _ -> matches_cond (get_path d k) cond)
        kvs
  | _ -> true

(* Heuristic used by $pull: a document argument is treated as a sub-selector,
   anything else as an equality target. *)
let is_selector_like = function Document _ -> true | _ -> false
